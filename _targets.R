library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("paws", "tidyverse", "arrow", "DBI", "duckdb"),
  repository = "aws",
  repository_meta = "aws",
  format = "feather",
  resources = tar_resources(
    aws = tar_resources_aws(
      endpoint = Sys.getenv("S3_ENDPOINT"),
      bucket = "scoavoux",
      prefix = "attachments_playlists"
    )
  )
)

tar_source("R")

# Parameters ------
# Right-censoring at the end of the observation window (replays after a like,
# like at first sight): horizons in months, and the variant reported as the
# main estimate (see summarise_ltfs_censoring()). PI to confirm.
censoring_horizons     <- c(6, 12)
censoring_main_variant <- "fixed_horizon_12m"
# Genre diversity of playlists: size window and minimum share of tracks with a genre
genre_div_min_tracks   <- 10L
genre_div_max_tracks   <- 200L
genre_div_min_coverage <- 0.8
# Classifier diagnostics: gold standards and model annotations (see
# R/classifier_diagnostics.R). Every (model, gold set) pair of the same domain
# is evaluated. To add a model: add an entry to classifier_models (e.g. a local
# directory of per-category title CSVs, type "local_title_csv_dir"); to add a
# gold standard (e.g. hand-coded editorial titles): add an entry to gold_sets
# (long CSV: title, cat, valeur).
gold_sets <- list(
  users_handcoded = list(file = "data/groundtruth_reconciliation.csv", domain = "users")
  # editorial_handcoded = list(file = "../editorial_annotation/gold/editorial_gold.csv", domain = "editorial")
)
classifier_models <- list(
  `gpt-5.1` = list(type = "s3_title_csv", path = "records_w3/playlists_annotations/", domain = "users"),
  `gpt-5.1 editorial` = list(type = "local_wide_csv", 
                             path = "../editorial_annotation/csv/editorial_annotated.csv", domain = "editorial")
)
# Detailed play context (R/analyse_like_dynamics.R). Column names in the RAW
# stream files are UNKNOWN (best guesses; PI to confirm): for each role, the
# first candidate present in a file is used, and make_streams_ctx_file() stops
# with the list of available columns if none is. Check first with
# tar_make(streams_schema) (parquet footers only) then tar_read(streams_schema).
stream_context_cols <- list(
  detail = c("context_name", "context_type", "context", "context_detail", "listen_context"),
  id     = c("context_id", "context_item_id")
)
# Source of a play from the detailed context (regular expressions, case
# insensitive, on the detailed context value; see assign_context_sources()).
# context_4 = reco_algo / edito has priority over the algorithmic / editorial
# rules. Own playlists and the favourites playlist are found by context_id.
# CHECK output/context_detail_mapping.csv after the first run and adjust.
context_source_rules <- list(
  favourites   = "loved|favou?rite|coups?.?de.?c|my_?likes",
  algorithmic  = "flow|mix|radio|autoplay|smart|reco|discover|similar|channel|daily|release_radar|personali|after_?track",
  search       = "search",
  album_artist = "album|artist|discograph|top_?tracks?",
  editorial    = "editor|chart|selection|deezer_playlist"
)
# NULL: a context_id equal to one of the user's playlists counts as a play of
# that playlist whatever the detailed context; or a regex the detailed context
# must match (e.g. "playlist|loved|favo") to guard against id collisions.
own_playlist_detail_pattern <- NULL
context_detail_max_levels   <- 40L   # detailed contexts kept as such in the tables, the rest lumped
hazard_max_rank             <- 20L   # like hazard by play rank 1..20, then 21+
# Archiving regimes (R/analyse_regimes.R): EXPLORATORY, thresholds to discuss.
regime_params <- list(
  t1 = 0.2,              # flow: own-library share (favourites + own playlists) of plays < t1
  t0 = NA,               # ... or likes per 1,000 plays < t0 (NA = not used)
  t2 = 0.7,              # big playlist: favourites share of own-playlist plays >= t2 ...
  eff_big = 2,           # ... or effective number of playlists played < eff_big
  eff_dist = 3,          # distillation: effective number of playlists played >= eff_dist ...
  n_pl_dist = 3,         # ... and/or >= n_pl_dist own playlists with >= N plays
  dist_rule = "and",     # "and", "or", "eff" (eff_dist only), "npl" (n_pl_dist only)
  n_min_plays_pl = 20,   # N, whole window
  n_min_plays_pl_year = 10, # N, per calendar year
  min_plays = 1000,      # users with fewer plays (whole window) are "insufficient_data"
  min_plays_year = 200,  # same, per year
  k = 3:5                # clusters for the data-driven check
)
# Hook: CSV (hashed_id, regime_qual) mapping interviewees to panel ids, if it
# exists (unknown). Then uncomment the target regimes_interview_check.
interviewee_map_file <- NULL
dir.create("output", showWarnings = FALSE)
dir.create("data/temp", recursive = TRUE, showWarnings = FALSE)

list(
  # User data ------
  tar_target(users,                make_survey_data()),
  # ISEI: frozen snapshot (make_isei_data() needs an interactive Google login).
  # Re-read every run (cheap); downstream only reruns if the snapshot changed.
  tar_target(isei,                 read_isei_snapshot(), cue = tar_cue(mode = "always")),
  # Not used downstream:
  tar_target(favorites,            make_favorites_data(users = users)),
  tar_target(volume_play_files,    list_volume_play_files(), format = "rds"),
  tar_target(volume_play,          make_volume_play_data(volume_play_files$Key)),

  # Playlists data
  tar_target(playlists_raw,        make_playlist_data(users = users)),
  tar_target(unique_titles,        make_playlists_unique_titles(playlists = playlists_raw), format = "file", repository = "local"),
  # Not used downstream:
  tar_target(tracklists,           make_tracklist_data(playlists = playlists_raw)),
  # Listing (with ETags) is re-run every time, so new or modified annotation
  # files trigger a rebuild of `playlists`.
  tar_target(playlists_annotation_data_files, list_playlists_annotation_data_files(),
             format = "rds", cue = tar_cue(mode = "always")),
  tar_target(playlists,            make_playlists_annotation_data(playlists_annotation_data_files, playlists_raw)),

  ## Streaming data ------
  tar_target(streaming_data_files, list_streaming_data_files(), format = "rds"),
  # One DuckDB pass over the 19 files on S3 -> local parquet (respondents only)
  tar_target(streams_users_file,   make_streams_users_file(streaming_data_files, users),
             format = "file", repository = "local"),

  # Like at first sight ------
  tar_target(favorites_replayed_file,     make_favorites_replayed_file(streams_users_file, "fixed",
                                                                       horizons_months = censoring_horizons),
             format = "file", repository = "local"),
  tar_target(favorites_replayed_file_old, make_favorites_replayed_file(streams_users_file, "old",
                                                                       horizons_months = censoring_horizons),
             format = "file", repository = "local"),
  # (~65M user x song rows: kept as a local parquet directory, not loaded in R)
  tar_target(ltfs_summary,         summarise_like_at_first_sight(c(fixed = favorites_replayed_file, 
                                                                   old = favorites_replayed_file_old)),
             repository = "local", format = "file"),
  tar_target(gg_ltfsight,          plot_like_at_first_sight(favorites_replayed_file, .what = "raw"), 
             repository = "local", format = "file"),
  tar_target(gg_ltfsight_zoomed,   plot_like_at_first_sight(favorites_replayed_file, .what = "raw", .zoomed = TRUE), 
             repository = "local", format = "file"),
  tar_target(gg_ltfsight_zoomed_old, plot_like_at_first_sight(favorites_replayed_file_old, .what = "raw", .zoomed = TRUE,
                                                              .suffix = "_oldlogic"), 
             repository = "local", format = "file"),
  tar_target(gg_ltfsight_dev,      plot_like_at_first_sight(favorites_replayed_file, .what = "by_device"),
             repository = "local", format = "file"),
  tar_target(gg_ltfsight_dev_old,  plot_like_at_first_sight(favorites_replayed_file_old, .what = "by_device",
                                                            .suffix = "_oldlogic"),
             repository = "local", format = "file"),
  tar_target(gg_replay_aft_loved,  plot_replay_after_loved(favorites_replayed_file),
             repository = "local", format = "file"),
  # Observation window (global end = export date) and plays by context
  tar_target(streams_overview,     make_streams_overview(streams_users_file)),
  # Right-censoring sensitivity: main estimate + variants (also writes
  # output/like_at_first_sight_censoring.csv)
  tar_target(ltfs_censoring,       summarise_ltfs_censoring(favorites_replayed_file, 
                                                            get_window_end(streams_overview),
                                                            horizons_months = censoring_horizons,
                                                            main_variant = censoring_main_variant)),
  # Context of plays before/after the like ("the song leaves the algorithm")
  tar_target(context_around_like,  make_context_around_like(streams_users_file)),
  tar_target(gg_context_around_like, plot_context_around_like(context_around_like),
             repository = "local", format = "file"),
  
  
  # Playlist use ------
  tar_target(users_playlists,       make_users_playlists_data(users, playlists, isei, volume_play)),
  tar_target(gg_noplaylists,        plot_noplaylists(users_playlists),
                                    repository = "local", format = "file"),
  tar_target(gg_nopl_socdem_ridge,  plot_noplaylists_socdem(users_playlists),
                                    repository = "local", format = "file"),
  tar_target(gg_nopl_socdem_boxpl,  plot_noplaylists_socdem(users_playlists, .type = "boxplot"),
                                    repository = "local", format = "file"),
  tar_target(gg_nopl_poisson,       plot_noplaylists_poisson(users_playlists),
                                    repository = "local", format = "file"),
  tar_target(gg_nopl_negbin,        plot_noplaylists_poisson(users_playlists, family = "negbin"),
                                    repository = "local", format = "file"),

  # Playlists cat ------
  tar_target(gg_pl_annotations,     plot_pl_annotations(users_playlists),
                                    repository = "local", format = "file"),
  tar_target(gg_pl_ann_socdem,      plot_pl_annotations_bysocdem(users_playlists),
                                    repository = "local", format = "file"),
  tar_target(gg_typepl_poisson,     plot_typeplaylists_poisson(users_playlists),
                                    repository = "local", format = "file"),
  tar_target(gg_typepl_negbin,      plot_typeplaylists_poisson(users_playlists, family = "negbin"),
                                    repository = "local", format = "file"),
  tar_target(tbl_poisson_negbin,    compare_poisson_negbin(users_playlists),
                                    repository = "local", format = "file"),
  tar_target(playlists_socdem,      summarise_playlists_by_socdem(users_playlists)),

  # Classifier quality (R/classifier_diagnostics.R) ------
  tar_target(gold_files,            purrr::map_chr(gold_sets, "file"),
                                    format = "file", repository = "local"),
  tar_target(gold_standards,        { gold_files; read_gold_standards(gold_files, gold_sets) }),
  # local annotation files of the models (tracked so that changes trigger a rebuild)
  tar_target(classifier_local_files, unlist(purrr::map(Filter(function(m) m$type != "s3_title_csv", classifier_models),
                                                       function(m) if(dir.exists(m$path)) list.files(m$path, full.names = TRUE) else m$path)),
                                    format = "file", repository = "local"),
  tar_target(classifier_annotations, { classifier_local_files
                                       read_model_annotations(classifier_models, playlists_annotation_data_files) }),
  tar_target(classifier_diagnostics, compute_classifier_diagnostics(classifier_annotations, gold_standards)),
  tar_target(classifier_diagnostics_legacy_file, "data/diagnostics_gpt5.1.csv",
                                    format = "file", repository = "local"),
  tar_target(classifier_diagnostics_legacy, read_classifier_diagnostics_legacy(classifier_diagnostics_legacy_file)),
  tar_target(groundtruth_file,      "data/groundtruth_reconciliation.csv",
                                    format = "file", repository = "local"),
  tar_target(groundtruth_summary,   summarise_groundtruth(groundtruth_file)),

  # Genre diversity of playlists by type ------
  # DuckDB: playlists -> tracks -> Deezer album genre (artist genre fallback),
  # local parquet in data/temp/playlist_genres/
  tar_target(playlist_genre_counts_dir, make_playlist_genre_counts(playlists),
                                    format = "file", repository = "local"),
  tar_target(playlist_genre_diversity, compute_playlist_genre_diversity(playlist_genre_counts_dir)),
  tar_target(genre_div,             summarise_genre_diversity_by_type(playlist_genre_diversity, playlists,
                                                                      min_tracks = genre_div_min_tracks,
                                                                      max_tracks = genre_div_max_tracks,
                                                                      min_coverage = genre_div_min_coverage),
                                    format = "rds"),
  tar_target(gg_genre_div,          plot_genre_diversity_by_type(genre_div),
                                    repository = "local", format = "file"),

  # Editorial playlists (local files, outside code/) ------
  tar_target(editorial_raw_file,    "../data/deezer_editorial_playlists.csv",
                                    format = "file", repository = "local"),
  tar_target(editorial_annotated_file, "../editorial_annotation/csv/editorial_annotated.csv",
                                    format = "file", repository = "local"),
  tar_target(editorial,             read_editorial_annotated(editorial_annotated_file)),
  tar_target(editorial_accounts,    count_editorial_accounts(editorial_raw_file)),
  tar_target(editorial_shares,      compute_editorial_shares(editorial)),
  tar_target(editorial_misc,        summarise_editorial_misc(editorial)),
  tar_target(editorial_vs_users,    compare_editorial_users(editorial_shares, desc_stats)),
  tar_target(gg_editorial_vs_users, plot_editorial_vs_users(editorial_vs_users),
                                    repository = "local", format = "file"),


  # Supplementary analysis ------
  tar_target(desc_stats,            compute_descriptive_stats(playlists, users, users_playlists),
                                    format = "rds"),
  tar_target(tbl_supp_desc,         write_descriptive_stats(desc_stats),
                                    repository = "local", format = "file"),

  # Detailed play context (R/analyse_like_dynamics.R) ------
  # Schema of the raw stream files (parquet footers only: cheap)
  tar_target(streams_schema,        make_streams_schema(streaming_data_files), format = "rds", repository = "local"),
  # Second DuckDB pass over the 19 files on S3 (+ detailed context, context_id)
  tar_target(streams_ctx_file,      make_streams_ctx_file(streaming_data_files, streams_users_file, streams_schema,
                                                          stream_context_cols),
                                    format = "file", repository = "local"),
  # (context_4, detail) -> source; also writes output/context_detail_mapping.csv (CHECK IT)
  tar_target(context_mapping,       make_context_mapping(streams_ctx_file, playlists_raw, context_source_rules,
                                                         own_playlist_detail_pattern, context_detail_max_levels),
                                    repository = "local"),

  # Dynamics of the like: selection-artifact check (A) and sources of replays (B)
  tar_target(like_dynamics,         make_like_dynamics(streams_ctx_file, context_mapping, playlists_raw,
                                                       get_window_end(streams_overview),
                                                       horizons_months = censoring_horizons,
                                                       max_rank = hazard_max_rank),
                                    format = "rds", repository = "local"),
  tar_target(like_hazard,           summarise_like_hazard(like_dynamics), repository = "local"),
  tar_target(gg_like_hazard,        plot_like_hazard(like_hazard, censoring_main_variant),
                                    repository = "local", format = "file"),
  tar_target(gg_like_hazard_source, plot_like_hazard(like_hazard, censoring_main_variant, dim = "source",
                                                     filename = "output/like_hazard_by_source.png"),
                                    repository = "local", format = "file"),
  tar_target(never_liked_survival,  summarise_never_liked_survival(like_dynamics), repository = "local"),
  tar_target(first_play_like_rates, summarise_first_play_like_rates(like_dynamics), repository = "local"),
  tar_target(replays_by_source,     summarise_replays_by_source(like_dynamics), repository = "local"),
  tar_target(gg_replays_by_source,  plot_replays_by_source(replays_by_source, censoring_main_variant),
                                    repository = "local", format = "file"),
  tar_target(like_dynamics_numbers, misc_like_dynamics(like_hazard, never_liked_survival, first_play_like_rates,
                                                       replays_by_source, context_mapping,
                                                       main_variant = censoring_main_variant),
                                    repository = "local"),

  # Archiving regimes, EXPLORATORY (R/analyse_regimes.R) (C) ------
  tar_target(playlist_membership_file, make_playlist_membership_file(playlists_raw),
                                    format = "file", repository = "local"),
  tar_target(user_play_profiles,    make_user_play_profiles(streams_ctx_file, context_mapping, playlists_raw,
                                                            playlist_membership_file),
                                    format = "rds", repository = "local"),
  # user x period (whole window "all" + calendar years): features + rule-based regime
  tar_target(user_regimes,          make_user_regimes(user_play_profiles, playlists_raw, regime_params),
                                    format = "rds", repository = "local"),
  tar_target(regimes_clusters,      cluster_regimes(user_regimes, ks = regime_params$k),
                                    format = "rds", repository = "local"),
  tar_target(regimes_summary,       summarise_regimes(user_regimes, regimes_clusters, users_playlists),
                                    format = "rds", repository = "local"),
  tar_target(gg_regimes_features,   plot_regimes_features(user_regimes),
                                    repository = "local", format = "file"),
  tar_target(regimes_numbers,       misc_regimes(regimes_summary, user_regimes, regime_params),
                                    repository = "local"),
  # Validation hook (needs interviewee_map_file, see above):
  # tar_target(interviewee_map,       interviewee_map_file, format = "file", repository = "local"),
  # tar_target(regimes_interview_check, validate_regimes_interviews(user_regimes, interviewee_map),
  #                                   repository = "local"),

  # Every number quoted in the papers/slides (notes/numbers_provenance.md) ------
  tar_target(misc_numbers,          make_misc_numbers(
                                      desc_stats = desc_stats,
                                      ltfs_summary_file = ltfs_summary,
                                      ltfs_censoring = ltfs_censoring,
                                      streams_overview = streams_overview,
                                      context_around_like = context_around_like,
                                      poisson_negbin_files = tbl_poisson_negbin,
                                      classifier_diagnostics = classifier_diagnostics,
                                      classifier_diagnostics_legacy = classifier_diagnostics_legacy,
                                      groundtruth_summary = groundtruth_summary,
                                      editorial_shares = editorial_shares,
                                      editorial_accounts = editorial_accounts,
                                      editorial_misc = editorial_misc,
                                      genre_div = genre_div,
                                      playlists_socdem = playlists_socdem,
                                      like_dynamics_numbers = like_dynamics_numbers,
                                      regimes_numbers = regimes_numbers)),
  tar_target(misc_numbers_csv,      write_misc_numbers(misc_numbers),
                                    repository = "local", format = "file")

)
