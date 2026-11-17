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
  tar_target(favorites_replayed_file,     make_favorites_replayed_file(streams_users_file, "fixed"),
             format = "file", repository = "local"),
  tar_target(favorites_replayed_file_old, make_favorites_replayed_file(streams_users_file, "old"),
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


  # Supplementary analysis ------
  tar_target(tbl_supp_desc,         table_descriptive_stats(playlists, users, users_playlists),
                                    repository = "local", format = "file")

)
