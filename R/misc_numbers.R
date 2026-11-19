# Classifier diagnostics: see R/classifier_diagnostics.R.

# Size of the hand-coded ground truth (long file: title, cat, valeur). The test
# set used for the diagnostics is the titles coded on every category
# (test_annotation_strategy_ellmer.R keeps titles with non-missing "person").
summarise_groundtruth <- function(file){
  g <- readr::read_csv(file, show_col_types = FALSE)
  w <- tidyr::pivot_wider(g, names_from = cat, values_from = valeur, values_fn = dplyr::first)
  tibble(name = c("groundtruth_n_titles", "groundtruth_n_titles_testset", "groundtruth_n_codes"),
         value = c(n_distinct(g$title), sum(!is.na(w$person)), nrow(g)),
         description = c("Hand-coded ground truth: number of distinct titles",
                         "Hand-coded ground truth: titles coded on all categories (test set of the F1 table)",
                         "Hand-coded ground truth: number of (title, category) codes"))
}

# Misc numbers ------
# One tidy table holding every number quoted in the papers/slides that comes
# from the data: name / value / description / source (target). See
# notes/numbers_provenance.md for the mapping from quoted numbers to names.
# Names are built programmatically from the source tables, so new rows in a
# source table show up here automatically.
slugify <- function(x){
  x %>% tolower() %>%
    str_replace_all(">=", "ge") %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_remove_all("^_|_$")
}

subset_slug <- c("all songs" = "all", "3 contexts (as in figures)" = "3ctx",
                 "first play: organic" = "organic", "first play: editorial" = "edito",
                 "first play: algorithmic" = "algo")

make_misc_numbers <- function(desc_stats, ltfs_summary_file, ltfs_censoring, streams_overview,
                              context_around_like, poisson_negbin_files, classifier_diagnostics,
                              classifier_diagnostics_legacy,
                              groundtruth_summary, editorial_shares, editorial_accounts, editorial_misc,
                              genre_div, playlists_socdem,
                              like_dynamics_numbers = NULL, regimes_numbers = NULL){
  out <- list()
  add <- function(df, source) out[[length(out) + 1]] <<-
    mutate(df, name = as.character(name), value = as.numeric(value), description = as.character(description),
           source = source) %>% select(name, value, description, source)

  # Descriptive statistics (respondents, playlists, titles, per-user counts)
  add(desc_stats$res %>% transmute(name = paste0("desc_", slugify(Var)), value = Val, description = Var),
      "desc_stats$res")
  add(tibble(name = "desc_mean_labels_per_annotated_playlist", value = desc_stats$mean_labels,
             description = "Mean number of labels per annotated personal playlist"), "desc_stats$mean_labels")
  cats <- desc_stats$cats %>% mutate(slug = slugify(category))
  add(bind_rows(
    transmute(cats, name = paste0("cat_share_playlists_", slug), value = share_of_playlists,
              description = str_glue("Share of annotated personal playlists with label: {category}")),
    transmute(cats, name = paste0("cat_n_playlists_", slug), value = n_playlists_with_label,
              description = str_glue("Number of annotated personal playlists with label: {category}")),
    transmute(filter(cats, !is.na(peruser_avg_share_of_labels)), name = paste0("cat_peruser_avg_share_", slug),
              value = peruser_avg_share_of_labels,
              description = str_glue("Per-user average share of labels (plot_pl_annotations metric): {category}"))),
    "desc_stats$cats")

  # Like at first sight, no right-censoring correction (both left-censoring logics)
  ls <- readr::read_csv(ltfs_summary_file[1], show_col_types = FALSE) %>%
    mutate(sub = subset_slug[subset]) %>%
    pivot_longer(-c(logic, subset, sub), names_to = "metric")
  add(transmute(ls, name = str_glue("ltfs_{logic}_{sub}_{metric}"), value,
                description = str_glue("{metric}; {subset}; left-censoring logic {logic}; no right-censoring correction")),
      "ltfs_summary (output/like_at_first_sight_summary.csv)")
  w <- ls %>% filter(sub == "all", metric == "n_user_songs") %>% select(logic, value) %>% deframe()
  add(tibble(name = c("ltfs_leftcens_pairs_dropped", "ltfs_leftcens_share_pairs_dropped"),
             value = c(w[["old"]] - w[["fixed"]], 1 - w[["fixed"]] / w[["old"]]),
             description = c("User x song pairs dropped by the left-censoring fix (old - fixed)",
                             "Share of user x song pairs dropped by the left-censoring fix")),
      "ltfs_summary")

  # Right-censoring variants; "ltfs_main_*" = the variant flagged main
  lc <- ltfs_censoring %>%
    mutate(sub = subset_slug[subset]) %>%
    select(-horizon_months, -window_end) %>%
    pivot_longer(-c(variant, subset, sub, main), names_to = "metric") %>%
    filter(!is.na(value))
  add(transmute(lc, name = str_glue("ltfs_cens_{variant}_{sub}_{metric}"), value,
                description = str_glue("{metric}; {subset}; right-censoring variant {variant}; left-censoring fixed")),
      "ltfs_censoring (output/like_at_first_sight_censoring.csv)")
  add(transmute(filter(lc, main), name = str_glue("ltfs_main_{sub}_{metric}"), value,
                description = str_glue("MAIN ESTIMATE ({variant}): {metric}; {subset}")),
      "ltfs_censoring (main variant)")

  # Observation window, plays by context
  add(transmute(streams_overview, name = paste0("streams_", name), value,
                description = paste0("Stream extract (respondents, songs, 2018+): ", name,
                                     ifelse(is.na(label), "", paste0(" = ", label)))),
      "streams_overview")

  # Context of plays around the like
  cal <- context_around_like %>%
    mutate(window = case_when(rel_rank < 0 ~ "before", rel_rank == 0 ~ "at", TRUE ~ "after")) %>%
    group_by(like_type, window, context_4) %>% summarize(n = sum(n), .groups = "drop_last") %>%
    mutate(value = n / sum(n)) %>% ungroup()
  cal1 <- context_around_like %>% filter(rel_rank %in% c(-1, 1))
  add(bind_rows(
    transmute(cal, name = str_glue("like_ctx_{slugify(like_type)}_{window}_{slugify(context_4)}"), value,
              description = str_glue("Share of plays in context {context_4}, {window} the like (ranks -10..-1 / 0 / 1..10), likes {like_type}")),
    transmute(cal1, name = str_glue("like_ctx_{slugify(like_type)}_rel{ifelse(rel_rank < 0, 'm1', 'p1')}_{slugify(context_4)}"),
              value = share,
              description = str_glue("Share of plays in context {context_4} at relative rank {rel_rank} from the like, likes {like_type}")),
    context_around_like %>% filter(like_type == "later", rel_rank < 0, context_4 == "reco_algo") %>%
      summarize(value = max(share)) %>%
      transmute(name = "like_ctx_later_max_reco_share_before_like", value,
                description = "Max share of algorithmic plays at any rank before the like (likes after play 1)")),
    "context_around_like")

  # Poisson vs negative binomial
  mods <- readr::read_csv(poisson_negbin_files[1], show_col_types = FALSE)
  coefs <- readr::read_csv(poisson_negbin_files[2], show_col_types = FALSE)
  add(mods %>% pivot_longer(-outcome, names_to = "metric") %>%
        transmute(name = str_glue("model_{outcome}_{metric}"), value,
                  description = str_glue("Count model of {outcome}: {metric}")),
      "tbl_poisson_negbin (output/poisson_vs_negbin_models.csv)")
  add(coefs %>% select(outcome, term, est_poisson, p_poisson, est_negbin, p_negbin) %>%
        pivot_longer(-c(outcome, term), names_to = "metric") %>%
        transmute(name = str_glue("coef_{outcome}_{slugify(term)}_{metric}"), value,
                  description = str_glue("Count model of {outcome}, term {term}: {metric}")),
      "tbl_poisson_negbin (output/poisson_vs_negbin_coefficients.csv)")
  vol <- coefs %>% filter(outcome == "n_playlists", term == "log(n_play_2023)")
  add(tibble(name = c("derived_nplaylists_pct_per_10pct_volume_negbin", "derived_nplaylists_pct_per_10pct_volume_poisson"),
             value = c(1.1^vol$est_negbin - 1, 1.1^vol$est_poisson - 1),
             description = c("Number of playlists: % change for +10% plays (1.1^beta - 1), NB",
                             "Number of playlists: % change for +10% plays (1.1^beta - 1), Poisson")),
      "tbl_poisson_negbin (derived)")
  add(tibble(name = c("models_n_coefficients", "models_n_sign_changes", "models_n_significance_changes",
                      "models_n_sig_poisson_to_nonsig_negbin"),
             value = c(nrow(coefs), sum(coefs$sign_change), sum(coefs$significance_change),
                       sum(coefs$sig_poisson & !coefs$sig_negbin)),
             description = c("Poisson vs NB: number of non-intercept coefficients compared",
                             "Poisson vs NB: sign changes", "Poisson vs NB: significance changes (p < .05)",
                             "Poisson vs NB: significant under Poisson, not under NB")),
      "tbl_poisson_negbin")

  # Classifier: pipeline diagnostics, by model x gold set x category
  cdg <- classifier_diagnostics %>% 
    mutate(slug = str_glue("{slugify(model)}__{slugify(gold_set)}"))
  add(cdg %>% pivot_longer(c(n_titles, n_gold_positive, precision, recall, f1, accuracy, coverage), names_to = "metric") %>% 
        transmute(name = str_glue("classifier_{slug}_{category}_{metric}"), value,
                  description = str_glue("Classifier {model} vs gold standard {gold_set}, {category}: {metric}")),
      "classifier_diagnostics (output/classifier_diagnostics.csv)")
  add(cdg %>% group_by(slug, model, gold_set) %>% 
        summarize(f1_min = min(f1), f1_max = max(f1), f1_mean = mean(f1), .groups = "drop") %>% 
        pivot_longer(starts_with("f1_"), names_to = "metric") %>% 
        transmute(name = str_glue("classifier_{slug}_{metric}"), value,
                  description = str_glue("Classifier {model} vs {gold_set}: {metric} over categories")),
      "classifier_diagnostics")
  # Legacy frozen test-run table (the F1 table in deck B comes from it)
  add(classifier_diagnostics_legacy %>% pivot_longer(-category, names_to = "metric") %>% 
        transmute(name = str_glue("classifier_legacy_{category}_{metric}"), value,
                  description = str_glue("LEGACY frozen test run (data/diagnostics_gpt5.1.csv), {category}: {metric}")),
      "classifier_diagnostics_legacy (data/diagnostics_gpt5.1.csv)")
  add(groundtruth_summary, "groundtruth_summary (data/groundtruth_reconciliation.csv)")

  # Editorial playlists
  add(editorial_accounts %>% transmute(name = paste0("editorial_n_", slugify(account_type)), value = n_playlists,
                                       description = str_glue("Scraped Deezer editorial playlists, account type {account_type}")),
      "editorial_accounts (../data/deezer_editorial_playlists.csv)")
  es <- editorial_shares %>% mutate(slug = slugify(category))
  add(bind_rows(
    es %>% distinct(group, top_coding, n_playlists, total_fans, mean_labels) %>% filter(top_coding == "as_is") %>%
      pivot_longer(c(n_playlists, total_fans, mean_labels), names_to = "metric") %>%
      transmute(name = str_glue("editorial_{group}_{metric}"), value,
                description = str_glue("Editorial playlists, {group}: {metric}")),
    es %>% pivot_longer(c(share_raw, share_fans_weighted, n_with_label), names_to = "metric") %>%
      filter(!is.na(value)) %>%
      transmute(name = str_glue("editorial_{group}_{top_coding}_{slug}_{metric}"), value,
                description = str_glue("Editorial playlists, {group}, top coded {top_coding}: {category}, {metric}"))),
    "editorial_shares (data/editorial/editorial_annotated.csv, from S3)")
  add(editorial_misc, "editorial_misc")

  # Genre diversity by playlist type, one block per genre definition
  # (genre_def: main = song-level Deezer item genre; song_only; style_tags;
  # deezer_album_old = previous album-genre version). Names of the main
  # definition are unchanged (genrediv_...); the others are prefixed
  # (genrediv_<genre_def>_...).
  gs <- genre_div$summary
  gpre <- function(gd) ifelse(gd == "main", "genrediv", paste0("genrediv_", gd))
  add(gs %>% select(-min_tracks, -max_tracks, -min_coverage) %>%
        pivot_longer(-c(genre_def, subset, category), names_to = "metric") %>%
        filter(!is.na(value)) %>%
        transmute(name = str_glue("{gpre(genre_def)}_{slugify(subset)}_{slugify(category)}_{metric}"), value,
                  description = str_glue("Genre diversity of personal playlists, genre definition {genre_def} ({genre_div$params$min_tracks}-{genre_div$params$max_tracks} tracks, coverage >= {genre_div$params$min_coverage}); {subset}; {category}: {metric}")),
      "genre_div$summary (output/genre_diversity_by_type.csv)")
  add(genre_div$models %>% filter(str_detect(term, "^category")) %>%
        pivot_longer(c(estimate, p), names_to = "metric") %>%
        transmute(name = str_glue("{gpre(genre_def)}_model_{outcome}_{slugify(term)}_{metric}"), value,
                  description = str_glue("OLS of {outcome} on type (ref. genre) + log(n_tracks), single-label playlists, genre definition {genre_def}: {term}, {metric}")),
      "genre_div$models (output/genre_diversity_by_type_models.csv)")

  # Who makes playlists
  add(playlists_socdem %>%
        transmute(name = str_glue("socdem_{slugify(variable)}_{ifelse(is.na(group), slugify(coalesce(term, '')), slugify(group))}_{statistic}"),
                  value, description = str_glue("{variable} {coalesce(group, term, '')}: {statistic}")),
      "playlists_socdem")

  # Dynamics of the like, sources of replays (R/analyse_like_dynamics.R)
  if(!is.null(like_dynamics_numbers))
    add(like_dynamics_numbers, "like_dynamics_numbers (like_hazard, never_liked_survival, first_play_like_rates, replays_by_source, context_mapping)")
  # Archiving regimes, EXPLORATORY (R/analyse_regimes.R)
  if(!is.null(regimes_numbers))
    add(regimes_numbers, "regimes_numbers (user_regimes, regimes_summary; EXPLORATORY)")

  res <- bind_rows(out)
  dups <- res$name[duplicated(res$name)]
  if(length(dups)) warning("Duplicated misc number names: ", paste(head(unique(dups), 10), collapse = ", "))
  res
}

write_misc_numbers <- function(misc_numbers, filename = "output/misc_numbers.csv"){
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(misc_numbers, filename)
  filename
}
