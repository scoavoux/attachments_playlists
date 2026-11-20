# Inscription (sorting) regimes — EXPLORATORY ------
#
# Added 2026-11-20. A SECOND typology, next to the play-based archiving regimes
# (R/analyse_regimes.R, untouched). The qualitative typology (flow cocoon /
# one big playlist / distillation) is about how people SORT music; the
# play-based regimes are about where their plays come from. This module
# classifies users from what they inscribe and how they split it up, so that
# the two can be cross-tabulated (inscription x listening): where they
# disagree is a finding in itself.
#
# Decisions (Samuel, 2026-11-20):
#  - data-driven first: Gaussian mixture (mclust), number of components and
#    covariance model chosen by BIC; no rule-based version for now;
#  - VOLUME MATTERS: counts enter as log(1 + x), never as ratios to activity
#    (number of likes / playlists is what separates the flow cocoon from the
#    others);
#  - saved albums, saved artists and followed playlists are inscription and
#    are used;
#  - no turnover (likes in last 12 months / stock) and no "age of the
#    collection": both mostly measure the length of the subscription;
#  - renewal is a SECOND AXIS, not used in the mixture;
#  - scope: same users as the play-based regimes (whole window, not
#    "insufficient_data", i.e. >= regime_params$min_plays plays).
#
# Data. The snapshots have NO dates: favourites file (item_id, item_type,
# hashed_id; item_type song / album / artist / playlist / show) and playlists
# file (playlist_id, title, tracks, hashed_id). Renewal therefore comes from
# the 2018-01 .. 2023-05 logs, through proxies:
#  - like activity: months with >= 1 like among months with >= 1 play, and
#    distinct songs liked per active month (click_loved);
#  - playlist activation: the first YEAR an own playlist is played (from
#    user_play_profiles$pl_plays) stands for its creation; activations are
#    counted from first_activation_year (2019) on, since every pre-2018
#    playlist would look "created" in 2018; new playlists per active year =
#    activations / (active months from 2019 on / 12);
#  - share of the snapshot's own playlists played since recent_from_year.
# Known limits: playlists deleted before the snapshot are invisible (past
# sorting undercounted); first play is not creation.
#
# Features of the mixture (sorting_params$features), per user:
#  volume   : log1p of liked songs, saved albums, saved artists, followed
#             playlists (favourited playlists that are not the user's own),
#             own playlists (snapshot, excl. the loved-tracks playlist),
#             distinct tracks in own playlists, own playlists with
#             >= min_tracks_list tracks;
#  structure: lists = the favourites (size = liked songs) + each own
#             playlist (size = distinct tracks); share of the largest list
#             in the sum of list sizes; log1p(effective number of lists =
#             exp(Shannon entropy of list sizes)); share of liked songs also
#             in >= 1 own playlist ("redistribution"). For a user with an
#             empty archive the structure features are set to 0.

# Snapshot features (favourites + playlists) ------
# One row per user in scope; reads the favourites file on S3 (respondents in
# scope only) and the local membership parquet (R/analyse_regimes.R).
make_sorting_snapshot <- function(user_regimes, playlists_raw, playlist_membership_file, params,
                                  favorites_key = "records_w3/favorites/RECORDS_hashed_user_favorites.parquet"){
  ids <- sorting_scope_ids(user_regimes)
  con <- duck_connect(memory_limit = "2GB", threads = 2)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "SET http_timeout = 60000; SET http_retries = 5;")
  duckdb::duckdb_register(con, "ids_r", tibble::tibble(hashed_id = ids))
  DBI::dbExecute(con, "CREATE TEMP TABLE ids AS SELECT DISTINCT hashed_id FROM ids_r")
  duckdb::duckdb_register(con, "ownpl_r", user_playlist_ids(playlists_raw))
  DBI::dbExecute(con, "CREATE TEMP TABLE ownpl AS SELECT o.* FROM ownpl_r o SEMI JOIN ids USING (hashed_id)")
  DBI::dbExecute(con, sprintf("
    CREATE TEMP TABLE fav AS
    SELECT f.hashed_id, f.item_type, CAST(f.item_id AS VARCHAR) AS item_id
    FROM read_parquet('%s') f SEMI JOIN ids USING (hashed_id)
    WHERE f.item_type IN ('song', 'album', 'artist', 'playlist')", s3_uri(favorites_key)))
  DBI::dbExecute(con, sprintf("
    CREATE TEMP TABLE m AS
    SELECT m.hashed_id, m.pid, m.is_fav, m.song_id
    FROM read_parquet('%s/*.parquet') m SEMI JOIN ids USING (hashed_id)", playlist_membership_file))
  q <- function(sql) tibble::as_tibble(DBI::dbGetQuery(con, sql))
  fav_counts <- q("
    SELECT f.hashed_id,
      CAST(count(DISTINCT item_id) FILTER (WHERE item_type = 'song') AS DOUBLE) AS n_liked_songs,
      CAST(count(DISTINCT item_id) FILTER (WHERE item_type = 'album') AS DOUBLE) AS n_saved_albums,
      CAST(count(DISTINCT item_id) FILTER (WHERE item_type = 'artist') AS DOUBLE) AS n_saved_artists,
      CAST(count(DISTINCT item_id) FILTER (WHERE item_type = 'playlist' AND o.pid IS NULL) AS DOUBLE) AS n_followed_playlists,
      CAST(count(DISTINCT item_id) FILTER (WHERE item_type = 'playlist' AND o.pid IS NOT NULL) AS DOUBLE) AS n_fav_own_playlists
    FROM fav f LEFT JOIN ownpl o ON f.hashed_id = o.hashed_id AND f.item_id = o.pid
    GROUP BY f.hashed_id")
  # sizes of own playlists (excl. the loved-tracks playlist), one row per playlist
  own_sizes <- q("
    SELECT hashed_id, pid, CAST(count(DISTINCT song_id) AS DOUBLE) AS n_tracks
    FROM m WHERE NOT is_fav GROUP BY hashed_id, pid")
  # union of liked songs and own-playlist tracks; redistribution
  arch <- q("
    WITH fs AS (SELECT DISTINCT hashed_id, TRY_CAST(item_id AS BIGINT) AS song_id FROM fav WHERE item_type = 'song'),
    os AS (SELECT DISTINCT hashed_id, song_id FROM m WHERE NOT is_fav),
    u AS (SELECT hashed_id, song_id, bool_or(src = 'f') AS in_f, bool_or(src = 'o') AS in_o
          FROM (SELECT *, 'f' AS src FROM fs UNION ALL SELECT *, 'o' AS src FROM os) GROUP BY hashed_id, song_id)
    SELECT hashed_id, CAST(count(*) AS DOUBLE) AS n_archive_tracks,
      CAST(count(*) FILTER (WHERE in_o) AS DOUBLE) AS n_tracks_own_pl,
      CAST(count(*) FILTER (WHERE in_f AND in_o) AS DOUBLE) AS n_liked_in_own_pl
    FROM u GROUP BY hashed_id")
  # diagnostic: size of the loved-tracks playlist in the playlists file (should
  # track the number of liked songs in the favourites file)
  fav_pl <- q("SELECT hashed_id, CAST(count(DISTINCT song_id) AS DOUBLE) AS n_fav_pl_tracks
               FROM m WHERE is_fav GROUP BY hashed_id")
  n_own <- playlists_raw %>% filter(!(title %in% favorites_playlists_titles), hashed_id %in% ids) %>%
    distinct(hashed_id, playlist_id) %>% dplyr::count(hashed_id, name = "n_own_playlists")
  struct <- own_sizes %>% group_by(hashed_id) %>%
    summarize(n_pl_ge_min = sum(n_tracks >= params$min_tracks_list), median_own_pl_size = median(n_tracks),
              sizes = list(n_tracks), .groups = "drop")
  tibble::tibble(hashed_id = ids) %>%
    left_join(fav_counts, by = "hashed_id") %>% left_join(n_own, by = "hashed_id") %>%
    left_join(arch, by = "hashed_id") %>% left_join(fav_pl, by = "hashed_id") %>%
    left_join(struct, by = "hashed_id") %>%
    mutate(across(c(n_liked_songs, n_saved_albums, n_saved_artists, n_followed_playlists, n_fav_own_playlists,
                    n_own_playlists, n_archive_tracks, n_tracks_own_pl, n_liked_in_own_pl, n_fav_pl_tracks,
                    n_pl_ge_min), ~coalesce(as.double(.x), 0))) %>%
    mutate(list_stats = purrr::map2(n_liked_songs, sizes, function(nf, s){
      s <- c(nf, if(is.null(s)) numeric() else s); s <- s[s > 0]
      if(!length(s)) return(c(share_largest_list = 0, eff_n_lists = 0))
      p <- s / sum(s)
      c(share_largest_list = max(p), eff_n_lists = exp(-sum(p * log(p))))
    }),
    share_largest_list = purrr::map_dbl(list_stats, "share_largest_list"),
    eff_n_lists = purrr::map_dbl(list_stats, "eff_n_lists"),
    share_liked_in_own_pl = ifelse(n_liked_songs > 0, n_liked_in_own_pl / n_liked_songs, 0)) %>%
    select(-sizes, -list_stats)
}

sorting_scope_ids <- function(user_regimes){
  user_regimes %>% filter(period == "all", regime != "insufficient_data") %>% distinct(hashed_id) %>% pull(hashed_id)
}

# Monthly like activity (logs) ------
# One row per user x month with >= 1 play: plays and distinct songs liked.
# Aggregates in DuckDB over the local extract of the respondents' plays, in
# n_buckets passes over users (hash of hashed_id) to keep memory and spill
# small. Plays and likes are aggregated separately: the distinct count only
# runs on the (few) plays with click_loved = 1. (First version, one pass with
# count(DISTINCT) FILTER over all plays, ran out of temp space on the server.)
make_monthly_like_activity <- function(streams_users_file, user_subset = NULL, n_buckets = 16){
  con <- duck_connect(memory_limit = "2GB", threads = 2)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  sub <- ""
  if(!is.null(user_subset)){
    duckdb::duckdb_register(con, "user_subset", tibble::tibble(hashed_id = user_subset))
    sub <- "AND hashed_id IN (SELECT hashed_id FROM user_subset)"
  }
  month <- "CAST(strftime(make_timestamp(CAST(ts_listen AS BIGINT) * 1000000), '%Y-%m') AS VARCHAR)"
  src <- sprintf("read_parquet('%s/*.parquet')", streams_users_file)
  out <- vector("list", n_buckets)
  for(b in seq_len(n_buckets) - 1){
    w <- sprintf("WHERE hash(hashed_id) %% %d = %d %s", n_buckets, b, sub)
    out[[b + 1]] <- tibble::as_tibble(DBI::dbGetQuery(con, sprintf("
      WITH p AS (SELECT hashed_id, %1$s AS month, CAST(count(*) AS DOUBLE) AS n_plays
                 FROM %2$s %3$s GROUP BY ALL),
      l AS (SELECT hashed_id, month, CAST(count(*) AS DOUBLE) AS n_liked_songs
            FROM (SELECT DISTINCT hashed_id, %1$s AS month, song_id
                  FROM %2$s %3$s AND click_loved = 1)
            GROUP BY ALL)
      SELECT p.hashed_id, p.month, p.n_plays, coalesce(l.n_liked_songs, 0) AS n_liked_songs
      FROM p LEFT JOIN l USING (hashed_id, month)", month, src, w)))
  }
  bind_rows(out)
}

# Renewal features (second axis) ------
compute_sorting_renewal <- function(monthly, play_profiles, snapshot, params){
  ids <- snapshot$hashed_id
  lk <- monthly %>% filter(hashed_id %in% ids) %>%
    mutate(year = as.integer(substr(month, 1, 4))) %>%
    group_by(hashed_id) %>%
    summarize(n_active_months = n(), n_months_with_like = sum(n_liked_songs > 0),
              n_liked_window = sum(n_liked_songs),
              n_active_months_from_act = sum(year >= params$first_activation_year), .groups = "drop") %>%
    mutate(share_months_with_like = n_months_with_like / n_active_months,
           likes_per_active_month = n_liked_window / n_active_months)
  pl <- play_profiles$pl_plays %>% filter(!is_fav, hashed_id %in% ids, n_plays > 0) %>%
    group_by(hashed_id, pid) %>% summarize(first_year = min(year), last_year = max(year), .groups = "drop") %>%
    group_by(hashed_id) %>%
    summarize(n_own_pl_played = n(),
              n_pl_activated = sum(first_year >= params$first_activation_year),
              n_pl_played_recent = sum(last_year >= params$recent_from_year),
              median_pl_lifespan_years = median(last_year - first_year + 1), .groups = "drop")
  snapshot %>% select(hashed_id, n_own_playlists) %>%
    left_join(lk, by = "hashed_id") %>% left_join(pl, by = "hashed_id") %>%
    mutate(across(c(n_own_pl_played, n_pl_activated, n_pl_played_recent), ~coalesce(as.double(.x), 0)),
           new_pl_per_year = ifelse(coalesce(n_active_months_from_act, 0) > 0,
                                    n_pl_activated / (n_active_months_from_act / 12), NA_real_),
           share_own_pl_played_recent = ifelse(n_own_playlists > 0, pmin(n_pl_played_recent / n_own_playlists, 1), NA_real_),
           renewal = case_when(is.na(share_months_with_like) ~ NA_character_,
                               share_months_with_like >= params$renewal_like_months |
                                 coalesce(new_pl_per_year, 0) >= params$renewal_new_pl ~ "active",
                               TRUE ~ "frozen")) %>%
    select(-n_own_playlists)
}

# User table: snapshot + renewal + play-based regime ------
make_user_sorting <- function(snapshot, monthly, play_profiles, user_regimes, params){
  ren <- compute_sorting_renewal(monthly, play_profiles, snapshot, params)
  reg <- user_regimes %>% filter(period == "all") %>%
    transmute(hashed_id, n_plays, play_regime = as.character(regime), play_regime3 = as.character(regime3))
  snapshot %>% left_join(ren, by = "hashed_id") %>% left_join(reg, by = "hashed_id") %>%
    mutate(log_n_liked_songs = log1p(n_liked_songs), log_n_saved_albums = log1p(n_saved_albums),
           log_n_saved_artists = log1p(n_saved_artists), log_n_followed_playlists = log1p(n_followed_playlists),
           log_n_own_playlists = log1p(n_own_playlists), log_n_tracks_own_pl = log1p(n_tracks_own_pl),
           log_n_pl_ge_min = log1p(n_pl_ge_min), log_eff_n_lists = log1p(eff_n_lists),
           log_n_archive_tracks = log1p(n_archive_tracks))
}

# Gaussian mixture ------
# BIC over G x covariance models (mclust); hierarchical initialisation on a
# random subset of hc_subset users (the full hc is O(n^2) in memory); optional
# conjugate prior (priorControl) against degenerate components caused by the
# point masses at 0. Components are relabelled 1..G by increasing mean
# log_n_archive_tracks (1 = smallest archive). Stability: n_boot refits of the
# BIC-best (G, model) on boot_frac subsamples, predicted on all users, ARI
# with the full fit.
fit_sorting_mixture <- function(user_sorting, params){
  if(!requireNamespace("mclust", quietly = TRUE)) stop("Package mclust is required (install.packages('mclust')).")
  # Mclust(x = <mclustBIC>) evaluates calls to mclustBIC unqualified: attach
  suppressPackageStartupMessages(require(mclust))
  X <- scale(as.matrix(user_sorting[, params$features]))
  if(anyNA(X)) stop("NA in the mixture features: ", paste(params$features[colSums(is.na(X)) > 0], collapse = ", "))
  n <- nrow(X)
  set.seed(params$seed)
  init <- if(n > params$hc_subset) list(subset = sample.int(n, params$hc_subset)) else list()
  prior <- if(isTRUE(params$prior)) mclust::priorControl() else NULL
  bic <- mclust::mclustBIC(X, G = params$G, modelNames = params$model_names, prior = prior,
                           initialization = init, verbose = FALSE)
  score <- user_sorting$log_n_archive_tracks
  relabel <- function(cl){
    o <- order(tapply(score, cl, mean)); match(cl, sort(unique(cl))[o])
  }
  # (with x = bic, prior and initialisation are taken from the stored BIC object)
  fit_g <- function(g) mclust::Mclust(X, G = g, x = bic, verbose = FALSE)
  best <- mclust::Mclust(X, x = bic, verbose = FALSE)
  assign <- tibble::tibble(hashed_id = user_sorting$hashed_id,
                           cluster_best = relabel(best$classification),
                           uncertainty_best = best$uncertainty)
  models <- tibble::tibble(solution = "cluster_best", G = best$G, model = best$modelName, bic = best$bic,
                           loglik = best$loglik)
  for(g in setdiff(params$G_profiles, best$G)){
    f <- fit_g(g)
    if(is.null(f)) next
    assign[[paste0("cluster_G", g)]] <- relabel(f$classification)
    models <- bind_rows(models, tibble::tibble(solution = paste0("cluster_G", g), G = f$G, model = f$modelName,
                                               bic = f$bic, loglik = f$loglik))
  }
  ari <- numeric()
  for(b in seq_len(params$n_boot)){
    idx <- sample.int(n, floor(params$boot_frac * n))
    ib <- if(length(idx) > params$hc_subset) list(subset = sample.int(length(idx), params$hc_subset)) else list()
    fb <- tryCatch(mclust::Mclust(X[idx, , drop = FALSE], G = best$G, modelNames = best$modelName, prior = prior,
                                  initialization = ib, verbose = FALSE), error = function(e) NULL)
    if(is.null(fb)) next
    ari[b] <- mclust::adjustedRandIndex(predict(fb, X)$classification, best$classification)
  }
  bic_tbl <- as.data.frame(unclass(bic)[, , drop = FALSE]) %>% tibble::rownames_to_column("G") %>%
    pivot_longer(-G, names_to = "model", values_to = "bic") %>% mutate(G = as.integer(G))
  list(features = params$features, n_users = n, best_G = best$G, best_model = best$modelName,
       bic = bic_tbl, models = models, assign = assign, stability_ari = ari[!is.na(ari)],
       prior = isTRUE(params$prior), hc_subset = params$hc_subset)
}

cramers_v <- function(a, b){
  tab <- table(a, b)
  if(min(dim(tab)) < 2) return(NA_real_)
  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
  unname(sqrt(chi / (sum(tab) * (min(dim(tab)) - 1))))
}

# Summaries ------
# Writes to output/: sorting_bic.csv, sorting_models.csv, sorting_profiles.csv
# (per solution x cluster: n, share, median / mean / quartiles of the raw
# features and renewal features, mean standardised mixture features),
# sorting_x_regimes.csv, sorting_association.csv, sorting_by_socdem.csv,
# sorting_renewal.csv, sorting_stability.csv, sorting_diagnostics.csv.
sorting_profile_vars <- c("n_liked_songs", "n_saved_albums", "n_saved_artists", "n_followed_playlists",
                          "n_own_playlists", "n_tracks_own_pl", "n_archive_tracks", "n_pl_ge_min",
                          "median_own_pl_size", "share_largest_list", "eff_n_lists", "share_liked_in_own_pl",
                          "n_plays", "share_months_with_like", "likes_per_active_month", "new_pl_per_year",
                          "share_own_pl_played_recent", "median_pl_lifespan_years")

summarise_sorting <- function(user_sorting, sorting_mixture, users_playlists, params, dir = "output"){
  out <- function(x) file.path(dir, paste0("sorting_", x, ".csv"))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  a <- sorting_mixture$assign
  cl_cols <- grep("^cluster_", names(a), value = TRUE)
  Z <- as_tibble(scale(as.matrix(user_sorting[, sorting_mixture$features]))) %>%
    rename_with(~paste0("z_", .x))
  d <- bind_cols(user_sorting, Z) %>% left_join(a, by = "hashed_id")
  socdem <- users_playlists %>% select(hashed_id, gender, age_cat, degree, isei_quartile) %>%
    mutate(across(-hashed_id, as.character))
  per_sol <- function(fun) bind_rows(lapply(cl_cols, function(cc) mutate(fun(mutate(d, cluster = .data[[cc]])),
                                                                          solution = cc, .before = 1)))
  profiles <- per_sol(function(x) x %>% group_by(cluster) %>%
    summarize(n_users = n(), mean_uncertainty_best = mean(uncertainty_best),
              share_renewal_active = mean(renewal == "active", na.rm = TRUE),
              across(all_of(sorting_profile_vars),
                     list(median = ~median(.x, na.rm = TRUE), mean = ~mean(.x, na.rm = TRUE),
                          q25 = ~quantile(.x, .25, na.rm = TRUE, names = FALSE),
                          q75 = ~quantile(.x, .75, na.rm = TRUE, names = FALSE)), .names = "{.col}__{.fn}"),
              across(starts_with("z_"), list(mean = ~mean(.x)), .names = "{.col}__{.fn}"),
              .groups = "drop") %>%
    mutate(share = n_users / sum(n_users))) %>%
    pivot_longer(contains("__"), names_to = c("feature", "stat"), names_sep = "__")
  xt <- per_sol(function(x) bind_rows(
    dplyr::count(x, cluster, play_regime = play_regime, name = "n_users") %>% mutate(level = "fine"),
    dplyr::count(x, cluster, play_regime = play_regime3, name = "n_users") %>% mutate(level = "coarse")) %>%
      group_by(level, cluster) %>% mutate(share_of_cluster = n_users / sum(n_users)) %>%
      group_by(level, play_regime) %>% mutate(share_of_play_regime = n_users / sum(n_users)) %>% ungroup())
  assoc <- per_sol(function(x) tibble::tibble(
    level = c("fine", "coarse"),
    cramers_v = c(cramers_v(x$cluster, x$play_regime), cramers_v(x$cluster, x$play_regime3)),
    ari = c(adjusted_rand_index(x$cluster, x$play_regime), adjusted_rand_index(x$cluster, x$play_regime3)),
    n_users = nrow(x)))
  soc <- per_sol(function(x) bind_rows(lapply(c("gender", "age_cat", "degree", "isei_quartile"), function(v)
    x %>% left_join(socdem, by = "hashed_id") %>% filter(!is.na(.data[[v]])) %>%
      dplyr::count(group = .data[[v]], cluster, name = "n_users") %>% group_by(group) %>%
      mutate(share = n_users / sum(n_users)) %>% ungroup() %>% mutate(variable = v, .before = 1))))
  ren <- per_sol(function(x) dplyr::count(x, cluster, renewal, name = "n_users") %>% group_by(cluster) %>%
                   mutate(share_of_cluster = n_users / sum(n_users)) %>% ungroup())
  stab <- tibble::tibble(G = sorting_mixture$best_G, model = sorting_mixture$best_model,
                         n_boot = length(sorting_mixture$stability_ari),
                         ari_min = suppressWarnings(min(sorting_mixture$stability_ari)),
                         ari_median = median(sorting_mixture$stability_ari),
                         ari_mean = mean(sorting_mixture$stability_ari))
  diag <- tibble::tribble(
    ~name, ~value,
    "n_users_in_scope", nrow(user_sorting),
    "n_empty_archive", sum(user_sorting$n_archive_tracks == 0),
    "n_no_own_playlist", sum(user_sorting$n_own_playlists == 0),
    "n_no_liked_song", sum(user_sorting$n_liked_songs == 0),
    "n_with_favourited_own_playlist", sum(user_sorting$n_fav_own_playlists > 0),
    "cor_log_liked_songs_vs_log_loved_tracks_playlist", suppressWarnings(stats::cor(user_sorting$log_n_liked_songs, log1p(user_sorting$n_fav_pl_tracks))),
    "n_renewal_missing", sum(is.na(user_sorting$renewal)),
    "best_G", sorting_mixture$best_G,
    "mixture_prior", as.numeric(sorting_mixture$prior))
  write_csv(sorting_mixture$bic, out("bic"))
  write_csv(sorting_mixture$models, out("models"))
  write_csv(profiles, out("profiles"))
  write_csv(xt, out("x_regimes"))
  write_csv(assoc, out("association"))
  write_csv(soc, out("by_socdem"))
  write_csv(ren, out("renewal"))
  write_csv(stab, out("stability"))
  write_csv(mutate(diag, best_model = sorting_mixture$best_model), out("diagnostics"))
  list(profiles = profiles, crosstab = xt, association = assoc, socdem = soc, renewal = ren, stability = stab,
       diagnostics = diag, models = sorting_mixture$models, best_G = sorting_mixture$best_G,
       best_model = sorting_mixture$best_model)
}

# Plots ------
sorting_feature_labels <- c(z_log_n_liked_songs = "Liked songs", z_log_n_saved_albums = "Saved albums",
                            z_log_n_saved_artists = "Saved artists", z_log_n_followed_playlists = "Followed playlists",
                            z_log_n_own_playlists = "Own playlists", z_log_n_tracks_own_pl = "Tracks in own playlists",
                            z_log_n_pl_ge_min = "Own playlists >= 10 tracks", z_share_largest_list = "Share of largest list",
                            z_log_eff_n_lists = "Effective n. of lists", z_share_liked_in_own_pl = "Liked songs redistributed")

# Heatmap: mean standardised mixture feature per cluster, one panel per
# solution. Writes output/sorting_profiles.png.
plot_sorting_profiles <- function(sorting_summary, filename = "output/sorting_profiles.png"){
  set_ggplot_options()
  p <- sorting_summary$profiles
  d <- p %>% filter(stat == "mean", str_starts(feature, "z_")) %>%
    mutate(feature = factor(coalesce(sorting_feature_labels[feature], feature), levels = rev(unname(sorting_feature_labels))),
           cluster_lab = sprintf("%d\n(%.0f%%)", cluster, 100 * share),
           solution = str_remove(solution, "^cluster_"))
  gg <- ggplot(d, aes(cluster_lab, feature, fill = value)) + geom_tile() +
    geom_text(aes(label = sprintf("%.1f", value)), size = 2.6) +
    facet_wrap(~solution, scales = "free_x") +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0) +
    labs(x = "Component (share of users)", y = "", fill = "Mean z",
         caption = "EXPLORATORY Gaussian mixture on inscription features (log counts, list structure); users with >= 1,000 plays.")
  ggsave(filename, gg, width = 12, height = 7)
  filename
}

# Inscription component x play-based regime (fine), share of each component.
# Writes output/sorting_x_regimes.png.
plot_sorting_x_regimes <- function(sorting_summary, filename = "output/sorting_x_regimes.png"){
  set_ggplot_options()
  lev <- c("flow", "catalogue", "big_playlist", "distillation", "archive_other", "mixed")
  d <- sorting_summary$crosstab %>% filter(level == "fine", play_regime %in% lev) %>%
    mutate(play_regime = factor(play_regime, levels = lev), solution = str_remove(solution, "^cluster_"))
  gg <- ggplot(d, aes(play_regime, factor(cluster), fill = share_of_cluster)) + geom_tile() +
    geom_text(aes(label = scales::percent(share_of_cluster, accuracy = 1)), size = 2.6) +
    facet_wrap(~solution, scales = "free_y") +
    scale_fill_gradient(low = "white", high = "#b2182b", labels = scales::percent) +
    labs(x = "Play-based regime (main rule)", y = "Inscription component", fill = "Share of\ncomponent",
         caption = "EXPLORATORY. Rows sum to 100% (users classified in both typologies).") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  ggsave(filename, gg, width = 12, height = 7)
  filename
}

# Misc numbers ------
misc_sorting <- function(sorting_summary, params){
  lab <- str_glue("EXPLORATORY inscription mixture (mclust, BIC-best {sorting_summary$best_model}, G = {sorting_summary$best_G}; features: {paste(params$features, collapse = ', ')})")
  p <- sorting_summary$profiles %>% filter(solution == "cluster_best")
  bind_rows(
    p %>% distinct(cluster, n_users, share) %>%
      pivot_longer(c(n_users, share), names_to = "stat") %>%
      transmute(name = str_glue("sorting_best_c{cluster}_{stat}"), value,
                description = str_glue("{lab}: {stat} of component {cluster}")),
    p %>% filter(stat == "median", feature %in% sorting_profile_vars) %>%
      transmute(name = str_glue("sorting_best_c{cluster}_median_{feature}"), value,
                description = str_glue("{lab}: median of {feature} in component {cluster}")),
    sorting_summary$association %>% pivot_longer(c(cramers_v, ari), names_to = "stat") %>%
      transmute(name = str_glue("sorting_{str_remove(solution, '^cluster_')}_vs_play_regime_{level}_{stat}"), value,
                description = str_glue("EXPLORATORY: {stat}, inscription components ({solution}) x play-based regimes ({level})")),
    tibble::tibble(name = c("sorting_best_G", "sorting_stability_ari_median", "sorting_stability_ari_min"),
                   value = c(sorting_summary$best_G, sorting_summary$stability$ari_median,
                             sorting_summary$stability$ari_min),
                   description = c(str_glue("{lab}: number of components"),
                                   str_glue("{lab}: median ARI of {sorting_summary$stability$n_boot} subsample refits with the full fit"),
                                   str_glue("{lab}: minimum ARI of the subsample refits")))
  ) %>% mutate(name = as.character(name), description = as.character(description))
}
