# Archiving regimes from the logs — EXPLORATORY ------
#
# Written 2026-11-18, not run on real data. Thresholds are PLACEHOLDERS, to be
# discussed (parameter `regime_params` in _targets.R).
#
# Qualitative typology (interviews):
#  (i)   "cocon du flux": living in the flow, memory of one's taste delegated
#        to history / recommendations, little or no inscription;
#  (ii)  "one big playlist": everything liked goes into the favourites (one
#        main list), which is listened to as such;
#  (iii) "distillation": liked music is redistributed into several personal
#        playlists that are actually played.
# Many users HAVE many playlists, so playlist counts cannot tell the regimes
# apart: the operationalisation uses PLAYS (where plays come from), from the
# detailed-context extract (R/analyse_like_dynamics.R; the source of each play
# is assigned there: favourites / own_playlist / album_artist / search /
# algorithmic / editorial / other).
#
# Features, per user, over the whole window (period "all") and per calendar
# year (period "2018".."2023"; 2023 = January to May only):
#  - share of plays by source: share_fav (favourites playlist), share_own_pl
#    (other own playlists), share_other_library (album/artist pages + search;
#    "followed items" cannot be identified, so all album/artist/search plays
#    count), share_algo, share_edito, share_other;
#  - own_library_share = share_fav + share_own_pl;
#  - likes_per_1000 = like clicks per 1,000 plays;
#  - concentration of plays across own playlists incl. favourites (favourites
#    = one playlist): hhi (Herfindahl), eff_n_pl = exp(Shannon entropy)
#    (effective number of playlists played; 0 if no own-playlist plays),
#    n_pl_ge_N = number of own playlists (excl. favourites) with >= N plays in
#    the period;
#  - fav_share_own = favourites plays / (favourites + own-playlist plays);
#  - redistribution of likes (period "all" only): share of songs liked in the
#    logs that are in >= 1 own playlist other than the favourites
#    (share_liked_in_other_pl), and share of the tracks of the favourites
#    playlist that are also in another own playlist
#    (share_fav_tracks_in_other_pl; playlist snapshot, no logs).
#
# (a) Rule-based classification (classify_regimes_rules(); users with fewer
#     than min_plays plays in the period are "insufficient_data"):
#      flow        : own_library_share < t1  [or likes_per_1000 < t0 if t0 set]
#      big_playlist: not flow AND (fav_share_own >= t2 OR eff_n_pl < eff_big)
#      distillation: not flow AND not big_playlist AND
#                    eff_n_pl >= eff_dist  {and|or}  n_pl_ge_N >= n_pl_dist
#                    (dist_rule = "and" / "or" / "eff" / "npl")
#      mixed       : the rest.
# (b) Data-driven check: Gaussian mixture (mclust, if installed) or k-means on
#     standardised features, k = 3..5, cross-tabulated with (a), with the
#     adjusted Rand index.
# Validation against the interviewees' qualitative classification: see
# validate_regimes_interviews() (hook, needs a mapping interviewee -> panel id
# that may not exist).

# Track membership of the respondents' playlists (incl. favourites) ------
# Local parquet: hashed_id, pid (normalised playlist id), is_fav, song_id
# (BIGINT). Reads the tracks column of the playlists file on S3 (<= 285 MB).
make_playlist_membership_file <- function(playlists_raw, path = "data/temp/playlist_membership"){
  unlink(path, recursive = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  # Filter to the respondents' playlists BEFORE unnesting the track lists, no
  # DISTINCT (a track repeated in a playlist is rare; downstream counts distinct)
  con <- duck_connect(memory_limit = "2GB", threads = 1)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "SET http_timeout = 60000; SET http_retries = 5;")
  duckdb::duckdb_register(con, "ownpl", user_playlist_ids(playlists_raw))
  DBI::dbExecute(con, "CREATE OR REPLACE TEMP MACRO normid(x) AS
    coalesce(CAST(TRY_CAST(x AS BIGINT) AS VARCHAR), CAST(x AS VARCHAR))")
  DBI::dbExecute(con, sprintf("
    COPY (
      SELECT hashed_id, pid, is_fav, TRY_CAST(song_id AS BIGINT) AS song_id
      FROM (SELECT o.hashed_id, o.pid, o.is_fav, unnest(p.tracks.list).element AS song_id
            FROM read_parquet('%s') p
            JOIN ownpl o ON normid(p.playlist_id) = o.pid AND p.hashed_id = o.hashed_id)
      WHERE song_id IS NOT NULL
    ) TO '%s' (FORMAT parquet, COMPRESSION zstd)", s3_uri(playlists_key), file.path(path, "membership.parquet")))
  path
}

# Play profiles per user and year (one pass over the detailed-context extract,
# all plays, no censoring) ------
make_user_play_profiles <- function(streams_ctx_file, context_mapping, playlists_raw, playlist_membership_file,
                                    n_buckets = 32, user_subset = NULL){
  mem <- sprintf("read_parquet('%s/*.parquet')", playlist_membership_file)
  setup0 <- ctx_setup(context_mapping, playlists_raw)
  setup <- function(con){
    setup0(con)
    DBI::dbExecute(con, sprintf("CREATE TEMP TABLE mem AS
      SELECT uid.uid, m.pid, m.is_fav, m.song_id FROM %s m JOIN uid USING (hashed_id)", mem))
  }
  res <- fr_bucket_loop(streams_ctx_file, "fixed", n_buckets = n_buckets, user_subset = user_subset,
                        extra_cols = c("context_detail", "context_id"),
                        setup = setup, annotate_sql = ctx_annotate_sql,
                        fun = function(con, kept, b){
    yr <- "CAST(year(epoch_ms(CAST(ts_listen AS BIGINT) * 1000)) AS INTEGER)"
    q <- function(sql) tibble::as_tibble(DBI::dbGetQuery(con, sql))
    list(
      by_source = q(sprintf("
        SELECT uid.hashed_id, %s AS year, source, CAST(count(*) AS DOUBLE) AS n_plays,
          CAST(sum(click_loved) AS DOUBLE) AS n_likes
        FROM s JOIN uid USING (uid) GROUP BY ALL", yr)),
      pl_plays = q(sprintf("
        SELECT uid.hashed_id, %s AS year,
          CASE WHEN source = 'favourites' THEN '__favourites__' ELSE context_id END AS pid,
          source = 'favourites' AS is_fav, CAST(count(*) AS DOUBLE) AS n_plays
        FROM s JOIN uid USING (uid)
        WHERE source IN ('favourites', 'own_playlist') GROUP BY ALL", yr)),
      liked = q("
        WITH lk AS (SELECT DISTINCT uid, TRY_CAST(song_id AS BIGINT) AS song_id FROM s WHERE click_loved = 1),
        m AS (SELECT uid, song_id, bool_or(is_fav) AS in_fav, bool_or(NOT is_fav) AS in_other
              FROM mem WHERE uid IN (SELECT uid FROM lk) GROUP BY uid, song_id)
        SELECT uid.hashed_id, CAST(count(*) AS DOUBLE) AS n_liked_songs,
          CAST(count(*) FILTER (WHERE m.in_fav) AS DOUBLE) AS n_liked_in_fav,
          CAST(count(*) FILTER (WHERE m.in_other) AS DOUBLE) AS n_liked_in_other_pl,
          CAST(count(*) FILTER (WHERE m.in_fav OR m.in_other) AS DOUBLE) AS n_liked_in_any_pl
        FROM lk LEFT JOIN m USING (uid, song_id) JOIN uid USING (uid)
        GROUP BY uid.hashed_id"))
  })
  con <- duck_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  fav_tracks <- tibble::as_tibble(DBI::dbGetQuery(con, sprintf("
    WITH m AS (SELECT hashed_id, song_id, bool_or(is_fav) AS in_fav, bool_or(NOT is_fav) AS in_other
               FROM %s GROUP BY hashed_id, song_id)
    SELECT hashed_id, CAST(count(*) FILTER (WHERE in_fav) AS DOUBLE) AS n_fav_tracks,
      CAST(count(*) FILTER (WHERE in_fav AND in_other) AS DOUBLE) AS n_fav_tracks_in_other_pl,
      CAST(count(*) FILTER (WHERE in_other) AS DOUBLE) AS n_tracks_other_pl
    FROM m GROUP BY hashed_id", mem)))
  list(by_source = bind_rows(purrr::map(res, "by_source")),
       pl_plays = bind_rows(purrr::map(res, "pl_plays")),
       liked = bind_rows(purrr::map(res, "liked")),
       fav_tracks = fav_tracks)
}

# Features per user x period ------
compute_regime_features <- function(profiles, playlists_raw, params){
  add_all <- function(d, keys, vals) bind_rows(
    mutate(d, period = as.character(year)),
    d %>% group_by(across(all_of(keys))) %>% summarize(across(all_of(vals), sum), .groups = "drop") %>%
      mutate(period = "all")) %>% select(-any_of("year"))
  bs <- add_all(profiles$by_source, c("hashed_id", "source"), c("n_plays", "n_likes"))
  wide <- bs %>% group_by(hashed_id, period) %>%
    # (n_plays is summed LAST: the source-specific sums need the per-source column)
    summarize(n_fav = sum(n_plays[source == "favourites"]), n_own = sum(n_plays[source == "own_playlist"]),
              n_lib = sum(n_plays[source %in% c("album_artist", "search")]),
              n_algo = sum(n_plays[source == "algorithmic"]), n_edito = sum(n_plays[source == "editorial"]),
              n_other = sum(n_plays[source == "other"]), n_likes = sum(n_likes), n_plays = sum(n_plays),
              .groups = "drop") %>%
    mutate(share_fav = n_fav / n_plays, share_own_pl = n_own / n_plays, share_other_library = n_lib / n_plays,
           share_algo = n_algo / n_plays, share_edito = n_edito / n_plays, share_other = n_other / n_plays,
           own_library_share = share_fav + share_own_pl,
           likes_per_1000 = 1000 * n_likes / n_plays,
           fav_share_own = ifelse(n_fav + n_own > 0, n_fav / (n_fav + n_own), NA_real_))
  pp <- add_all(profiles$pl_plays, c("hashed_id", "pid", "is_fav"), "n_plays")
  conc <- pp %>% group_by(hashed_id, period) %>%
    mutate(N = ifelse(period == "all", params$n_min_plays_pl, params$n_min_plays_pl_year), p = n_plays / sum(n_plays)) %>%
    summarize(hhi = sum(p^2), eff_n_pl = exp(-sum(p * log(p))),
              n_pl_ge_N = sum(!is_fav & n_plays >= N[1]), n_own_pl_played = sum(!is_fav), .groups = "drop")
  npl <- playlists_raw %>% filter(!(title %in% favorites_playlists_titles)) %>% count(hashed_id, name = "n_personal_playlists")
  red <- profiles$liked %>% full_join(profiles$fav_tracks, by = "hashed_id") %>%
    transmute(hashed_id, period = "all", n_liked_songs,
              share_liked_in_other_pl = n_liked_in_other_pl / n_liked_songs,
              share_liked_in_fav = n_liked_in_fav / n_liked_songs,
              share_liked_in_any_pl = n_liked_in_any_pl / n_liked_songs,
              n_fav_tracks, share_fav_tracks_in_other_pl = ifelse(n_fav_tracks > 0, n_fav_tracks_in_other_pl / n_fav_tracks, NA_real_))
  wide %>% left_join(conc, by = c("hashed_id", "period")) %>%
    mutate(across(c(eff_n_pl, n_pl_ge_N, n_own_pl_played), ~coalesce(.x, 0))) %>%
    left_join(npl, by = "hashed_id") %>% mutate(n_personal_playlists = coalesce(n_personal_playlists, 0L)) %>%
    left_join(red, by = c("hashed_id", "period"))
}

# (a) Rules ------
classify_regimes_rules <- function(features, params){
  p <- params
  min_pl <- ifelse(features$period == "all", p$min_plays, p$min_plays_year)
  flow <- features$own_library_share < p$t1
  if(!is.null(p$t0) && !is.na(p$t0)) flow <- flow | features$likes_per_1000 < p$t0
  big <- !flow & (coalesce(features$fav_share_own >= p$t2, FALSE) | features$eff_n_pl < p$eff_big)
  d_eff <- features$eff_n_pl >= p$eff_dist
  d_npl <- features$n_pl_ge_N >= p$n_pl_dist
  dist <- !flow & !big & switch(p$dist_rule, and = d_eff & d_npl, or = d_eff | d_npl, eff = d_eff, npl = d_npl,
                                stop("regime_params$dist_rule must be 'and', 'or', 'eff' or 'npl'"))
  features %>% mutate(regime_rule = case_when(n_plays < min_pl ~ "insufficient_data",
                                              flow ~ "flow", big ~ "big_playlist", dist ~ "distillation",
                                              TRUE ~ "mixed"))
}

regime_levels <- c("flow", "big_playlist", "distillation", "mixed", "insufficient_data")

# (b) Clusters, whole window, classified users ------
regime_cluster_features <- c("share_fav", "share_own_pl", "share_other_library", "share_algo", "share_edito",
                             "log_likes_per_1000", "log_eff_n_pl")

adjusted_rand_index <- function(a, b){
  tab <- table(a, b); n <- sum(tab); c2 <- function(x) sum(x * (x - 1) / 2)
  idx <- c2(tab); ea <- c2(rowSums(tab)); eb <- c2(colSums(tab)); exp_ <- ea * eb / (n * (n - 1) / 2)
  (idx - exp_) / ((ea + eb) / 2 - exp_)
}

cluster_regimes <- function(features, ks = 3:5, seed = 20261118){
  d <- features %>% filter(period == "all", regime_rule != "insufficient_data") %>%
    mutate(log_likes_per_1000 = log1p(likes_per_1000), log_eff_n_pl = log1p(eff_n_pl))
  X <- scale(as.matrix(d[, regime_cluster_features]))
  X[is.na(X)] <- 0
  use_mclust <- requireNamespace("mclust", quietly = TRUE)
  method <- ifelse(use_mclust, "mclust (Gaussian mixture, BIC-best model per k)", "k-means (nstart = 50)")
  assign <- d %>% select(hashed_id, regime_rule)
  centers <- list(); xt <- list(); ari <- c()
  for(k in ks){
    set.seed(seed)
    cl <- if(use_mclust) mclust::Mclust(X, G = k, verbose = FALSE)$classification else
      stats::kmeans(X, centers = k, nstart = 50, iter.max = 100)$cluster
    nm <- paste0("cluster_k", k)
    assign[[nm]] <- cl
    centers[[nm]] <- d %>% mutate(cluster = cl) %>% group_by(cluster) %>%
      summarize(n_users = n(), across(c(all_of(regime_cluster_features), own_library_share, fav_share_own,
                                         likes_per_1000, eff_n_pl, n_pl_ge_N), ~median(.x, na.rm = TRUE)),
                .groups = "drop") %>% mutate(k = k, .before = 1)
    xt[[nm]] <- count(assign, regime_rule, cluster = .data[[nm]]) %>% mutate(k = k, .before = 1)
    ari[nm] <- adjusted_rand_index(assign$regime_rule, cl)
  }
  list(method = method, features = regime_cluster_features, assign = assign,
       centers = bind_rows(centers), crosstab = bind_rows(xt), ari = ari)
}

make_user_regimes <- function(profiles, playlists_raw, params){
  compute_regime_features(profiles, playlists_raw, params) %>%
    classify_regimes_rules(params) %>%
    mutate(regime_rule = factor(regime_rule, levels = regime_levels))
}

# Summaries: shares by regime (overall, by socdem), transitions between years,
# cluster cross-tabs. Writes output/regimes_summary.csv,
# output/regimes_transitions.csv, output/regimes_clusters.csv.
summarise_regimes <- function(user_regimes, regimes_clusters, users_playlists,
                              filename = "output/regimes_summary.csv"){
  ur <- user_regimes %>% mutate(regime_rule = as.character(regime_rule))
  all <- ur %>% filter(period == "all") %>%
    left_join(users_playlists %>% select(hashed_id, gender, age_cat, degree, isei_quartile, n_play_2023),
              by = "hashed_id") %>%
    mutate(volume_quartile = paste0("Q", dplyr::ntile(n_play_2023, 4)),
           volume_quartile = ifelse(is.na(n_play_2023), NA, volume_quartile)) %>%
    mutate(across(c(gender, age_cat, degree, isei_quartile), as.character))
  classified <- filter(all, regime_rule != "insufficient_data")
  by_var <- function(v){
    x <- if(v == "all") mutate(classified, group = "all") else mutate(classified, group = .data[[v]])
    x %>% filter(!is.na(group)) %>% count(group, regime_rule, name = "n_users") %>%
      group_by(group) %>% mutate(share = n_users / sum(n_users)) %>% ungroup() %>% mutate(variable = v, .before = 1)
  }
  shares <- bind_rows(lapply(c("all", "gender", "age_cat", "degree", "isei_quartile", "volume_quartile"), by_var))
  shares <- bind_rows(shares,
                      count(all, regime_rule, name = "n_users") %>% mutate(variable = "all_incl_insufficient", group = "all",
                                                                           share = n_users / sum(n_users)))
  per_year <- ur %>% filter(period != "all") %>% count(period, regime_rule, name = "n_users") %>%
    group_by(period) %>% mutate(share = n_users / sum(n_users[regime_rule != "insufficient_data"]),
                                share = ifelse(regime_rule == "insufficient_data", NA, share)) %>% ungroup() %>%
    transmute(variable = "year", group = period, regime_rule, n_users, share)
  shares <- bind_rows(shares, per_year)
  # transitions year t -> t + 1, users classified in both years
  yr <- ur %>% filter(period != "all", regime_rule != "insufficient_data") %>%
    transmute(hashed_id, year = as.integer(period), regime_rule)
  tr <- inner_join(yr, mutate(yr, year = year - 1L), by = c("hashed_id", "year"), suffix = c("_from", "_to")) %>%
    count(year_from = year, regime_from = regime_rule_from, regime_to = regime_rule_to, name = "n_users") %>%
    group_by(year_from, regime_from) %>% mutate(share_of_from = n_users / sum(n_users)) %>% ungroup()
  feat_med <- classified %>% group_by(regime_rule) %>%
    summarize(across(c(n_plays, own_library_share, share_fav, share_own_pl, share_other_library, share_algo,
                       share_edito, likes_per_1000, fav_share_own, eff_n_pl, n_pl_ge_N, n_personal_playlists,
                       share_liked_in_other_pl, share_fav_tracks_in_other_pl), ~median(.x, na.rm = TRUE)),
              .groups = "drop")
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(shares, filename)
  write_csv(tr, str_replace(filename, "_summary\\.csv$", "_transitions.csv"))
  write_csv(feat_med, str_replace(filename, "_summary\\.csv$", "_features_median.csv"))
  write_csv(bind_rows(regimes_clusters$crosstab %>% mutate(table = "crosstab_rule_x_cluster"),
                      regimes_clusters$centers %>% mutate(table = "cluster_medians")) %>%
              mutate(method = regimes_clusters$method),
            str_replace(filename, "_summary\\.csv$", "_clusters.csv"))
  list(shares = shares, transitions = tr, feature_medians = feat_med,
       ari = regimes_clusters$ari, method = regimes_clusters$method)
}

plot_regimes_features <- function(user_regimes, filename = "output/regimes_features.png"){
  set_ggplot_options()
  feats <- c(own_library_share = "Own library share\n(favourites + own playlists)",
             share_fav = "Share favourites", share_own_pl = "Share own playlists",
             share_algo = "Share algorithmic", share_edito = "Share editorial",
             fav_share_own = "Favourites share of\nown-playlist plays",
             eff_n_pl = "Effective no. of\nplaylists played", n_pl_ge_N = "Own playlists with\n>= N plays",
             likes_per_1000 = "Likes per 1,000 plays", share_liked_in_other_pl = "Liked songs in another\nown playlist")
  d <- user_regimes %>% filter(period == "all", regime_rule != "insufficient_data") %>%
    select(hashed_id, regime_rule, all_of(names(feats))) %>%
    pivot_longer(-c(hashed_id, regime_rule), names_to = "feature") %>%
    filter(!is.na(value)) %>%
    mutate(feature = factor(feats[feature], levels = feats), regime_rule = droplevels(regime_rule))
  gg <- ggplot(d, aes(regime_rule, value)) +
    geom_boxplot(outlier.size = .3, outlier.alpha = .3) +
    facet_wrap(~feature, scales = "free_y", ncol = 5) +
    labs(x = "Regime (rule-based, EXPLORATORY thresholds)", y = "") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  ggsave(filename, gg, width = 14, height = 7)
  filename
}

misc_regimes <- function(regimes_summary, user_regimes, params){
  s <- regimes_summary$shares
  thr <- str_glue("EXPLORATORY rule-based regimes (t1 = {params$t1}, t2 = {params$t2}, eff_big = {params$eff_big}, eff_dist = {params$eff_dist}, N = {params$n_min_plays_pl}, n_pl_dist = {params$n_pl_dist}, dist_rule = {params$dist_rule}, min_plays = {params$min_plays})")
  tr <- regimes_summary$transitions
  bind_rows(
    s %>% transmute(name = str_glue("regime_{slugify(variable)}_{slugify(group)}_{regime_rule}_share"), value = share,
                    description = str_glue("{thr}: share of users in regime {regime_rule}; {variable} = {group}")),
    s %>% transmute(name = str_glue("regime_{slugify(variable)}_{slugify(group)}_{regime_rule}_n"), value = n_users,
                    description = str_glue("{thr}: number of users in regime {regime_rule}; {variable} = {group}")),
    regimes_summary$feature_medians %>% pivot_longer(-regime_rule, names_to = "feature") %>%
      transmute(name = str_glue("regime_median_{regime_rule}_{feature}"), value,
                description = str_glue("{thr}: median of {feature} among users in regime {regime_rule} (whole window)")),
    user_regimes %>% filter(period == "all", regime_rule != "insufficient_data") %>%
      summarize(across(c(own_library_share, share_fav, share_own_pl, share_algo, likes_per_1000, eff_n_pl,
                         fav_share_own, share_liked_in_other_pl, share_fav_tracks_in_other_pl),
                       ~median(.x, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "feature") %>%
      transmute(name = str_glue("regime_median_all_users_{feature}"), value,
                description = str_glue("Median of {feature} over classified users (whole window)")),
    tibble(name = str_glue("regime_cluster_ari_{names(regimes_summary$ari)}"), value = unname(regimes_summary$ari),
           description = str_glue("Adjusted Rand index, rule-based regimes vs {regimes_summary$method}, {names(regimes_summary$ari)}")),
    tr %>% group_by(year_from) %>% summarize(value = sum(n_users[regime_from == regime_to]) / sum(n_users)) %>%
      transmute(name = str_glue("regime_transition_{year_from}_{year_from + 1}_share_stable"), value,
                description = str_glue("{thr}: share of users in the same regime in {year_from} and {year_from + 1} (classified both years)")),
    tibble(name = "regime_transition_all_share_stable", value = sum(tr$n_users[tr$regime_from == tr$regime_to]) / sum(tr$n_users),
           description = str_glue("{thr}: share of year-to-year transitions that stay in the same regime, all years pooled"))
  ) %>% mutate(name = as.character(name), description = as.character(description))
}

# Hook: validation against the interviewees' qualitative classification ------
# Needs a CSV mapping each interviewee to a panel id, with columns hashed_id
# and regime_qual (flow / big_playlist / distillation / ...). Whether such a
# mapping exists is unknown; do not build one from personal data. To use: set
# `interviewee_map_file` in _targets.R and uncomment the target
# `regimes_interview_check`.
validate_regimes_interviews <- function(user_regimes, file, filename = "output/regimes_interview_check.csv"){
  m <- readr::read_csv(file, show_col_types = FALSE)
  if(!all(c("hashed_id", "regime_qual") %in% names(m)))
    stop("interviewee_map_file needs columns hashed_id, regime_qual; found: ", paste(names(m), collapse = ", "))
  res <- m %>% left_join(filter(user_regimes, period == "all") %>% select(hashed_id, regime_rule), by = "hashed_id") %>%
    count(regime_qual, regime_rule = coalesce(as.character(regime_rule), "not_in_logs"), name = "n")
  write_csv(res, filename)
  res
}
