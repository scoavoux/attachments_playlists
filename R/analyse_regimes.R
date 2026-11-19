# Archiving regimes from the logs — EXPLORATORY ------
#
# Revised 2026-11-19 (branch regimes-genre-v2): three-way typology agreed with
# the PI, replacing the rule "flow = own-library share < 20%" (41% "flow",
# which lumped album/artist-page listeners together with people living in the
# recommendation flow, contradicting the interviews). Thresholds are
# EXPLORATORY (parameter `regime_params` in _targets.R).
#
# Qualitative typology (interviews):
#  (i)   "cocon du flux": living in the flow, memory of one's taste delegated
#        to history / recommendations, little or no inscription;
#  (ii)  "one big playlist": everything liked goes into the favourites (one
#        main list), which is listened to as such;
#  (iii) "distillation": liked music is redistributed into several personal
#        playlists that are actually played;
#  plus, from the logs, a "catalogue" regime: self-directed listening through
#  album / artist pages, search and the collection, without much archiving.
#
# 1. Three-way family of each play (make_context_family_mapping(),
#    ctx_family_annotate_sql). From the source of the play
#    (R/analyse_like_dynamics.R: favourites / own_playlist / album_artist /
#    search / algorithmic / editorial / other) and the detailed context:
#    - archive   : favourites + the user's own playlists (context_id matches
#                  one of the user's playlists, or the loved-tracks page);
#    - catalogue : self-directed: album and artist pages, search, collection /
#                  saved albums and artists (incl. shuffled_collection: PI's
#                  decision), profile pages, history, recently played, and
#                  OTHER people's playlists chosen by the user;
#    - delegated : algorithmic + editorial (Flow, mixes, radios, editorial
#                  playlists);
#    - other     : unknown / residual (track_api, notifications, home-page
#                  tracks, "unknown"...).
#    Playlist-type contexts (detail contains "playlist": playlist_page,
#    profile_playlists, recently_played_playlist, dynamic_page_playlist,
#    playlist_api, menu_panel_playlist, search_playlist...) are resolved
#    EXPLICITLY at the play level, in this order:
#      a. context_id = one of the user's playlists / favourites -> archive
#         (except radio-type contexts, source_base algorithmic, e.g.
#         playlist_radio seeded by the favourites: delegated);
#      b. context_id = a scraped Deezer editorial playlist
#         (records_w3/playlists_editorial/deezer_editorial_playlists.csv, all
#         account types) -> delegated;
#      c. context_4 = edito -> delegated;
#      d. context_4 in unknown_id_context_4 (ext / other / NA): -> other. Check
#         on the full extract (2026-11-19, aggregate counts): for these
#         playlist contexts (e.g. other/playlist_page 6.8M plays,
#         ext/playlist_page 5.4M), the context_id matches NO playlist of any
#         respondent and no editorial playlist, so it is not interpretable;
#      e. otherwise (organic, someone else's playlist chosen by the user)
#         -> catalogue.
#    The rules per (context_4, detail) are documented in
#    output/context_detail_mapping.csv (columns family_*), and the realised
#    play counts per family in output/context_family_plays.csv.
#
# 2. Features, per user, over the whole window (period "all") and per calendar
#    year ("2018".."2023"; 2023 = January to May only):
#    - share_archive / share_catalogue / share_delegated: shares among plays of
#      KNOWN family (denominator excludes "other"; main choice: "other" mixes
#      unidentifiable contexts and would mechanically lower all three shares,
#      i.e. push users to "mixed", for users whose device/app reports
#      uninformative contexts); share_*_all: same with all plays in the
#      denominator (robustness, reported with denom = "all"); share_other =
#      other / all plays;
#    - share_fav, share_own_pl (of all plays), fav_share_own = favourites plays
#      / archive plays; likes_per_1000; concentration of archive plays across
#      own playlists incl. favourites: hhi, eff_n_pl = exp(Shannon entropy),
#      n_pl_ge_N = own playlists (excl. favourites) with >= N plays;
#      n_personal_playlists; redistribution of likes (period "all").
#
# 3. Regimes (classify_regimes_3way(); users with fewer than min_plays plays,
#    all plays, are "insufficient_data"; min_plays = 1000 for the whole window,
#    min_plays_year = 500 per calendar year):
#      flow      : share_delegated >= t
#      archive   : share_archive >= t, split as before into
#                  big_playlist : fav_share_own >= t2 OR eff_n_pl < eff_big
#                  distillation : eff_n_pl >= eff_dist {and|or} n_pl_ge_N >= n_pl_dist
#                  archive_other: the rest of archive users
#      catalogue : share_catalogue >= t
#      mixed     : none of the three >= t.
#    If t < 0.5 two shares can both pass: the dominant one (argmax among those
#    >= t) wins; the number of users qualifying twice and of exact ties is
#    reported (output/regimes_qualification.csv). Main t = t_main (0.4); grid
#    t_grid (0.3 ... 0.5) plus "plurality" (argmax of the three shares, no
#    threshold, never "mixed"); both denominators.
# 4. Data-driven check: Gaussian mixture (mclust, if installed) or k-means on
#    standardised features, k = 3..5, cross-tabulated with the main regimes
#    (adjusted Rand index). Cheap (one row per user), kept.
# Validation against the interviewees' qualitative classification: see
# validate_regimes_interviews() (hook, needs a mapping interviewee -> panel id
# that may not exist).

regime_families <- c("archive", "catalogue", "delegated", "other")

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

# Ids of the scraped Deezer editorial playlists (normalised strings)
read_editorial_playlist_ids <- function(file){
  readr::read_csv(file, show_col_types = FALSE, col_types = readr::cols(.default = "c")) %>%
    transmute(pid = normid_r(id)) %>% filter(!is.na(pid)) %>% distinct()
}

# Three-way family rules per (context_4, detail) ------
# Adds to the context mapping (make_context_mapping()): is_playlist_ctx,
# family_base (no id match), family_if_own_id (context_id = one of the user's
# playlists), family_if_editorial_id (context_id = an editorial playlist), and
# a readable family_rule. Writes output/context_detail_mapping.csv (the only
# writer of that file).
make_context_family_mapping <- function(context_mapping, params, filename = "output/context_detail_mapping.csv"){
  unk <- params$unknown_id_context_4
  m <- context_mapping %>%
    mutate(is_playlist_ctx = str_detect(context_detail, regex(params$playlist_ctx, ignore_case = TRUE)) &
             source_base != "algorithmic",
           family_base = case_when(source_base == "favourites" ~ "archive",
                                   source_base %in% c("algorithmic", "editorial") ~ "delegated",
                                   is_playlist_ctx & context_4 %in% unk ~ "other",
                                   is_playlist_ctx ~ "catalogue",
                                   source_base %in% c("search", "album_artist") ~ "catalogue",
                                   TRUE ~ "other"),
           family_if_own_id = ifelse(source_base == "algorithmic", "delegated", "archive"),
           family_if_editorial_id = ifelse(is_playlist_ctx, "delegated", family_base),
           # main family of the pair given the observed id matches (plays whose
           # id matches the user's playlists / not), for reading the table
           share_id_own = (n_id_is_favourites + n_id_is_own_playlist) / n_plays,
           family_rule = str_glue("own id -> {family_if_own_id}; editorial id -> {family_if_editorial_id}; else -> {family_base}"))
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(arrange(m, desc(n_plays)), filename)
  m
}

# SQL (over s0, from fr_bucket_loop) adding `source` (as in ctx_annotate_sql,
# R/analyse_like_dynamics.R, left untouched so that the like-dynamics targets
# do not rerun) and the three-way `family`. Needs the tables ctxmap, ownpl_u
# and edids (registered by ctx_family_setup()).
ctx_family_annotate_sql <- "
  SELECT s0.uid, s0.ts_listen, s0.song_id, s0.click_loved,
    coalesce(s0.context_4, 'NA') AS context_4,
    coalesce(s0.context_detail, 'NA') AS context_detail,
    s0.context_id,
    CASE WHEN o.is_fav AND coalesce(m.own_playlist_allowed, true) THEN 'favourites'
         WHEN o.pid IS NOT NULL AND coalesce(m.own_playlist_allowed, true) THEN 'own_playlist'
         ELSE coalesce(m.source_base, 'other') END AS source,
    CASE WHEN o.pid IS NOT NULL AND coalesce(m.own_playlist_allowed, true) THEN coalesce(m.family_if_own_id, 'archive')
         WHEN e.pid IS NOT NULL THEN coalesce(m.family_if_editorial_id, 'other')
         ELSE coalesce(m.family_base, 'other') END AS family
  FROM s0
  LEFT JOIN ctxmap m ON coalesce(s0.context_4, 'NA') = m.context_4
                    AND coalesce(s0.context_detail, 'NA') = m.context_detail
  LEFT JOIN ownpl_u o ON s0.uid = o.uid AND s0.context_id = o.pid
  LEFT JOIN edids e ON s0.context_id = e.pid"

ctx_family_setup <- function(context_family_map, playlists_raw, editorial_ids){
  force(context_family_map); force(playlists_raw); force(editorial_ids)
  function(con){
    duckdb::duckdb_register(con, "ctxmap_r", select(context_family_map, context_4, context_detail, source_base,
                                                    own_playlist_allowed, family_base, family_if_own_id,
                                                    family_if_editorial_id))
    DBI::dbExecute(con, "CREATE TEMP TABLE ctxmap AS SELECT * FROM ctxmap_r")
    duckdb::duckdb_register(con, "ownpl_r", user_playlist_ids(playlists_raw))
    DBI::dbExecute(con, "CREATE TEMP TABLE ownpl_u AS
      SELECT uid.uid, o.pid, o.is_fav FROM ownpl_r o JOIN uid USING (hashed_id)")
    duckdb::duckdb_register(con, "edids_r", editorial_ids)
    DBI::dbExecute(con, "CREATE TEMP TABLE edids AS SELECT DISTINCT pid FROM edids_r")
  }
}

# Play profiles per user and year (one pass over the detailed-context extract,
# all plays, no censoring) ------
make_user_play_profiles <- function(streams_ctx_file, context_family_map, playlists_raw, playlist_membership_file,
                                    editorial_raw_file, n_buckets = 32, user_subset = NULL){
  mem <- sprintf("read_parquet('%s/*.parquet')", playlist_membership_file)
  setup0 <- ctx_family_setup(context_family_map, playlists_raw, read_editorial_playlist_ids(editorial_raw_file))
  setup <- function(con){
    setup0(con)
    DBI::dbExecute(con, sprintf("CREATE TEMP TABLE mem AS
      SELECT uid.uid, m.pid, m.is_fav, m.song_id FROM %s m JOIN uid USING (hashed_id)", mem))
  }
  res <- fr_bucket_loop(streams_ctx_file, "fixed", n_buckets = n_buckets, user_subset = user_subset,
                        extra_cols = c("context_detail", "context_id"),
                        setup = setup, annotate_sql = ctx_family_annotate_sql,
                        fun = function(con, kept, b){
    yr <- "CAST(year(epoch_ms(CAST(ts_listen AS BIGINT) * 1000)) AS INTEGER)"
    q <- function(sql) tibble::as_tibble(DBI::dbGetQuery(con, sql))
    list(
      by_source = q(sprintf("
        SELECT uid.hashed_id, %s AS year, source, family, CAST(count(*) AS DOUBLE) AS n_plays,
          CAST(sum(click_loved) AS DOUBLE) AS n_likes
        FROM s JOIN uid USING (uid) GROUP BY ALL", yr)),
      # aggregate over users: realised family per context (documentation)
      by_ctx = q("
        SELECT context_4, context_detail, source, family, CAST(count(*) AS DOUBLE) AS n_plays
        FROM s GROUP BY ALL"),
      pl_plays = q(sprintf("
        SELECT uid.hashed_id, %s AS year,
          CASE WHEN source = 'favourites' THEN '__favourites__' ELSE context_id END AS pid,
          source = 'favourites' AS is_fav, CAST(count(*) AS DOUBLE) AS n_plays
        FROM s JOIN uid USING (uid)
        WHERE family = 'archive' AND source IN ('favourites', 'own_playlist') GROUP BY ALL", yr)),
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
  con <- duck_connect(memory_limit = "1500MB", threads = 2)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  sub <- ""
  if(!is.null(user_subset)){
    duckdb::duckdb_register(con, "user_subset", tibble::tibble(hashed_id = user_subset))
    sub <- "WHERE hashed_id IN (SELECT hashed_id FROM user_subset)"
  }
  fav_tracks <- tibble::as_tibble(DBI::dbGetQuery(con, sprintf("
    WITH m AS (SELECT hashed_id, song_id, bool_or(is_fav) AS in_fav, bool_or(NOT is_fav) AS in_other
               FROM %s %s GROUP BY hashed_id, song_id)
    SELECT hashed_id, CAST(count(*) FILTER (WHERE in_fav) AS DOUBLE) AS n_fav_tracks,
      CAST(count(*) FILTER (WHERE in_fav AND in_other) AS DOUBLE) AS n_fav_tracks_in_other_pl,
      CAST(count(*) FILTER (WHERE in_other) AS DOUBLE) AS n_tracks_other_pl
    FROM m GROUP BY hashed_id", mem, sub)))
  list(by_source = bind_rows(purrr::map(res, "by_source")),
       by_ctx = bind_rows(purrr::map(res, "by_ctx")) %>%
         group_by(context_4, context_detail, source, family) %>% summarize(n_plays = sum(n_plays), .groups = "drop"),
       pl_plays = bind_rows(purrr::map(res, "pl_plays")),
       liked = bind_rows(purrr::map(res, "liked")),
       fav_tracks = fav_tracks)
}

# Realised plays per family, by (context_4, detail) and overall. Writes
# output/context_family_plays.csv.
summarise_context_family_plays <- function(profiles, filename = "output/context_family_plays.csv"){
  d <- profiles$by_ctx
  res <- bind_rows(
    d %>% group_by(family) %>% summarize(n_plays = sum(n_plays), .groups = "drop") %>%
      mutate(table = "overall", context_4 = "(all)", context_detail = "(all)", source = "(all)"),
    d %>% group_by(source, family) %>% summarize(n_plays = sum(n_plays), .groups = "drop") %>%
      mutate(table = "by_source", context_4 = "(all)", context_detail = "(all)"),
    d %>% mutate(table = "by_context")) %>%
    group_by(table, context_4, context_detail, source) %>%
    mutate(share_within = n_plays / sum(n_plays)) %>% ungroup() %>%
    mutate(share_of_all_plays = n_plays / sum(d$n_plays)) %>%
    select(table, context_4, context_detail, source, family, n_plays, share_within, share_of_all_plays) %>%
    arrange(table, desc(n_plays))
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(res, filename)
  res
}

# Features per user x period ------
compute_regime_features <- function(profiles, playlists_raw, params){
  add_all <- function(d, keys, vals) bind_rows(
    mutate(d, period = as.character(year)),
    d %>% group_by(across(all_of(keys))) %>% summarize(across(all_of(vals), sum), .groups = "drop") %>%
      mutate(period = "all")) %>% select(-any_of("year"))
  bs <- add_all(profiles$by_source, c("hashed_id", "source", "family"), c("n_plays", "n_likes"))
  wide <- bs %>% group_by(hashed_id, period) %>%
    # (n_plays is summed LAST: the specific sums need the per-row column)
    summarize(n_archive = sum(n_plays[family == "archive"]), n_catalogue = sum(n_plays[family == "catalogue"]),
              n_delegated = sum(n_plays[family == "delegated"]), n_other = sum(n_plays[family == "other"]),
              n_fav = sum(n_plays[family == "archive" & source == "favourites"]),
              n_own = sum(n_plays[family == "archive" & source == "own_playlist"]),
              n_likes = sum(n_likes), n_plays = sum(n_plays), .groups = "drop") %>%
    mutate(n_known = n_archive + n_catalogue + n_delegated,
           share_archive = n_archive / n_known, share_catalogue = n_catalogue / n_known,
           share_delegated = n_delegated / n_known,
           share_archive_all = n_archive / n_plays, share_catalogue_all = n_catalogue / n_plays,
           share_delegated_all = n_delegated / n_plays, share_other = n_other / n_plays,
           share_fav = n_fav / n_plays, share_own_pl = n_own / n_plays,
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

# Classification ------
regime_levels <- c("flow", "catalogue", "big_playlist", "distillation", "archive_other", "mixed", "insufficient_data")
regime3_levels <- c("flow", "catalogue", "archive", "mixed", "insufficient_data")

rule_label <- function(t) ifelse(is.na(t), "plurality", sprintf("t%.2f", t))

# t: threshold (NA = plurality, argmax of the three shares); denom: "known"
# (shares among plays of known family; main) or "all" (all plays incl. other).
# Returns features' keys + regime, regime3, n_qualify (shares >= t), tie (the
# dominant qualifying share is tied).
classify_regimes_3way <- function(features, t, denom = "known", params){
  p <- params
  sfx <- ifelse(denom == "all", "_all", "")
  M <- as.matrix(features[, paste0(c("share_archive", "share_catalogue", "share_delegated"), sfx)])
  M[is.na(M)] <- 0
  qual <- if(is.na(t)) matrix(TRUE, nrow(M), 3) else M >= t
  masked <- ifelse(qual, M, -Inf)
  top_val <- apply(masked, 1, max)
  top <- max.col(masked, ties.method = "first")
  n_qual <- rowSums(qual)
  tie <- rowSums(masked == top_val & is.finite(masked)) > 1
  min_pl <- ifelse(features$period == "all", p$min_plays, p$min_plays_year)
  insuff <- features$n_plays < min_pl | features$n_known == 0
  regime3 <- ifelse(n_qual == 0 | !is.finite(top_val), "mixed", c("archive", "catalogue", "flow")[top])
  big <- coalesce(features$fav_share_own >= p$t2, FALSE) | features$eff_n_pl < p$eff_big
  d_eff <- features$eff_n_pl >= p$eff_dist
  d_npl <- features$n_pl_ge_N >= p$n_pl_dist
  dist <- switch(p$dist_rule, and = d_eff & d_npl, or = d_eff | d_npl, eff = d_eff, npl = d_npl,
                 stop("regime_params$dist_rule must be 'and', 'or', 'eff' or 'npl'"))
  regime <- case_when(regime3 != "archive" ~ regime3, big ~ "big_playlist", dist ~ "distillation",
                      TRUE ~ "archive_other")
  tibble(hashed_id = features$hashed_id, period = features$period,
         rule = rule_label(t), threshold = t, denom = denom,
         regime = factor(ifelse(insuff, "insufficient_data", regime), levels = regime_levels),
         regime3 = factor(ifelse(insuff, "insufficient_data", regime3), levels = regime3_levels),
         n_qualify = ifelse(insuff | is.na(t), NA_integer_, as.integer(n_qual)),
         tie = ifelse(insuff, NA, tie))
}

# All (threshold, denominator) variants, long; is_main flags the main rule.
classify_regimes_grid <- function(features, params){
  g <- tidyr::expand_grid(t = params$t_grid, denom = params$denoms)
  bind_rows(purrr::map2(g$t, g$denom, ~classify_regimes_3way(features, .x, .y, params))) %>%
    mutate(is_main = rule == rule_label(params$t_main) & denom == params$denom_main)
}

make_user_regimes <- function(profiles, playlists_raw, params){
  f <- compute_regime_features(profiles, playlists_raw, params)
  main <- classify_regimes_3way(f, params$t_main, params$denom_main, params) %>%
    select(hashed_id, period, regime, regime3, n_qualify, tie)
  left_join(f, main, by = c("hashed_id", "period"))
}

# (b) Clusters, whole window, classified users ------
regime_cluster_features <- c("share_archive", "share_catalogue", "share_delegated", "fav_share_own0",
                             "log_likes_per_1000", "log_eff_n_pl")

adjusted_rand_index <- function(a, b){
  tab <- table(a, b); n <- sum(tab); c2 <- function(x) sum(x * (x - 1) / 2)
  idx <- c2(tab); ea <- c2(rowSums(tab)); eb <- c2(colSums(tab)); exp_ <- ea * eb / (n * (n - 1) / 2)
  (idx - exp_) / ((ea + eb) / 2 - exp_)
}

cluster_regimes <- function(user_regimes, ks = 3:5, seed = 20261118){
  d <- user_regimes %>% filter(period == "all", regime != "insufficient_data") %>%
    mutate(log_likes_per_1000 = log1p(likes_per_1000), log_eff_n_pl = log1p(eff_n_pl),
           fav_share_own0 = coalesce(fav_share_own, 0))
  X <- scale(as.matrix(d[, regime_cluster_features]))
  X[is.na(X)] <- 0
  use_mclust <- requireNamespace("mclust", quietly = TRUE)
  method <- ifelse(use_mclust, "mclust (Gaussian mixture, BIC-best model per k)", "k-means (nstart = 50)")
  assign <- d %>% select(hashed_id, regime)
  centers <- list(); xt <- list(); ari <- c()
  for(k in ks){
    if(nrow(X) <= k) next
    set.seed(seed)
    cl <- if(use_mclust) mclust::Mclust(X, G = k, verbose = FALSE)$classification else
      stats::kmeans(X, centers = k, nstart = 50, iter.max = 100)$cluster
    nm <- paste0("cluster_k", k)
    assign[[nm]] <- cl
    centers[[nm]] <- d %>% mutate(cluster = cl) %>% group_by(cluster) %>%
      summarize(n_users = n(), across(c(share_archive, share_catalogue, share_delegated, share_other,
                                         fav_share_own, likes_per_1000, eff_n_pl, n_pl_ge_N), ~median(.x, na.rm = TRUE)),
                .groups = "drop") %>% mutate(k = k, .before = 1)
    xt[[nm]] <- count(assign, regime, cluster = .data[[nm]]) %>% mutate(k = k, .before = 1)
    ari[nm] <- adjusted_rand_index(assign$regime, cl)
  }
  list(method = method, features = regime_cluster_features, assign = assign,
       centers = bind_rows(centers), crosstab = bind_rows(xt), ari = ari)
}

# Sarle's bimodality coefficient (> 5/9 suggests bi- or multimodality)
bimodality_coefficient <- function(x){
  x <- x[!is.na(x)]; n <- length(x)
  if(n < 4 || sd(x) == 0) return(NA_real_)
  m <- mean(x); s <- sqrt(mean((x - m)^2))
  g <- mean((x - m)^3) / s^3; k <- mean((x - m)^4) / s^4 - 3
  (g^2 + 1) / (k + 3 * (n - 1)^2 / ((n - 2) * (n - 3)))
}

# Summaries ------
# Writes to output/: regimes_summary.csv (shares by rule x denominator x
# variable x group, fine and coarse regimes), regimes_qualification.csv,
# regimes_features_median.csv, regimes_transitions.csv,
# regimes_transitions_stability.csv, regimes_share_distribution.csv,
# regimes_share_stats.csv, regimes_clusters.csv.
summarise_regimes <- function(user_regimes, regimes_clusters, users_playlists, params,
                              filename = "output/regimes_summary.csv"){
  out <- function(x) str_replace(filename, "_summary\\.csv$", x)
  socdem <- users_playlists %>% select(hashed_id, gender, age_cat, degree, isei_quartile, n_play_2023) %>%
    mutate(volume_quartile = ifelse(is.na(n_play_2023), NA, paste0("Q", dplyr::ntile(n_play_2023, 4)))) %>%
    mutate(across(c(gender, age_cat, degree, isei_quartile), as.character)) %>% select(-n_play_2023)
  grid <- classify_regimes_grid(user_regimes, params) %>%
    mutate(across(c(regime, regime3), as.character))
  long <- bind_rows(mutate(grid, level = "fine"), mutate(grid, level = "coarse", regime = regime3)) %>%
    select(-regime3)
  all <- long %>% filter(period == "all") %>% left_join(socdem, by = "hashed_id")
  keys <- c("rule", "threshold", "denom", "is_main", "level")
  by_var <- function(v){
    x <- filter(all, regime != "insufficient_data")
    x <- if(v == "all") mutate(x, group = "all") else mutate(x, group = .data[[v]])
    x %>% filter(!is.na(group)) %>% count(across(all_of(keys)), group, regime, name = "n_users") %>%
      group_by(across(all_of(keys)), group) %>% mutate(share = n_users / sum(n_users)) %>% ungroup() %>%
      mutate(variable = v)
  }
  shares <- bind_rows(lapply(c("all", "gender", "age_cat", "degree", "isei_quartile", "volume_quartile"), by_var))
  shares <- bind_rows(shares,
                      count(all, across(all_of(keys)), regime, name = "n_users") %>%
                        group_by(across(all_of(keys))) %>% mutate(share = n_users / sum(n_users)) %>% ungroup() %>%
                        mutate(variable = "all_incl_insufficient", group = "all"),
                      long %>% filter(period != "all") %>% count(across(all_of(keys)), group = period, regime, name = "n_users") %>%
                        group_by(across(all_of(keys)), group) %>%
                        mutate(share = ifelse(regime == "insufficient_data", NA,
                                              n_users / sum(n_users[regime != "insufficient_data"]))) %>%
                        ungroup() %>% mutate(variable = "year")) %>%
    select(all_of(keys), variable, group, regime, n_users, share)
  # double qualification / ties
  qualif <- grid %>% filter(regime != "insufficient_data") %>%
    group_by(rule, threshold, denom, is_main, period) %>%
    summarize(n_classified = n(), n_qualify_0 = sum(n_qualify == 0, na.rm = TRUE),
              n_qualify_1 = sum(n_qualify == 1, na.rm = TRUE), n_qualify_2plus = sum(n_qualify >= 2, na.rm = TRUE),
              n_ties = sum(tie, na.rm = TRUE), .groups = "drop") %>%
    mutate(share_qualify_2plus = n_qualify_2plus / n_classified)
  # median features per regime (whole window)
  feats <- c("n_plays", "share_archive", "share_catalogue", "share_delegated", "share_other", "share_fav",
             "share_own_pl", "fav_share_own", "likes_per_1000", "eff_n_pl", "n_pl_ge_N", "n_personal_playlists",
             "share_liked_in_other_pl", "share_fav_tracks_in_other_pl")
  ura <- user_regimes %>% filter(period == "all") %>% select(hashed_id, all_of(feats))
  feat_med <- long %>% filter(period == "all", regime != "insufficient_data") %>%
    inner_join(ura, by = "hashed_id") %>%
    group_by(across(all_of(keys)), regime) %>%
    summarize(n_users = n(), across(all_of(feats), ~median(.x, na.rm = TRUE)), .groups = "drop")
  # transitions year t -> t + 1, users classified in both years (fine regimes)
  yr <- grid %>% filter(period != "all", regime != "insufficient_data") %>%
    transmute(rule, threshold, denom, is_main, hashed_id, year = as.integer(period), regime)
  tr <- inner_join(yr, mutate(yr, year = year - 1L), by = c("rule", "threshold", "denom", "is_main", "hashed_id", "year"),
                   suffix = c("_from", "_to")) %>%
    count(rule, threshold, denom, is_main, year_from = year, regime_from, regime_to, name = "n_users") %>%
    group_by(rule, threshold, denom, year_from, regime_from) %>% mutate(share_of_from = n_users / sum(n_users)) %>% ungroup()
  stab <- bind_rows(
    tr %>% group_by(rule, threshold, denom, is_main, year_from) %>%
      summarize(n_users = sum(n_users), share_stable = sum(n_users[regime_from == regime_to]) / sum(n_users),
                .groups = "drop") %>% mutate(years = paste0(year_from, "-", year_from + 1)),
    tr %>% group_by(rule, threshold, denom, is_main) %>%
      summarize(n_users = sum(n_users), share_stable = sum(n_users[regime_from == regime_to]) / sum(n_users),
                .groups = "drop") %>% mutate(years = "pooled")) %>% select(-year_from)
  # distributions of the shares (users with >= min_plays, whole window)
  dd <- user_regimes %>% filter(period == "all", n_plays >= params$min_plays, n_known > 0) %>%
    select(hashed_id, share_archive, share_catalogue, share_delegated, share_archive_all, share_catalogue_all,
           share_delegated_all, share_other) %>%
    pivot_longer(-hashed_id, names_to = "share")
  bw <- 0.02
  dist <- dd %>% mutate(bin_lo = pmin(floor(value / bw) * bw, 1 - bw)) %>%
    count(share, bin_lo, name = "n_users") %>% group_by(share) %>% mutate(density = n_users / sum(n_users) / bw) %>% ungroup()
  sstats <- dd %>% group_by(share) %>%
    summarize(n_users = n(), mean = mean(value), sd = sd(value),
              q10 = quantile(value, .1), q25 = quantile(value, .25), median = median(value),
              q75 = quantile(value, .75), q90 = quantile(value, .9),
              share_lt_10 = mean(value < .1), share_ge_40 = mean(value >= .4), share_ge_60 = mean(value >= .6),
              bimodality_coef = bimodality_coefficient(value), .groups = "drop")
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(shares, filename)
  write_csv(qualif, out("_qualification.csv"))
  write_csv(feat_med, out("_features_median.csv"))
  write_csv(tr, out("_transitions.csv"))
  write_csv(stab, out("_transitions_stability.csv"))
  write_csv(dist, out("_share_distribution.csv"))
  write_csv(sstats, out("_share_stats.csv"))
  ct <- regimes_clusters$crosstab
  if(nrow(ct)) ct <- mutate(ct, table = "crosstab_main_regime_x_cluster", regime = as.character(regime))
  write_csv(bind_rows(ct,
                      regimes_clusters$centers %>% mutate(table = "cluster_medians")) %>%
              mutate(method = regimes_clusters$method),
            out("_clusters.csv"))
  list(shares = shares, qualification = qualif, transitions = tr, stability = stab, feature_medians = feat_med,
       share_dist = dist, share_stats = sstats, ari = regimes_clusters$ari, method = regimes_clusters$method)
}

# Distribution plots of the three shares (aggregates only: histograms and
# 2-D binned counts). Writes output/regimes_shares_hist.png and
# output/regimes_shares_ternary.png (ternary-style: each user is a point of the
# simplex archive / catalogue / delegated, shown as counts per hexagon bin)
# and output/regimes_shares_pairs.png (pairwise binned counts).
plot_regimes_shares <- function(user_regimes, params, dir = "output"){
  set_ggplot_options()
  lab <- c(share_archive = "Archive\n(favourites + own playlists)", share_catalogue = "Catalogue\n(self-directed)",
           share_delegated = "Delegated\n(algorithmic + editorial)", share_other = "Other / unknown\n(share of all plays)")
  d <- user_regimes %>% filter(period == "all", n_plays >= params$min_plays, n_known > 0)
  h <- d %>% select(all_of(names(lab))) %>% pivot_longer(everything(), names_to = "share") %>%
    mutate(share = factor(lab[share], levels = lab))
  f1 <- file.path(dir, "regimes_shares_hist.png")
  g1 <- ggplot(h, aes(value)) +
    geom_histogram(binwidth = .02, boundary = 0) +
    { if(!is.na(params$t_main)) geom_vline(xintercept = params$t_main, linetype = 2) } +
    facet_wrap(~share, nrow = 1) +
    labs(x = "Share of plays (archive / catalogue / delegated: among plays of known family)", y = "Users",
         caption = str_glue("Users with >= {params$min_plays} plays, whole window. Dashed line: main threshold t = {params$t_main}. EXPLORATORY."))
  ggsave(f1, g1, width = 14, height = 4)
  tern <- d %>% transmute(x = share_delegated + share_archive / 2, y = share_archive * sqrt(3) / 2)
  tri <- tibble(x = c(0, 1, .5, 0), y = c(0, 0, sqrt(3) / 2, 0))
  f2 <- file.path(dir, "regimes_shares_ternary.png")
  g2 <- ggplot(tern, aes(x, y)) +
    geom_bin_2d(binwidth = c(.025, .025)) +
    geom_path(data = tri) +
    annotate("text", x = c(-.03, 1.03, .5), y = c(-.03, -.03, sqrt(3) / 2 + .04),
             label = c("Catalogue", "Delegated", "Archive")) +
    scale_fill_viridis_c(trans = "log10") +
    coord_equal() + theme_void() +
    labs(fill = "Users", caption = "Ternary-style plot of the three shares (counts per bin). EXPLORATORY.")
  ggsave(f2, g2, width = 7, height = 6.5)
  pr <- bind_rows(
    transmute(d, pair = "Archive vs delegated", x = share_archive, y = share_delegated),
    transmute(d, pair = "Archive vs catalogue", x = share_archive, y = share_catalogue),
    transmute(d, pair = "Catalogue vs delegated", x = share_catalogue, y = share_delegated))
  f3 <- file.path(dir, "regimes_shares_pairs.png")
  g3 <- ggplot(pr, aes(x, y)) + geom_bin_2d(binwidth = c(.025, .025)) +
    scale_fill_viridis_c(trans = "log10") + facet_wrap(~pair) + coord_equal() +
    labs(x = "First share", y = "Second share", fill = "Users")
  ggsave(f3, g3, width = 13, height = 4.5)
  c(f1, f2, f3)
}

plot_regimes_features <- function(user_regimes, filename = "output/regimes_features.png"){
  set_ggplot_options()
  feats <- c(share_archive = "Archive share", share_catalogue = "Catalogue share",
             share_delegated = "Delegated share", share_other = "Other / unknown share",
             fav_share_own = "Favourites share of\narchive plays",
             eff_n_pl = "Effective no. of\nplaylists played", n_pl_ge_N = "Own playlists with\n>= N plays",
             n_personal_playlists = "No. of personal\nplaylists",
             likes_per_1000 = "Likes per 1,000 plays", share_liked_in_other_pl = "Liked songs in another\nown playlist")
  d <- user_regimes %>% filter(period == "all", regime != "insufficient_data") %>%
    select(hashed_id, regime, all_of(names(feats))) %>%
    pivot_longer(-c(hashed_id, regime), names_to = "feature") %>%
    filter(!is.na(value)) %>%
    mutate(feature = factor(feats[feature], levels = feats), regime = droplevels(regime))
  gg <- ggplot(d, aes(regime, value)) +
    geom_boxplot(outlier.size = .3, outlier.alpha = .3) +
    facet_wrap(~feature, scales = "free_y", ncol = 5) +
    labs(x = "Regime (three-way typology, main threshold, EXPLORATORY)", y = "") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  ggsave(filename, gg, width = 14, height = 7)
  filename
}

misc_regimes <- function(regimes_summary, user_regimes, params){
  thr <- str_glue("EXPLORATORY three-way regimes (main: t = {params$t_main}, denominator = plays of {params$denom_main} family; t2 = {params$t2}, eff_big = {params$eff_big}, eff_dist = {params$eff_dist}, N = {params$n_min_plays_pl}, n_pl_dist = {params$n_pl_dist}, dist_rule = {params$dist_rule}, min_plays = {params$min_plays}, min_plays_year = {params$min_plays_year})")
  s <- regimes_summary$shares
  main <- filter(s, is_main)
  pre <- function(level) ifelse(level == "coarse", "regime3", "regime")
  grid_all <- s %>% filter(variable %in% c("all", "year"))
  q <- regimes_summary$qualification
  st <- regimes_summary$stability
  bind_rows(
    main %>% transmute(name = str_glue("{pre(level)}_{slugify(variable)}_{slugify(group)}_{regime}_share"), value = share,
                       description = str_glue("{thr}: share of users in regime {regime} ({level}); {variable} = {group}")),
    main %>% transmute(name = str_glue("{pre(level)}_{slugify(variable)}_{slugify(group)}_{regime}_n"), value = n_users,
                       description = str_glue("{thr}: number of users in regime {regime} ({level}); {variable} = {group}")),
    grid_all %>% filter(variable == "all") %>%
      transmute(name = str_glue("{pre(level)}_grid_{rule}_{denom}_{regime}_share"), value = share,
                description = str_glue("EXPLORATORY sensitivity: share of users in regime {regime} ({level}), rule {rule}, denominator {denom} (whole window)")),
    q %>% filter(period == "all") %>%
      transmute(name = str_glue("regime_qualif_{rule}_{denom}_share_2plus"), value = share_qualify_2plus,
                description = str_glue("EXPLORATORY: share of classified users with >= 2 shares above the threshold, rule {rule}, denominator {denom} (whole window)")),
    q %>% filter(period == "all") %>%
      transmute(name = str_glue("regime_qualif_{rule}_{denom}_n_ties"), value = n_ties,
                description = str_glue("EXPLORATORY: number of users with a tied dominant share, rule {rule}, denominator {denom} (whole window)")),
    regimes_summary$feature_medians %>% filter(is_main, level == "fine") %>%
      select(-rule, -threshold, -denom, -is_main, -level) %>%
      pivot_longer(-regime, names_to = "feature") %>%
      transmute(name = str_glue("regime_median_{regime}_{feature}"), value,
                description = str_glue("{thr}: median of {feature} among users in regime {regime} (whole window)")),
    regimes_summary$share_stats %>% pivot_longer(-share, names_to = "stat") %>%
      transmute(name = str_glue("regime_dist_{share}_{stat}"), value,
                description = str_glue("EXPLORATORY: distribution across users (>= {params$min_plays} plays, whole window) of {share}: {stat}")),
    st %>% filter(is_main) %>%
      transmute(name = str_glue("regime_transition_{slugify(years)}_share_stable"), value = share_stable,
                description = str_glue("{thr}: share of users in the same regime in consecutive years ({years}; users classified both years)")),
    st %>% filter(years == "pooled") %>%
      transmute(name = str_glue("regime_transition_grid_{rule}_{denom}_share_stable"), value = share_stable,
                description = str_glue("EXPLORATORY sensitivity: share of year-to-year transitions staying in the same regime, rule {rule}, denominator {denom}, pooled")),
    tibble(name = str_glue("regime_cluster_ari_{names(regimes_summary$ari)}"), value = unname(regimes_summary$ari),
           description = str_glue("EXPLORATORY: adjusted Rand index, main three-way regimes vs {regimes_summary$method}, {names(regimes_summary$ari)}"))
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
  res <- m %>% left_join(filter(user_regimes, period == "all") %>% select(hashed_id, regime), by = "hashed_id") %>%
    count(regime_qual, regime = coalesce(as.character(regime), "not_in_logs"), name = "n")
  write_csv(res, filename)
  res
}
