# Detailed play context, dynamics of the like, sources of replays ------
#
# Written 2026-11-18 WITHOUT access to the raw schema (not run on real data).
# ASSUMPTIONS TO CONFIRM (parameters at the top of _targets.R):
#  - Besides context_4 (organic / edito / reco_algo / other), the raw stream
#    files carry a DETAILED context column and a CONTEXT ID column (id of the
#    playlist, album, artist... the play came from). Their names are unknown:
#    `stream_context_cols` gives candidate names, the first one present in a
#    file is used. make_streams_ctx_file() stops with the list of available
#    columns if none is found. Run `tar_make(streams_schema)` first (reads only
#    the parquet footers, a few MB) and check `tar_read(streams_schema)`.
#  - For plays from a playlist, context_id = the playlist_id of the playlists
#    file (records_w3/playlists_data/...). Ids are compared as normalised
#    strings (5, 5.0 and '5' are equal).
#  - The user's playlists are the rows of `playlists_raw` with their hashed_id;
#    the favourites playlist ("Loved tracks" / "Coups de cœur") is identified
#    by its title, as everywhere else in the pipeline
#    (favorites_playlists_titles, R/analyse_playlist_use.R).
#
# Source of a play (7 categories, see assign_context_sources()):
#  1. favourites   : context_id = the user's favourites playlist, or the
#                    detailed context matches rules$favourites;
#  2. own_playlist : context_id = another playlist of the user;
#  then, for the other plays, from (context_4, detailed context):
#  5. algorithmic  : context_4 = reco_algo, or detail matches rules$algorithmic;
#  6. editorial    : context_4 = edito, or detail matches rules$editorial (tested last);
#  4. search       : detail matches rules$search;
#  3. album_artist : detail matches rules$album_artist;
#  (order of the tests: favourites rule, context_4 reco_algo, context_4 edito,
#  algorithmic rule, search, album_artist, editorial rule)
#  7. other        : everything else (incl. other users' playlists, unknown).
# The context_4 classification (precomputed by Deezer) has priority over the
# regular expressions for algorithmic/editorial plays. The full mapping, with
# play counts and the number of plays whose context_id matches one of the
# user's playlists, is written to output/context_detail_mapping.csv: CHECK IT
# before reading any result.

stream_sources <- c("favourites", "own_playlist", "album_artist", "search",
                    "algorithmic", "editorial", "other")
stream_source_labels <- c(favourites = "Favourites (loved tracks)", own_playlist = "Own playlists",
                          album_artist = "Album / artist page", search = "Search",
                          algorithmic = "Algorithmic", editorial = "Editorial playlists",
                          other = "Other / unknown")

# Schema of the raw stream files (DESCRIBE reads only the parquet footer) ------
make_streams_schema <- function(streaming_data_files){
  con <- duck_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "SET http_timeout = 60000; SET http_retries = 5;")
  purrr::map_dfr(streaming_data_files$Key, function(k){
    d <- DBI::dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM read_parquet('%s')", s3_uri(k)))
    tibble::tibble(key = k, column_name = d$column_name, column_type = d$column_type)
  })
}

# Resolve the candidate names against the schema of each file. Returns a tibble
# key / detail / id (column names to read). Stops with the available columns if
# a role has no candidate in some file.
resolve_stream_context_cols <- function(streams_schema, stream_context_cols){
  purrr::map_dfr(split(streams_schema, streams_schema$key), function(s){
    res <- purrr::imap_chr(stream_context_cols, function(cands, role){
      hit <- intersect(cands, s$column_name)
      if(!length(hit)) stop(sprintf(paste0(
        "Stream context column '%s' not found in %s.\n  Candidates tried: %s\n",
        "  Available columns: %s\n  -> set stream_context_cols$%s in _targets.R"),
        role, s$key[1], paste(cands, collapse = ", "), paste(s$column_name, collapse = ", "), role), call. = FALSE)
      hit[1]
    })
    tibble::tibble(key = s$key[1], !!!as.list(res))
  })
}

# Local extract with the detailed context ------
# Same rows as make_streams_users_file() (respondents, is_listened = 1, songs,
# 2018+), plus context_detail and context_id (normalised strings). Kept as a
# separate extract so that the existing targets do not depend on the guessed
# column names. Respondents are taken from the existing local extract (no
# download of the `users` target).
make_streams_ctx_file <- function(streaming_data_files, streams_users_file, streams_schema,
                                  stream_context_cols, path = "data/temp/streams_ctx", max_attempts = 3){
  colmap <- resolve_stream_context_cols(streams_schema, stream_context_cols)
  keys <- streaming_data_files$Key
  miss <- setdiff(keys, colmap$key)
  if(length(miss)) stop("streams_schema does not cover: ", paste(miss, collapse = ", "), " (rerun streams_schema)")
  message("Stream context columns used:\n",
          paste(utils::capture.output(print(dplyr::count(colmap, detail, id))), collapse = "\n"))
  unlink(path, recursive = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  for(i in seq_along(keys)){
    cm <- colmap[colmap$key == keys[i], ]
    is_long <- stringr::str_detect(keys[i], "streams_long")
    out <- file.path(path, sprintf("part_%02d.parquet", i))
    sql <- sprintf("
      COPY (
        SELECT hashed_id, ts_listen, %s, click_loved, CAST(context_4 AS VARCHAR) AS context_4,
          CAST(\"%s\" AS VARCHAR) AS context_detail,
          coalesce(CAST(TRY_CAST(\"%s\" AS BIGINT) AS VARCHAR), CAST(\"%s\" AS VARCHAR)) AS context_id
        FROM read_parquet('%s')
        WHERE is_listened = 1 AND ts_listen >= 1514764800 %s
          AND hashed_id IN (SELECT hashed_id FROM u)
      ) TO '%s' (FORMAT parquet, COMPRESSION zstd)",
      ifelse(is_long, "song_id", "media_id AS song_id"), cm$detail, cm$id, cm$id,
      s3_uri(keys[i]), ifelse(is_long, "", "AND media_type = 'song'"), out)
    for(attempt in seq_len(max_attempts)){
      ok <- tryCatch({
        con <- duck_connect()
        DBI::dbExecute(con, "SET http_timeout = 60000; SET http_retries = 5;")
        DBI::dbExecute(con, sprintf("CREATE TEMP TABLE u AS SELECT DISTINCT hashed_id FROM read_parquet('%s/*.parquet')",
                                    streams_users_file))
        DBI::dbExecute(con, sql)
        TRUE
      }, error = function(e){ message("Attempt ", attempt, " failed for ", keys[i], ": ", conditionMessage(e)); FALSE },
      finally = try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE))
      if(ok) break
      if(attempt == max_attempts) stop("Could not extract ", keys[i])
    }
  }
  path
}

# The user's playlists (incl. favourites), ids normalised as strings
user_playlist_ids <- function(playlists_raw){
  playlists_raw %>%
    transmute(hashed_id,
              pid = normid_r(playlist_id),
              is_fav = title %in% favorites_playlists_titles) %>%
    filter(!is.na(pid)) %>%
    group_by(hashed_id, pid) %>% summarize(is_fav = any(is_fav), .groups = "drop")
}

# R twin of the SQL macro normid(): 5, 5.0, "5" -> "5"
normid_r <- function(x){
  if(bit64::is.integer64(x)) return(as.character(x))
  y <- suppressWarnings(as.numeric(as.character(x)))
  ifelse(!is.na(y) & y == round(y) & abs(y) < 9e15, format(y, scientific = FALSE, trim = TRUE), as.character(x))
}

# Context mapping ------
# Counts plays by (context_4, context_detail), with the number of plays whose
# context_id is one of the user's (favourites / other) playlists, then assigns
# a base source to each pair with assign_context_sources(). Writes
# output/context_detail_mapping.csv.
make_context_mapping <- function(streams_ctx_file, playlists_raw, rules, own_playlist_detail_pattern = NULL,
                                 max_levels = 40L, filename = "output/context_detail_mapping.csv"){
  con <- duck_connect(memory_limit = "1500MB", threads = 2)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  duckdb::duckdb_register(con, "ownpl", user_playlist_ids(playlists_raw))
  d <- tibble::as_tibble(DBI::dbGetQuery(con, sprintf("
    SELECT coalesce(s.context_4, 'NA') AS context_4, coalesce(s.context_detail, 'NA') AS context_detail,
      CAST(count(*) AS DOUBLE) AS n_plays,
      CAST(count(DISTINCT s.hashed_id) AS DOUBLE) AS n_users,
      CAST(sum(s.click_loved) AS DOUBLE) AS n_like_clicks,
      CAST(count(*) FILTER (WHERE s.context_id IS NULL) AS DOUBLE) AS n_context_id_missing,
      CAST(approx_count_distinct(s.context_id) AS DOUBLE) AS n_distinct_context_id,
      CAST(count(*) FILTER (WHERE o.is_fav) AS DOUBLE) AS n_id_is_favourites,
      CAST(count(*) FILTER (WHERE NOT o.is_fav) AS DOUBLE) AS n_id_is_own_playlist
    FROM read_parquet('%s/*.parquet') s
    LEFT JOIN ownpl o ON s.hashed_id = o.hashed_id AND s.context_id = o.pid
    GROUP BY ALL", streams_ctx_file)))
  m <- assign_context_sources(d, rules, own_playlist_detail_pattern, max_levels)
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(arrange(m, desc(n_plays)), filename)
  m
}

# Base source of (context_4, context_detail) pairs from the rules (named list
# of regular expressions, matched case-insensitively on context_detail).
# own_playlist_allowed: whether a context_id match with the user's playlists
# counts for this detail value (pattern NULL = always).
assign_context_sources <- function(d, rules, own_playlist_detail_pattern = NULL, max_levels = 40L){
  rx <- function(r) if(is.null(r) || is.na(r) || r == "") rep(FALSE, nrow(d)) else
    str_detect(d$context_detail, regex(r, ignore_case = TRUE))
  d %>%
    mutate(source_base = case_when(rx(rules$favourites) ~ "favourites",
                                   context_4 == "reco_algo" ~ "algorithmic",
                                   context_4 == "edito" ~ "editorial",
                                   rx(rules$algorithmic) ~ "algorithmic",
                                   rx(rules$search) ~ "search",
                                   rx(rules$album_artist) ~ "album_artist",
                                   rx(rules$editorial) ~ "editorial",
                                   TRUE ~ "other"),
           own_playlist_allowed = if(is.null(own_playlist_detail_pattern)) TRUE else
             str_detect(context_detail, regex(own_playlist_detail_pattern, ignore_case = TRUE))) %>%
    group_by(context_detail) %>% mutate(n_detail = sum(n_plays)) %>% ungroup() %>%
    mutate(detail_rank = dense_rank(desc(n_detail)),
           detail_lumped = ifelse(detail_rank <= max_levels, context_detail, "(other values)"),
           share_plays = n_plays / sum(n_plays)) %>%
    select(-n_detail, -detail_rank)
}

# SQL (over s0, from fr_bucket_loop) adding `detail` (lumped) and `source` to
# each play. Needs the tables ctxmap and ownpl_u (registered by
# ctx_setup()).
ctx_annotate_sql <- "
  SELECT s0.uid, s0.ts_listen, s0.song_id, s0.click_loved,
    coalesce(s0.context_4, 'NA') AS context_4,
    coalesce(m.detail_lumped, '(other values)') AS detail,
    s0.context_id,
    CASE WHEN o.is_fav AND coalesce(m.own_playlist_allowed, true) THEN 'favourites'
         WHEN o.pid IS NOT NULL AND coalesce(m.own_playlist_allowed, true) THEN 'own_playlist'
         ELSE coalesce(m.source_base, 'other') END AS source
  FROM s0
  LEFT JOIN ctxmap m ON coalesce(s0.context_4, 'NA') = m.context_4
                    AND coalesce(s0.context_detail, 'NA') = m.context_detail
  LEFT JOIN ownpl_u o ON s0.uid = o.uid AND s0.context_id = o.pid"

ctx_setup <- function(context_mapping, playlists_raw){
  force(context_mapping); force(playlists_raw)
  function(con){
    duckdb::duckdb_register(con, "ctxmap_r", select(context_mapping, context_4, context_detail, detail_lumped,
                                                    source_base, own_playlist_allowed))
    DBI::dbExecute(con, "CREATE TEMP TABLE ctxmap AS SELECT * FROM ctxmap_r")
    duckdb::duckdb_register(con, "ownpl_r", user_playlist_ids(playlists_raw))
    DBI::dbExecute(con, "CREATE TEMP TABLE ownpl_u AS
      SELECT uid.uid, o.pid, o.is_fav FROM ownpl_r o JOIN uid USING (hashed_id)")
  }
}

# Dynamics of the like (one pass over the extract) ------
# Left-censoring fix as in make_favorites_replayed_file(logic = "fixed"):
# only (user, song) pairs first played more than one year after the user's
# first observed play. Right-censoring: variant "none" (all plays until the
# end of the window E) and, for each horizon H, "fixed_horizon_Hm": pairs
# first played before E - H, only their plays within H of the first play, and
# "liked" = liked within H (same definition as summarise_ltfs_censoring()).
# Returns a list of two count tables:
#  - plays: n_plays by variant, rank_k (play rank, capped at max_rank + 1 =
#    "max_rank+"), like_group (never / first_sight / later), phase (before /
#    at / after the like, never_liked), context_4, detail, source;
#  - pairs: n_pairs by variant, like_group, n_plays_pair (capped at 200).
# Everything in Tasks A and B is derived from these two tables.
make_like_dynamics <- function(streams_ctx_file, context_mapping, playlists_raw, window_end,
                               horizons_months = c(6, 12), max_rank = 20L,
                               n_buckets = 32, user_subset = NULL){
  E <- as.numeric(window_end)
  hz <- horizon_seconds(horizons_months)
  variants <- c(list(none = NULL), as.list(setNames(hz, paste0("fixed_horizon_", names(hz), "m"))))
  res <- fr_bucket_loop(streams_ctx_file, "fixed", n_buckets = n_buckets, user_subset = user_subset,
                        extra_cols = c("context_detail", "context_id"),
                        setup = ctx_setup(context_mapping, playlists_raw),
                        annotate_sql = ctx_annotate_sql,
                        fun = function(con, kept, b){
    DBI::dbExecute(con, "DROP TABLE IF EXISTS p; DROP TABLE IF EXISTS pl;")
    DBI::dbExecute(con, sprintf("
      CREATE TEMP TABLE p AS
      WITH kept AS (%s)
      SELECT uid, song_id, ts_listen, click_loved, context_4, detail, source,
        row_number() OVER (PARTITION BY uid, song_id ORDER BY ts_listen) AS n
      FROM kept", kept))
    DBI::dbExecute(con, "
      CREATE TEMP TABLE pl AS
      SELECT uid, song_id, min(ts_listen) AS ts_first,
        min(n) FILTER (WHERE click_loved = 1) AS L,
        min(ts_listen) FILTER (WHERE click_loved = 1) AS ts_like
      FROM p GROUP BY uid, song_id")
    out <- purrr::imap(variants, function(H, v){
      if(is.null(H)){
        lh <- "pl.L"; where <- "TRUE"
      } else {
        lh <- sprintf("CASE WHEN pl.ts_like <= pl.ts_first + %.0f THEN pl.L END", H)
        where <- sprintf("pl.ts_first <= %.0f AND p.ts_listen <= pl.ts_first + %.0f", E - H, H)
      }
      pv <- sprintf("
        pv AS (
          SELECT p.uid, p.song_id, p.n, p.context_4, p.detail, p.source, %s AS LH
          FROM p JOIN pl USING (uid, song_id)
          WHERE %s)", lh, where)
      plays <- DBI::dbGetQuery(con, sprintf("
        WITH %s
        SELECT '%s' AS variant, CAST(least(n, %d) AS INTEGER) AS rank_k,
          CASE WHEN LH IS NULL THEN 'never' WHEN LH = 1 THEN 'first_sight' ELSE 'later' END AS like_group,
          CASE WHEN LH IS NULL THEN 'never_liked' WHEN n < LH THEN 'before'
               WHEN n = LH THEN 'at' ELSE 'after' END AS phase,
          context_4, detail, source, CAST(count(*) AS DOUBLE) AS n_plays
        FROM pv GROUP BY ALL", pv, v, max_rank + 1L))
      pairs <- DBI::dbGetQuery(con, sprintf("
        WITH %s
        SELECT '%s' AS variant, like_group, CAST(least(np, 200) AS INTEGER) AS n_plays_pair,
          CAST(count(*) AS DOUBLE) AS n_pairs
        FROM (SELECT uid, song_id, max(n) AS np,
                CASE WHEN any_value(LH) IS NULL THEN 'never' WHEN any_value(LH) = 1 THEN 'first_sight'
                     ELSE 'later' END AS like_group
              FROM pv GROUP BY uid, song_id)
        GROUP BY ALL", pv, v))
      list(plays = plays, pairs = pairs)
    })
    list(plays = bind_rows(purrr::map(out, "plays")), pairs = bind_rows(purrr::map(out, "pairs")))
  })
  plays <- bind_rows(purrr::map(res, "plays")) %>%
    group_by(variant, rank_k, like_group, phase, context_4, detail, source) %>%
    summarize(n_plays = sum(n_plays), .groups = "drop")
  pairs <- bind_rows(purrr::map(res, "pairs")) %>%
    group_by(variant, like_group, n_plays_pair) %>%
    summarize(n_pairs = sum(n_pairs), .groups = "drop")
  list(plays = as_tibble(plays), pairs = as_tibble(pairs), max_rank = max_rank)
}

# A1. Discrete-time hazard of the like ------
# At rank k, the plays "at risk" are the k-th plays of pairs not liked before
# k (phases before / at / never_liked); events = likes at k (phase "at").
# hazard = events / at risk, overall and by the context of the k-th play
# (dim = context_4, source, detail). share_at_risk = share of at-risk plays at k
# in that context; share_likes = share of likes at k in that context;
# rel_propensity = share_likes / share_at_risk = hazard(context) / hazard(all).
# rank "2+" pools all ranks >= 2 (likes that are not at first sight).
summarise_like_hazard <- function(like_dynamics, filename = "output/like_hazard.csv"){
  mr <- like_dynamics$max_rank
  d <- like_dynamics$plays %>%
    filter(phase %in% c("before", "at", "never_liked")) %>%
    mutate(rank = ifelse(rank_k > mr, paste0(mr + 1, "+"), as.character(rank_k)),
           is_like = phase == "at")
  d <- bind_rows(d, filter(d, rank_k >= 2) %>% mutate(rank = "2+"))
  one <- function(dim){
    x <- if(dim == "all") mutate(d, level = "all") else mutate(d, level = .data[[dim]])
    x %>%
      group_by(variant, rank, level) %>%
      summarize(n_at_risk = sum(n_plays), n_likes = sum(n_plays[is_like]), .groups = "drop") %>%
      group_by(variant, rank) %>%
      mutate(hazard = n_likes / n_at_risk,
             share_at_risk = n_at_risk / sum(n_at_risk),
             share_likes = n_likes / sum(n_likes),
             rel_propensity = share_likes / share_at_risk) %>%
      ungroup() %>% mutate(dim = dim)
  }
  res <- bind_rows(lapply(c("all", "context_4", "source", "detail"), one)) %>%
    mutate(rank_num = suppressWarnings(as.integer(str_remove(rank, "\\+$")))) %>%
    select(variant, dim, level, rank, rank_num, everything()) %>%
    arrange(variant, dim, level, rank_num)
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(res, filename)
  res
}

plot_like_hazard <- function(like_hazard, variant = "fixed_horizon_12m", dim = "context_4",
                             filename = "output/like_hazard.png"){
  set_ggplot_options()
  v <- variant; dm <- dim
  lv <- c(all = "All", organic = "Organic", edito = "Editorial", reco_algo = "Algorithmic", stream_source_labels)
  d <- like_hazard %>%
    filter(.data$variant == v, .data$dim %in% c("all", dm), rank != "2+") %>%
    filter(.data$dim == "all" | dm != "context_4" | level %in% c("organic", "edito", "reco_algo")) %>%
    mutate(level = factor(coalesce(lv[level], level), levels = unique(c(lv, sort(unique(level))))) %>% droplevels())
  mr <- max(d$rank_num)
  long <- d %>%
    select(level, rank_num, hazard, share_at_risk, share_likes) %>%
    pivot_longer(c(hazard, share_at_risk, share_likes), names_to = "metric") %>%
    filter(!(metric != "hazard" & level == "All")) %>%
    mutate(metric = factor(metric, levels = c("hazard", "share_at_risk", "share_likes"),
                           labels = c("P(like at play k | not liked before)",
                                      "Share of at-risk plays at k", "Share of likes at k")))
  gg <- ggplot(long, aes(rank_num, value, colour = level)) +
    geom_line() + geom_point(size = 1) +
    facet_wrap(~metric, scales = "free_y", nrow = 1) +
    scale_x_continuous(breaks = c(1, 5, 10, 15, mr), labels = c(1, 5, 10, 15, paste0(mr, "+"))) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Play rank k of the (user, song) pair", y = "",
         colour = ifelse(dim == "context_4", "Context of play k", "Source of play k"),
         caption = str_glue("Left-censoring fixed; right-censoring: {v}"))
  ggsave(filename, gg, width = 12, height = 4.5)
  filename
}

# A2. Survival of never-liked songs ------
# (i) distribution of the number of plays per pair, never liked vs liked;
# (ii) for never-liked pairs with >= 2 plays, composition of their plays 2+ by
# context_4 and by source (and, for comparison, the plays after the like of
# liked pairs).
summarise_never_liked_survival <- function(like_dynamics, filename = "output/never_liked_survival.csv"){
  pr <- like_dynamics$pairs %>%
    mutate(liked = ifelse(like_group == "never", "never_liked", "liked"))
  dist <- bind_rows(pr, mutate(pr, liked = like_group) %>% filter(liked != "never")) %>%
    group_by(variant, liked, n_plays_pair) %>% summarize(n = sum(n_pairs), .groups = "drop") %>%
    group_by(variant, liked) %>% arrange(n_plays_pair, .by_group = TRUE) %>%
    mutate(share = n / sum(n), cum_share = cumsum(share)) %>% ungroup() %>%
    transmute(table = "n_plays_distribution", variant, group = liked, dim = "n_plays_pair",
              level = ifelse(n_plays_pair >= 200, "200+", as.character(n_plays_pair)), n, share, cum_share)
  comp <- like_dynamics$plays %>%
    filter((phase == "never_liked" & rank_k >= 2) | phase == "after") %>%
    mutate(group = ifelse(phase == "never_liked", "never_liked_plays_2plus", "liked_plays_after_like"))
  comp <- bind_rows(lapply(c("context_4", "source"), function(dm)
    comp %>% group_by(variant, group, level = .data[[dm]]) %>% summarize(n = sum(n_plays), .groups = "drop") %>%
      group_by(variant, group) %>% mutate(share = n / sum(n)) %>% ungroup() %>%
      mutate(table = "replay_composition", dim = dm, cum_share = NA_real_)))
  res <- bind_rows(dist, comp) %>% select(table, variant, group, dim, level, n, share, cum_share)
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(res, filename)
  res
}

# A3. Like rate by context of the first play ------
# Among all pairs: P(like at play 1 | first-play context) and P(ever liked |
# first-play context) (ever = within the window, or within H for the
# fixed-horizon variants).
summarise_first_play_like_rates <- function(like_dynamics, filename = "output/first_play_like_rates.csv"){
  d <- like_dynamics$plays %>% filter(rank_k == 1)
  res <- bind_rows(lapply(c("all", "context_4", "source", "detail"), function(dm){
    x <- if(dm == "all") mutate(d, level = "all") else mutate(d, level = .data[[dm]])
    x %>% group_by(variant, level) %>%
      summarize(n_pairs = sum(n_plays),
                n_liked_at_play1 = sum(n_plays[like_group == "first_sight"]),
                n_ever_liked = sum(n_plays[like_group != "never"]), .groups = "drop") %>%
      mutate(dim = dm, p_like_at_play1 = n_liked_at_play1 / n_pairs, p_ever_liked = n_ever_liked / n_pairs)
  })) %>%
    group_by(variant, dim) %>% mutate(share_of_pairs = n_pairs / sum(n_pairs)) %>% ungroup() %>%
    select(variant, dim, level, everything())
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(res, filename)
  res
}

# B. Where replays come from ------
# Share of plays by source, for: liked pairs, plays before the like (later
# likes only), at the like, after the like (by like_group); never-liked pairs,
# first play and replays (plays 2+).
summarise_replays_by_source <- function(like_dynamics, filename = "output/replays_by_source.csv"){
  d <- like_dynamics$plays %>%
    mutate(segment = case_when(phase == "after" & like_group == "first_sight" ~ "after_like_first_sight",
                               phase == "after" ~ "after_like_later",
                               phase == "before" ~ "before_like",
                               phase == "at" ~ "at_like",
                               phase == "never_liked" & rank_k == 1 ~ "never_liked_play1",
                               TRUE ~ "never_liked_replays"))
  d <- bind_rows(d, filter(d, phase == "after") %>% mutate(segment = "after_like_all"))
  res <- d %>%
    group_by(variant, segment, source) %>% summarize(n_plays = sum(n_plays), .groups = "drop") %>%
    complete(nesting(variant, segment), source = stream_sources, fill = list(n_plays = 0)) %>%
    group_by(variant, segment) %>% mutate(share = n_plays / sum(n_plays)) %>% ungroup()
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(res, filename)
  res
}

plot_replays_by_source <- function(replays_by_source, variant = "fixed_horizon_12m",
                                   filename = "output/replays_by_source.png"){
  set_ggplot_options()
  seg <- c(never_liked_play1 = "Never liked:\nfirst play", never_liked_replays = "Never liked:\nreplays",
           before_like = "Liked later:\nbefore the like", at_like = "Liked:\nplay of the like",
           after_like_first_sight = "Liked at 1st play:\nafter the like", after_like_later = "Liked later:\nafter the like")
  v <- variant
  d <- replays_by_source %>% filter(variant == v, segment %in% names(seg)) %>%
    mutate(segment = factor(seg[segment], levels = seg),
           source = factor(stream_source_labels[source], levels = rev(stream_source_labels)))
  gg <- ggplot(d, aes(segment, share, fill = source)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    labs(x = "", y = "Share of plays", fill = "Source of the play",
         caption = str_glue("Left-censoring fixed; right-censoring: {v}")) +
    guides(fill = guide_legend(reverse = TRUE))
  ggsave(filename, gg, width = 10, height = 5)
  filename
}

# Misc numbers for A and B ------
misc_like_dynamics <- function(like_hazard, never_liked_survival, first_play_like_rates, replays_by_source,
                               context_mapping, main_variant = "fixed_horizon_12m"){
  # rows of the main variant are duplicated under the variant name "main"
  vs <- function(v) v
  vlab <- function(v) ifelse(v == "main", paste0("MAIN ESTIMATE (", main_variant, ")"), v)
  keep_ranks <- c("1", "2", "3", "5", "10", "2+", paste0(max(like_hazard$rank_num, na.rm = TRUE), "+"))
  hz <- like_hazard %>%
    filter(dim %in% c("all", "context_4", "source"), rank %in% keep_ranks) %>%
    select(variant, dim, level, rank, hazard, share_at_risk, share_likes, rel_propensity, n_at_risk, n_likes) %>%
    pivot_longer(c(hazard, share_at_risk, share_likes, rel_propensity, n_at_risk, n_likes), names_to = "metric") %>%
    filter(!(dim == "all" & metric %in% c("share_at_risk", "share_likes", "rel_propensity")), !is.na(value))
  hz <- bind_rows(hz %>% filter(variant == main_variant) %>% mutate(variant = "main"), hz) %>%
    transmute(name = str_glue("hazard_{vs(variant)}_{slugify(dim)}_{slugify(level)}_rank{str_replace(rank, fixed('+'), 'plus')}_{metric}"),
              value,
              description = str_glue("Like hazard, {vlab(variant)}: {metric} at play rank {rank} ({dim} = {level}); pairs not liked before"))
  sv <- never_liked_survival
  s1 <- sv %>% filter(table == "n_plays_distribution", level %in% c("1", "2", "5", "10")) %>%
    transmute(variant, name0 = str_glue("{slugify(group)}_share_pairs_le_{level}_plays"), value = cum_share,
              d = str_glue("share of {group} pairs with <= {level} plays"))
  s2 <- sv %>% filter(table == "replay_composition") %>%
    transmute(variant, name0 = str_glue("{slugify(group)}_{dim}_{slugify(level)}_share"), value = share,
              d = str_glue("{group}: share of plays with {dim} = {level}"))
  sv <- bind_rows(s1, s2)
  sv <- bind_rows(sv %>% filter(variant == main_variant) %>% mutate(variant = "main"), sv) %>%
    transmute(name = str_glue("survival_{vs(variant)}_{name0}"), value,
              description = str_glue("Never-liked survival, {vlab(variant)}: {d}"))
  fp <- first_play_like_rates %>% filter(dim != "detail") %>%
    pivot_longer(c(p_like_at_play1, p_ever_liked, n_pairs, share_of_pairs), names_to = "metric")
  fp <- bind_rows(fp %>% filter(variant == main_variant) %>% mutate(variant = "main"), fp) %>%
    transmute(name = str_glue("firstplay_{vs(variant)}_{slugify(dim)}_{slugify(level)}_{metric}"), value,
              description = str_glue("Pairs by first-play {dim} = {level}, {vlab(variant)}: {metric}"))
  rb <- bind_rows(replays_by_source %>% filter(variant == main_variant) %>% mutate(variant = "main"), replays_by_source) %>%
    transmute(name = str_glue("replaysrc_{vs(variant)}_{segment}_{source}_share"), value = share,
              description = str_glue("Share of plays from source {source}, segment {segment}, {vlab(variant)}"))
  cm <- context_mapping %>% group_by(source_base) %>%
    summarize(share = sum(n_plays) / sum(context_mapping$n_plays),
              id_fav = sum(n_id_is_favourites) / sum(context_mapping$n_plays),
              id_own = sum(n_id_is_own_playlist) / sum(context_mapping$n_plays), .groups = "drop")
  cmn <- bind_rows(
    transmute(cm, name = str_glue("ctxmap_share_plays_source_base_{source_base}"), value = share,
              description = str_glue("Share of all plays (extract) whose (context_4, detail) maps to {source_base}, before the id match")),
    tibble(name = c("ctxmap_share_plays_id_is_favourites", "ctxmap_share_plays_id_is_own_playlist", "ctxmap_n_detail_values"),
           value = c(sum(cm$id_fav), sum(cm$id_own), n_distinct(context_mapping$context_detail)),
           description = c("Share of all plays whose context_id is the user's favourites playlist",
                           "Share of all plays whose context_id is another playlist of the user",
                           "Number of distinct values of the detailed context")))
  bind_rows(hz, sv, fp, rb, cmn) %>% mutate(name = as.character(name), description = as.character(description))
}
