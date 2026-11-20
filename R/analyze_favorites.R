# Like at first sight ------
# For each (user, song): number of plays, context of the first play, rank of the
# play at which the like button was first clicked, and number of plays after
# (and excluding) that like. Computed in DuckDB with window functions over the
# local stream extract (see make_streams_users_file()).
#
# Left-censoring: we only observe plays from 2018 on, so a song "discovered"
# early in a user's history may in fact have been played before. We use the
# first year of each user's observed history as a burn-in period.
#  - logic = "fixed" (intended): keep only songs whose FIRST play (over the
#    user's whole history) is more than one year after the user's first observed
#    play; all plays of these songs are counted.
#  - logic = "old" (original R code): drop every PLAY that happened during the
#    user's first year. A song first heard in year 1 is then kept and its first
#    post-year-1 play is wrongly treated as its discovery.
# 1 year = lubridate::duration(1, "year") = 365.25 days = 31557600 seconds.
make_favorites_replayed_file <- function(streams_file, logic = c("fixed", "old"),
                                         path = NULL, user_subset = NULL,
                                         n_buckets = 32,
                                         horizons_months = c(6, 12)){
  logic <- match.arg(logic)
  if(is.null(path)) path <- str_glue("data/temp/favorites_replayed_{logic}")
  unlink(path, recursive = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  # Right-censoring (end of the observation window): besides the play counts
  # over the whole window, we store the timestamps of the first play and of the
  # like, and play counts within fixed horizons (in months) after the first
  # play and after the like, so that summarise_ltfs_censoring() can restrict
  # to songs/likes with enough follow-up. 1 month = 1/12 of 365.25 days.
  hz <- horizon_seconds(horizons_months)
  hz_sql <- paste0(purrr::imap_chr(hz, function(sec, h) sprintf("
            CAST(sum(CASE WHEN ts_listen <= ts_first_w + %1$.0f THEN 1 ELSE 0 END) AS INTEGER)
              AS nlisten_%2$sm_after_discovery,
            CASE WHEN max(scl) > 0 THEN
              CAST(sum(CASE WHEN scl > 0 AND ts_listen <= ts_like_w + %1$.0f THEN 1 ELSE 0 END) - 1 AS INTEGER) END
              AS nlisten_%2$sm_after_clicked_love,", sec, h)), collapse = "")
  fr_bucket_loop(streams_file, logic, n_buckets = n_buckets, user_subset = user_subset,
                 fun = function(con, kept, b){
    DBI::dbExecute(con, sprintf("
      COPY (
        WITH kept AS (%s),
        p AS (
          SELECT uid, song_id, ts_listen, click_loved, context_4,
            row_number() OVER (PARTITION BY uid, song_id ORDER BY ts_listen) AS n,
            sum(click_loved) OVER (PARTITION BY uid, song_id ORDER BY ts_listen
                                   ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS scl,
            min(ts_listen) OVER (PARTITION BY uid, song_id) AS ts_first_w,
            min(CASE WHEN click_loved = 1 THEN ts_listen END) OVER (PARTITION BY uid, song_id) AS ts_like_w
          FROM kept
        ),
        agg AS (
          SELECT uid, song_id,
            CAST(count(*) AS INTEGER) AS nlisten_after_discovery,
            first(context_4 ORDER BY n) AS context_at_discovery,
            CASE WHEN max(scl) > 0 THEN CAST(count(*) FILTER (WHERE scl > 0) - 1 AS INTEGER) END
              AS nlisten_after_clicked_love,
            CAST(min(n) FILTER (WHERE click_loved = 1) AS INTEGER) AS nlisten_at_clicked_love,
            first(context_4 ORDER BY n) FILTER (WHERE click_loved = 1) AS context_at_clicked_love,
            %s
            min(ts_first_w) AS ts_first_play,
            min(ts_like_w) AS ts_clicked_love
          FROM p GROUP BY uid, song_id
        )
        SELECT uid.hashed_id, agg.* EXCLUDE (uid)
        FROM agg JOIN uid USING (uid)
      ) TO '%s' (FORMAT parquet, COMPRESSION zstd)", kept, hz_sql,
      file.path(path, sprintf("bucket_%02d.parquet", b))))
  })
  return(as.character(path))
}

# Horizons in months -> seconds (1 year = 365.25 days, as lubridate), named by month
horizon_seconds <- function(horizons_months){
  setNames(as.numeric(horizons_months) / 12 * 31557600, horizons_months)
}

# SQL selecting the plays kept under each left-censoring logic, from the
# bucket tables `s` (plays) and `uf` (first observed play of each user).
fr_kept_sql <- function(logic = c("fixed", "old")){
  logic <- match.arg(logic)
  if(logic == "old"){
    "
      SELECT s.* FROM s JOIN uf USING (uid)
      WHERE s.ts_listen > uf.user_first + 31557600"
  } else {
    "
      SELECT s.* FROM s
      SEMI JOIN (
        SELECT sf.uid, sf.song_id 
        FROM (SELECT uid, song_id, min(ts_listen) AS song_first FROM s GROUP BY uid, song_id) sf
        JOIN uf USING (uid)
        WHERE sf.song_first > uf.user_first + 31557600
      ) k USING (uid, song_id)"
  }
}

# Shared bucket loop over users of the local stream extract. For each bucket b,
# creates the temp tables `s` (plays of the bucket's users, integer uid) and
# `uf` (user first play), then calls fun(con, kept_sql, b). Returns the list of
# fun() results.
# Integer user ids make sorts and hash tables much lighter than 64-char
# hashes; users are processed in buckets to bound memory (all computations
# are within-user, so buckets are independent).
#
# Optional (used by R/analyse_like_dynamics.R and R/analyse_regimes.R; the
# defaults reproduce the original behaviour):
#  - extra_cols: further columns of the extract to copy into `s`;
#  - setup: function(con) run once after the `uid` table is created (e.g. to
#    register lookup tables keyed by uid);
#  - annotate_sql: a SELECT over `s0` (the raw bucket table, with uid) whose
#    result becomes `s` (e.g. to add the source of each play).
fr_bucket_loop <- function(streams_file, logic, fun, n_buckets = 32, user_subset = NULL,
                           extra_cols = character(), setup = NULL, annotate_sql = NULL){
  # Low budget: this runs on a 14 GB laptop with ~3 GB free; DuckDB spills
  # the window sorts to temp_directory.
  con <- duck_connect(memory_limit = "1500MB", threads = 2)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  subset_sql <- ""
  if(!is.null(user_subset)){
    duckdb::duckdb_register(con, "user_subset", tibble::tibble(hashed_id = user_subset))
    subset_sql <- "WHERE hashed_id IN (SELECT hashed_id FROM user_subset)"
  }
  DBI::dbExecute(con, sprintf("
    CREATE TEMP TABLE uid AS
    SELECT hashed_id, CAST(row_number() OVER (ORDER BY hashed_id) AS INTEGER) AS uid
    FROM (SELECT DISTINCT hashed_id FROM read_parquet('%s/*.parquet') %s)", streams_file, subset_sql))
  if(!is.null(setup)) setup(con)
  kept <- fr_kept_sql(logic)
  cols <- paste0("s.", c("ts_listen", "song_id", "click_loved", "context_4", extra_cols), collapse = ", ")
  out <- vector("list", n_buckets)
  for(b in seq_len(n_buckets) - 1){
    DBI::dbExecute(con, "DROP TABLE IF EXISTS s; DROP TABLE IF EXISTS s0; DROP TABLE IF EXISTS uf;")
    DBI::dbExecute(con, sprintf("
      CREATE TEMP TABLE %s AS
      SELECT uid.uid, %s
      FROM read_parquet('%s/*.parquet') s JOIN uid USING (hashed_id)
      WHERE uid.uid %% %d = %d", ifelse(is.null(annotate_sql), "s", "s0"), cols, streams_file, n_buckets, b))
    if(!is.null(annotate_sql)){
      DBI::dbExecute(con, paste("CREATE TEMP TABLE s AS", annotate_sql))
      DBI::dbExecute(con, "DROP TABLE s0")
    }
    DBI::dbExecute(con, "
      CREATE TEMP TABLE uf AS SELECT uid, min(ts_listen) AS user_first FROM s GROUP BY uid")
    out[[b + 1]] <- fun(con, kept, b)
  }
  out
}

# The full table has ~65M rows (user x song); load it in R only if needed:
# arrow::open_dataset(tar_read(favorites_replayed_file)) %>% filter(...) %>% collect()
fr_glob <- function(dir) paste0(dir, "/*.parquet")

# Run an aggregation query on a favorites_replayed directory (table `fr`)
fr_query <- function(dir, sql){
  con <- duck_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, sprintf("CREATE VIEW fr AS SELECT * FROM read_parquet('%s')", fr_glob(dir)))
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

# Key like-at-first-sight numbers, computed in DuckDB from the parquet files
# (one per logic), overall and by context of first play.
summarise_like_at_first_sight <- function(files, filename = "output/like_at_first_sight_summary.csv"){
  one <- function(dir, logic){
    q <- function(where, label){
      fr_query(dir, sprintf("
        SELECT '%s' AS logic, '%s' AS subset,
          count(*) AS n_user_songs,
          count(nlisten_at_clicked_love) AS n_liked,
          avg(CASE WHEN nlisten_at_clicked_love IS NOT NULL THEN 1.0 ELSE 0 END) AS share_songs_liked,
          avg(CASE WHEN nlisten_at_clicked_love = 1 THEN 1.0 WHEN nlisten_at_clicked_love > 1 THEN 0 END) AS share_likes_at_play1,
          avg(CASE WHEN nlisten_at_clicked_love <= 5 THEN 1.0 WHEN nlisten_at_clicked_love > 5 THEN 0 END) AS share_likes_by_play5,
          median(nlisten_at_clicked_love) AS median_play_rank_at_like,
          median(nlisten_after_clicked_love) AS median_replays_after_like,
          avg(nlisten_after_clicked_love) AS mean_replays_after_like,
          median(nlisten_after_discovery) FILTER (WHERE nlisten_at_clicked_love IS NOT NULL) AS median_plays_liked,
          median(nlisten_after_discovery) FILTER (WHERE nlisten_at_clicked_love IS NULL) AS median_plays_never_liked,
          avg(nlisten_after_discovery) FILTER (WHERE nlisten_at_clicked_love IS NOT NULL) AS mean_plays_liked,
          avg(nlisten_after_discovery) FILTER (WHERE nlisten_at_clicked_love IS NULL) AS mean_plays_never_liked,
          median(nlisten_after_discovery) FILTER (WHERE nlisten_at_clicked_love = 1) AS median_plays_liked_at_play1,
          median(nlisten_after_clicked_love) FILTER (WHERE nlisten_at_clicked_love = 1) AS median_replays_after_like_at_play1
        FROM fr %s", logic, label, where))
    }
    bind_rows(
      q("", "all songs"),
      q("WHERE context_at_discovery IN ('organic', 'edito', 'reco_algo')", "3 contexts (as in figures)"),
      q("WHERE context_at_discovery = 'organic'", "first play: organic"),
      q("WHERE context_at_discovery = 'edito'", "first play: editorial"),
      q("WHERE context_at_discovery = 'reco_algo'", "first play: algorithmic")
    )
  }
  res <- purrr::imap(files, one) %>% bind_rows()
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(res, filename)
  return(filename)
}

# Plots: aggregation is done in DuckDB, only count tables come back to R.
# Same figures as before (counts are passed as weights).
plot_like_at_first_sight <- function(favorites_replayed_file, 
                                     .what = c("raw", "by_device"),
                                     .zoomed = FALSE, .suffix = ""){
  set_ggplot_options()
  options(scipen = 99)
  .what = .what[1]
  d <- fr_query(favorites_replayed_file, "
    SELECT context_at_discovery, least(nlisten_at_clicked_love, 500) AS nlisten_at_clicked_love, count(*) AS n
    FROM fr
    WHERE nlisten_at_clicked_love IS NOT NULL 
      AND context_at_discovery IN ('edito', 'organic', 'reco_algo')
    GROUP BY ALL") %>% 
    mutate(n = as.numeric(n), nlisten_at_clicked_love = as.integer(nlisten_at_clicked_love),
           context_at_discovery2 = recode_device(context_at_discovery))
  if(.what == "raw"){
    if(.zoomed){
      gg <- d %>% 
        filter(nlisten_at_clicked_love <= 10) %>% 
        ggplot(aes(nlisten_at_clicked_love, weight = n)) +
        geom_bar() +
#        scale_y_log10(label = scales::comma, breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000)) +
        labs(x = "No. play before clicked like button", y = "") +
        scale_x_continuous(breaks = 1:10)
    } else if(!.zoomed){
      gg <- d %>% 
        ggplot(aes(nlisten_at_clicked_love, weight = n)) +
        geom_histogram() +
        scale_y_log10(label = scales::comma, breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000)) +
        labs(x = "No. play before clicked like button", y = "")
    }
  } else if(.what == "by_device"){
    gg <- d %>% 
      count(context_at_discovery2, nlisten_at_clicked_love, wt = n) %>% 
      group_by(context_at_discovery2) %>% 
      mutate(f = n / sum(n)) %>% 
      ungroup() %>% 
      filter(nlisten_at_clicked_love < 11) %>% 
      ggplot(aes(nlisten_at_clicked_love, f, fill = context_at_discovery2)) +
        geom_col(position = "dodge") +
        labs(x = "No. play before clicked like button", y = "", fill = "Context of first play") +
        scale_x_continuous(breaks = 1:10)
  }
  zoomed_char <- ifelse(.zoomed, "zoomed", "unzoomed")
  filename <- str_glue("output/like_at_first_sight_{.what}_{zoomed_char}{.suffix}.png")
  ggsave(filename, gg)
  return(filename)
}

plot_replay_after_loved <- function(favorites_replayed_file){
  set_ggplot_options()
  d <- fr_query(favorites_replayed_file, "
    SELECT context_at_discovery,
      CASE WHEN nlisten_at_clicked_love IS NULL THEN 'Never'
           WHEN nlisten_at_clicked_love = 1 THEN 'At first sight'
           ELSE 'Later' END AS liked,
      least(nlisten_after_discovery, 500) AS nlisten_after_discovery,
      count(*) AS n
    FROM fr
    WHERE context_at_discovery IN ('edito', 'organic', 'reco_algo')
    GROUP BY ALL") %>% 
    mutate(n = as.numeric(n),
           liked = factor(liked, levels = c("Never", "At first sight", "Later")),
           context_at_discovery2 = recode_device(context_at_discovery)) %>% 
    group_by(context_at_discovery2, liked) %>% 
    mutate(f = n/sum(n))
  gg <- d %>% 
    filter(nlisten_after_discovery < 30) %>% 
    ggplot(aes(nlisten_after_discovery, f)) +
      geom_col() +
      facet_grid(liked ~ context_at_discovery2, scale="free_y", switch = "y") +
      labs(x = "Total number of play", y = "Liked...") +
      theme()
  
  filename <- str_glue("output/replay_after_loved.png")
  ggsave(filename, gg)
  return(filename)
}

# Observation window and context of plays ------
# Overview of the local stream extract: window start/end (epoch seconds and
# dates), number of plays and users, share of plays by context_4. The window
# end is global (the date of the data export, the same for all users): it is
# used for the right-censoring corrections below.
make_streams_overview <- function(streams_file){
  con <- duck_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  g <- sprintf("read_parquet('%s/*.parquet')", streams_file)
  a <- DBI::dbGetQuery(con, sprintf("
    SELECT CAST(min(ts_listen) AS DOUBLE) AS window_start_ts, CAST(max(ts_listen) AS DOUBLE) AS window_end_ts,
      CAST(count(*) AS DOUBLE) AS n_plays, CAST(count(DISTINCT hashed_id) AS DOUBLE) AS n_users,
      CAST(sum(click_loved) AS DOUBLE) AS n_like_clicks
    FROM %s", g))
  ctx <- DBI::dbGetQuery(con, sprintf("
    SELECT coalesce(CAST(context_4 AS VARCHAR), 'NA') AS context_4, CAST(count(*) AS DOUBLE) AS n
    FROM %s GROUP BY ALL", g))
  bind_rows(
    tibble(name = names(a), value = as.numeric(unlist(a[1, ]))),
    tibble(name = paste0("share_plays_context_", ctx$context_4), value = ctx$n / sum(ctx$n))
  ) %>% 
    mutate(label = case_when(str_detect(name, "_ts$") ~ format(as.POSIXct(value, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"),
                             TRUE ~ NA_character_))
}

get_window_end <- function(streams_overview){
  streams_overview$value[streams_overview$name == "window_end_ts"]
}

# Right-censoring of replays and likes ------
# Songs first played (or liked) near the end of the observation window have
# less time to be replayed, so replay counts after a like, and total plays of
# liked vs never-liked songs, are biased downwards. Like at first sight is
# affected too: a song first played late can only be liked after few plays
# (later likes fall outside the window), which inflates the share of likes at
# play 1. Variants (H = horizon in months, E = window end):
#  - none: no correction (as in summarise_like_at_first_sight()).
#  - exclude_first_play_last_Hm: drop songs first played after E - H. All
#    metrics, plays counted until E.
#  - exclude_like_last_Hm: replay metrics only, on likes given before E - H
#    (plays counted until E). Like-at-first-sight metrics are NA (dropping
#    recent likes but not recent non-likes would bias them).
#  - fixed_horizon_Hm: equal follow-up for everyone. Like at first sight:
#    songs first played before E - H, "liked" = liked within H of the first
#    play. Replays: plays within H after the like, for likes given before
#    E - H. Plays liked vs not liked: plays within H of the first play, songs
#    first played before E - H, liked within H vs not liked within H.
# The left-censoring fix (logic = "fixed") is applied upstream in all variants.
# `main_variant` is flagged as the estimate to report (PI to confirm).
summarise_ltfs_censoring <- function(dir, window_end, horizons_months = c(6, 12),
                                     main_variant = "fixed_horizon_12m",
                                     filename = "output/like_at_first_sight_censoring.csv"){
  E <- as.numeric(window_end)
  hz <- horizon_seconds(horizons_months)
  subsets <- c("all songs" = "TRUE",
               "3 contexts (as in figures)" = "context_at_discovery IN ('organic', 'edito', 'reco_algo')",
               "first play: organic" = "context_at_discovery = 'organic'",
               "first play: editorial" = "context_at_discovery = 'edito'",
               "first play: algorithmic" = "context_at_discovery = 'reco_algo'")
  liked <- "nlisten_at_clicked_love IS NOT NULL"
  v <- list(none = list(S = "TRUE", L = liked, R = liked,
                        RE = "nlisten_after_clicked_love", P = "nlisten_after_discovery", ltfs = TRUE, h = NA))
  for(h in names(hz)){
    cut <- sprintf("%.0f", E - hz[[h]])
    v[[str_glue("exclude_first_play_last_{h}m")]] <- 
      list(S = str_glue("ts_first_play <= {cut}"), L = liked, R = str_glue("{liked} AND ts_first_play <= {cut}"),
           RE = "nlisten_after_clicked_love", P = "nlisten_after_discovery", ltfs = TRUE, h = as.numeric(h))
    v[[str_glue("exclude_like_last_{h}m")]] <- 
      list(S = "TRUE", L = liked, R = str_glue("ts_clicked_love <= {cut}"),
           RE = "nlisten_after_clicked_love", P = "nlisten_after_discovery", ltfs = FALSE, h = as.numeric(h))
    v[[str_glue("fixed_horizon_{h}m")]] <- 
      list(S = str_glue("ts_first_play <= {cut}"), 
           L = sprintf("ts_clicked_love <= ts_first_play + %.0f", hz[[h]]),
           R = str_glue("ts_clicked_love <= {cut}"),
           RE = str_glue("nlisten_{h}m_after_clicked_love"), P = str_glue("nlisten_{h}m_after_discovery"),
           ltfs = TRUE, h = as.numeric(h))
  }
  one <- function(x, variant, subset_label, subset_where){
    L <- sprintf("coalesce((%s), false)", x$L)
    r <- fr_query(dir, str_glue("
      SELECT
        count(*) FILTER (WHERE {x$S}) AS n_user_songs,
        count(*) FILTER (WHERE {x$S} AND {L}) AS n_liked,
        avg(CASE WHEN {L} THEN 1.0 ELSE 0 END) FILTER (WHERE {x$S}) AS share_songs_liked,
        avg(CASE WHEN nlisten_at_clicked_love = 1 THEN 1.0 ELSE 0 END) FILTER (WHERE {x$S} AND {L}) AS share_likes_at_play1,
        avg(CASE WHEN nlisten_at_clicked_love <= 5 THEN 1.0 ELSE 0 END) FILTER (WHERE {x$S} AND {L}) AS share_likes_by_play5,
        median(nlisten_at_clicked_love) FILTER (WHERE {x$S} AND {L}) AS median_play_rank_at_like,
        count(*) FILTER (WHERE {x$R}) AS n_likes_replays,
        median({x$RE}) FILTER (WHERE {x$R}) AS median_replays_after_like,
        avg({x$RE}) FILTER (WHERE {x$R}) AS mean_replays_after_like,
        median({x$P}) FILTER (WHERE {x$S} AND {L}) AS median_plays_liked,
        median({x$P}) FILTER (WHERE {x$S} AND NOT {L}) AS median_plays_not_liked,
        avg({x$P}) FILTER (WHERE {x$S} AND {L}) AS mean_plays_liked,
        avg({x$P}) FILTER (WHERE {x$S} AND NOT {L}) AS mean_plays_not_liked
      FROM fr WHERE {subset_where}")) %>% 
      mutate(across(everything(), as.numeric))
    if(!x$ltfs){
      r <- mutate(r, across(c(n_user_songs, n_liked, share_songs_liked, share_likes_at_play1, share_likes_by_play5,
                              median_play_rank_at_like, median_plays_liked, median_plays_not_liked,
                              mean_plays_liked, mean_plays_not_liked), ~NA_real_))
    }
    tibble(variant = variant, horizon_months = x$h, subset = subset_label) %>% bind_cols(r)
  }
  res <- purrr::imap(v, function(x, variant){
    purrr::imap(subsets, function(w, lab) one(x, variant, lab, w)) %>% bind_rows()
  }) %>% bind_rows() %>% 
    mutate(main = variant == main_variant,
           window_end = format(as.POSIXct(E, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"))
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(res, filename)
  res
}

# Context of plays around the like ------
# For liked songs (left-censoring logic "fixed"), counts of plays by context_4
# and rank relative to the like (rel_rank = play rank - like rank, from
# -max_rel to +max_rel; 0 = the play at which the like was clicked), separately
# for likes at first sight and later likes. Used for "after the like, the song
# leaves the algorithm" (share of recommended plays before vs after the like).
make_context_around_like <- function(streams_file, max_rel = 10L, n_buckets = 32, user_subset = NULL){
  res <- fr_bucket_loop(streams_file, "fixed", n_buckets = n_buckets, user_subset = user_subset,
                        fun = function(con, kept, b){
    DBI::dbGetQuery(con, sprintf("
      WITH kept AS (%s),
      p AS (
        SELECT uid, song_id, context_4, click_loved,
          row_number() OVER (PARTITION BY uid, song_id ORDER BY ts_listen) AS n
        FROM kept
      ),
      lr AS (
        SELECT uid, song_id, min(n) FILTER (WHERE click_loved = 1) AS like_rank
        FROM p GROUP BY uid, song_id
      )
      SELECT CASE WHEN lr.like_rank = 1 THEN 'at first sight' ELSE 'later' END AS like_type,
        CAST(p.n - lr.like_rank AS INTEGER) AS rel_rank,
        coalesce(CAST(p.context_4 AS VARCHAR), 'NA') AS context_4,
        CAST(count(*) AS DOUBLE) AS n
      FROM p JOIN lr USING (uid, song_id)
      WHERE lr.like_rank IS NOT NULL AND p.n - lr.like_rank BETWEEN -%d AND %d
      GROUP BY ALL", kept, max_rel, max_rel))
  })
  bind_rows(res) %>% 
    group_by(like_type, rel_rank, context_4) %>% 
    summarize(n = sum(n), .groups = "drop") %>% 
    group_by(like_type, rel_rank) %>% 
    mutate(share = n / sum(n)) %>% 
    ungroup() %>% 
    as_tibble()
}

plot_context_around_like <- function(context_around_like, filename = "output/context_around_like.png"){
  set_ggplot_options()
  d <- context_around_like %>% 
    filter(context_4 %in% c("organic", "edito", "reco_algo")) %>% 
    mutate(context = recode_device(context_4))
  gg <- ggplot(d, aes(rel_rank, share, colour = context)) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_line() + geom_point() +
    facet_wrap(~like_type) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Play rank relative to the like (0 = play at which the song is liked)",
         y = "Share of plays", colour = "Context of play")
  ggsave(filename, gg, width = 9, height = 4.5)
  filename
}
