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
                                         n_buckets = 32){
  logic <- match.arg(logic)
  if(is.null(path)) path <- str_glue("data/temp/favorites_replayed_{logic}")
  unlink(path, recursive = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  # Low budget: this runs on a 14 GB laptop with ~3 GB free; DuckDB spills
  # the window sorts to temp_directory.
  con <- duck_connect(memory_limit = "1500MB", threads = 2)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  subset_sql <- ""
  if(!is.null(user_subset)){
    duckdb::duckdb_register(con, "user_subset", tibble::tibble(hashed_id = user_subset))
    subset_sql <- "WHERE hashed_id IN (SELECT hashed_id FROM user_subset)"
  }
  # Integer user ids make sorts and hash tables much lighter than 64-char
  # hashes; users are processed in buckets to bound memory (all computations
  # are within-user, so buckets are independent).
  DBI::dbExecute(con, sprintf("
    CREATE TEMP TABLE uid AS 
    SELECT hashed_id, CAST(row_number() OVER (ORDER BY hashed_id) AS INTEGER) AS uid
    FROM (SELECT DISTINCT hashed_id FROM read_parquet('%s/*.parquet') %s)", streams_file, subset_sql))
  if(logic == "old"){
    kept <- "
      SELECT s.* FROM s JOIN uf USING (uid)
      WHERE s.ts_listen > uf.user_first + 31557600"
  } else {
    kept <- "
      SELECT s.* FROM s
      SEMI JOIN (
        SELECT sf.uid, sf.song_id 
        FROM (SELECT uid, song_id, min(ts_listen) AS song_first FROM s GROUP BY uid, song_id) sf
        JOIN uf USING (uid)
        WHERE sf.song_first > uf.user_first + 31557600
      ) k USING (uid, song_id)"
  }
  for(b in seq_len(n_buckets) - 1){
    DBI::dbExecute(con, "DROP TABLE IF EXISTS s; DROP TABLE IF EXISTS uf;")
    DBI::dbExecute(con, sprintf("
      CREATE TEMP TABLE s AS
      SELECT uid.uid, s.ts_listen, s.song_id, s.click_loved, s.context_4
      FROM read_parquet('%s/*.parquet') s JOIN uid USING (hashed_id)
      WHERE uid.uid %% %d = %d", streams_file, n_buckets, b))
    DBI::dbExecute(con, "
      CREATE TEMP TABLE uf AS SELECT uid, min(ts_listen) AS user_first FROM s GROUP BY uid")
    DBI::dbExecute(con, sprintf("
      COPY (
        WITH kept AS (%s),
        p AS (
          SELECT uid, song_id, click_loved, context_4,
            row_number() OVER (PARTITION BY uid, song_id ORDER BY ts_listen) AS n,
            sum(click_loved) OVER (PARTITION BY uid, song_id ORDER BY ts_listen
                                   ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS scl
          FROM kept
        ),
        agg AS (
          SELECT uid, song_id,
            CAST(count(*) AS INTEGER) AS nlisten_after_discovery,
            first(context_4 ORDER BY n) AS context_at_discovery,
            CASE WHEN max(scl) > 0 THEN CAST(count(*) FILTER (WHERE scl > 0) - 1 AS INTEGER) END
              AS nlisten_after_clicked_love,
            CAST(min(n) FILTER (WHERE click_loved = 1) AS INTEGER) AS nlisten_at_clicked_love,
            first(context_4 ORDER BY n) FILTER (WHERE click_loved = 1) AS context_at_clicked_love
          FROM p GROUP BY uid, song_id
        )
        SELECT uid.hashed_id, agg.* EXCLUDE (uid)
        FROM agg JOIN uid USING (uid)
      ) TO '%s' (FORMAT parquet, COMPRESSION zstd)", kept, file.path(path, sprintf("bucket_%02d.parquet", b))))
  }
  return(as.character(path))
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
        scale_y_log10(label = scales::comma, breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000)) +
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
