# Genre diversity of personal playlists, by playlist type ------
# Question: are mood (and other context-/user-centered) playlists
# genre-homogeneous or genre-mixed, compared with genre playlists?
#
# Sources (all on S3, bucket scoavoux). ASSUMPTIONS, to confirm:
#  - Playlist contents: records_w3/playlists_data/RECORDS_hashed_user_playlists.parquet,
#    column tracks.list[].element = Deezer song_id (as in make_tracklist_data()).
#  - Song -> album and main artist: records_w3/items/song.snappy.parquet (newer)
#    and records_w3/items/songs.snappy.parquet (older), columns song_id,
#    album_id, artist_id; the newer file has priority, as in the mauvaisgenre
#    data paper (data_paper/R/make_songs_likes.R).
#  - Genre: Deezer genre of the ALBUM (Deezer has no track-level genre; the
#    album genre is what the Deezer interface shows), from
#    interim/prod/genres_from_albums.parquet (album_id, artist_id, genre_id,
#    record_type, fans) + interim/dict/deezer_genre_mapping.csv (genre_id,
#    genre), as built for the mauvaisgenre data paper (data_paper/R/make_genre.R).
#    If an album has several rows/genres, the one with the smallest genre_id is
#    kept (arbitrary but deterministic; n_album_genres records how often).
#  - Fallback when the album has no genre: the main genre of the track's main
#    artist (genre with the largest sum of log(fans + 1) over the artist's
#    non-compilation albums; simplified from load_dz_genre_album()).
#    genre_source records "album" / "artist" / NA per track.
# The column names are checked at run time (DESCRIBE): the function stops with
# an explicit message if one is missing.
# Diversity rises mechanically with the number of tracks: the summaries are
# restricted to playlists with min_tracks..max_tracks tracks and enough genre
# coverage, and size-robust indices are reported (Hurlbert's PIE = probability
# that two tracks drawn without replacement differ in genre; rarefied genre
# richness at 10 tracks; Shannon entropy normalised by its maximum given size).

genre_sources <- list(
  songs_new = "records_w3/items/song.snappy.parquet",
  songs_old = "records_w3/items/songs.snappy.parquet",
  album_genres = "interim/prod/genres_from_albums.parquet",
  genre_mapping = "interim/dict/deezer_genre_mapping.csv"
)

# DuckDB: personal playlists -> tracks -> album/artist genre. Writes two local
# parquet files: per playlist x genre track counts, and per-playlist counts
# (tracks, tracks found in the song tables, tracks with a genre, genre source,
# distinct artists). Returns the directory.
make_playlist_genre_counts <- function(playlists, path = "data/temp/playlist_genres",
                                       sources = genre_sources){
  unlink(path, recursive = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  con <- duck_connect(memory_limit = "1500MB", threads = 2)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "SET http_timeout = 60000; SET http_retries = 5;")
  cols_of <- function(src) DBI::dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM %s", src))$column_name
  need <- function(src, cols, label){
    miss <- setdiff(cols, cols_of(src))
    if(length(miss)) stop(label, ": missing column(s) ", paste(miss, collapse = ", "),
                          ". Check the genre source assumptions in R/analyse_genre_diversity.R")
  }
  src_songs_new <- sprintf("read_parquet('%s')", s3_uri(sources$songs_new))
  src_songs_old <- sprintf("read_parquet('%s')", s3_uri(sources$songs_old))
  src_alb <- sprintf("read_parquet('%s')", s3_uri(sources$album_genres))
  src_map <- sprintf("read_csv('%s', header = true)", s3_uri(sources$genre_mapping))
  need(src_songs_new, c("song_id", "album_id", "artist_id"), sources$songs_new)
  need(src_songs_old, c("song_id", "album_id", "artist_id"), sources$songs_old)
  need(src_alb, c("album_id", "artist_id", "genre_id", "record_type", "fans"), sources$album_genres)
  need(src_map, c("genre_id", "genre"), sources$genre_mapping)

  # Ids may be stored as BIGINT, DOUBLE or VARCHAR depending on the file:
  # normalise to a canonical string (5, 5.0 and '5' all become '5').
  DBI::dbExecute(con, "CREATE OR REPLACE TEMP MACRO normid(x) AS
    coalesce(CAST(TRY_CAST(x AS BIGINT) AS VARCHAR), CAST(x AS VARCHAR))")
  duckdb::duckdb_register(con, "pl_r",
                          dplyr::distinct(dplyr::select(filter_personal_playlists(playlists), playlist_id)))
  # Tracks of personal playlists (a song counted once per playlist)
  DBI::dbExecute(con, sprintf("
    CREATE TEMP TABLE pt AS
    SELECT DISTINCT playlist_id, normid(song_id) AS song_id FROM (
      SELECT p.playlist_id, unnest(p.tracks.list).element AS song_id
      FROM read_parquet('%s') p
      SEMI JOIN pl_r ON p.playlist_id = pl_r.playlist_id)
    WHERE song_id IS NOT NULL", s3_uri(playlists_key)))
  DBI::dbExecute(con, "CREATE TEMP TABLE ps AS SELECT DISTINCT song_id FROM pt")
  # Song -> album, main artist (newer file first)
  DBI::dbExecute(con, sprintf("
    CREATE TEMP TABLE songs AS
    SELECT song_id, arg_min(album_id, pri) AS album_id, arg_min(artist_id, pri) AS artist_id
    FROM (
      SELECT normid(song_id) AS song_id, normid(album_id) AS album_id,
             normid(artist_id) AS artist_id, 1 AS pri FROM %s
      UNION ALL
      SELECT normid(song_id), normid(album_id), normid(artist_id), 2 FROM %s
    ) x
    SEMI JOIN ps USING (song_id)
    GROUP BY song_id", src_songs_new, src_songs_old))
  # Album genres (with names), one genre per album
  DBI::dbExecute(con, sprintf("
    CREATE TEMP TABLE alb AS
    SELECT normid(a.album_id) AS album_id, normid(a.artist_id) AS artist_id,
           CAST(a.record_type AS VARCHAR) AS record_type, CAST(a.fans AS DOUBLE) AS fans,
           TRY_CAST(a.genre_id AS BIGINT) AS genre_id, CAST(g.genre AS VARCHAR) AS genre
    FROM %s a JOIN %s g ON normid(a.genre_id) = normid(g.genre_id)
    WHERE g.genre IS NOT NULL", src_alb, src_map))
  DBI::dbExecute(con, "
    CREATE TEMP TABLE album_genre AS
    SELECT album_id, arg_min(genre, genre_id) AS genre, count(DISTINCT genre) AS n_album_genres
    FROM alb GROUP BY album_id")
  DBI::dbExecute(con, "
    CREATE TEMP TABLE artist_genre AS
    SELECT artist_id, arg_max(genre, w) AS genre FROM (
      SELECT artist_id, genre, sum(ln(coalesce(fans, 0) + 1)) AS w
      FROM alb WHERE coalesce(record_type, '') <> 'compilation'
      GROUP BY artist_id, genre)
    GROUP BY artist_id")
  DBI::dbExecute(con, "
    CREATE TEMP TABLE tg AS
    SELECT pt.playlist_id, pt.song_id, s.artist_id, ag.n_album_genres,
      (s.song_id IS NOT NULL) AS in_song_table,
      coalesce(ag.genre, arg.genre) AS genre,
      CASE WHEN ag.genre IS NOT NULL THEN 'album' WHEN arg.genre IS NOT NULL THEN 'artist' END AS genre_source
    FROM pt
    LEFT JOIN songs s USING (song_id)
    LEFT JOIN album_genre ag ON s.album_id = ag.album_id
    LEFT JOIN artist_genre arg ON s.artist_id = arg.artist_id")
  DBI::dbExecute(con, sprintf("
    COPY (
      SELECT normid(playlist_id) AS playlist_id, genre, CAST(count(*) AS INTEGER) AS n
      FROM tg WHERE genre IS NOT NULL GROUP BY ALL
    ) TO '%s' (FORMAT parquet)", file.path(path, "playlist_genre_counts.parquet")))
  DBI::dbExecute(con, sprintf("
    COPY (
      SELECT normid(playlist_id) AS playlist_id,
        CAST(count(*) AS INTEGER) AS n_tracks,
        CAST(count(*) FILTER (WHERE in_song_table) AS INTEGER) AS n_tracks_in_song_table,
        CAST(count(genre) AS INTEGER) AS n_tracks_with_genre,
        CAST(count(*) FILTER (WHERE genre_source = 'album') AS INTEGER) AS n_tracks_album_genre,
        CAST(count(*) FILTER (WHERE genre_source = 'artist') AS INTEGER) AS n_tracks_artist_genre,
        CAST(count(*) FILTER (WHERE n_album_genres > 1) AS INTEGER) AS n_tracks_multi_genre_album,
        CAST(count(DISTINCT artist_id) AS INTEGER) AS n_artists
      FROM tg GROUP BY playlist_id
    ) TO '%s' (FORMAT parquet)", file.path(path, "playlist_track_counts.parquet")))
  # Number of genres in the mapping (for normalised entropy)
  n_genres <- DBI::dbGetQuery(con, "SELECT count(DISTINCT genre) AS n FROM alb")$n
  writeLines(as.character(n_genres), file.path(path, "n_genres_total.txt"))
  return(path)
}

# Rarefied richness: expected number of distinct genres among m tracks drawn
# without replacement from a playlist with genre counts n (Hurlbert 1971).
rarefied_richness <- function(n, m){
  N <- sum(n)
  if(N < m) return(NA_real_)
  sum(1 - exp(lchoose(N - n, m) - lchoose(N, m)))
}

# Per-playlist diversity indices (one row per personal playlist with >= 1
# track in the playlist file).
compute_playlist_genre_diversity <- function(dir, rarefy_m = 10L){
  counts <- arrow::read_parquet(file.path(dir, "playlist_genre_counts.parquet"))
  tracks <- arrow::read_parquet(file.path(dir, "playlist_track_counts.parquet"))
  G <- as.numeric(readLines(file.path(dir, "n_genres_total.txt")))
  div <- counts %>%
    group_by(playlist_id) %>%
    summarize(
      n_genres = n(),
      N = sum(n),
      shannon = { p <- n / sum(n); -sum(p * log(p)) },
      gini_simpson = 1 - sum((n / sum(n))^2),
      # Hurlbert's PIE: unbiased with respect to playlist size
      pie = ifelse(sum(n) > 1, sum(n) / (sum(n) - 1) * (1 - sum((n / sum(n))^2)), NA_real_),
      dominant_share = max(n) / sum(n),
      dominant_genre = genre[which.max(n)],
      rarefied_richness = rarefied_richness(n, rarefy_m),
      .groups = "drop") %>%
    mutate(shannon_norm = ifelse(pmin(N, G) > 1, shannon / log(pmin(N, G)), NA_real_)) %>%
    select(-N)
  tracks %>%
    left_join(div, by = "playlist_id") %>%
    mutate(genre_coverage = n_tracks_with_genre / n_tracks)
}

# Join with categories: by label (multi-label: a playlist counts in every label
# it carries), single-label playlists only, and families. Filters on size and
# genre coverage. Returns list(summary, models, data_for_plot).
summarise_genre_diversity_by_type <- function(playlist_genre_diversity, playlists,
                                              min_tracks = 10L, max_tracks = 200L,
                                              min_coverage = 0.8,
                                              filename = "output/genre_diversity_by_type.csv"){
  an_cols <- names(playlists)[str_detect(names(playlists), "^an_")]
  pl <- filter_personal_playlists(playlists) %>%
    filter(if_any(all_of(an_cols), ~!is.na(.x))) %>%
    select(playlist_id, all_of(an_cols)) %>%
    # playlist ids are VARCHAR in the genre files (avoids integer64 mismatches)
    mutate(playlist_id = as.character(playlist_id)) %>%
    mutate(n_labels = rowSums(across(all_of(an_cols)) == 1, na.rm = TRUE))
  d <- playlist_genre_diversity %>%
    inner_join(pl, by = "playlist_id") %>%
    mutate(in_sample = n_tracks >= min_tracks & n_tracks <= max_tracks &
             !is.na(genre_coverage) & genre_coverage >= min_coverage)
  long <- d %>%
    filter(in_sample) %>%
    pivot_longer(all_of(an_cols), names_to = "category", values_to = "value") %>%
    filter(value == 1) %>%
    mutate(category = str_remove(category, "^an_"))
  nolabel <- d %>% filter(in_sample, n_labels == 0) %>% mutate(category = "(no label)")
  stats <- function(x){
    x %>% summarize(n_playlists = n(),
                    median_n_tracks = median(n_tracks),
                    median_n_artists = median(n_artists),
                    median_n_genres = median(n_genres), mean_n_genres = mean(n_genres),
                    median_rarefied_richness = median(rarefied_richness, na.rm = TRUE),
                    mean_rarefied_richness = mean(rarefied_richness, na.rm = TRUE),
                    median_shannon = median(shannon), mean_shannon = mean(shannon),
                    median_shannon_norm = median(shannon_norm, na.rm = TRUE),
                    median_gini_simpson = median(gini_simpson),
                    median_pie = median(pie, na.rm = TRUE), mean_pie = mean(pie, na.rm = TRUE),
                    median_dominant_share = median(dominant_share), mean_dominant_share = mean(dominant_share),
                    share_dominant_ge_80 = mean(dominant_share >= .8),
                    share_single_genre = mean(n_genres == 1),
                    .groups = "drop")
  }
  by_label <- bind_rows(long, nolabel) %>%
    group_by(category) %>% stats() %>% mutate(subset = "all labelled playlists (multi-label)")
  by_label_single <- bind_rows(filter(long, n_labels == 1), nolabel) %>%
    group_by(category) %>% stats() %>% mutate(subset = "single-label playlists")
  by_family <- long %>%
    mutate(category = paste0("(family) ", recode_playlists(category, .what = "large_cat"))) %>%
    distinct(playlist_id, category, .keep_all = TRUE) %>%
    group_by(category) %>% stats() %>% mutate(subset = "all labelled playlists (multi-label)")
  overall <- d %>% filter(in_sample) %>% stats() %>% mutate(category = "(all in sample)", subset = "all")
  coverage <- tibble(
    category = "(coverage)", subset = "all personal annotated playlists",
    n_playlists = nrow(d),
    share_in_sample = mean(d$in_sample),
    share_tracks_in_song_table = sum(d$n_tracks_in_song_table) / sum(d$n_tracks),
    share_tracks_with_genre = sum(d$n_tracks_with_genre) / sum(d$n_tracks),
    share_tracks_artist_fallback = sum(d$n_tracks_artist_genre) / sum(d$n_tracks_with_genre),
    share_tracks_multi_genre_album = sum(d$n_tracks_multi_genre_album) / sum(d$n_tracks_album_genre))
  summary <- bind_rows(overall, by_label, by_label_single, by_family, coverage) %>%
    mutate(min_tracks = min_tracks, max_tracks = max_tracks, min_coverage = min_coverage) %>%
    relocate(subset, category)

  # Models on single-label playlists (reference = genre), controlling for size
  ms <- bind_rows(filter(long, n_labels == 1)) %>%
    mutate(category = relevel(factor(category), ref = "genre"))
  fit <- function(y) {
    m <- lm(as.formula(str_glue("{y} ~ category + log(n_tracks)")), data = ms)
    cf <- summary(m)$coefficients
    tibble(outcome = y, term = rownames(cf), estimate = cf[, 1], se = cf[, 2], p = cf[, 4], n_obs = nobs(m))
  }
  models <- bind_rows(fit("pie"), fit("dominant_share"), fit("shannon_norm"), fit("rarefied_richness"))

  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(summary, filename)
  write_csv(models, str_replace(filename, "\\.csv$", "_models.csv"))
  list(summary = summary, models = models,
       params = list(min_tracks = min_tracks, max_tracks = max_tracks, min_coverage = min_coverage),
       plot_data = bind_rows(mutate(long, subset = "All labelled playlists (multi-label)"),
                             mutate(filter(long, n_labels == 1), subset = "Single-label playlists")) %>%
         select(subset, playlist_id, category, n_tracks, pie, dominant_share, shannon_norm, rarefied_richness))
}

plot_genre_diversity_by_type <- function(genre_div, filename = "output/genre_diversity_by_type.png"){
  set_ggplot_options()
  d <- genre_div$plot_data %>%
    select(subset, category, `Dominant genre share` = dominant_share,
           `Genre diversity (Hurlbert's PIE)` = pie) %>%
    pivot_longer(-c(subset, category), names_to = "measure") %>%
    filter(!is.na(value)) %>%
    mutate(family = recode_playlists(category, .what = "large_cat"),
           category = recode_playlists(category, .what = "category"),
           category = fct_rev(category))
  gg <- ggplot(d, aes(value, category, fill = family)) +
    geom_boxplot(outlier.shape = NA) +
    facet_grid(subset ~ measure, scales = "free_x") +
    labs(x = "", y = "", fill = "",
         caption = with(genre_div$params, str_glue(
           "Personal playlists with {min_tracks}-{max_tracks} tracks and >= {100 * min_coverage}% of tracks with a genre (Deezer album genre)."))) +
    theme(legend.position = "bottom")
  ggsave(filename, gg, width = 10, height = 7)
  filename
}
