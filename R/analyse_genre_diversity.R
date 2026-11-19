# Genre diversity of personal playlists, by playlist type ------
# Question: are mood (and other context-/user-centered) playlists
# genre-homogeneous or genre-mixed, compared with genre playlists?
#
# Revised 2026-11-19 (branch regimes-genre-v2): genre at the SONG level (PI).
# Sources (all on S3, bucket scoavoux):
#  - Playlist contents: records_w3/playlists_data/RECORDS_hashed_user_playlists.parquet,
#    column tracks.list[].element = Deezer song_id (as in make_tracklist_data()).
#  - Item tables (Deezer's own metadata): records_w3/items/song.snappy.parquet
#    (newer, priority) and records_w3/items/songs.snappy.parquet (older): per
#    song_id, album_id, artist_id, main_genre (VARCHAR, ~32 values: pop,
#    hip_hop, electronic, rock, french_music...) and tag_labels
#    (STRUCT(list STRUCT(element VARCHAR)[]): finer style tags, plus decade and
#    two-letter language tags). records_w3/items/album.snappy.parquet and
#    artist.snappy.parquet have main_genre / tag_labels at their level. For
#    each field the newer song file has priority, field by field (a NULL in the
#    newer file falls back to the older one).
#  - Previous genre (kept as a comparison): Deezer genre of the ALBUM, from
#    interim/prod/genres_from_albums.parquet + interim/dict/deezer_genre_mapping.csv
#    (mauvaisgenre data paper, data_paper/R/make_genre.R); smallest genre_id if
#    several; fallback = main genre of the main artist (largest sum of
#    log(fans + 1) over the artist's non-compilation albums).
#
# Genre definitions (column genre_def in the outputs):
#  - main             : item-table vocabulary only: song main_genre, else the
#                       item album main_genre, else the item artist main_genre
#                       (genre_source_main = song / album_items / artist_items).
#                       The Deezer album-genre vocabulary is NOT mixed in: mixing
#                       two vocabularies inside one playlist would inflate
#                       diversity (only ~10% of songs share their album's
#                       genre label across the two vocabularies).
#  - song_only        : song main_genre only (coverage computed on these tracks).
#  - style_tags       : song tag_labels minus decade tags (^[0-9]{2}s$,
#                       ^[0-9]{4}s$) and two-letter language tags (^[a-z]{2}$);
#                       see style_tag_drop_regex. Each track gets ONE style tag:
#                       its most frequent style tag, frequency = number of
#                       distinct songs of the respondents' personal playlists
#                       carrying it (ties: alphabetical). Chosen over a
#                       per-track tag-share split because tags are probably
#                       nested (e.g. rock + alternative_rock): splitting would
#                       make even a one-style playlist look diverse. One tag per
#                       track also keeps integer counts, hence the same indices
#                       (PIE, rarefied richness) as the other definitions.
#                       output/genre_style_tags_vocabulary.csv lists all tags
#                       with counts and whether they are dropped: CHECK IT and
#                       refine style_tag_drop_regex if needed.
#  - deezer_album_old : previous definition (Deezer album genre, artist fallback).
# For information, genre_source_chain records, per track, the first source that
# has a genre in the order song / album_items / album_deezer / artist (Deezer
# artist fallback); its shares are reported in the coverage rows.
#
# Diversity rises mechanically with the number of tracks: the summaries are
# restricted to playlists with min_tracks..max_tracks tracks and enough genre
# coverage (per definition), and size-robust indices are reported (Hurlbert's
# PIE = probability that two tracks drawn without replacement differ in genre;
# rarefied genre richness at 10 tracks; Shannon entropy normalised by its
# maximum given size).
# MAIN measure (PI, 2026-11-18): Hurlbert's PIE, i.e. the size-unbiased version
# of 1 - HHI (penalises unbalanced genre shares, not only genre counts); reported
# with the effective number of genres 1/HHI for readability. Short playlists are
# mechanically homogeneous: playlists < min_tracks are excluded, and models
# control for log(n_tracks).

genre_sources <- list(
  songs_new = "records_w3/items/song.snappy.parquet",
  songs_old = "records_w3/items/songs.snappy.parquet",
  albums_items = "records_w3/items/album.snappy.parquet",
  artists_items = "records_w3/items/artist.snappy.parquet",
  album_genres = "interim/prod/genres_from_albums.parquet",
  genre_mapping = "interim/dict/deezer_genre_mapping.csv"
)
genre_defs <- c("main", "song_only", "style_tags", "deezer_album_old")
# Decade tags (10s, 90s, 2020s) and two-letter language tags (en, fr)
style_tag_drop_regex <- "^[0-9]{2}s$|^[0-9]{4}s$|^[a-z]{2}$"

# DuckDB: personal playlists -> tracks -> genre under each definition. Writes
# local parquet files: per playlist x genre_def x genre track counts, and
# per-playlist counts (tracks, tracks found in the song tables, tracks with a
# genre per definition and source, distinct artists); plus the genre
# vocabularies with counts. Returns the directory.
make_playlist_genre_counts <- function(playlists, path = "data/temp/playlist_genres",
                                       sources = genre_sources, drop_regex = style_tag_drop_regex,
                                       memory_limit = "1500MB", threads = 2L){
  unlink(path, recursive = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  con <- duck_connect(memory_limit = memory_limit, threads = threads)
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
  src_alb_items <- sprintf("read_parquet('%s')", s3_uri(sources$albums_items))
  src_art_items <- sprintf("read_parquet('%s')", s3_uri(sources$artists_items))
  src_alb <- sprintf("read_parquet('%s')", s3_uri(sources$album_genres))
  src_map <- sprintf("read_csv('%s', header = true)", s3_uri(sources$genre_mapping))
  need(src_songs_new, c("song_id", "album_id", "artist_id", "main_genre", "tag_labels"), sources$songs_new)
  need(src_songs_old, c("song_id", "album_id", "artist_id", "main_genre", "tag_labels"), sources$songs_old)
  need(src_alb_items, c("album_id", "main_genre"), sources$albums_items)
  need(src_art_items, c("artist_id", "main_genre"), sources$artists_items)
  need(src_alb, c("album_id", "artist_id", "genre_id", "record_type", "fans"), sources$album_genres)
  need(src_map, c("genre_id", "genre"), sources$genre_mapping)

  # Ids may be stored as BIGINT, DOUBLE or VARCHAR depending on the file:
  # normalise to a canonical string (5, 5.0 and '5' all become '5').
  DBI::dbExecute(con, "CREATE OR REPLACE TEMP MACRO normid(x) AS
    coalesce(CAST(TRY_CAST(x AS BIGINT) AS VARCHAR), CAST(x AS VARCHAR))")
  # Empty strings count as missing genres
  DBI::dbExecute(con, "CREATE OR REPLACE TEMP MACRO nz(x) AS nullif(trim(CAST(x AS VARCHAR)), '')")
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
  # Song -> album, main artist, main_genre, tags (newer file first, field by field)
  item_song <- function(src, pri) sprintf("
    SELECT normid(song_id) AS song_id, normid(album_id) AS album_id, normid(artist_id) AS artist_id,
      nz(main_genre) AS main_genre,
      list_transform(tag_labels.list, lambda x: lower(trim(x.element))) AS tags, %d AS pri
    FROM %s", pri, src)
  DBI::dbExecute(con, sprintf("
    CREATE TEMP TABLE songs AS
    SELECT song_id,
      arg_min(album_id, pri) FILTER (WHERE album_id IS NOT NULL) AS album_id,
      arg_min(artist_id, pri) FILTER (WHERE artist_id IS NOT NULL) AS artist_id,
      arg_min(main_genre, pri) FILTER (WHERE main_genre IS NOT NULL) AS main_genre,
      arg_min(tags, pri) FILTER (WHERE len(tags) > 0) AS tags
    FROM (%s UNION ALL %s) x
    SEMI JOIN ps USING (song_id)
    GROUP BY song_id", item_song(src_songs_new, 1L), item_song(src_songs_old, 2L)))
  # Item-table album and artist main_genre
  DBI::dbExecute(con, sprintf("
    CREATE TEMP TABLE album_items AS
    SELECT normid(album_id) AS album_id, any_value(nz(main_genre)) AS genre
    FROM %s WHERE nz(main_genre) IS NOT NULL
      AND normid(album_id) IN (SELECT album_id FROM songs)
    GROUP BY 1", src_alb_items))
  DBI::dbExecute(con, sprintf("
    CREATE TEMP TABLE artist_items AS
    SELECT normid(artist_id) AS artist_id, any_value(nz(main_genre)) AS genre
    FROM %s WHERE nz(main_genre) IS NOT NULL
      AND normid(artist_id) IN (SELECT artist_id FROM songs)
    GROUP BY 1", src_art_items))
  # Style tags: drop decade / language tags; one tag per song = its most
  # frequent style tag across the songs of the respondents' personal playlists
  DBI::dbExecute(con, "
    CREATE TEMP TABLE song_tags AS
    SELECT DISTINCT song_id, tag FROM (SELECT song_id, unnest(tags) AS tag FROM songs)
    WHERE tag IS NOT NULL AND tag <> ''")
  DBI::dbExecute(con, sprintf("
    CREATE TEMP TABLE tag_vocab AS
    SELECT tag, CAST(count(*) AS DOUBLE) AS n_songs, regexp_matches(tag, '%s') AS dropped
    FROM song_tags GROUP BY tag", drop_regex))
  DBI::dbExecute(con, "
    CREATE TEMP TABLE song_style AS
    SELECT st.song_id, st.tag AS style_tag
    FROM song_tags st JOIN tag_vocab v USING (tag)
    WHERE NOT v.dropped
    QUALIFY row_number() OVER (PARTITION BY st.song_id ORDER BY v.n_songs DESC, st.tag) = 1")
  # Previous definition: Deezer album genres (with names), one genre per album
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
      coalesce(s.main_genre, ai.genre, ari.genre) AS genre_main,
      CASE WHEN s.main_genre IS NOT NULL THEN 'song' WHEN ai.genre IS NOT NULL THEN 'album_items'
           WHEN ari.genre IS NOT NULL THEN 'artist_items' END AS genre_source_main,
      s.main_genre AS genre_song,
      ss.style_tag AS genre_style,
      coalesce(ag.genre, arg.genre) AS genre_deezer,
      CASE WHEN ag.genre IS NOT NULL THEN 'album' WHEN arg.genre IS NOT NULL THEN 'artist' END AS genre_source_deezer,
      CASE WHEN s.main_genre IS NOT NULL THEN 'song' WHEN ai.genre IS NOT NULL THEN 'album_items'
           WHEN ag.genre IS NOT NULL THEN 'album_deezer' WHEN arg.genre IS NOT NULL THEN 'artist' END AS genre_source_chain
    FROM pt
    LEFT JOIN songs s USING (song_id)
    LEFT JOIN album_items ai ON s.album_id = ai.album_id
    LEFT JOIN artist_items ari ON s.artist_id = ari.artist_id
    LEFT JOIN song_style ss ON pt.song_id = ss.song_id
    LEFT JOIN album_genre ag ON s.album_id = ag.album_id
    LEFT JOIN artist_genre arg ON s.artist_id = arg.artist_id")
  def_cols <- c(main = "genre_main", song_only = "genre_song", style_tags = "genre_style",
                deezer_album_old = "genre_deezer")
  DBI::dbExecute(con, sprintf("
    COPY (%s) TO '%s' (FORMAT parquet)",
    paste(sprintf("SELECT normid(playlist_id) AS playlist_id, '%s' AS genre_def, %s AS genre,
                     CAST(count(*) AS INTEGER) AS n
                   FROM tg WHERE %s IS NOT NULL GROUP BY ALL", names(def_cols), def_cols, def_cols),
          collapse = " UNION ALL "),
    file.path(path, "playlist_genre_counts.parquet")))
  DBI::dbExecute(con, sprintf("
    COPY (
      SELECT normid(playlist_id) AS playlist_id,
        CAST(count(*) AS INTEGER) AS n_tracks,
        CAST(count(*) FILTER (WHERE in_song_table) AS INTEGER) AS n_tracks_in_song_table,
        %s,
        CAST(count(*) FILTER (WHERE genre_source_main = 'song') AS INTEGER) AS n_src_main_song,
        CAST(count(*) FILTER (WHERE genre_source_main = 'album_items') AS INTEGER) AS n_src_main_album_items,
        CAST(count(*) FILTER (WHERE genre_source_main = 'artist_items') AS INTEGER) AS n_src_main_artist_items,
        CAST(count(*) FILTER (WHERE genre_source_chain = 'song') AS INTEGER) AS n_src_chain_song,
        CAST(count(*) FILTER (WHERE genre_source_chain = 'album_items') AS INTEGER) AS n_src_chain_album_items,
        CAST(count(*) FILTER (WHERE genre_source_chain = 'album_deezer') AS INTEGER) AS n_src_chain_album_deezer,
        CAST(count(*) FILTER (WHERE genre_source_chain = 'artist') AS INTEGER) AS n_src_chain_artist,
        CAST(count(*) FILTER (WHERE genre_source_deezer = 'album') AS INTEGER) AS n_tracks_album_genre,
        CAST(count(*) FILTER (WHERE genre_source_deezer = 'artist') AS INTEGER) AS n_tracks_artist_genre,
        CAST(count(*) FILTER (WHERE n_album_genres > 1) AS INTEGER) AS n_tracks_multi_genre_album,
        CAST(count(DISTINCT artist_id) AS INTEGER) AS n_artists
      FROM tg GROUP BY playlist_id
    ) TO '%s' (FORMAT parquet)",
    paste(sprintf("CAST(count(%s) AS INTEGER) AS n_with_genre_%s", def_cols, names(def_cols)), collapse = ",\n        "),
    file.path(path, "playlist_track_counts.parquet")))
  # Vocabularies (catalogue-level counts: number of distinct playlist songs per
  # genre label and definition), and the style-tag list with the drop flag
  DBI::dbExecute(con, sprintf("
    COPY (%s) TO '%s' (FORMAT parquet)",
    paste(sprintf("SELECT '%s' AS genre_def, %s AS genre, CAST(count(DISTINCT song_id) AS DOUBLE) AS n_songs
                   FROM tg WHERE %s IS NOT NULL GROUP BY ALL", names(def_cols), def_cols, def_cols),
          collapse = " UNION ALL "),
    file.path(path, "genre_vocabulary.parquet")))
  DBI::dbExecute(con, sprintf("COPY (SELECT * FROM tag_vocab ORDER BY n_songs DESC) TO '%s' (FORMAT parquet)",
                              file.path(path, "style_tag_vocabulary.parquet")))
  return(path)
}

# Rarefied richness: expected number of distinct genres among m tracks drawn
# without replacement from a playlist with genre counts n (Hurlbert 1971).
rarefied_richness <- function(n, m){
  N <- sum(n)
  if(N < m) return(NA_real_)
  sum(1 - exp(lchoose(N - n, m) - lchoose(N, m)))
}

# Per-playlist diversity indices, one row per personal playlist x genre_def.
# Coverage = tracks with a genre under that definition / tracks.
compute_playlist_genre_diversity <- function(dir, rarefy_m = 10L){
  counts <- arrow::read_parquet(file.path(dir, "playlist_genre_counts.parquet"))
  tracks <- arrow::read_parquet(file.path(dir, "playlist_track_counts.parquet"))
  voc <- arrow::read_parquet(file.path(dir, "genre_vocabulary.parquet"))
  G <- count(voc, genre_def, name = "G")
  div <- counts %>%
    group_by(genre_def, playlist_id) %>%
    summarize(
      n_genres = n(),
      N = sum(n),
      shannon = { p <- n / sum(n); -sum(p * log(p)) },
      gini_simpson = 1 - sum((n / sum(n))^2),
      # effective number of genres (Hill number of order 2 = 1 / Herfindahl-Hirschman index)
      eff_genres = 1 / sum((n / sum(n))^2),
      # Hurlbert's PIE: unbiased with respect to playlist size
      pie = ifelse(sum(n) > 1, sum(n) / (sum(n) - 1) * (1 - sum((n / sum(n))^2)), NA_real_),
      dominant_share = max(n) / sum(n),
      dominant_genre = genre[which.max(n)],
      rarefied_richness = rarefied_richness(n, rarefy_m),
      .groups = "drop") %>%
    left_join(G, by = "genre_def") %>%
    mutate(shannon_norm = ifelse(pmin(N, G) > 1, shannon / log(pmin(N, G)), NA_real_)) %>%
    select(-N, -G)
  wc <- tracks %>% select(playlist_id, starts_with("n_with_genre_")) %>%
    pivot_longer(-playlist_id, names_to = "genre_def", names_prefix = "n_with_genre_",
                 values_to = "n_tracks_with_genre")
  tracks %>% select(-starts_with("n_with_genre_")) %>%
    tidyr::crossing(genre_def = genre_defs) %>%
    left_join(wc, by = c("playlist_id", "genre_def")) %>%
    left_join(div, by = c("genre_def", "playlist_id")) %>%
    mutate(genre_coverage = n_tracks_with_genre / n_tracks)
}

# Join with categories: by label (multi-label: a playlist counts in every label
# it carries), single-label playlists only, and families, for each genre
# definition. Filters on size and genre coverage (per definition). Returns
# list(summary, models, plot_data, vocabulary, params). Writes
# output/genre_diversity_by_type.csv, ..._models.csv (column genre_def),
# output/genre_vocabulary.csv and output/genre_style_tags_vocabulary.csv.
summarise_genre_diversity_by_type <- function(playlist_genre_diversity, playlists,
                                              min_tracks = 10L, max_tracks = 200L,
                                              min_coverage = 0.8,
                                              genre_counts_dir = NULL,
                                              filename = "output/genre_diversity_by_type.csv"){
  an_cols <- names(playlists)[str_detect(names(playlists), "^an_")]
  pl <- filter_personal_playlists(playlists) %>%
    filter(if_any(all_of(an_cols), ~!is.na(.x))) %>%
    select(playlist_id, all_of(an_cols)) %>%
    # playlist ids are VARCHAR in the genre files (avoids integer64 mismatches)
    mutate(playlist_id = as.character(playlist_id)) %>%
    mutate(n_labels = rowSums(across(all_of(an_cols)) == 1, na.rm = TRUE))
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
                    median_eff_genres = median(eff_genres), mean_eff_genres = mean(eff_genres),
                    median_pie = median(pie, na.rm = TRUE), mean_pie = mean(pie, na.rm = TRUE),
                    median_dominant_share = median(dominant_share), mean_dominant_share = mean(dominant_share),
                    share_dominant_ge_80 = mean(dominant_share >= .8),
                    share_single_genre = mean(n_genres == 1),
                    .groups = "drop")
  }
  fit <- function(ms, y) {
    m <- lm(as.formula(str_glue("{y} ~ category + log(n_tracks)")), data = ms)
    cf <- summary(m)$coefficients
    tibble(outcome = y, term = rownames(cf), estimate = cf[, 1], se = cf[, 2], p = cf[, 4], n_obs = nobs(m))
  }
  one_def <- function(gd){
    d <- playlist_genre_diversity %>%
      filter(genre_def == gd) %>%
      inner_join(pl, by = "playlist_id") %>%
      mutate(in_sample = n_tracks >= min_tracks & n_tracks <= max_tracks &
               !is.na(genre_coverage) & genre_coverage >= min_coverage & !is.na(n_genres))
    long <- d %>%
      filter(in_sample) %>%
      pivot_longer(all_of(an_cols), names_to = "category", values_to = "value") %>%
      filter(value == 1) %>%
      mutate(category = str_remove(category, "^an_"))
    nolabel <- d %>% filter(in_sample, n_labels == 0) %>% mutate(category = "(no label)")
    by_label <- bind_rows(long, nolabel) %>%
      group_by(category) %>% stats() %>% mutate(subset = "all labelled playlists (multi-label)")
    by_label_single <- bind_rows(filter(long, n_labels == 1), nolabel) %>%
      group_by(category) %>% stats() %>% mutate(subset = "single-label playlists")
    by_family <- long %>%
      mutate(category = paste0("(family) ", recode_playlists(category, .what = "large_cat"))) %>%
      distinct(playlist_id, category, .keep_all = TRUE) %>%
      group_by(category) %>% stats() %>% mutate(subset = "all labelled playlists (multi-label)")
    overall <- d %>% filter(in_sample) %>% stats() %>% mutate(category = "(all in sample)", subset = "all")
    s <- function(x) sum(x, na.rm = TRUE)
    coverage <- tibble(
      category = "(coverage)", subset = "all personal annotated playlists",
      n_playlists = nrow(d),
      share_in_sample = mean(d$in_sample),
      share_tracks_in_song_table = s(d$n_tracks_in_song_table) / s(d$n_tracks),
      share_tracks_with_genre = s(d$n_tracks_with_genre) / s(d$n_tracks),
      # source shares, among tracks with a genre under the main definition / the
      # full chain song > album_items > album_deezer > artist / the old definition
      share_src_main_song = s(d$n_src_main_song) / s(d$n_tracks),
      share_src_main_album_items = s(d$n_src_main_album_items) / s(d$n_tracks),
      share_src_main_artist_items = s(d$n_src_main_artist_items) / s(d$n_tracks),
      share_src_chain_song = s(d$n_src_chain_song) / s(d$n_tracks),
      share_src_chain_album_items = s(d$n_src_chain_album_items) / s(d$n_tracks),
      share_src_chain_album_deezer = s(d$n_src_chain_album_deezer) / s(d$n_tracks),
      share_src_chain_artist = s(d$n_src_chain_artist) / s(d$n_tracks),
      share_tracks_artist_fallback_old = s(d$n_tracks_artist_genre) / (s(d$n_tracks_album_genre) + s(d$n_tracks_artist_genre)),
      share_tracks_multi_genre_album_old = s(d$n_tracks_multi_genre_album) / s(d$n_tracks_album_genre))
    summary <- bind_rows(overall, by_label, by_label_single, by_family, coverage)
    # Models on single-label playlists (reference = genre), controlling for size
    ms <- filter(long, n_labels == 1)
    ys <- c("pie", "eff_genres", "dominant_share", "shannon_norm", "rarefied_richness")
    models <- if("genre" %in% ms$category && n_distinct(ms$category) > 1){
      ms <- mutate(ms, category = relevel(factor(category), ref = "genre"))
      bind_rows(lapply(ys, function(y) tryCatch(fit(ms, y), error = function(e){
        warning("genre diversity model ", y, " (", gd, ") failed: ", conditionMessage(e)); NULL })))
    } else tibble()
    list(summary = mutate(summary, genre_def = gd, .before = 1),
         models = mutate(models, genre_def = gd, .before = 1),
         plot_data = bind_rows(mutate(long, subset = "All labelled playlists (multi-label)"),
                               mutate(filter(long, n_labels == 1), subset = "Single-label playlists")) %>%
           transmute(genre_def = gd, subset, playlist_id, category, n_tracks, pie, eff_genres, dominant_share,
                     shannon_norm, rarefied_richness))
  }
  res <- lapply(genre_defs, one_def)
  summary <- bind_rows(purrr::map(res, "summary")) %>%
    mutate(min_tracks = min_tracks, max_tracks = max_tracks, min_coverage = min_coverage) %>%
    relocate(genre_def, subset, category)
  models <- bind_rows(purrr::map(res, "models"))
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(summary, filename)
  write_csv(models, str_replace(filename, "\\.csv$", "_models.csv"))
  vocab <- NULL
  if(!is.null(genre_counts_dir)){
    vocab <- arrow::read_parquet(file.path(genre_counts_dir, "genre_vocabulary.parquet")) %>%
      group_by(genre_def) %>% mutate(share_songs = n_songs / sum(n_songs)) %>% ungroup() %>%
      arrange(genre_def, desc(n_songs))
    write_csv(vocab, file.path(dirname(filename), "genre_vocabulary.csv"))
    write_csv(arrow::read_parquet(file.path(genre_counts_dir, "style_tag_vocabulary.parquet")),
              file.path(dirname(filename), "genre_style_tags_vocabulary.csv"))
  }
  list(summary = summary, models = models, vocabulary = vocab,
       params = list(min_tracks = min_tracks, max_tracks = max_tracks, min_coverage = min_coverage),
       plot_data = bind_rows(purrr::map(res, "plot_data")))
}

plot_genre_diversity_by_type <- function(genre_div, filename = "output/genre_diversity_by_type.png",
                                         genre_def = "main"){
  set_ggplot_options()
  gd <- genre_def
  d <- genre_div$plot_data %>%
    filter(.data$genre_def == gd) %>%
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
           "Personal playlists with {min_tracks}-{max_tracks} tracks and >= {100 * min_coverage}% of tracks with a genre (Deezer item metadata: song main genre, else album, else artist)."))) +
    theme(legend.position = "bottom")
  ggsave(filename, gg, width = 10, height = 7)
  filename
}
