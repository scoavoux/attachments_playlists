# User-level data ------
make_survey_data <- function(){
  require(tidyverse)
  s3 <- initialize_s3()
  f <- s3$get_object(Bucket = "scoavoux", 
                     Key = "records_w3/survey/RECORDS_Wave3_apr_june_23_responses_corrected.csv")
  survey <- f$Body %>% rawToChar() %>% data.table::fread() %>% tibble()
  
  # filter only 
  survey <- survey %>% 
    filter(Progress == 100,
           country == "FR")
  return(survey)
}


# Number of song plays per user in the "short" (2023) stream files.
# DuckDB reads only the 3 needed columns straight from S3 and aggregates there.
# Note: the previous arrow::open_dataset() version read the whole
# streams_short/ folder, i.e. the 10 part- files AND the 10
# stream_with_context_hashed_part- files. `files` lets us choose explicitly.
make_volume_play_data <- function(files){
  con <- duck_connect(memory_limit = "1500MB", threads = 2)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  sql <- sprintf("
    SELECT hashed_id, CAST(count(*) AS INTEGER) AS n_play_2023
    FROM read_parquet(%s)
    WHERE media_type = 'song' AND is_listened = 1
    GROUP BY hashed_id", sql_file_list(files))
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

list_volume_play_files <- function(){
  f <- list_s3_objects("records_w3/streams/streams_short/")
  # Only the 10 part- files (2022 - May 2023). The original arrow version read
  # the whole folder, so n_play_2023 also counted the stream_with_context files
  # (2023-2024 data, excluded everywhere else). See notes/code_audit.md.
  dplyr::filter(f, 
                stringr::str_detect(Key, "part-"),
                !stringr::str_detect(Key, "stream_with_context"),
                stringr::str_detect(Key, "\\.parquet$"))
}

# ISEI ------
# make_isei_data() (below) needs an interactive Google Sheets login
# (googlesheets4::gs4_auth) and cannot run in a non-interactive tar_make().
# The pipeline therefore reads a frozen snapshot of the `isei` target, written
# once to S3 with snapshot_isei_to_s3(). To update ISEI coding: run
# make_isei_data(tar_read(users)) interactively, then snapshot_isei_to_s3(<result>).
isei_snapshot_key <- "attachments_playlists/snapshots/isei.parquet"

read_isei_snapshot <- function(){
  con <- duck_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  tibble::as_tibble(DBI::dbGetQuery(con, sprintf("SELECT hashed_id, isei FROM read_parquet('%s')", s3_uri(isei_snapshot_key))))
}

snapshot_isei_to_s3 <- function(isei = targets::tar_read(isei)){
  tmp <- tempfile(fileext = ".parquet")
  arrow::write_parquet(isei, tmp)
  s3 <- initialize_s3()
  s3$put_object(Bucket = "scoavoux", Key = isei_snapshot_key, Body = readBin(tmp, "raw", file.size(tmp)))
  invisible(isei_snapshot_key)
}

# Legacy (not called by the pipeline) ------
# this is from onmivore project.
make_isei_data <- function(survey_raw){
  # Packages and data ------
  require(tidyverse)
  require(tidytable)
  require(janitor)
  require(readxl)
  
  s3 <- initialize_s3()
  s3$download_file(Bucket = "scoavoux", 
                   Key = "PCS2020/L66_Matrice_codification_PCS2020_collecte_2023.xlsx", 
                   Filename = "data/temp/L66_Matrice_codification_PCS2020_collecte_2023.xlsx")
  pcs <- read_excel("data/temp/L66_Matrice_codification_PCS2020_collecte_2023.xlsx", sheet = 2, skip=8)
  
  pcs <- clean_names(pcs) %>% 
    filter(libm != "2023") %>% 
    select(libm:libf) %>% 
    pivot_longer(names_to = "sexe", values_to = "orig_pcs_prof") %>% 
    distinct(orig_pcs_prof)
  
  # Cleaning up data ------
  clean_up <- function(string){
    string %>% 
      stringi::stri_trans_general("Latin-ASCII") %>% 
      tolower() %>% 
      str_replace_all(c("[-,\\.']" = " ")) %>% 
      str_replace_all(c("\\s+" = " "))
  }
  
  pcs <- pcs %>% 
    distinct(orig_pcs_prof) %>% 
    mutate(pcs_prof = clean_up(orig_pcs_prof))
  
  survey_raw <- select(survey_raw, hashed_id, E_FR_prof_femme:E_FR_prof_retr_homme) %>% 
    pivot_longer(-hashed_id, values_to = "orig_survey_prof") %>% 
    filter(orig_survey_prof != "") %>% 
    mutate(orig_survey_prof = str_trim(orig_survey_prof)) %>% 
    select(-name) %>% 
    right_join(survey_raw)
  
  all_profs <- survey_raw %>%  
    count(orig_survey_prof) %>% 
    mutate(survey_prof = clean_up(orig_survey_prof)) %>% 
    filter(!is.na(orig_survey_prof))
  
  # there are 4735 different labels for 10412 individuals
  nrow(all_profs)
  objective <- sum(all_profs$n)
  objective
  
  # Finding matches ------
  ## Very much incomplete
  matches <- inner_join(all_profs, rename(pcs, survey_prof = "pcs_prof")) %>% 
    select(-survey_prof) %>% 
    mutate(niveau = 4, 
           type="exact match")
  
  # exact match finds 2509 for 7330 individuals
  nrow(matches)
  sum(matches$n)
  
  # we export and go through openrefine
  f <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/survey/pcs_openrefine1.csv")
  or1 <- f$Body %>% rawToChar() %>% read_csv()
  
  or1 <- or1 %>% select(-survey_prof)
  or1 <- or1 %>% 
    filter(!(orig_survey_prof %in% matches$orig_survey_prof))
  
  matches <- or1 %>% 
    mutate(niveau=4,
           type = "openrefine_handcoded") %>% 
    bind_rows(matches)
  
  # with first openrefine wave we are at 2787 labels for 7813 individuals
  nrow(matches)
  sum(matches$n)
  sum(matches$n)/objective
  
  # Handcoding by research assistants
  
  
  #TODO: encore 25% de statuts à récupérer
  ## Ongoing
  # all_profs <- filter(all_profs, !(orig_survey_prof %in% matches$orig_survey_prof))
  # 
  # survey_raw %>%
  #   filter(orig_survey_prof %in% all_profs$orig_survey_prof) %>%
  #   select(orig_survey_prof, E_statut_pub_priv:E_encadre) %>%
  #   left_join(all_profs) %>%
  #   select(-orig_survey_prof, -n) %>%
  #   count(survey_prof, E_statut_pub_priv, E_taille_entreprise, E_position_pub, E_position_priv, E_encadre) %>%
  #   arrange(desc(n)) %>%
  #   write_csv("profs.csv")
  # Récupérer les codes
  library(googlesheets4)
  gs4_auth(email = "samuel.coavoux@gmail.com")
  recoprof <- read_sheet("1DcVZkiNRS9XbLFJxl_qRtbF1Um9xKkothWOLuUb28vw", col_types = "c") %>% 
    filter(!is.na(PCS4))
  recoprof <- recoprof %>% 
    mutate(
      E_statut_pub_priv = ifelse(is.na(E_statut_pub_priv), "", E_statut_pub_priv),
      E_taille_entreprise = ifelse(is.na(E_taille_entreprise), "", E_taille_entreprise),
      E_position_pub = ifelse(is.na(E_position_pub), "", E_position_pub),
      E_position_priv = ifelse(is.na(E_position_priv), "", E_position_priv),
      E_encadre = ifelse(is.na(E_encadre), "", E_encadre)
    )
  recoprof <- recoprof %>% 
    select(-n, -PCS3, -PCS2, -Inclassable)
  
  pppcs <- read_excel("data/temp/L66_Matrice_codification_PCS2020_collecte_2023.xlsx", sheet = 2, skip=8)
  pppcs <- pppcs %>% 
    slice(-1) %>% 
    select(-libf, -liste, -natlib) %>% 
    pivot_longer(-libm) %>% 
    filter(!is.na(value)) %>% 
    group_by(value) %>% 
    slice(1) %>% 
    select(clean_pcs_prof = "libm", PCS4 = "value")
  handcoded <- survey_raw %>% 
    select(hashed_id, orig_survey_prof, E_statut_pub_priv, E_taille_entreprise,
           E_position_pub, E_position_priv, E_encadre) %>% 
    left_join(select(all_profs, -n)) %>% 
    inner_join(recoprof) %>% 
    select(hashed_id, PCS4) %>% 
    left_join(pppcs) %>% 
    select(-PCS4)
  
  (sum(matches$n) + nrow(handcoded))/objective
  
  # Match databases ------
  survey_raw <-  matches %>% 
    select(orig_survey_prof, clean_pcs_prof = orig_pcs_prof, pcs_niveau = niveau) %>% 
    right_join(survey_raw) %>% 
    left_join(rename(handcoded, clean_pcs_prof2 = "clean_pcs_prof")) %>% 
    mutate(clean_pcs_prof = ifelse(is.na(clean_pcs_prof), clean_pcs_prof2, clean_pcs_prof)) %>% 
    select(-clean_pcs_prof2)
  
  ## Prepare PCS encoding table
  pcs_cod <- read_xlsx("data/temp/L66_Matrice_codification_PCS2020_collecte_2023.xlsx", sheet = 2, skip = 8) %>% filter(libm != "2023") %>% 
    select(-liste, -natlib, -codeu) %>% 
    pivot_longer(libm:libf, values_to = "clean_pcs_prof") %>% 
    select(-name) %>% 
    pivot_longer(-clean_pcs_prof, names_to = "condition_pcs", values_to = "pcs4") %>% 
    distinct()
  
  ## Prepare ISCO encoding table
  s3$download_file(Bucket = "scoavoux", 
                   Key = "PCS2020/L72_Matrice_codification_ISCO_collecte_2023.xlsx", 
                   Filename = "data/temp/L72_Matrice_codification_ISCO_collecte_2023.xlsx")
  
  isco_cod <- read_xlsx("data/temp/L72_Matrice_codification_ISCO_collecte_2023.xlsx", sheet = 2, skip = 7) %>% 
    filter(libm != "2023") %>% 
    select(-id, -liste, -codeu) %>% 
    pivot_longer(libm:libf, values_to = "clean_pcs_prof") %>% 
    select(-name) %>% 
    pivot_longer(-clean_pcs_prof, names_to = "condition_isco", values_to = "isco4") %>% 
    distinct()
  
  profs <- select(survey_raw, hashed_id, clean_pcs_prof, E_statut_pub_priv:E_position_priv, E_encadre) %>% 
    filter(!is.na(clean_pcs_prof)) %>% 
    mutate(across(everything(), ~ifelse(.x == "", NA, .x))) %>% 
    mutate(condition_pcs = case_when(E_statut_pub_priv == "Vous travaill(i)ez, sans être rémunéré(e), avec un membre de votre famille" ~ "aid_fam", 
                                     E_statut_pub_priv == "À votre compte (y compris gérant de société ou chef d’entreprise salarié)" &
                                       E_taille_entreprise %in% c("Une seule personne : vous travaillez seul(e) / vous travailliez seul(e)",
                                                                  "Entre 2 et 10 personnes") ~ "inde_0_9", 
                                     E_statut_pub_priv == "À votre compte (y compris gérant de société ou chef d’entreprise salarié)" &
                                       E_taille_entreprise == "Entre 11 et 49 personnes" ~ "inde_10_49", 
                                     E_statut_pub_priv == "À votre compte (y compris gérant de société ou chef d’entreprise salarié)" &
                                       E_taille_entreprise == "50 personnes ou plus" ~ "inde_sup49", 
                                     E_statut_pub_priv == "À votre compte (y compris gérant de société ou chef d’entreprise salarié)" ~ "inde_nr", 
                                     E_statut_pub_priv %in% c("Salarié(e) d’une entreprise (y compris d’une association ou de la Sécurité sociale)", "Salarié(e) d’un particulier") &
                                       E_position_priv == "Ingénieur(e), cadre d’entreprise" ~ "priv_cad", 
                                     E_statut_pub_priv %in% c("Salarié(e) d’une entreprise (y compris d’une association ou de la Sécurité sociale)", "Salarié(e) d’un particulier") &
                                       E_position_priv == "Technicien(ne)" ~ "priv_tec", 
                                     E_statut_pub_priv %in% c("Salarié(e) d’une entreprise (y compris d’une association ou de la Sécurité sociale)", "Salarié(e) d’un particulier") &
                                       E_position_priv == "Agent de maîtrise (y compris administrative ou commerciale)" ~ "priv_am", 
                                     E_statut_pub_priv %in% c("Salarié(e) d’une entreprise (y compris d’une association ou de la Sécurité sociale)", "Salarié(e) d’un particulier") &
                                       E_position_priv == "Employé(e) de bureau, de commerce, de services" ~ "priv_emp", 
                                     E_statut_pub_priv %in% c("Salarié(e) d’une entreprise (y compris d’une association ou de la Sécurité sociale)", "Salarié(e) d’un particulier") & 
                                       E_position_priv == "Ouvrier (ouvrière) qualifié(e), technicien(ne) d'atelier"~ "priv_oq", 
                                     E_statut_pub_priv %in% c("Salarié(e) d’une entreprise (y compris d’une association ou de la Sécurité sociale)", "Salarié(e) d’un particulier") & 
                                       E_position_priv == "Manœuvre, ouvrier (ouvrière) spécialisé(e)"~ "priv_opq", 
                                     E_statut_pub_priv %in% c("Salarié(e) d’une entreprise (y compris d’une association ou de la Sécurité sociale)", "Salarié(e) d’un particulier") ~ "priv_nr", 
                                     E_statut_pub_priv == "Salarié(e) de la fonction publique (État, territoriale ou hospitalière)" &
                                       E_position_pub == "Agent de catégorie A de la fonction publique" ~ "pub_catA", 
                                     E_statut_pub_priv == "Salarié(e) de la fonction publique (État, territoriale ou hospitalière)" &
                                       E_position_pub %in% c("Agent de catégorie B de la fonction publique", "Technicien(ne)") ~ "pub_catB", 
                                     E_statut_pub_priv == "Salarié(e) de la fonction publique (État, territoriale ou hospitalière)" &
                                       E_position_pub %in% c("Agent de catégorie C de la fonction publique",
                                                             "Ouvrier (ouvrière) qualifié(e)",
                                                             "Manœuvre, ouvrier (ouvrière) spécialisé(e)") ~ "pub_catC", 
                                     E_statut_pub_priv == "Salarié(e) de la fonction publique (État, territoriale ou hospitalière)" ~ "pub_nr", 
                                     TRUE ~ "ssvaran"),
           condition_isco = case_when(E_position_priv == "Ingénieur(e), cadre d’entreprise" | E_position_pub == "Agent de catégorie A de la fonction publique" & E_encadre %in% c("Oui, mais ce n’est pas (ce n’était pas) ma tâche principale", "Oui, et c’est (c’était) ma tâche principale") ~ "ingcad_supv", 
                                      E_position_priv == "Ingénieur(e), cadre d’entreprise" | E_position_pub == "Agent de catégorie A de la fonction publique" & E_encadre == "Non" ~ "ingcad_nsupv", 
                                      E_position_priv %in% c("Technicien(ne)", "Agent de maîtrise (y compris administrative ou commerciale)") | E_position_pub %in% c("Agent de catégorie B de la fonction publique", "Technicien(ne)") & E_encadre %in% c("Oui, mais ce n’est pas (ce n’était pas) ma tâche principale", "Oui, et c’est (c’était) ma tâche principale") ~ "tecam_supv", 
                                      E_position_priv %in% c("Technicien(ne)", "Agent de maîtrise (y compris administrative ou commerciale)") | E_position_pub %in% c("Agent de catégorie B de la fonction publique", "Technicien(ne)") & E_encadre == "Non" ~ "tecam_nsupv", 
                                      E_position_priv %in% c("Employé(e) de bureau, de commerce, de services", "Ouvrier (ouvrière) qualifié(e), technicien(ne) d'atelier", "Manœuvre, ouvrier (ouvrière) spécialisé(e)") | E_position_pub %in% c("Agent de catégorie C de la fonction publique", "Ouvrier (ouvrière) qualifié(e)", "Manœuvre, ouvrier (ouvrière) spécialisé(e)") & E_encadre %in% c("Oui, mais ce n’est pas (ce n’était pas) ma tâche principale", "Oui, et c’est (c’était) ma tâche principale") ~ "ouvemp_supv", 
                                      E_position_priv %in% c("Employé(e) de bureau, de commerce, de services", "Ouvrier (ouvrière) qualifié(e), technicien(ne) d'atelier", "Manœuvre, ouvrier (ouvrière) spécialisé(e)") | E_position_pub %in% c("Agent de catégorie C de la fonction publique", "Ouvrier (ouvrière) qualifié(e)", "Manœuvre, ouvrier (ouvrière) spécialisé(e)") & E_encadre == "Non" ~ "ouvemp_nsupv", 
                                      E_encadre %in% c("Oui, mais ce n’est pas (ce n’était pas) ma tâche principale", "Oui, et c’est (c’était) ma tâche principale") ~ "nr_supv", 
                                      E_encadre == "Non" ~ "nr_nsupv", 
                                      E_statut_pub_priv == "À votre compte (y compris gérant de société ou chef d’entreprise salarié)" &
                                        E_taille_entreprise %in% c("Une seule personne : vous travaillez seul(e) / vous travailliez seul(e)",
                                                                   "Entre 2 et 10 personnes") ~ "inde_0_9", 
                                      E_statut_pub_priv == "À votre compte (y compris gérant de société ou chef d’entreprise salarié)" &
                                        E_taille_entreprise %in% c("Entre 11 et 49 personnes", "50 personnes ou plus") ~ "inde_sup10", 
                                      E_statut_pub_priv == "À votre compte (y compris gérant de société ou chef d’entreprise salarié)" ~ "inde_nr", 
                                      TRUE ~ "ssvaran")
    ) %>% 
    select(hashed_id, clean_pcs_prof, starts_with("condition_"))
  
  f <- s3$get_object(Bucket = "scoavoux", Key = "PCS2020/isco_isei.csv")
  isco_isei <- f$Body %>% rawToChar() %>% read_csv() %>% 
    rename(isco4 = "isco")
  
  result <- profs %>% 
    left_join(pcs_cod, by = c("clean_pcs_prof", "condition_pcs")) %>% 
    left_join(isco_cod, by = c("clean_pcs_prof", "condition_isco")) %>% 
    left_join(isco_isei, by = "isco4") %>% 
    mutate(pcs1 = str_extract(pcs4, "^\\w"),
           pcs2 = str_extract(pcs4, "^\\w{2}"),
           pcs3 = str_extract(pcs4, "^\\w{3}"),
           isco1 = str_extract(isco4, "^\\w"),
           isco2 = str_extract(isco4, "^\\w{2}"),
           isco3 = str_extract(isco4, "^\\w{3}")) %>% 
    select(hashed_id, isei)
  
  return(result)
}


# Playlist data ------
# DuckDB reads the parquet straight from S3 (only the needed columns) and keeps
# survey respondents. The old version used exists(filename), which tests for an
# R object, not a file, so it re-downloaded the 285 MB file on every run.
playlists_key <- "records_w3/playlists_data/RECORDS_hashed_user_playlists.parquet"

make_playlist_data <- function(users){
  con <- duck_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  duckdb::duckdb_register(con, "users_ids", dplyr::distinct(dplyr::select(users, hashed_id)))
  sql <- sprintf("
    SELECT DISTINCT p.playlist_id, p.title, p.hashed_id
    FROM read_parquet('%s') p
    SEMI JOIN users_ids u ON p.hashed_id = u.hashed_id", s3_uri(playlists_key))
  # DISTINCT: 191 playlists appear twice (same id, user and title) in the raw file
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

make_playlists_unique_titles <- function(playlists){
  pt <- distinct(playlists, title)
  filename <- "data/temp/unique_playlists_titles.csv"
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(pt, filename)
  return(filename)
}

# Not used downstream (kept for exploratory work).
make_tracklist_data <- function(playlists){
  con <- duck_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  duckdb::duckdb_register(con, "pl", dplyr::distinct(dplyr::select(playlists, playlist_id)))
  sql <- sprintf("
    SELECT playlist_id, song_id FROM (
      SELECT p.playlist_id, unnest(p.tracks.list).element AS song_id
      FROM read_parquet('%s') p
      SEMI JOIN pl ON p.playlist_id = pl.playlist_id)", s3_uri(playlists_key))
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

# Not used downstream (kept for exploratory work).
make_favorites_data <- function(users){
  con <- duck_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  duckdb::duckdb_register(con, "users_ids", dplyr::distinct(dplyr::select(users, hashed_id)))
  sql <- sprintf("
    SELECT f.* FROM read_parquet('%s') f
    SEMI JOIN users_ids u ON f.hashed_id = u.hashed_id",
    s3_uri("records_w3/favorites/RECORDS_hashed_user_favorites.parquet"))
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

# Stream data ------
list_streaming_data_files <- function(){
  f <- list_s3_objects("records_w3/streams/")
  # the analysis has been done before we got the 2024 data and there has been so
  # much idiosyncratic work done to clean up artists list, etc. that I really 
  # don't want to add the newer data now.
  # So... we exclude it, plain and simple.
  # (records_w3/new_streams/ is not used either.)
  dplyr::filter(f, 
                stringr::str_detect(Key, "part-"),
                !stringr::str_detect(Key, "stream_with_context"),
                stringr::str_detect(Key, "\\.parquet$"))
}

# DuckDB reads the 19 stream files straight from S3: column projection +
# filters (survey respondents, is_listened = 1, songs only, year > 2017) are
# pushed into the scan, and the result is written to a local directory of
# parquet files (one per source file, ~1.3 GB in total) that the
# like-at-first-sight queries read. Replaces the 19 `streaming_data` branches,
# which downloaded each 1.3-1.7 GB file whole into R memory and uploaded the
# result back to S3 (~6 GB).
# One COPY per source file with an HTTP timeout and retries: a single query over
# all files was seen to hang indefinitely on a stalled S3 connection.
make_streams_users_file <- function(streaming_data_files, users,
                                    path = "data/temp/streams_users",
                                    max_attempts = 3){
  unlink(path, recursive = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  keys <- streaming_data_files$Key
  for(i in seq_along(keys)){
    is_long <- stringr::str_detect(keys[i], "streams_long")
    out <- file.path(path, sprintf("part_%02d.parquet", i))
    # 1514764800 = 2018-01-01 00:00:00 UTC, i.e. year(as_datetime(ts_listen)) > 2017
    sql <- sprintf("
      COPY (
        SELECT hashed_id, ts_listen, %s, click_loved, context_4
        FROM read_parquet('%s')
        WHERE is_listened = 1 AND ts_listen >= 1514764800 %s
          AND hashed_id IN (SELECT hashed_id FROM u)
      ) TO '%s' (FORMAT parquet, COMPRESSION zstd)",
      ifelse(is_long, "song_id", "media_id AS song_id"),
      s3_uri(keys[i]),
      ifelse(is_long, "", "AND media_type = 'song'"),
      out)
    for(attempt in seq_len(max_attempts)){
      ok <- tryCatch({
        con <- duck_connect()
        DBI::dbExecute(con, "SET http_timeout = 60000; SET http_retries = 5;")
        duckdb::duckdb_register(con, "users_ids", dplyr::distinct(dplyr::select(users, hashed_id)))
        DBI::dbExecute(con, "CREATE TEMP TABLE u AS SELECT hashed_id FROM users_ids")
        DBI::dbExecute(con, sql)
        TRUE
      }, error = function(e){ message("Attempt ", attempt, " failed for ", keys[i], ": ", conditionMessage(e)); FALSE },
      finally = try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE))
      if(ok) break
      if(attempt == max_attempts) stop("Could not extract ", keys[i])
    }
  }
  return(path)
}

# Playlist annotations ------
list_playlists_annotation_data_files <- function(){
  f <- list_s3_objects("records_w3/playlists_annotations/")
  dplyr::filter(f, stringr::str_detect(Key, "\\.csv$"))
}

# Each annotation CSV has two columns: title and one 0/1 label (NA when the
# annotation failed). Titles in the CSVs are whitespace-trimmed (and a title can
# appear several times, from playlist titles that differ only by whitespace),
# so we keep the first row per trimmed title and join on trimws(title).
# The old version (fread + chained left_join without `by`) joined on the raw
# title and so left every playlist whose title has leading/trailing spaces
# unannotated (13,075 playlists).
make_playlists_annotation_data <- function(playlists_annotation_data_files, playlists){
  con <- duck_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  read_one <- function(key){
    d <- DBI::dbGetQuery(con, sprintf(
      "SELECT * FROM read_csv('%s', header = true, all_varchar = true, nullstr = 'NA')", s3_uri(key)))
    names(d)[1] <- "title"
    d[[2]] <- as.integer(d[[2]])
    d %>% 
      tibble::as_tibble() %>% 
      mutate(title = trimws(title)) %>% 
      filter(!is.na(title)) %>% 
      distinct(title, .keep_all = TRUE)
  }
  o <- purrr::map(playlists_annotation_data_files$Key, read_one) %>% 
    purrr::reduce(dplyr::full_join, by = "title") %>% 
    rename_with(~paste0("an_", .x), -title) %>% 
    select(title, sort(setdiff(names(.), "title")))
  playlists %>% 
    mutate(.key = trimws(title)) %>% 
    left_join(rename(o, .key = "title"), by = ".key") %>% 
    select(-.key)
}
