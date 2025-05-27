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

# Playlist data ------
make_playlist_data <- function(playlist_file = "data/temp/playlists.parquet", users){
  s3 <- initialize_s3()
  filename <- "data/temp/playlists.parquet"
  if(!exists(filename)){
    s3$download_file(Bucket = "scoavoux", 
                     Key = "/records_w3/playlists_data/RECORDS_hashed_user_playlists.parquet", 
                     Filename = filename)
  }
  # import playlists
  playlists <- read_parquet(filename, col_select = c(1, 2, 4)) %>% 
  # and restrict to users
    inner_join(select(users, hashed_id))
  
  return(playlists)
}

#
make_playlists_unique_titles <- function(playlists){
  pt <- distinct(playlists, title)
  filename <- "data/temp/unique_playlists_titles.csv"
  write_csv(pt, filename)
  return(filename)
}

make_tracklist_data <- function(playlist_file = "data/temp/playlists.parquet", playlists){
  s3 <- initialize_s3()
  filename <- "data/temp/playlists.parquet"
  if(!exists(filename)){
    s3$download_file(Bucket = "scoavoux", 
                     Key = "/records_w3/playlists_data/RECORDS_hashed_user_playlists.parquet", 
                     Filename = filename)
  }
  tracklist <- read_parquet(filename, col_select = c(1, 3)) %>% unnest(tracks) %>% 
    unnest(list) %>% 
    rename(song_id = element) %>% 
    inner_join(select(playlists, playlist_id))
  return(tracklist)
}


make_favorites_data <- function(users){
  s3 <- initialize_s3()
  filename <- "data/temp/favorites.parquet"
  s3$download_file(Bucket = "scoavoux", Key = "/records_w3/favorites/RECORDS_hashed_user_favorites.parquet", Filename = filename)
  favorites <- read_parquet(filename) %>% 
    inner_join(select(users, hashed_id))
  return(favorites)
}

# Stream data ------
list_streaming_data_files <- function(){
  require(tidyverse)
  s3 <- initialize_s3()
  
  stream_data_files <- s3$list_objects_v2(Bucket = "scoavoux", Prefix = "records_w3/streams")$Content %>% map(~.x$Key) %>% 
    unlist()
  stream_data_files <- stream_data_files[str_detect(stream_data_files, "part-")]
  # the analysis has been done before we got the 2024 data and there has been so
  # much idiosyncratic work done to clean up artists list, etc. that I really 
  # don't want to add the newer data now.
  # So... we exclude it, plain and simple.
  stream_data_files <- stream_data_files[!str_detect(stream_data_files, "stream_with_context")]
  return(stream_data_files)
}

make_stream_data_onefile <- function(file, users){
  require(tidytable)
  require(arrow)
  require(lubridate)
  
  users <- select(users, hashed_id)
  
  s3 <- initialize_s3()
  # users <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/RECORDS_hashed_user_group.parquet")$Body %>% 
  #   read_parquet()
  if(str_detect(file, "long")){
    streams <- s3$get_object(Bucket = "scoavoux", Key = file)$Body %>% 
      read_parquet(col_select = c("hashed_id", "ts_listen", "song_id",
                                  "is_listened", "click_loved", "context_4")) %>% 
      right_join(users)
  } else if(str_detect(file, "short")) {
    streams <- s3$get_object(Bucket = "scoavoux", Key = file)$Body %>% 
      read_parquet(col_select = c("hashed_id", "ts_listen", "media_id",
                                  "is_listened", "click_loved", "context_4", 
                                  "media_type")) %>% 
      filter(media_type == "song") %>% 
      rename(song_id = "media_id") %>% 
      select(-media_type) %>% 
      right_join(users)
  }
  streams <- streams %>% 
    filter(year(as_datetime(ts_listen)) > 2017, 
           is_listened == 1) %>% 
    select(-is_listened)
  
  return(streams)
}

