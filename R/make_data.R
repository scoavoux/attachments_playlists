download_playlist_data <- function(){
  s3 <- initialize_s3()
  filename <- "data/temp/playlists.parquet"
  s3$download_file(Bucket = "scoavoux", Key = "/records_w3/playlists_data/RECORDS_hashed_user_playlists.parquet", Filename = filename)
  return(filename)
}

make_playlist_data <- function(playlist_file = "data/temp/playlists.parquet"){
  playlists <- read_parquet(playlist_file, col_select = c(1, 2, 4))
  return(playlists)
}

make_tracklist_data <- function(playlist_file = "data/temp/playlists.parquet"){
  tracklist <- read_parquet(playlist_file, col_select = c(1, 3)) %>% unnest(tracks) %>% 
    unnest(list) %>% 
    rename(song_id = element)
  return(tracklist)
}


make_favorites_data <- function(){
  s3 <- initialize_s3()
  filename <- "data/temp/favorites.parquet"
  s3$download_file(Bucket = "scoavoux", Key = "/records_w3/favorites/RECORDS_hashed_user_favorites.parquet", Filename = filename)
  favorites <- read_parquet(filename)
  return(favorites)
}



