library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("paws", "tidyverse", "arrow"),
  repository = "aws",
  repository_meta = "aws",
  format = "feather",
  resources = tar_resources(
    aws = tar_resources_aws(
      endpoint = Sys.getenv("S3_ENDPOINT"),
      bucket = "scoavoux",
      prefix = "attachments_playlists"
    )
  )
)

tar_source("R")

list(
  # Data ------
  tar_target(users,                make_survey_data()),
  tar_target(isei,                 make_isei_data(users)),
  tar_target(playlists,            make_playlist_data(users = users)),
  tar_target(unique_titles,        make_playlists_unique_titles(playlists = playlists), format = "file", repository = "local"),
  tar_target(tracklists,           make_tracklist_data(playlists = playlists)),
  tar_target(favorites,            make_favorites_data(users = users)),
  
  ## Streaming data ------
  tar_target(streaming_data_files, list_streaming_data_files(), format = "rds"),
  tar_target(streaming_data,       make_stream_data_onefile(streaming_data_files, users), pattern = streaming_data_files),
  
  # Like at first sight ------
  tar_target(favorites_replayed,   make_favorites_replayed_data(streaming_data)),
  tar_target(gg_like_at_first_sight_raw,   plot_like_at_first_sight(favorites_replayed, .what = "raw"), 
             repository = "local", format = "file"),
  tar_target(gg_like_at_first_sight_bydevice,   plot_like_at_first_sight(favorites_replayed, .what = "by_device"),
             repository = "local", format = "file"),
  
  # Playlist use ------
  tar_target(users_playlists,              make_users_playlists_data(users, playlists, isei)),
  tar_target(gg_noplaylists_socdem,        plot_noplaylists_socdem(users_playlists), 
                                           repository = "local", format = "file")

)
