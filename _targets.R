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
  # User data ------
  tar_target(users,                make_survey_data()),
  tar_target(isei,                 make_isei_data(users)),
  tar_target(favorites,            make_favorites_data(users = users)),
  
  # Playlists data
  tar_target(playlists_raw,        make_playlist_data(users = users)),
  tar_target(unique_titles,        make_playlists_unique_titles(playlists = playlists_raw), format = "file", repository = "local"),
  tar_target(tracklists,           make_tracklist_data(playlists = playlists_raw)),
  tar_target(playlists_annotation_data_files, list_playlists_annotation_data_files(), format = "qs"),
  tar_target(playlists,            make_playlists_annotation_data(playlists_annotation_data_files, playlists_raw)),
  
  ## Streaming data ------
  tar_target(streaming_data_files, list_streaming_data_files(), format = "rds"),
  tar_target(streaming_data,       make_stream_data_onefile(streaming_data_files, users), pattern = streaming_data_files),
  
  # Like at first sight ------
  tar_target(favorites_replayed,   make_favorites_replayed_data(streaming_data)),
  tar_target(gg_ltfsight,          plot_like_at_first_sight(favorites_replayed, .what = "raw"), 
             repository = "local", format = "file"),
  tar_target(gg_ltfsight_dev,      plot_like_at_first_sight(favorites_replayed, .what = "by_device"),
             repository = "local", format = "file"),
  tar_target(gg_replay_aft_loved,  plot_replay_after_loved(favorites_replayed),
             repository = "local", format = "file"),
  
  
  # Playlist use ------
  tar_target(users_playlists,              make_users_playlists_data(users, playlists, isei)),
  tar_target(gg_noplaylists_socdem,        plot_noplaylists_socdem(users_playlists), 
                                           repository = "local", format = "file"),
  
  # Playlists cat ------
  tar_target(gg_pl_annotations,     plot_pl_annotations(users_playlists),
                                    repository = "local", format = "file"),
  tar_target(gg_pl_ann_socdem,      plot_pl_annotations_bysocdem(users_playlists),
                                    repository = "local", format = "file")
  

)
