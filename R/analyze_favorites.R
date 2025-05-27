make_favorites_replayed_data <- function(streaming_data){
  require(tidytable)
  
  streaming_data <- streaming_data %>% 
    mutate(date = as_datetime(ts_listen)) %>% 
    select(-ts_listen) %>% 
    arrange(hashed_id, date, song_id) %>% 
    group_by(hashed_id) %>% 
    # remove all songs already played the first year
    filter(date > (first(date) + lubridate::duration(1, units = "year"))) %>% 
    group_by(hashed_id, song_id)

  nlisten_after_discovery <- streaming_data %>% 
    summarize(nlisten_after_discovery = n(),
              context_at_discovery = first(context_4)) %>% 
    ungroup()
  
  nlisten_at_clicked_love <- streaming_data %>% 
    mutate(n = row_number()) %>% 
    filter(click_loved == 1) %>% 
    slice(1) %>% 
    select(hashed_id, 
           song_id, 
           nlisten_at_clicked_love = "n",
           context_at_clicked_love = "context_4") %>% 
    ungroup()
  
  nlisten_after_clicked_love <- streaming_data %>% 
    mutate(scl = cumsum(click_loved)) %>% 
    filter(scl > 0) %>% 
    summarize(nlisten_after_clicked_love = n()-1) %>% 
    ungroup()
  
  res <- nlisten_after_discovery %>% 
    full_join(nlisten_after_clicked_love) %>% 
    full_join(nlisten_at_clicked_love)
  
  return(res)
}

plot_like_at_first_sight <- function(favorites_replayed){
  
}