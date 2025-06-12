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

plot_like_at_first_sight <- function(favorites_replayed, 
                                     .what = c("raw", "by_device"),
                                     .zoomed = FALSE){
  set_ggplot_options()
  options(scipen = 99)
  .what = .what[1]
  favorites_replayed <- favorites_replayed %>% 
    filter(!is.na(nlisten_at_clicked_love)) %>% 
    mutate(nlisten_at_clicked_love = ifelse(nlisten_at_clicked_love > 500, 500, nlisten_at_clicked_love)) %>% 
    filter(context_at_discovery %in% c("edito", "organic", "reco_algo")) %>% 
    mutate(context_at_discovery2 = recode_device(context_at_discovery))
  if(.what == "raw"){
    if(.zoomed){
      favorites_replayed <- favorites_replayed %>% 
        filter(nlisten_at_clicked_love <= 10) %>% 
        mutate(nlisten_at_clicked_love = as.integer(nlisten_at_clicked_love))
      gg <- favorites_replayed %>% 
        ggplot(aes(nlisten_at_clicked_love)) +
        geom_bar() +
        scale_y_log10(label = scales::comma, breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000)) +
        labs(x = "No. play before clicked like button", y = "") +
        scale_x_continuous(breaks = 1:10)
      
      
    } else if(!.zoomed){
      gg <- favorites_replayed %>% 
        ggplot(aes(nlisten_at_clicked_love)) +
        geom_histogram() +
        scale_y_log10(label = scales::comma, breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000)) +
        labs(x = "No. play before clicked like button", y = "")
    }
    
  } else if(.what == "by_device"){
    gg <- favorites_replayed %>% 
      count(context_at_discovery2, nlisten_at_clicked_love) %>% 
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
  filename <- str_glue("output/like_at_first_sight_{.what}_{zoomed_char}.png")
  ggsave(filename, gg)
  return(filename)
}

plot_replay_after_loved <- function(favorites_replayed){
  set_ggplot_options()
  favorites_replayed <- favorites_replayed %>% 
    filter(context_at_discovery %in% c("edito", "organic", "reco_algo")) %>% 
    mutate(nlisten_after_discovery = ifelse(nlisten_after_discovery > 500, 500, nlisten_after_discovery),
           liked = case_when(is.na(nlisten_at_clicked_love) ~ "Never",
                             nlisten_at_clicked_love == 1 ~ "At first sight",
                             nlisten_at_clicked_love > 1 ~ "Later") %>% 
             factor(levels = c("Never", "At first sight", "Later")),
           context_at_discovery2 = recode_device(context_at_discovery))
  d <- favorites_replayed %>% 
    count(context_at_discovery2, liked, nlisten_after_discovery) %>% 
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