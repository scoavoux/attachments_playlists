recode_playlists <- function(x, .what){
  if(.what == "large_cat"){
    res <- c(genre  = "Music-centered",
             period = "Music-centered",
             artiste = "Music-centered",
             contexte = "Context-centered",
             personne = "Context-centered",
             events = "Context-centered",
             mood = "User-centered",
             top = "User-centered",
             moment = "User-centered")[x]
    res <- factor(res, levels = c("Music-centered", "Context-centered", "User-centered"))
  } else if(.what == "category"){
    res <- factor(x, levels = c("genre", 
                                "period", 
                                "artiste", 
                                "contexte", 
                                "personne", 
                                "events", 
                                "mood", 
                                "top", 
                                "moment"),
                  labels =  c("Genre",
                              "Period",
                              "Artist",
                              "Context",
                              "Person",
                              "Event",
                              "Mood",
                              "Top",
                              "Moment"))
  }
  return(res)
}

plot_pl_annotations <- function(users_playlists){
  set_ggplot_options()
  #todo: ridges?
  d <- users_playlists %>% 
    select(hashed_id, starts_with("an_")) %>% 
    pivot_longer(-hashed_id) %>% 
    mutate(name = str_remove(name, "^an_"),
           cat = recode_playlists(name, .what = "large_cat"),
           name = recode_playlists(name, .what = "category")) %>% 
    group_by(hashed_id) %>% 
    mutate(f = value/sum(value)) %>% 
    group_by(name, cat) %>% 
    summarize(m = mean(f, na.rm=TRUE))
  gg <- d %>%  ggplot(aes(m, name)) +
     geom_col() +
     facet_wrap(~cat, nrow=3, scales = "free_y") +
     labs(x = "Average share of playlists", y="")
  
  filename <- str_glue("output/playlist_annotations_desc.png")
  ggsave(filename, gg)
  return(filename)
}

plot_pl_annotations_bysocdem <- function(users_playlists){
  set_ggplot_options()
  d <- users_playlists %>% 
    select(hashed_id, gender, degree, age_cat, isei_quartile, starts_with("an_")) %>% 
    pivot_longer(starts_with("an_"), names_to = "category", values_to = "count") %>% 
    group_by(hashed_id) %>% 
    mutate(f = count / sum(count)) %>% 
    select(-count) %>% 
    pivot_longer(gender:isei_quartile, names_to = "socdem", values_to = "socdem_value") %>% 
    filter(!is.na(socdem_value)) %>% 
    mutate(category = str_remove(category, "^an_"),
           large_cat = recode_playlists(category, .what = "large_cat"),
           category = recode_playlists(category, .what = "category"),
           socdem = factor(socdem, 
                           levels = c("age_cat", "degree", "gender", "isei_quartile"),
                           labels = c("Age class", "Education", "Gender", "ISEI"))) %>% 
    group_by(socdem, socdem_value, large_cat, category) %>% 
    summarize(m = mean(f, na.rm=TRUE))

  gg <- d %>% 
    ggplot(aes(m, socdem_value, fill = category)) +
      geom_col() +
      facet_grid(socdem~large_cat, scale = "free_y") +
      labs(x = "Average share of playlists", y = "", fill = "")

  filename <- str_glue("output/playlist_annotations_by_socdem.png")
  ggsave(filename, gg)
  return(filename)
  
}