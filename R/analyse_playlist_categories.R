recode_playlists <- function(x, .what){
  if(.what == "large_cat"){
    res <- c(genre  = "Music-centered",
             period = "Music-centered",
             artiste = "Music-centered",
             contexte = "Context-centered",
             personne = "Context-centered",
             evenement = "Context-centered",
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
                                "evenement", 
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
      facet_grid(socdem~large_cat, scale = "free_y", space = "free_y") +
      labs(x = "Average share of playlists", y = "", fill = "") +
      scale_fill_brewer(palette = "Set1")

  filename <- str_glue("output/playlist_annotations_by_socdem.png")
  ggsave(filename, gg)
  return(filename)
  
}


plot_typeplaylists_poisson <- function(users_playlists){
  library(modelsummary)
  mod <- list()
  for(x in c("an_artiste", "an_contexte", "an_evenement",
             "an_genre", "an_moment", "an_mood",
             "an_period", "an_personne", "an_top")){
    formula <- as.formula(str_glue("{x} ~ gender + age + isei + degree  + log(n_play_2023)"))
    mod[[x]] <- glm(formula, family = "poisson", data = users_playlists)
  }
  names(mod) <- names(mod) %>% 
    str_remove("an_") %>% 
    recode_playlists(.what = "category") %>% 
    as.character()
  gg <- modelplot(mod, coef_omit = "(Intercept)", 
                  coef_rename = rev(c("Log vol. play", "Graduate education", "College education", "High school education", "ISEI", "Age in years", "Woman")))
  filename <- str_glue("output/type_playlist_poisson_reg.png")
  ggsave(filename, gg)
  return(filename)
}