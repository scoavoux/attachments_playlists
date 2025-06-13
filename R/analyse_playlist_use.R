make_users_playlists_data <- function(users, playlists, isei, volume_play){
  d <- users %>% 
    select(hashed_id, E_gender, E_birth_year, E_diploma) %>% 
    left_join(isei) %>% 
    mutate(gender = factor(E_gender, 
                           levels = c("Un homme", "Une femme"),
                           labels = c("Men", "Women")),
           degree = ifelse(E_diploma == "", NA, E_diploma) %>% 
             factor() %>% 
             fct_collapse(`No high school` = c("Aucun diplôme",
                                              "CEP (certificat d'études primaires)", 
                                              "BEPC, brevet élementaire, brevet des collèges",
                                              "CAP, BEP, brevet de compagnon"),
                          `High school` = c("Bac général, brevet supérieur", 
                                         "Bac pro ou techno, brevet professionnel ou de technicien, BEA, BEC, BEI, BEH, capacité en droit"),
                          `College` = c(
                            "DEUG, BTS, DUT, DEUST, diplôme des professions sociales ou de la santé, d'infirmier.ère", 
                            "Licence, licence pro, maîtrise, BUT"), 
                          `Graduate` = c(
                            "Master, diplôme d'ingénieur.e, DEA, DESS", 
                            "Doctorat (y compris médecine, pharmacie, dentaire), HDR")
             ),
           age = 2023-E_birth_year,
           age_cat = cut(2023-E_birth_year, 
                         breaks = c(0, 25, 35, 45, 55, 100), 
                         labels = c("24-", "25-34", "35-44", "45-54", "55+")),
           isei_quartile = cut(isei, 
                               breaks = c(0, quantile(isei, seq(.25, .75, .25), na.rm=TRUE), 100),
                               labels = paste0("Q", 1:4)))
  
  playlists <- playlists %>% 
    filter(!(title %in% c("Loved tracks", "Loved Tracks", "Coups de cœur")))
  np <-  count(playlists, hashed_id, name = "n_playlists")
  ap <- playlists %>% 
    select(-playlist_id, -title) %>% 
    pivot_longer(-hashed_id) %>% 
    filter(!is.na(value)) %>% 
    group_by(hashed_id, name) %>% 
    summarize(n = sum(value)) %>% 
    pivot_wider(names_from = name, values_from = n, values_fill = 0)
  d <- d %>% 
    left_join(np) %>%
    left_join(ap) %>% 
    left_join(volume_play) %>% 
    mutate(n_playlists = ifelse(is.na(n_playlists), 0, n_playlists))
  return(d)
}

plot_noplaylists <- function(users_playlists){
  set_ggplot_options()
  d <- users_playlists %>% 
    select(n_playlists) %>% 
    filter(!is.na(n_playlists)) %>% 
    filter(n_playlists < 50)
  count(d, n_playlists)
  gg <- d %>% 
    ggplot(aes(n_playlists)) +
      geom_density() +
      labs(x = "No. of playlists", y = "")
  filename <- str_glue("output/noplaylists.png")
  ggsave(filename, gg)
  return(filename)
  
}

# make table of NO of playlist by gender, education level, etc.
plot_noplaylists_socdem <- function(users_playlists, .type = c("ridge", "boxplot")){
  set_ggplot_options()
  d <- users_playlists %>% 
    select(n_playlists, gender, age_cat, isei_quartile, degree) %>% 
    pivot_longer(-n_playlists) %>% 
    filter(!is.na(value)) %>% 
    filter(n_playlists < 50) %>% 
    mutate(name = factor(name, levels = c("age_cat", "degree", "gender", "isei_quartile"),
                         labels = c("Age class", "Degree", "Gender", "ISEI")))
  .type = .type[1]
  if(.type == "ridge"){
    gg <- d %>% 
      ggplot(aes(n_playlists, value)) +
        ggridges::geom_density_ridges() +
        facet_wrap(~name, scales = "free_y") +
        labs(x = "No. of playlists", y = "")
  } else if(.type == "boxplot"){
    gg <- d %>% 
      ggplot(aes(n_playlists, value)) +
        geom_boxplot() +
        facet_wrap(~name, scales = "free_y") +
        labs(x = "No. of playlists", y = "")
  }
  filename <- str_glue("output/noplaylists_socdem_{.type}.png")
  ggsave(filename, gg)
  return(filename)
  
}

plot_noplaylists_poisson <- function(users_playlists){
  library(modelsummary)
  mod_full <- glm(n_playlists ~ gender + age + isei + degree  + log(n_play_2023), family = "poisson", data = users_playlists)
  gg <- modelplot(mod_full, coef_omit = "(Intercept)", 
            coef_rename = rev(c("Log vol. play", "Graduate education", "College education", "High school education", "ISEI", "Age in years", "Woman")))
  filename <- str_glue("output/no_playlist_poisson_reg.png")
  ggsave(filename, gg)
  return(filename)
}

  
