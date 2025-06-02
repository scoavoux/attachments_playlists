# make table of NO of playlist by gender, education level, etc.
function(users, playlists, isei){
  d <- users %>% 
    select(hashed_id, E_gender, E_birth_year, E_diploma) %>% 
    left_join(isei) %>% 
    left_join(count(playlists, hashed_id, name = "n_playlists"))
  d <- d %>% 
    mutate(gender = factor(E_gender, levels = c("Un homme", "Une femme")),
           degree = ifelse(E_diploma == "", NA, E_diploma) %>% 
             factor() %>% 
             fct_collapse(less_highschool = c("Aucun diplôme",
                                              "CEP (certificat d'études primaires)", 
                                              "BEPC, brevet élementaire, brevet des collèges",
                                              "CAP, BEP, brevet de compagnon"),
                          highschool = c("Bac général, brevet supérieur", 
                                         "Bac pro ou techno, brevet professionnel ou de technicien, BEA, BEC, BEI, BEH, capacité en droit"),
                          highered_short = c(
                            "DEUG, BTS, DUT, DEUST, diplôme des professions sociales ou de la santé, d'infirmier.ère", 
                            "Licence, licence pro, maîtrise, BUT"), 
                          highered_long = c(
                            "Master, diplôme d'ingénieur.e, DEA, DESS", 
                            "Doctorat (y compris médecine, pharmacie, dentaire), HDR")
             ),
           age = 2023-E_birth_year,
           age_cat = cut(2023-E_birth_year, breaks = c(0, 25, 35, 45, 55, 100)),
           isei_quartile = cut(isei, breaks = c(0, quantile(isei, seq(.25, .75, .25), na.rm=TRUE), 100)))
  ggplot(d, aes(n_playlists)) +
    geom_histogram() +
    scale_x_log10()
  d %>% 
    mutate(np = cut(n_playlists, c(0, 1, 2, 3, 5, 10, 20, 50, 100, 5000))) %>% 
    janitor::tabyl(np, E_diploma) %>% 
    adorn_percentages()
  lm(log(n_playlists)~E_gender + age + isei, d) %>% 
    summary()
}
  #tar_load(c(users, playlists, isei))

  
