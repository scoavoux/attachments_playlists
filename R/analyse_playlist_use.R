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
           # right = FALSE so that the classes match their labels
           # (the default right = TRUE put 25-year-olds in "24-", 35 in "25-34"...)
           age_cat = cut(2023-E_birth_year, 
                         breaks = c(0, 25, 35, 45, 55, 100), 
                         right = FALSE,
                         labels = c("24-", "25-34", "35-44", "45-54", "55+")),
           isei_quartile = cut(isei, 
                               breaks = c(0, quantile(isei, seq(.25, .75, .25), na.rm=TRUE), 100),
                               labels = paste0("Q", 1:4)))
  
  playlists <- filter_personal_playlists(playlists)
  np <-  count(playlists, hashed_id, name = "n_playlists")
  ap <- playlists %>% 
    select(hashed_id, starts_with("an_")) %>% 
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
                         labels = c("Age class", "Degree", "Gender", "Social status\n (ISEI quartile)")))
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

# Favourites playlists are automatically created by Deezer, not by the user.
favorites_playlists_titles <- c("Loved tracks", "Loved Tracks", "Coups de cœur")
filter_personal_playlists <- function(playlists){
  filter(playlists, !(title %in% favorites_playlists_titles))
}

# Named (not positional) coefficient labels
coef_labels <- c("genderWomen" = "Woman",
                 "age" = "Age in years",
                 "isei" = "Social status (ISEI)",
                 "degreeHigh school" = "High school education",
                 "degreeCollege" = "College education",
                 "degreeGraduate" = "Graduate education",
                 "log(n_play_2023)" = "Log vol. play")

playlist_count_formula <- function(y){
  as.formula(str_glue("{y} ~ gender + age + isei + degree + log(n_play_2023)"))
}

fit_count_model <- function(y, data, family = c("poisson", "negbin")){
  family <- match.arg(family)
  f <- playlist_count_formula(y)
  if(family == "poisson"){
    glm(f, family = "poisson", data = data)
  } else {
    m <- MASS::glm.nb(f, data = data)
    m$call$formula <- f
    m
  }
}

plot_noplaylists_poisson <- function(users_playlists, family = c("poisson", "negbin")){
  library(modelsummary)
  family <- match.arg(family)
  mod_full <- fit_count_model("n_playlists", users_playlists, family)
  gg <- modelplot(mod_full, coef_omit = "(Intercept)", coef_map = rev(coef_labels))
  suffix <- c(poisson = "poisson", negbin = "negbin")[family]
  filename <- str_glue("output/no_playlist_{suffix}_reg.png")
  ggsave(filename, gg)
  return(filename)
}

# Poisson vs negative binomial, for the number of playlists and for each
# playlist category count. Writes a model-level table (dispersion, theta, AIC,
# LR test) and a coefficient-level table (estimates, SE, p, sign/significance
# changes).
compare_poisson_negbin <- function(users_playlists, 
                                   filename = "output/poisson_vs_negbin_models.csv"){
  outcomes <- c("n_playlists", sort(names(users_playlists)[str_detect(names(users_playlists), "^an_")]))
  models <- list(); coefs <- list()
  for(y in outcomes){
    mp <- fit_count_model(y, users_playlists, "poisson")
    mn <- fit_count_model(y, users_playlists, "negbin")
    pearson <- sum(residuals(mp, type = "pearson")^2) / df.residual(mp)
    lr <- 2 * (as.numeric(logLik(mn)) - as.numeric(logLik(mp)))
    models[[y]] <- tibble(outcome = y, n_obs = nobs(mp), 
                          mean_y = mean(mp$y), var_y = var(mp$y),
                          poisson_pearson_dispersion = pearson,
                          poisson_deviance_dispersion = deviance(mp) / df.residual(mp),
                          negbin_theta = mn$theta, negbin_theta_se = mn$SE.theta,
                          aic_poisson = AIC(mp), aic_negbin = AIC(mn),
                          lr_stat = lr, 
                          # boundary test: halve the chi2(1) p-value
                          lr_p = 0.5 * pchisq(lr, df = 1, lower.tail = FALSE))
    cp <- summary(mp)$coefficients; cn <- summary(mn)$coefficients
    coefs[[y]] <- tibble(outcome = y, term = rownames(cp),
                         est_poisson = cp[, 1], se_poisson = cp[, 2], p_poisson = cp[, 4],
                         est_negbin = cn[rownames(cp), 1], se_negbin = cn[rownames(cp), 2], 
                         p_negbin = cn[rownames(cp), 4]) %>% 
      filter(term != "(Intercept)") %>% 
      mutate(sig_poisson = p_poisson < .05, sig_negbin = p_negbin < .05,
             sign_change = sign(est_poisson) != sign(est_negbin),
             significance_change = sig_poisson != sig_negbin)
  }
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(bind_rows(models), filename)
  write_csv(bind_rows(coefs), str_replace(filename, "_models\\.csv$", "_coefficients.csv"))
  return(c(filename, str_replace(filename, "_models\\.csv$", "_coefficients.csv")))
}


