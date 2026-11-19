# Descriptive statistics ------
# `playlists` contains every playlist of the respondents, including the
# favourites playlists automatically created by Deezer ("Loved tracks",
# "Coups de cœur"). All analyses (users_playlists) exclude them, so the
# descriptive numbers below are computed on personal playlists, with the totals
# including favourites given only for reconciliation.
# A playlist can carry several labels: category shares are reported per
# playlist ("share of annotated playlists with label X", rows do not sum to 1)
# and, for comparison, with the per-user average-share metric used in
# plot_pl_annotations() (labels of a user normalised to sum to 1, then averaged
# over users).
# compute_descriptive_stats() returns the numbers (target desc_stats, also fed
# to misc_numbers); write_descriptive_stats() writes the md/csv tables.
compute_descriptive_stats <- function(playlists, users, users_playlists){
  personal <- filter_personal_playlists(playlists)
  an_cols <- names(playlists)[str_detect(names(playlists), "^an_")]
  annotated <- personal %>% filter(if_any(all_of(an_cols), ~!is.na(.x)))
  fully_annotated <- personal %>% filter(if_all(all_of(an_cols), ~!is.na(.x)))
  per_user <- count(personal, hashed_id)
  per_user_all <- users %>% select(hashed_id) %>% left_join(per_user, by = "hashed_id") %>%
    mutate(n = coalesce(n, 0L))

  res <- tribble(~Var, ~Val,
    "Respondents (survey, complete, France)", nrow(users),
    "All playlists of respondents, incl. favourites playlists", nrow(playlists),
    "  of which favourites playlists (Loved tracks / Coups de coeur)", nrow(playlists) - nrow(personal),
    "Respondents with >= 1 playlist of any kind", n_distinct(playlists$hashed_id),
    "Respondents with >= 1 personal playlist", nrow(per_user),
    "Share of respondents with >= 1 personal playlist", nrow(per_user) / nrow(users),
    "Share of respondents without any personal playlist", 1 - nrow(per_user) / nrow(users),
    "Personal playlists (excl. favourites)", nrow(personal),
    "Unique personal playlist titles (exact)", n_distinct(personal$title),
    "Unique personal playlist titles (trimmed, case-insensitive)", n_distinct(tolower(trimws(personal$title))),
    "Personal playlists with >= 1 category annotation", nrow(annotated),
    "Share of personal playlists annotated (>= 1 category)", nrow(annotated) / nrow(personal),
    "Share of personal playlists annotated (all 9 categories)", nrow(fully_annotated) / nrow(personal),
    "Personal playlists per respondent: median (all respondents)", median(per_user_all$n),
    "Personal playlists per respondent: mean (all respondents)", mean(per_user_all$n),
    "Personal playlists per user: median (users with >= 1)", median(per_user$n),
    "Personal playlists per user: mean (users with >= 1)", mean(per_user$n),
    "Personal playlists per user: max", max(per_user$n),
    "Max playlists per user incl. favourites (old table)", max(count(playlists, hashed_id)$n)
  )

  # Category shares, per playlist (among playlists annotated on that category)
  long <- annotated %>%
    select(playlist_id, all_of(an_cols)) %>%
    pivot_longer(-playlist_id, names_to = "category", values_to = "value") %>%
    mutate(category = str_remove(category, "^an_"),
           family = recode_playlists(category, .what = "large_cat"))
  per_cat <- long %>%
    group_by(family, category) %>%
    summarize(n_playlists_with_label = sum(value == 1, na.rm = TRUE),
              share_of_playlists = mean(value == 1, na.rm = TRUE), .groups = "drop")
  per_fam <- long %>%
    group_by(playlist_id, family) %>%
    summarize(any = any(value == 1, na.rm = TRUE), .groups = "drop") %>%
    group_by(family) %>%
    summarize(n_playlists_with_label = sum(any), share_of_playlists = mean(any), .groups = "drop") %>%
    mutate(category = paste0("(family) ", family))
  nlabels <- long %>% group_by(playlist_id) %>% summarize(k = sum(value == 1, na.rm = TRUE))
  per_nolabel <- tibble(family = NA, category = "(no label)",
                        n_playlists_with_label = sum(nlabels$k == 0),
                        share_of_playlists = mean(nlabels$k == 0))
  # Per-user average share of labels (metric of plot_pl_annotations)
  peruser <- users_playlists %>%
    select(hashed_id, starts_with("an_")) %>%
    pivot_longer(-hashed_id, names_to = "category") %>%
    mutate(category = str_remove(category, "^an_")) %>%
    group_by(hashed_id) %>%
    mutate(f = value / sum(value)) %>%
    group_by(category) %>%
    summarize(peruser_avg_share_of_labels = mean(f, na.rm = TRUE))
  peruser_fam <- peruser %>%
    mutate(family = recode_playlists(category, .what = "large_cat")) %>%
    group_by(family) %>%
    summarize(peruser_avg_share_of_labels = sum(peruser_avg_share_of_labels)) %>%
    mutate(category = paste0("(family) ", family)) %>% select(-family)
  cats <- bind_rows(per_cat, per_fam, per_nolabel) %>%
    left_join(bind_rows(peruser, peruser_fam), by = "category") %>%
    mutate(family = as.character(family))

  list(res = res, cats = cats, mean_labels = mean(nlabels$k),
       n_annotated = nrow(annotated), n_personal = nrow(personal))
}

# Writes the tables of compute_descriptive_stats() (target desc_stats).
write_descriptive_stats <- function(desc_stats){
  res <- desc_stats$res; cats <- desc_stats$cats
  dir.create("output", showWarnings = FALSE)
  filename <- "output/supplementary_stats.md"
  knitr::kable(res, digits = 3, format = "pipe") %>% writeLines(filename)
  write_csv(res, "output/supplementary_stats.csv")
  filename2 <- "output/playlist_categories_shares.md"
  c(str_glue("Mean number of labels per annotated playlist: {round(desc_stats$mean_labels, 3)}"), "",
    knitr::kable(cats, digits = 3, format = "pipe")) %>% writeLines(filename2)
  write_csv(cats, "output/playlist_categories_shares.csv")
  return(c(filename, "output/supplementary_stats.csv", filename2, "output/playlist_categories_shares.csv"))
}
