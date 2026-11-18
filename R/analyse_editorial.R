# Editorial (Deezer) playlists vs users' personal playlists ------
# Local inputs (outside code/, not on S3):
#  - ../data/deezer_editorial_playlists.csv: every playlist scraped from the
#    Deezer public API (notes/deezer_api.md), with account_type and fans.
#  - ../editorial_annotation/csv/editorial_annotated.csv: the subset annotated
#    with the users' 9-category LLM pipeline (editorial_annotation/
#    run_annotation.R): account types deezer_editor_fr, deezer_brand_account,
#    third_party_chart_curator. One 0/1 column per category.
# Shares = share of playlists carrying the label (multi-label: rows do not sum
# to 1), raw or weighted by fans.
# "top" does not mean the same thing on both sides: for users, personal
# favourites (user-centered); for editorial playlists, popularity charts
# ("Top France", "Les titres du moment"). top_coding = "charts" moves editorial
# "top" out of the user-centered family into a separate "charts" family.

editorial_categories <- c("genre", "period", "artist", "context", "person", "event", "mood", "top", "moment")

# Groups of accounts compared. "deezer_owned" (Deezer FR editors + Deezer brand
# accounts) is the main comparison; third-party chart curators (Digster,
# Topsify, Filtr...) are reported separately.
editorial_groups <- list(
  deezer_owned = c("deezer_editor_fr", "deezer_brand_account"),
  deezer_editor_fr = "deezer_editor_fr",
  deezer_brand_account = "deezer_brand_account",
  third_party_chart_curator = "third_party_chart_curator",
  all_annotated = c("deezer_editor_fr", "deezer_brand_account", "third_party_chart_curator")
)

read_editorial_annotated <- function(file){
  readr::read_csv(file, show_col_types = FALSE,
                  col_types = readr::cols(id = "c", creator_id = "c", .default = readr::col_guess())) %>%
    mutate(fans = as.numeric(fans),
           across(all_of(editorial_categories), as.integer))
}

# Number of scraped playlists by account type (whole scrape, annotated or not)
count_editorial_accounts <- function(file){
  readr::read_csv(file, show_col_types = FALSE, col_types = readr::cols(.default = "c")) %>%
    count(account_type, name = "n_playlists") %>%
    bind_rows(tibble(account_type = "(all scraped)", n_playlists = sum(.$n_playlists)))
}

# Family of each category; with top_coding = "charts", "top" becomes its own
# family (charts/popularity) instead of user-centered.
category_family <- function(category, top_coding = c("as_is", "charts")){
  top_coding <- match.arg(top_coding)
  fam <- as.character(recode_playlists(category, .what = "large_cat"))
  if(top_coding == "charts") fam[category == "top"] <- "Charts (editorial top)"
  fam
}

# Shares by category and family, raw and fans-weighted, for each account group
# and both codings of "top". Long tibble.
compute_editorial_shares <- function(editorial, groups = editorial_groups){
  one <- function(accounts, group, top_coding){
    d <- filter(editorial, account_type %in% accounts)
    long <- d %>%
      select(id, fans, all_of(editorial_categories)) %>%
      pivot_longer(all_of(editorial_categories), names_to = "category", values_to = "value") %>%
      mutate(family = category_family(category, top_coding))
    w <- function(x, f) sum(x * f, na.rm = TRUE) / sum(f[!is.na(x)])
    per_cat <- long %>%
      group_by(family, category) %>%
      summarize(n_with_label = sum(value == 1, na.rm = TRUE),
                share_raw = mean(value == 1, na.rm = TRUE),
                share_fans_weighted = w(value == 1, fans), .groups = "drop")
    per_pl_fam <- long %>%
      group_by(id, fans, family) %>%
      summarize(any = any(value == 1, na.rm = TRUE), .groups = "drop")
    per_fam <- per_pl_fam %>%
      group_by(family) %>%
      summarize(n_with_label = sum(any), share_raw = mean(any),
                share_fans_weighted = w(any, fans), .groups = "drop") %>%
      mutate(category = paste0("(family) ", family))
    k <- long %>% group_by(id, fans) %>% summarize(k = sum(value == 1, na.rm = TRUE), .groups = "drop")
    none <- tibble(family = NA_character_, category = "(no label)", n_with_label = sum(k$k == 0),
                   share_raw = mean(k$k == 0), share_fans_weighted = w(k$k == 0, k$fans))
    # Mood playlists that also carry a genre label
    mg <- tibble(family = NA_character_, category = "(mood and genre)",
                 n_with_label = sum(d$mood == 1 & d$genre == 1, na.rm = TRUE),
                 share_raw = sum(d$mood == 1 & d$genre == 1, na.rm = TRUE) / sum(d$mood == 1, na.rm = TRUE),
                 share_fans_weighted = NA_real_)
    bind_rows(per_cat, per_fam, none, mg) %>%
      mutate(group = group, top_coding = top_coding, n_playlists = nrow(d),
             total_fans = sum(d$fans, na.rm = TRUE), mean_labels = mean(k$k), .before = 1)
  }
  tidyr::expand_grid(group = names(groups), top_coding = c("as_is", "charts")) %>%
    purrr::pmap(function(group, top_coding) one(groups[[group]], group, top_coding)) %>%
    bind_rows()
}

# Other editorial figures quoted in the notes: share of fans held by the Deezer
# chart/best-of accounts, and the most-followed playlist.
summarise_editorial_misc <- function(editorial, groups = editorial_groups){
  d <- filter(editorial, account_type %in% groups$deezer_owned)
  by_creator <- d %>%
    group_by(creator_name) %>%
    summarize(n = n(), fans = sum(fans, na.rm = TRUE), .groups = "drop") %>%
    mutate(share_fans = fans / sum(fans)) %>%
    arrange(desc(fans))
  top1 <- slice_max(d, fans, n = 1, with_ties = FALSE)
  tibble(name = c("editorial_deezer_owned_share_fans_charts_bestof",
                  "editorial_deezer_owned_max_fans",
                  "editorial_deezer_owned_n_creators"),
         value = c(sum(by_creator$share_fans[str_detect(by_creator$creator_name, regex("chart|best of", ignore_case = TRUE))]),
                   top1$fans, nrow(by_creator)),
         description = c("Deezer-owned editorial playlists: share of fans held by creator accounts whose name contains 'Charts' or 'Best Of'",
                         str_glue("Deezer-owned editorial playlists: fans of the most-followed playlist ('{top1$title}')"),
                         "Deezer-owned editorial playlists: number of creator accounts"))
}

# Users vs editorial, one row per category/family. Users: share of annotated
# personal playlists with the label (per playlist) and per-user average share.
compare_editorial_users <- function(editorial_shares, desc_stats, group = "deezer_owned",
                                    filename = "output/editorial_vs_users.csv"){
  u <- desc_stats$cats %>%
    select(category, users_per_playlist = share_of_playlists, users_peruser_avg = peruser_avg_share_of_labels)
  e <- editorial_shares %>%
    filter(.data$group == !!group) %>%
    select(top_coding, category, share_raw, share_fans_weighted) %>%
    pivot_wider(names_from = top_coding, values_from = c(share_raw, share_fans_weighted),
                names_glue = "editorial_{.value}_{top_coding}")
  res <- full_join(u, e, by = "category") %>% mutate(editorial_group = group, .before = 1)
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(res, filename)
  res
}

plot_editorial_vs_users <- function(editorial_vs_users, filename = "output/editorial_vs_users.png"){
  set_ggplot_options()
  d <- editorial_vs_users %>%
    filter(category %in% editorial_categories) %>%
    select(category, Users = users_per_playlist,
           `Deezer editorial` = editorial_share_raw_as_is,
           `Deezer editorial (fans-weighted)` = editorial_share_fans_weighted_as_is) %>%
    pivot_longer(-category, names_to = "source", values_to = "share") %>%
    mutate(family = recode_playlists(category, .what = "large_cat"),
           category = recode_playlists(category, .what = "category"),
           category = if_else(category == "Top", "Top (editorial = charts)", as.character(category)),
           category = factor(category, levels = rev(unique(category[order(family)]))))
  gg <- ggplot(d, aes(share, category, fill = source)) +
    geom_col(position = "dodge") +
    facet_grid(family ~ ., scales = "free_y", space = "free_y") +
    scale_x_continuous(labels = scales::percent) +
    labs(x = "Share of playlists with the label", y = "", fill = "")
  ggsave(filename, gg, width = 8, height = 6)
  filename
}
