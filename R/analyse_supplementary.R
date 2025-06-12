table_descriptive_stats <- function(playlists, users){
  res <- tribble(~Var,~Val,
          "No of playlists", nrow(playlists),
          "No of unique playlists titles", distinct(playlists, title) %>% nrow(),
          "Number of respondents", nrow(users))
  filename <- str_glue("output/supplementary_stats.md")
  knitr::kable(res) %>% 
    kableExtra::save_kable(filename)
  return(filename)
}
