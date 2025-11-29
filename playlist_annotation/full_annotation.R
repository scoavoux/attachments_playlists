## This scripts performs FS annotation on the whole corpus.
## Ran Nov. 28-29, 2025

# Libraries ------
library(tidyverse)
library(ellmer)
library(jsonlite)    # to parse the results
library(targets)
source("playlist_annotation/annotation_functions.R")

# Data ------

tar_load(unique_titles)
titles <- read_csv(unique_titles)
titles <- titles %>% 
  mutate(playlist_id = row_number())
prompts <- prepare_prompts(titles, 50L)

# Categories ------
categories <- c("genre", "artist", "period", "context", "moment", "top", "event", "person", "mood")

cat_done <- rep(FALSE, 9)
names(cat_done) <- categories

for(cat in categories){
  if(cat_done[cat]) {
    print(str_glue("Skipping category {cat}: already done"))
    next
    }
  prompt_path <- str_glue("playlist_annotation/prompts/prompt_{cat}.txt")
  
  batch_done <- rep(FALSE, 5)
  
  for(i in 1:5){
    if(batch_done[i]){
      print(str_glue("Skipping category {cat} batch {i}/5: already done"))
      next
      }
    json_path <- str_glue("playlist_annotation/full_annotation/json/results_{cat}_{i}.json")
    
    pr <- slice(prompts, seq(((i-1)*500+1), i*500))
    annotate_playlists(pr,
                       prompt_path = prompt_path,
                       json_output_path = json_path)
    batch_done[i] <- TRUE
    print(str_glue("{cat} batch {i}/5 done"))
  }
  cat_done[cat] <- TRUE
}

json_files <- dir("playlist_annotation/full_annotation/json", full.names = TRUE)

for(cat in categories){
  cat_files <- json_files[str_detect(json_files, cat)]
  csv_path <- str_glue("playlist_annotation/full_annotation/csv/full_{cat}.csv")
  d <- map(cat_files, read_annotation_results) %>% 
    bind_rows() %>% 
    rename(playlist_id = "id") %>% 
    mutate(playlist_id = as.integer(playlist_id))
  d <- left_join(titles, d) %>% 
    select(-playlist_id)
  write_csv(d, csv_path)
}


