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
    csv_path <- str_glue("playlist_annotation/full_annotation/csv/results_{cat}_{i}.csv")
    pr <- slice(prompts, seq(((i-1)*500+1), i*500))
    annotate_playlists(pr,
                       prompt_path = prompt_path,
                       json_output_path = json_path)
    batch_done[i] <- TRUE
    print(str_glue("{cat} batch {i}/5 done"))
  }
  cat_done[cat] <- TRUE
}

d <- read_annotation_results(json_path)
write_csv(d, csv_path)
