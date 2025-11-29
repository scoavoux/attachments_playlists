## This scripts runs the few shots annotations of a thousand playlists
## manually annotated by the two authors and one research assistant.
## It produces detailed results as well as diagnostics metrics.
## Ran last Nov. 27, 2025

# Libraries ------
library(tidyverse)
library(ellmer)
library(jsonlite)    # to parse the results
library(caret)       # to compute diagnostics

source("playlist_annotation/annotation_functions.R")

# Data ------
groundtruth <- read_csv("data/groundtruth_reconciliation.csv")
groundtruth <- groundtruth |> 
  pivot_wider(names_from = cat, values_from = valeur) |> 
  mutate(across(where(is.numeric), ~factor(.x, levels = c(0, 1))),
         playlist_id = row_number())

groundtruth_period <- groundtruth
groundtruth <- filter(groundtruth, !is.na(person))

prompts <- prepare_prompts(groundtruth, 50L)
# should we do extended for period?

# Run once -------
categories <- c("genre", "artist", "period", "context", "moment", "top", "event", "person", "mood")

for(cat in categories){
  prompt_path <- str_glue("playlist_annotation/prompts/prompt_{cat}.txt")
  json_path <- str_glue("playlist_annotation/test_results/test_{cat}.json")
  csv_path <- str_glue("playlist_annotation/test_results/test_results_{cat}.csv")
  # annotate_playlists(prompts,
  #                    prompt_path = prompt_path,
  #                    json_output_path = json_path)
  print(str_glue("{cat} done"))
  d <- read_annotation_results(json_path)
  write_csv(d, csv_path)
}

diagnostics <- vector("list", length = length(categories))
names(diagnostics) <- categories
errors <- vector("list", length = length(categories))
names(errors) <- categories
for(cat in categories){
  testset <- str_glue("playlist_annotation/test_results/test_results_{cat}.csv") %>% 
    read_csv()
  diagnostics[[cat]] <- compute_diagnostics(testset, groundtruth, cat)
  errors[[cat]] <- extract_errors(testset, groundtruth, cat)
}
diagnostics <- bind_rows(diagnostics)

bind_rows(errors) %>% 
  pivot_longer(genre:mood) %>% 
  filter(!is.na(value)) %>% 
  write_csv("errors.csv")

write_csv(diagnostics, "data/diagnostics_gpt5.1.csv")
