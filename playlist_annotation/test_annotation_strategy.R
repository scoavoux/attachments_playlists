# TODO -------
# Dernier problème est celui de period.
# Ajouter 1000 points supplémentaires.
# fit regex sur ensemble des titres qui récupère bien les periodes, 
# en particulier les XXX0s/XXX0's/XXX0', années XXX0, années X0.
# ATTENTION à l'overfit: prendre un validation set si on fait ça.
# Enfin, utiliser design based supervised model pour prendre en compte les erreurs
# dans les analyses suivanes
# https://naokiegami.com/paper/dsl_ss.pdf

# Libraries ------
library(tidyverse)
library(openai)      # to communicate with openai api
library(jsonlite)    # to parse the results
library(caret)       # to compute diagnostics

# Functions ------
annotate_playlists <- function(playlists_titles, prompt_path, rows = NULL){
  library(tidyverse)
  library(openai)    # to communicate with openai api
  library(jsonlite)  # to parse the results
  
  assistant_prompt <- read_file(prompt_path)
  
  if(!is.null(rows)){
    playlists_titles <- slice(playlists_titles, rows)
  }
  
  prompts <- playlists_titles %>% 
    mutate(gpt_instructions = paste0("playlist_id: ", playlist_id, ", title: ", title)) %>% 
    mutate(group = floor((row_number() -1)/10) %>% factor()) %>% 
    summarise(gpt_instructions = paste0(gpt_instructions, collapse = "\n"), .by = group,
              gpt = NA)
  #prompts <- mutate(prompts, done=FALSE, gpt = NA)
  
  # send prompts to gpt and get the results
  for(i in 1:nrow(prompts)){
    prompts$gpt[i] <- create_chat_completion(
      model = "gpt-4o-mini",
      messages = list(
        list(
          "role" = "system",
          "content" = assistant_prompt
        ),
        list(
          "role" = "user",
          "content" = prompts$gpt_instructions[i]
        )
      )
    )$choices$message.content[1]
    #print(i)
  }
  
  # format the results as a data frame
  res <- paste0("[", 
                paste(str_replace_all(prompts$gpt, "\\}[\\s]+\\{", "},{"), collapse = ","),
                "]") %>% fromJSON()
  return(res)
}

compute_diagnostics <- function(testset, groundtruth, cat){
  x <- testset |> 
    mutate(across(everything(), as.integer)) |> 
    rename(playlist_id = "id", gpt = cat) |> 
    mutate(gpt = factor(gpt, levels = c(0, 1))) |> 
    left_join(select(groundtruth, title, playlist_id, !!enquo(cat)))
  if(cat == "period"){
    x <- x |>
      mutate(gpt = as.character(gpt),
             gpt = ifelse(str_detect(title, "\\d{4}"), 0, gpt),
             gpt = ifelse(str_detect(title, "\\d{1,3}0['’]s?"), 1, gpt),
             gpt = ifelse(str_detect(title, "ann[ée]es?\\s*\\d{1,3}0"), 1, gpt),
             gpt = ifelse(str_detect(title, "\\b[Oo]ld(ies)?\\b"), 1, gpt),
             gpt = ifelse(str_detect(title, "\\b[tT]rad(itionnel|itional|\\.)?\\b"), 1, gpt),
             gpt = factor(gpt, levels = c(0,1)))
  }
  
  cm <- caret::confusionMatrix(x$gpt, x[,cat], positive = "1")
  n <- cm$byClass[c("Precision", "Recall", "F1")]
  res <- tibble(category = cat,
                Precision = n["Precision"],
                Recall = n["Recall"],
                F1 = n["F1"])
  res
  return(res)
}


# Load data ------
groundtruth <- read_csv("data/groundtruth_reconciliation.csv")
groundtruth <- groundtruth |> 
  pivot_wider(names_from = cat, values_from = valeur) |> 
  mutate(across(where(is.numeric), ~factor(.x, levels = c(0, 1))),
         playlist_id = row_number())

# GPT annotate groundtruth ------
gpt_test_artist_person 	      <-  annotate_playlists(groundtruth, prompt_path = "playlist_annotation/prompts/prompt_artist_person.txt"		)
gpt_test_person 				      <-  annotate_playlists(groundtruth, prompt_path = "playlist_annotation/prompts/prompt_person.txt"				)
gpt_test_artist 				      <-  annotate_playlists(groundtruth, prompt_path = "playlist_annotation/prompts/prompt_artist.txt"				)
gpt_test_event_moment_period 	<-  annotate_playlists(groundtruth, prompt_path = "playlist_annotation/prompts/prompt_event_moment_period.txt"	)
gpt_test_event 					      <-  annotate_playlists(groundtruth, prompt_path = "playlist_annotation/prompts/prompt_event.txt"				)
gpt_test_moment 				      <-  annotate_playlists(groundtruth, prompt_path = "playlist_annotation/prompts/prompt_moment.txt"				)
gpt_test_period 				      <-  annotate_playlists(groundtruth, prompt_path = "playlist_annotation/prompts/prompt_period.txt"				)
gpt_test_genre 					      <-  annotate_playlists(groundtruth, prompt_path = "playlist_annotation/prompts/prompt_genre.txt"				)
gpt_test_context 				      <-  annotate_playlists(groundtruth, prompt_path = "playlist_annotation/prompts/prompt_context.txt"				)
gpt_test_mood 					      <-  annotate_playlists(groundtruth, prompt_path = "playlist_annotation/prompts/prompt_mood.txt"					)
gpt_test_top 					        <-  annotate_playlists(groundtruth, prompt_path = "playlist_annotation/prompts/prompt_top.txt"					)
gpt_test_neuf_categories 		  <-  annotate_playlists(groundtruth, prompt_path = "playlist_annotation/prompts/prompt_neuf_categories.txt"		)

# Compute diagnostics ------
diagnostics <- bind_rows(
  compute_diagnostics(gpt_test_artist_person      , groundtruth, "artist") %>% 
    mutate(attempt = "artist person"),
  compute_diagnostics(gpt_test_artist_person      , groundtruth, "person") %>% 
    mutate(attempt = "artist person"),
  compute_diagnostics(gpt_test_person             , groundtruth, "person"),
  compute_diagnostics(gpt_test_artist             , groundtruth, "artist"),
  compute_diagnostics(gpt_test_event_moment_period, groundtruth, "event") %>% 
    mutate(attempt = "event moment period"),
  compute_diagnostics(gpt_test_event_moment_period, groundtruth, "moment") %>% 
    mutate(attempt = "event moment period"),
  compute_diagnostics(gpt_test_event_moment_period, groundtruth, "period") %>% 
    mutate(attempt = "event moment period"),
  compute_diagnostics(gpt_test_event              , groundtruth, "event"),
  compute_diagnostics(gpt_test_moment             , groundtruth, "moment"),
  compute_diagnostics(gpt_test_period             , groundtruth, "period"),
  compute_diagnostics(gpt_test_genre              , groundtruth, "genre"),
  compute_diagnostics(gpt_test_context            , groundtruth, "context"),
  compute_diagnostics(gpt_test_mood               , groundtruth, "mood"),
  compute_diagnostics(gpt_test_top                , groundtruth, "top"),
  compute_diagnostics(gpt_test_neuf_categories    , groundtruth, "person") %>% 
    mutate(attempt = "multinomial"),
  compute_diagnostics(gpt_test_neuf_categories    , groundtruth, "artist") %>% 
    mutate(attempt = "multinomial"),
  compute_diagnostics(gpt_test_neuf_categories    , groundtruth, "event") %>% 
    mutate(attempt = "multinomial"),
  compute_diagnostics(gpt_test_neuf_categories    , groundtruth, "moment") %>% 
    mutate(attempt = "multinomial"),
  compute_diagnostics(gpt_test_neuf_categories    , groundtruth, "period") %>% 
    mutate(attempt = "multinomial"),
  compute_diagnostics(gpt_test_neuf_categories    , groundtruth, "genre") %>% 
    mutate(attempt = "multinomial"),
  compute_diagnostics(gpt_test_neuf_categories    , groundtruth, "context") %>% 
    mutate(attempt = "multinomial"),
  compute_diagnostics(gpt_test_neuf_categories    , groundtruth, "mood") %>% 
    mutate(attempt = "multinomial"),
  compute_diagnostics(gpt_test_neuf_categories    , groundtruth, "top") %>% 
    mutate(attempt = "multinomial")
) %>% 
  mutate(attempt = ifelse(is.na(attempt), "binomial", attempt))
diagnostics
