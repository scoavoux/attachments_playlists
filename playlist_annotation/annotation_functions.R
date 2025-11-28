# Prepare prompts ------
##' This function makes a tibble containing formatted prompts to a given batch 
##' size from a tibble of playlists titles and ids
prepare_prompts <- function(playlists, batch_size){
  prompts <- playlists %>% 
    mutate(gpt_instructions = paste0("playlist_id: ", playlist_id, ", title: ", title)) %>% 
    mutate(group = floor((row_number() -1)/batch_size) %>% factor()) %>% 
    summarise(gpt_instructions = paste0(gpt_instructions, collapse = "\n"), .by = group)
  return(prompts)
  
}

# Annotate playlists ---
##' This function runs the annotation with openai batch api
##' It write the results as a json file.

annotate_playlists <- function(prompts, 
                               prompt_path, 
                               json_output_path, 
                               model = "openai/gpt-5.1",
                               .wait=TRUE){
  assistant <- chat(name = model, 
                    system_prompt = read_file(prompt_path))
  
  batch <- batch_chat(
    assistant, 
    as.list(prompts$gpt_instructions), 
    path = json_output_path, 
    wait = .wait
  )
  return(batch)
}

# Read_annotation_results ------
##' This functions reads a json file with the annotation results
##' and outputs a tibble with structured results.
read_annotation_results <- function(path_to_json){
  js <- read_json(path_to_json)
  
  results <- map(js$results, function(.x){
    gpt <- .x$body$output[[1]]$content[[1]]$text
    res <- paste0("[", 
                  paste(str_replace_all(gpt, "\\}[\\s]+\\{", "},{"), collapse = ","),
                  "]") %>% fromJSON()
    return(res)
  }) %>% 
    bind_rows() %>% 
    tibble()
  return(results)
}

# Compute diagnostics
##' Compute precision, recall and F1 score given annotations and 
##' groundtruth data.
compute_diagnostics <- function(testset, groundtruth, cat){
  x <- testset |> 
    mutate(across(everything(), as.integer)) |> 
    rename(playlist_id = "id", gpt = cat) |> 
    mutate(gpt = factor(gpt, levels = c(0, 1))) |> 
    left_join(select(groundtruth, title, playlist_id, !!enquo(cat)))
  # if(cat == "period"){
  #   x <- correct_period(x)
  # }
  
  cm <- caret::confusionMatrix(x$gpt, x[[cat]], positive = "1")
  n <- cm$byClass[c("Precision", "Recall", "F1")]
  res <- tibble(category = cat,
                Precision = n["Precision"],
                Recall = n["Recall"],
                F1 = n["F1"])
  
  return(res)
}

# Extract error
##' Extract the errors 
extract_errors <- function(testset, groundtruth, cat){
  x <- testset |> 
    mutate(across(everything(), as.integer)) |> 
    rename(playlist_id = "id", gpt = cat) |> 
    mutate(gpt = factor(gpt, levels = c(0, 1))) |> 
    left_join(select(groundtruth, title, playlist_id, !!enquo(cat))) %>% 
    tibble()
  
  cat_sym <- as.symbol(cat)
  errors <- x %>% 
    filter({{cat_sym}} != gpt)
  
  return(errors)
}

