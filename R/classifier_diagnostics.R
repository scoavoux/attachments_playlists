# Classifier diagnostics (precision / recall / F1) ------
# Agreement between LLM annotations of playlist titles and hand-coded gold
# standards, computed in the pipeline (previously only in the frozen
# data/diagnostics_gpt5.1.csv, written by playlist_annotation/
# test_annotation_strategy_ellmer.R from a test run whose JSON outputs are not
# in the repo).
#
# Parametrised by model and by gold standard (see the parameters in
# _targets.R):
#  - gold standards: long CSV files (title, cat, valeur = 0/1/NA), one per gold
#    set, each with a domain ("users" = users' personal playlists, "editorial"
#    = Deezer editorial playlists).
#  - model annotations: one source per model, each with a domain and a type:
#      "s3_title_csv": the production annotation CSVs on S3 (one file per
#        category: title + one 0/1 column named after the category), as read
#        by make_playlists_annotation_data();
#      "local_title_csv_dir": the same format, in a local directory;
#      "local_wide_csv": one local CSV with a title column and one 0/1 column
#        per category (e.g. ../editorial_annotation/csv/editorial_annotated.csv).
#  - every (model, gold set) pair with the same domain is evaluated.
# Titles are matched on trimws(title) (the annotation CSVs hold trimmed
# titles); the first row per (title, category) is kept on each side.
# Same definitions as compute_diagnostics() (caret, positive class = 1):
# precision = TP / (TP + FP), recall = TP / (TP + FN), F1 = harmonic mean.
#
# NB: the production run annotated every unique title again, so its agreement
# with the gold standard can differ slightly from the test run in
# data/diagnostics_gpt5.1.csv (same prompts and model; LLM outputs are not
# deterministic). Both are kept: the legacy file as `classifier_diagnostics_legacy`.

classifier_categories <- c("genre", "artist", "period", "context", "moment", "top", "event", "person", "mood")

# Gold standards: named list of list(file =, domain =) -> long tibble
read_gold_standards <- function(files, gold_sets){
  purrr::imap(gold_sets, function(g, name){
    readr::read_csv(g$file, show_col_types = FALSE, col_types = readr::cols(.default = "c")) %>%
      transmute(gold_set = name, domain = g$domain, title = trimws(title), category = cat,
                gold = suppressWarnings(as.integer(valeur))) %>%
      filter(!is.na(title), !is.na(gold), category %in% classifier_categories) %>%
      distinct(gold_set, title, category, .keep_all = TRUE)
  }) %>% bind_rows()
}

# One model source -> long tibble (model, domain, title, category, pred)
read_model_annotations_one <- function(src, name, s3_files = NULL){
  to_long <- function(d){
    d %>%
      mutate(title = trimws(as.character(title))) %>%
      pivot_longer(any_of(classifier_categories), names_to = "category", values_to = "pred") %>%
      mutate(pred = suppressWarnings(as.integer(pred))) %>%
      filter(!is.na(title), !is.na(pred)) %>%
      distinct(title, category, .keep_all = TRUE) %>%
      transmute(model = name, domain = src$domain, title, category, pred)
  }
  read_title_csv <- function(path_or_uri, con){
    d <- DBI::dbGetQuery(con, sprintf(
      "SELECT * FROM read_csv('%s', header = true, all_varchar = true, nullstr = 'NA')", path_or_uri))
    names(d)[1] <- "title"
    to_long(tibble::as_tibble(d))
  }
  if(src$type == "s3_title_csv"){
    keys <- s3_files$Key[str_starts(s3_files$Key, src$path)]
    con <- duck_connect(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    purrr::map(s3_uri(keys), read_title_csv, con = con) %>% bind_rows()
  } else if(src$type == "local_title_csv_dir"){
    con <- duckdb_local_connect(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    purrr::map(list.files(src$path, pattern = "\\.csv$", full.names = TRUE), read_title_csv, con = con) %>% bind_rows()
  } else if(src$type == "local_wide_csv"){
    readr::read_csv(src$path, show_col_types = FALSE, col_types = readr::cols(.default = "c")) %>%
      select(title, any_of(classifier_categories)) %>%
      to_long()
  } else stop("Unknown annotation source type: ", src$type)
}

# Local DuckDB connection without httpfs/S3 (for local files)
duckdb_local_connect <- function(){
  DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
}

read_model_annotations <- function(model_sources, s3_files = NULL){
  purrr::imap(model_sources, read_model_annotations_one, s3_files = s3_files) %>% bind_rows()
}

# Precision / recall / F1 for every (model, gold set) pair of the same domain,
# by category, written to output/classifier_diagnostics.csv.
compute_classifier_diagnostics <- function(model_annotations, gold_standards,
                                           filename = "output/classifier_diagnostics.csv"){
  res <- inner_join(gold_standards, model_annotations, by = c("domain", "title", "category"),
                    relationship = "many-to-many") %>%
    group_by(model, gold_set, domain, category) %>%
    summarize(n_titles = n(),
              n_gold_positive = sum(gold == 1),
              tp = sum(pred == 1 & gold == 1), fp = sum(pred == 1 & gold == 0),
              fn = sum(pred == 0 & gold == 1), tn = sum(pred == 0 & gold == 0),
              .groups = "drop") %>%
    mutate(precision = tp / (tp + fp), recall = tp / (tp + fn),
           f1 = 2 * precision * recall / (precision + recall),
           accuracy = (tp + tn) / n_titles) %>%
    # coverage: share of gold titles found in the model's annotations
    left_join(gold_standards %>% count(gold_set, category, name = "n_gold_titles"),
              by = c("gold_set", "category")) %>%
    mutate(coverage = n_titles / n_gold_titles,
           category = factor(category, levels = classifier_categories)) %>%
    arrange(model, gold_set, category) %>%
    mutate(category = as.character(category))
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  write_csv(res, filename)
  res
}

# Legacy: the frozen test-run diagnostics (data/diagnostics_gpt5.1.csv)
read_classifier_diagnostics_legacy <- function(file){
  readr::read_csv(file, show_col_types = FALSE) %>%
    rename_with(tolower)
}
