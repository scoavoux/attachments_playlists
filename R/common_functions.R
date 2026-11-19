# Common functions ------
initialize_s3 <- function(){
  require("paws")
  s3 <- paws::s3(config = list(
    credentials = list(
      creds = list(
        access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
        secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
        session_token = Sys.getenv("AWS_SESSION_TOKEN")
      )),
    endpoint = paste0("https://", Sys.getenv("AWS_S3_ENDPOINT")),
    region = Sys.getenv("AWS_DEFAULT_REGION")))

  return(s3)
}

# List objects under a prefix, following pagination (list_objects_v2 returns
# at most 1000 keys per call). Returns a tibble with Key, Size and ETag, so that
# downstream targets rerun when a file's content changes, not only its name.
list_s3_objects <- function(prefix, bucket = "scoavoux"){
  s3 <- initialize_s3()
  token <- NULL
  out <- list()
  repeat {
    args <- list(Bucket = bucket, Prefix = prefix)
    if(!is.null(token)) args$ContinuationToken <- token
    r <- do.call(s3$list_objects_v2, args)
    out <- c(out, lapply(r$Contents, function(x) tibble::tibble(Key = x$Key, Size = x$Size, ETag = x$ETag)))
    if(!isTRUE(r$IsTruncated)) break
    token <- r$NextContinuationToken
  }
  dplyr::bind_rows(out)
}

# DuckDB connection with httpfs and an S3 secret built from the environment
# (~/.Renviron). memory_limit + temp_directory let large window/sort
# operations spill to disk instead of exhausting RAM.
duck_connect <- function(memory_limit = "2GB",
                         threads = 4L,
                         temp_directory = "data/temp/duckdb_tmp"){
  require(DBI)
  require(duckdb)
  # One subdirectory per connection: successive connections in the same R
  # process must not clean up each other's spill files.
  temp_directory <- file.path(temp_directory,
                              paste0("c", Sys.getpid(), "_", format(Sys.time(), "%H%M%OS3"),
                                     "_", sample.int(1e6, 1)))
  dir.create(temp_directory, recursive = TRUE, showWarnings = FALSE)
  # shared_home: cache the httpfs extension in ~/.duckdb instead of re-downloading it
  con <- dbConnect(duckdb::duckdb(shared_home = TRUE), bigint = "integer64")
  dbExecute(con, sprintf("SET memory_limit = '%s'", memory_limit))
  dbExecute(con, sprintf("SET threads = %d", as.integer(threads)))
  dbExecute(con, sprintf("SET temp_directory = '%s'", normalizePath(temp_directory)))
  dbExecute(con, "SET preserve_insertion_order = false")
  dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  # Values are interpolated into the statement but never printed.
  sql <- sprintf(
    "CREATE OR REPLACE SECRET s3sec (TYPE s3, KEY_ID '%s', SECRET '%s', SESSION_TOKEN '%s', ENDPOINT '%s', REGION '%s', URL_STYLE 'path')",
    Sys.getenv("AWS_ACCESS_KEY_ID"), Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    Sys.getenv("AWS_SESSION_TOKEN"), Sys.getenv("AWS_S3_ENDPOINT"),
    Sys.getenv("AWS_DEFAULT_REGION", "us-east-1"))
  invisible(dbExecute(con, sql))
  con
}

s3_uri <- function(key, bucket = "scoavoux"){
  paste0("s3://", bucket, "/", sub("^/", "", key))
}

# SQL list literal of S3 URIs, e.g. ['s3://...', 's3://...']
sql_file_list <- function(keys){
  paste0("[", paste0("'", s3_uri(keys), "'", collapse = ", "), "]")
}

set_ggplot_options <- function(){
  theme_set(theme_minimal(base_size = 13))
}

recode_device <- function(x){
  factor(x, levels = c("organic", "edito", "reco_algo"), labels = c("Organic", "Editorial", "Algorithmic recommendation"))
}

# Download one S3 object to a local path (for small inputs that file targets
# read locally). Returns the path, for format = "file" targets.
fetch_s3_file <- function(key, dest, bucket = "scoavoux"){
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  s3 <- initialize_s3()
  s3$download_file(Bucket = bucket, Key = key, Filename = dest)
  dest
}
