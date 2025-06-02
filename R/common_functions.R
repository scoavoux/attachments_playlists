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

set_ggplot_options <- function(){
  theme_set(theme_minimal(base_size = 13))
}

recode_device <- function(x){
  factor(x, levels = c("organic", "edito", "reco_algo"), labels = c("Organic", "Editorial", "Algorithmic recommendation"))
}
