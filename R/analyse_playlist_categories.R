make_playlists_categories <- function(x){
  c(genre  = "Music-centered",
    period = "Music-centered",
    artiste = "Music-centered",
    context = "Context-centered",
    personne = "Context-centered",
    events = "Context-centered",
    moods = "User-centered",
    top = "User-centered",
    time = "User-centered")[x]
}

recode_playlists_annotations <- function(x){
  
}

plot_descriptive_ <- function(users_playlists){
  set_ggplot_options()
  users_playlists %>% 
    select(hashed_id, starts_with("an_")) %>% 
    pivot_longer(-hashed_id) %>% 
    mutate(name = str_remove(name, "^an_"),
           cat = make_playlists_categories(name)) %>% 
    group_by(name, cat) %>% 
    summarize(m = mean(value, na.rm=TRUE)) %>% 
    ggplot(aes(m, name)) +
     geom_col() +
     facet_wrap(~cat, nrow=3, scales = "free_y") +
     labs(x = "Average number of playlists")
}
