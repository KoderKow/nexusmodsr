get_mod_files <- function(game_domain_name, mod_id, category = NULL){
  clean_game <- clean_game_name(game_domain_name)

  url <- glue::glue("{base_url}/{clean_game}/mods/{mod_id}/files.json")

  if (not_null(category)) {
    url <- glue::glue("{url}?{category}")
  }

  d <- make_get_request(url)

  return(d)
}
