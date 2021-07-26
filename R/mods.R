#' Get recently updated mods
#'
#' @param game_domain_name A character. Game domain to get the recent mod updates for.
#' @param period A character. Requests the wanted period. Format is "1d", "1w", and "1m" for day, week and month.
#'
#' @return A tibble with `mod_id`, `lastest_file_update`, and `lastest_mod_activity`
#' @export
#' @family Mods
#' @references https://app.swaggerhub.com/apis-docs/NexusMods/nexus-mods_public_api_params_in_form_data/1.0#/Mods/get_v1_games_game_domain_mods_updates.json
#'
#' @examples
#' \dontrun{
#' library(nexusmodsr)
#'
#' nexus_auth()
#'
#' recent_updates <- mods_updated("Stardew Valley", "1w")
#' }
mods_updated <- function(game_domain_name, period) {
  base_url <- "https://api.nexusmods.com/v1/games"

  clean_game <- clean_game_name(game_domain_name)

  url <- glue::glue("{base_url}/{clean_game}/mods/updated.json?period={period}")

  d <- mods_get_request(url)

  return(d)
}

#' Get changelogs
#'
#' @inheritParams  mods_updated
#' @param mod_id A numeric. ID of the mod to get changelogs for.
#'
#' @return
#' @export
#' @family Mods
#' @references https://app.swaggerhub.com/apis-docs/NexusMods/nexus-mods_public_api_params_in_form_data/1.0#/Mods/get_v1_games_game_domain_mods_mod_id_changelogs.json
#'
#' @examples
#' \dontrun{
#' library(nexusmodsr)
#'
#' nexus_auth()
#'
#' changelogs <- get_changelogs("Stardew Valley", 2400)
#' }
get_changelogs <- function(
  game_domain_name,
  mod_id
) {
  clean_game <- clean_game_name(game_domain_name)

  url <- glue::glue("{base_url}/{clean_game}/mods/{mod_id}/changelogs.json")

  d <- mods_get_request(url)

  return(d)
}

#' Get latest 10 added mods
#'
#' Retrieve 10 latest added mods for a specified game
#'
#' @inheritParams mods_updated
#'
#' @return
#' @export
#' @family Mods
#' @references https://app.swaggerhub.com/apis-docs/NexusMods/nexus-mods_public_api_params_in_form_data/1.0#/Mods/get_v1_games_game_domain_mods_latest_added.json
#'
#' @examples
#' \dontrun{
#' library(nexusmodsr)
#'
#' nexus_auth()
#'
#' lastest_added_mods <- get_latest_added_mods("Stardew Valley")
#' }
get_latest_added_mods <- function(game_domain_name) {
  clean_game <- clean_game_name(game_domain_name)

  url <- glue::glue("{base_url}/{clean_game}/mods/latest_added.json")

  d <- mods_get_request(url)

  return(d)
}

#' Get latest 10 updated mods
#'
#' @inheritParams mods_updated
#'
#' @return
#' @export
#' @family Mods
#' @references https://app.swaggerhub.com/apis-docs/NexusMods/nexus-mods_public_api_params_in_form_data/1.0#/Mods/get_v1_games__game_domain_name__mods_latest_updated_json
#'
#' @examples
#' \dontrun{
#' library(nexusmodsr)
#'
#' nexus_auth()
#'
#' lastest_updated_mods <- get_latest_updated_mods("Stardew Valley")
#' }
get_latest_updated_mods <- function(game_domain_name) {
  clean_game <- clean_game_name(game_domain_name)

  url <- glue::glue("{base_url}/{clean_game}/mods/latest_updated.json")

  d <- mods_get_request(url)

  return(d)
}

#' Get trendings mods
#'
#' Retrieve 10 trending mods for a specified game
#'
#' @inheritParams mods_updated
#'
#' @return
#' @export
#' @family Mods
#' @references https://app.swaggerhub.com/apis-docs/NexusMods/nexus-mods_public_api_params_in_form_data/1.0#/Mods/get_v1_games_game_domain_mods_trending.json
#'
#' @examples
#' \dontrun{
#' library(nexusmodsr)
#'
#' nexus_auth()
#'
#' trending_mods <- get_trending_mods("Stardew Valley")
#' }
get_trending_mods <- function(game_domain_name) {
  clean_game <- clean_game_name(game_domain_name)

  url <- glue::glue("{base_url}/{clean_game}/mods/trending.json")

  d <- mods_get_request(url)

  return(d)
}

#' Get mod
#'
#' Retrieve specified mod, from a specified game. Cached for 5 minutes.
#'
#' @inheritParams mods_updated
#' @inheritParams get_changelogs
#'
#' @return
#' @export
#' @family Mods
#' @references https://app.swaggerhub.com/apis-docs/NexusMods/nexus-mods_public_api_params_in_form_data/1.0#/Mods/get_v1_games_game_domain_name_mods_id.json
#'
#' @examples
#' \dontrun{
#' library(nexusmodsr)
#'
#' nexus_auth()
#'
#' mod <- get_mod("Stardew Valley",  2400)
#' }
get_mod <- function(game_domain_name, mod_id) {
  clean_game <- clean_game_name(game_domain_name)

  url <- glue::glue("{base_url}/{clean_game}/mods/{mod_id}.json")

  d <- mods_get_request(url)

  return(d)
}

#' Search MD5 Hash
#'
#' Looks up a file MD5 file hash
#'
#' @inheritParams mods_updated
#' @param md5_hash A character. I'll be honest, I don't know what this means.
#'
#' @return
#' @export
#' @family Mods
#' @references https://app.swaggerhub.com/apis-docs/NexusMods/nexus-mods_public_api_params_in_form_data/1.0#/Mods/get_v1_games_game_domain_name_mods_md5_search_md5_hash.json
get_search_md5_hash <- function(game_domain_name, md5_hash) {
  clean_game <- clean_game_name(game_domain_name)

  url <- glue::glue("{base_url}/{clean_game}/mods/{md5_hash}.json")

  d <- mods_get_request(url)

  return(d)
}

#' Endorse a mod
#'
#' Endorse a mod
#'
#' @inheritParams mods_updated
#' @inheritParams get_changelogs
#' @param version A character. Mod version.
#'
#' @return
#' @export
#' @family Mods
#' @references https://app.swaggerhub.com/apis-docs/NexusMods/nexus-mods_public_api_params_in_form_data/1.0#/Mods/post_v1_games_game_domain_name_mods_id_endorse.json
#'
#' @examples
#' \dontrun{
#' library(nexusmodsr)
#'
#' nexus_auth()
#'
#' endorse <- post_endorse_mod("Stardew Valley",  2400, "3.11.0")
#' }
post_endorse_mod <- function(game_domain_name, mod_id, version) {
  clean_game <- clean_game_name(game_domain_name)

  url <- glue::glue("{base_url}/{clean_game}/mods/{mod_id}/endorse.json")

  d <- mods_post_request(url, mod_version = version)

  return(d)
}

#' Abstain a mod
#'
#' Abstain from endorsing a mod
#'
#' @inheritParams post_endorse_mod
#' @inheritParams get_changelogs
#'
#' @return
#' @export
#' @family mod
#' @references https://app.swaggerhub.com/apis-docs/NexusMods/nexus-mods_public_api_params_in_form_data/1.0#/Mods/post_v1_games_game_domain_name_mods_id_abstain.json
#'
#' @examples
#' \dontrun{
#' library(nexusmodsr)
#'
#' nexus_auth()
#'
#' abstain <- post_abstain_mod("Stardew Valley",  2400, "3.11.0")
#' }
post_abstain_mod <- function(game_domain_name, mod_id, version) {
  clean_game <- clean_game_name(game_domain_name)

  url <- glue::glue("{base_url}/{clean_game}/mods/{mod_id}/abstain.json")

  d <- mods_post_request(url, mod_version = version)

  return(d)
}
