#' Add API Key to all requests
#'
#' @param nexus_mods_key A character. Nexus Mods Personal API Key.
#'
#' @return Invisible authorization data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(nexusmodsr)
#'
#' nexus_mods_key()
#' }
nexus_auth <- function(
  nexus_mods_key = Sys.getenv("NEXUSMODS_KEY")
) {

  if (nexus_mods_key == "" | is.null(nexus_mods_key)) {
    stop("Nexus Mods Key is not detected. Set NEXUSMODS_KEY in your .Renviron.")
  }

  ## For help run {usethis::ui_value('vignette(\"authentication-set-up\", package = \"twitchr\")')} in the R console.

  httr::add_headers(
    apikey = nexus_mods_key
  ) %>%
    httr::set_config()

  return(invisible())
}
