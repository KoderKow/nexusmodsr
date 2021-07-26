clean_game_name <- function(game_domain_name) {
  game_domain_name |>
    stringr::str_remove_all(" ") |>
    stringr::str_to_lower()
}

mods_get_request <- function(url) {
  get_request <- httr::GET(url)

  if (httr::status_code(get_request) == 200) {
    d <-
      get_request |>
      httr::content() %>%
      {
        if (is.null(names(.))) {
          ## When multiple rows are returned
          lapply(., flatten_keep_names) %>%
            dplyr::bind_rows() |>
            transform_datetime()
        } else {
          ## When one row is returned
          flatten_keep_names(.) %>%
            dplyr::bind_rows() |>
            transform_datetime()
        }
      }

    if (nrow(d) == 0) {
      warning("The request was successful, however, there is no data. Returning NULL.")
      d <- NULL
    }

  } else {
    d <-
      get_request |>
      httr::content()

    msg <- glue::glue("GET request failed. {d$code}: {d$message}")

    stop(msg)
  }

  return(d)
}

mods_post_request <- function(url, mod_version) {
  body_list <- list(version = mod_version)

  post_request <- httr::POST(url, body = body_list)

  if (httr::status_code(post_request) == 200) {
    d <-
      post_request |>
      httr::content() %>%
      { if (is.null(names(.))) lapply(., flatten_keep_names) else flatten_keep_names(.)} %>%
      dplyr::bind_rows() |>
      transform_datetime()

    if (nrow(d) == 0) {
      warning("The request was successful, however, there is no data. Returning NULL.")
      d <- NULL
    }

  } else {
    d <-
      post_request |>
      httr::content()

    msg <- glue::glue("GET request failed. {d$code}: {d$message}")

    stop(msg)
  }

  httr::content(post_request)
}

make_mod_files_request <- function(get_request) {
  get_request <- httr::GET(url)

  if (httr::status_code(get_request) == 200) {
    d <-
      get_request |>
      httr::content()

    d_files <-
      d$files %>%
      lapply(., flatten_keep_names) %>%
      dplyr::bind_rows() |>
      dplyr::select(-id2) |>
      dplyr::rename(id = id1) |>
      transform_datetime()

    d_file_updates <- dplyr::bind_rows(d$file_updates)

    d <- list(
      files = d_files,
      file_updates = d_file_updates
    )
  } else {
    d <-
      get_request |>
      httr::content()

    msg <- glue::glue("GET request failed. {d$code}: {d$message}")

    stop(msg)
  }

  return(d)

}

#' Keep names while flattening a named list
#'
#' Source taken from the package rlist's list.flatten. Didn't want to load dependency. Added a step to clean the names to snake case.
#'
#' @param x list
#' @param use.names	logical. Should the names of x be kept?
#' @param classes	A character vector of class names, or "ANY" to match any class.
#' @references https://github.com/renkun-ken/rlist
#'
#' @noRd
flatten_keep_names <- function (x, use.names = TRUE, classes = "ANY") {
  len <- sum(rapply(x, function(x) 1L, classes = classes))
  y <- vector("list", len)
  i <- 0L
  current_env <- environment()
  items <- rapply(x, function(x) {
    i <- i + 1L
    y[[i]] <- x
    assign("i", i, envir = current_env)
    assign("y", y, envir = current_env)
    TRUE
  }, classes = classes)
  if (use.names && !is.null(nm <- names(items)))
    names(y) <- stringr::str_replace_all(nm, "\\.", "_")
  y
}

transform_datetime <- function(d) {
  d |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::ends_with("_time"),
        .fns = ~ {
          if (all(is_na(.x))) return(NA)

          .x |>
            stringr::str_remove("\\.000\\+00\\:00") |>
            lubridate::ymd_hms()
        }
      )
    )
}

