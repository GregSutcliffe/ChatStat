api_url <- function(url, host = NULL, port = NULL) {
  if (is.null(host)) {
    host <- Sys.getenv("host")
  }

  port <- if (!is.null(port)) {
    glue::glue(":{port}")
  } else {
    ""
  }

  glue::glue("https://{host}{port}{url}")
}

process_events <- function(events) {
  tibble::tibble(event = events) |>
    tidyr::hoist(event,
      time         = "origin_server_ts",
      type         = "type",
      sender       = "sender",
      message_type = c("content", "msgtype"),
      body         = c("content", "body")
    ) |>
    dplyr::select(!event) |>
    dplyr::mutate(time = lubridate::as_datetime(time / 1000)) |>
    dplyr::arrange(time)
}
