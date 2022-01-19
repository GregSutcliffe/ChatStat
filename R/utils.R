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
      id           = "event_id",
      time         = "origin_server_ts",
      type         = "type",
      sender       = "sender",
      message_type = c("content", "msgtype"),
      body         = c("content", "body")
    ) |>
    tibble::add_column(raw_event = events) |>
    dplyr::select(!event) |>
    dplyr::mutate(time = lubridate::as_datetime(time / 1000)) |>
    dplyr::arrange(time)
}

#' Render a basic HTML report based on a set of room data
#'
#' @param room_data A tibble of messages. Expects to be unpacked from
#' the nested format in get_rooms
#' @param output_file The place to write the report. Defaults
#' to `./chatstat_report.html`
#'
#' @return A list object from the Matrix API.
#'
#' @export
render_report <- function(room_data,
                          output_file = NULL) {
  rmd <- system.file("rmd","html_report.Rmd", package = 'ChatStat')

  if (is.null(output_file)) {
    output_file <- paste0(getwd(),'/chatstat_report.html')
  }

  rmarkdown::render(rmd,
                    params = list(data = room_data),
                    output_file = output_file)

  browseURL(paste0('file://',getwd(),output_file))
}
