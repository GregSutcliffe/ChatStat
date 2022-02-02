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

#' Create a new empty event tibble.
#' @noRd
empty_events <- function() {
  tibble::tibble(
    room = character(),
    id = character(),
    time = lubridate::as_datetime(NULL),
    type = character(),
    sender = character(),
    message_type = character(),
    body = character(),
    raw_event = list()
  )
}

#' Transform event data from the Matrix API into a tibble.
#'
#' @param room_id Room ID of the room the events belong to.
#' @param events List of events to include.
#'
#' @noRd
process_events <- function(room_id, events) {
  if (length(events) < 1) {
    return(NULL)
  }

  tibble::tibble(event = events) |>
    tidyr::hoist(event,
      id           = "event_id",
      time         = "origin_server_ts",
      type         = "type",
      sender       = "sender",
      message_type = c("content", "msgtype"),
      body         = c("content", "body")
    ) |>
    tibble::add_column(room = room_id, .before = 1) |>
    tibble::add_column(raw_event = events) |>
    dplyr::select(!event) |>
    dplyr::mutate(time = lubridate::as_datetime(time / 1000))
}

#' Normalize the event data.
#'
#' This function takes the event data and removes duplicates. Finally, the
#' events are arranged by room and time.
#'
#' @param events A tibble containing the event data.
#'
#' @noRd
normalize_events <- function(events) {
  events |>
    dplyr::distinct(room, id, .keep_all = TRUE) |>
    dplyr::arrange(room, time)
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
