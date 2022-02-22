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

#' Get the room IDs of all joined rooms.
#'
#' @param initial_sync The result of a call to [sync()]. This will be used to
#'   find the joined rooms.
#'
#' @return A character vector of room IDs.
#'
#' @export
all_rooms <- function(initial_sync) {
  names(initial_sync$rooms$join)
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
      id = "event_id",
      time = "origin_server_ts",
      type = "type",
      sender = "sender",
      message_type = c("content", "msgtype"),
      body = c("content", "body"),
      .ptype = list(
        room = character(),
        id = character(),
        time = lubridate::as_datetime(NULL),
        type = character(),
        sender = character(),
        message_type = character(),
        body = character(),
        raw_event = list()
      ),
      .transform = list(time = function(t) lubridate::as_datetime(t / 1000))
    ) |>
    tibble::add_column(room = room_id, .before = 1) |>
    tibble::add_column(raw_event = events) |>
    dplyr::select(!event)
}

#' Create a new empty members tibble.
#' @noRd
empty_members <- function() {
  tibble::tibble(
    room = character(),
    user_id = character(),
    display_name = character(),
    membership_state = character(),
    raw_data = list()
  )
}

#' Transform a members list from the API into a tibble.
#'
#' @param room_id Room ID the members belong to.
#' @param members The list of membership data.
#'
#' @noRd
process_members <- function(room_id, members) {
  tibble::tibble(raw_data = members) |>
    tidyr::hoist(
      raw_data,
      user_id = "user_id",
      display_name = c("content", "displayname"),
      membership_state = c("content", "membership"),
      .ptype = list(
        room = character(),
        user_id = character(),
        display_name = character(),
        membership_state = character(),
        raw_data = list()
      )
    ) |>
    tibble::add_column(room = room_id, .before = 1)
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

#' Render an HTML report based on a rooms object.
#'
#' Please note, that the current working directory has to be writable in order
#' to save the output as well as the intermediate files.
#'
#' @param rooms Rooms object to base the report on. See [rooms()].
#' @param output_file The place to write the report to. Defaults to
#'   `chatstat_report.html`.
#'
#' @export
render_report <- function(rooms, output_file = NULL) {
  rmd <- system.file("rmd", "html_report.Rmd", package = "ChatStat")

  if (is.null(output_file)) {
    output_file <- "chatstat_report.html"
  }

  rmarkdown::render(
    rmd,
    params = list(rooms = rooms),
    output_file = output_file,
    output_dir = getwd(),
    intermediates_dir = getwd(),
    envir = new.env()
  )
}
