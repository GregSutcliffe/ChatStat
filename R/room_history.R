#' Get the room events for a given room ID.
#'
#' @param room_id The room to get data for.
#' @param since  Stop paginating when reaching this time.
#'
#' @return A tibble containing event information.
#'
#' @export
room_history <- function(room_id, since) {
  # We expect since to be a POSIX datetime, but could be a string.
  since <- lubridate::as_datetime(since)

  rlog::log_info(
    glue::glue("Fetching events for room {room_id} since {since}.")
  )

  # TODO: Add better configuration.
  token <- Sys.getenv("token")

  # Perform an initial sync and get events for the room.
  initial_sync <- sync()
  timeline <- initial_sync$rooms$join[[room_id]]$timeline
  from <- timeline$prev_batch

  events <- process_events(timeline$events)

  rlog::log_info(glue::glue("Initial sync yielded {nrow(events)} events."))

  while (TRUE) {
    oldest_time <- events |>
      dplyr::slice_min(time) |>
      dplyr::pull(time)

    if (oldest_time < since) {
      rlog::log_info("Specified time has been reached. Stopping.")
      break
    }

    rlog::log_info(glue::glue("Oldest message is from {oldest_time}."))

    messages <- get_messages(room_id, from)
    new_event_count <- length(messages$chunk)

    if (new_event_count <= 0) {
      rlog::log_info("No more events. Stopping early.")
      break
    }

    rlog::log_info(glue::glue("Received {new_event_count} more events."))

    new_events <- process_events(messages$chunk)
    events <- events |> tibble::add_row(new_events, .before = 0)
    from <- messages$end
  }

  events |> dplyr::filter(time >= since)
}
