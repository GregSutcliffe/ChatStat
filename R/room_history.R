#' Get the room events for a given room ID.
#'
#' @param room_id      The room to get data for.
#' @param since        Stop paginating when reaching this time.
#' @param initial_sync Result of a prior call to [sync()].
#'
#' @return An object of class `ChatStat_room`.
#'
#' @noRd
get_room <- function(room_id, since, initial_sync) {
  # TODO: Add better configuration.
  token    <- Sys.getenv("token")
  timeline <- initial_sync$rooms$join[[room_id]]$timeline
  from     <- timeline$prev_batch

  events   <- process_events(timeline$events)
  rlog::log_debug(glue::glue("Initial sync yielded {nrow(events)} events."))

  rlog::log_info(
    glue::glue("Fetching events for room {room_id} since {since}.")
  )

  while (TRUE) {
    oldest_time <- events |>
      dplyr::slice(1) |>
      dplyr::pull(time)

    if (oldest_time < since) {
      rlog::log_info("Specified time has been reached. Stopping.")
      break
    }

    rlog::log_debug(glue::glue("Oldest message is from {oldest_time}."))

    messages        <- get_messages(room_id, from)
    new_event_count <- length(messages$chunk)

    if (new_event_count <= 0) {
      rlog::log_info("No more events. Stopping early.")
      break
    }

    rlog::log_debug(glue::glue("Received {new_event_count} more events."))

    new_events <- process_events(messages$chunk)
    events     <- events |> tibble::add_row(new_events, .before = 0)
    from       <- messages$end
  }

  events |> dplyr::filter(time >= since)

  room(
    id = room_id,
    alias = get_room_alias(room_id, initial_sync),
    since = since,
    events = events,
    next_token = initial_sync$next_batch
  )
}

#' Get the room events for a given room ID.
#'
#' @param room_ids Vector of room IDs to iterate over.
#' @param since    Stop paginating when reaching this time.
#' @param sync     (Optional) Result of a prior call to [sync()] to save
#'                 duplication.
#'
#' @return A tibble containing the rooms.
#'
#' @export
get_rooms <- function(room_ids, since, sync = NULL) {
  # We expect since to be a POSIX datetime, but could be a string.
  since <- lubridate::as_datetime(since)

  rlog::log_debug(glue::glue("Getting history for {length(room_ids)} rooms."))

  # Perform an initial sync and get events for the room.
  initial_sync <- if (is.null(sync)) {
    rlog::log_debug("Initial sync not provided - running sync() now.")
    sync()
  } else {
    rlog::log_debug("Initial sync provided - not running sync().")
    sync
  }

  tidyr::tibble(id = room_ids) |>
    dplyr::group_by(id) |>
    dplyr::mutate(room = purrr::map(id, get_room, since, initial_sync))
}
