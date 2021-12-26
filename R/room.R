#' Create the representation of a matrix room.
#'
#' @param id Matrix API identifier of the room.
#' @param since Date and time of the beginning of the included events.
#' @param events A `data.frame` containing the room events.
#' @param next_token Token for the next sync. This should be set from the
#'   `next_batch` item from the last `sync()` that was used to update this
#'   room.
#'
#' @return An object of class `ChatStat_room` that stores the parameters as
#'   named list items.
#'
#' @noRd
room <- function(id,
                 since = lubridate::now(),
                 events = tibble::tibble(),
                 next_token = NULL) {
  stopifnot(is.character(id) & length(id) == 1)

  structure(
    list(
      id = id,
      since = since,
      events = events,
      next_token = next_token
    ),
    class = "ChatStat_room"
  )
}

#' Update the room state using the Matrix API.
#'
#' This function extends the existing state of the room with new events from
#' the Matrix API. Events are added from a recent call to [sync()] and the gap
#' to the existing timeline is filled with additional calls to the messages
#' API.
#'
#' @param room The room to update.
#' @param sync The result of a recent call to [sync()]. This will be called
#'   automatically, if the parameter is missing. However, it is recommended to
#'   share the sync between multiple rooms to avoid duplicated work and to have
#'   a common base for comparing the timelines.
#'
#' @export
update_room <- function(room, sync = NULL) {
  if (!inherits(room, "ChatStat_room")) {
    stop(
      "Room can't be updated, because it is not a valid object of class ",
      "\"ChatStat_room\". See ChatStat::get_room() for creating one."
    )
  }

  if (is.null(room$next_token)) {
    stop(
      "Room can't be updated, because it has not been initialized using the ",
      "API. Use ChatStat::get_room() to get an updatable room object."
    )
  }

  if (is.null(sync)) {
    sync <- sync(since = room$next_token)
  }

  events <- room$events
  next_batch_token <- room$next_token
  prev_batch_token <- sync$rooms$join[[room$id]]$timeline$prev_batch

  # Retrieve messages from the gap between the last sync and the recent sync
  # and append them to the existing events.

  while (TRUE) {
    messages <- get_messages(
      room$id,
      from = next_batch_token,
      dir = "f",
      to = prev_batch_token
    )

    new_events <- messages$chunk

    if (length(new_events) < 1) {
      break
    }

    events <- events |> tibble::add_row(process_events(messages$chunk))

    next_batch_token <- messages$end
  }

  # Append the messages from the new sync.
  events <- events |> tibble::add_row(
    process_events(sync$rooms$join[[room$id]]$timeline$events)
  )

  room(
    id = room$id,
    since = room$since,
    events = events,
    next_token = sync$next_batch
  )
}
