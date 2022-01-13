#' Create the representation of matrix rooms.
#'
#' @param id Matrix API identifiers of the rooms.
#' @param since Date and time of the beginning of the included events.
#' @param events One `data.frame` containing the events for all rooms.
#' @param next_token Token for the next sync. This should be set from the
#'   `next_batch` item from the last `sync()` that was used to update the
#'   rooms.
#'
#' @return An object of class `ChatStat_rooms` that stores the parameters as
#'   named list items.
#'
#' @export
rooms <- function(id,
                  since = lubridate::now(),
                  events = tibble::tibble(),
                  next_token = character()) {
  if (!is.character(id)) {
    stop("'id' should be of type character.")
  } else if (!lubridate::is.timepoint(since)) {
    stop("'since' has to be a timepoint; see lubridate::is.timepoint().")
  } else if (length(since) > 1) {
    stop("'since' should be a single timepoint.")
  } else if (!tibble::is_tibble(events)) {
    stop("'events' has to be a tibble; see tibble::is_tibble().")
  } else if (!is.character(next_token)) {
    stop("'next_token' should be of type character.")
  } else if (length(next_token) > 1) {
    stop("'next_token' should be a single character string.")
  }

  structure(
    list(
      id = id,
      since = since,
      events = events,
      next_token = next_token
    ),
    class = "ChatStat_rooms"
  )
}

#' Update room state using the Matrix API.
#'
#' This function extends the existing state of the rooms with new events from
#' the Matrix API. Events are added from a recent call to [sync()] and the gap
#' to the existing timeline is filled with additional calls to the messages
#' API.
#'
#' @param rooms The rooms to update. See [rooms()].
#' @param sync The result of a recent call to [sync()]. This will be called
#'   automatically, if the parameter is missing. Note, that this should be a
#'   sync that was done using the `next_token` item from the rooms object as
#'   the `since` parameter.
#'
#' @export
update_rooms <- function(rooms, sync = NULL) {
  if (!inherits(rooms, "ChatStat_rooms")) {
    stop(
      "Rooms can't be updated, because it is not a valid object of class ",
      "\"ChatStat_rooms\". See ChatStat::rooms()."
    )
  }

  if (is.null(rooms$next_token)) {
    stop(
      "Rooms can't be updated, because it has not been initialized using the ",
      "API. Use ChatStat::get_room() to get an updatable rooms object."
    )
  }

  if (is.null(sync)) {
    sync <- sync(since = rooms$next_token)
  }

  events <- rooms$events
  next_batch_token <- rooms$next_token

  # Iterate through the rooms and update their state separately.
  for (room_id in rooms$id) {
    prev_batch_token <- sync$rooms$join[[room_id]]$timeline$prev_batch

    # Retrieve messages from the gap between the last sync and the recent sync
    # and append them to the existing events.
    while (TRUE) {
      messages <- get_messages(
        room_id,
        from = next_batch_token,
        dir = "f",
        to = prev_batch_token
      )

      new_events <- messages$chunk

      if (length(new_events) < 1) {
        break
      }

      events <- events |>
        tibble::add_row(process_events(room_id, messages$chunk))

      next_batch_token <- messages$end
    }

    # Append the messages from the new sync.
    events <- events |>
      tibble::add_row(
        process_events(room_id, sync$rooms$join[[room_id]]$timeline$events)
      )
  }

  rooms(
    id = rooms$id,
    since = rooms$since,
    events = normalize_events(events),
    next_token = sync$next_batch
  )
}
