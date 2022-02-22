#' Create the representation of matrix rooms.
#'
#' @param id Matrix API identifiers of the rooms.
#' @param since Date and time of the beginning of the included events.
#' @param events One `data.frame` containing the events for all rooms.
#' @param members A `data.frame` of current room memberships.
#' @param next_token Token for the next sync. This should be set from the
#'   `next_batch` item from the last `sync()` that was used to update the
#'   rooms.
#'
#' @return An object of class `ChatStat_rooms` that stores the parameters as
#'   named list items.
#'
#' @seealso [get_rooms()]
#'
#' @export
rooms <- function(id,
                  since = lubridate::now(),
                  events = tibble::tibble(),
                  members = tibble::tibble(),
                  next_token = character()) {
  if (!is.character(id)) {
    stop("'id' should be of type character.")
  } else if (!lubridate::is.timepoint(since)) {
    stop("'since' has to be a timepoint; see lubridate::is.timepoint().")
  } else if (length(since) > 1) {
    stop("'since' should be a single timepoint.")
  } else if (!tibble::is_tibble(events)) {
    stop("'events' has to be a tibble; see tibble::is_tibble().")
  } else if (!tibble::is_tibble(members)) {
    stop("'members' has to be a tibble; see tibble::is_tibble().")
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
      members = members,
      next_token = next_token
    ),
    class = "ChatStat_rooms"
  )
}

#' Print information on a rooms object.
#'
#' @param x The rooms object to print.
#' @param ... Ignored, for compatability with print.default
#'
#' @export
print.ChatStat_rooms <- function(x, ...) {
  room_id_width <- max(nchar(x$id)) + 3

  header <- paste0(
    format("Room ID", width = room_id_width),
    "Event count\n"
  )

  rows <- purrr::map(x$id, function(room_id) {
    event_count <- x$events |>
      dplyr::filter(room == room_id) |>
      dplyr::count() |>
      dplyr::pull(n)

    paste0(
      format(room_id, width = room_id_width),
      event_count,
      "\n"
    )
  })

  cat(
    "Matrix room state since",
    format(x$since),
    "\n\n",
    header,
    paste0(rows)
  )
}

#' Get the room events for one or more Matrix rooms.
#'
#' @param room_ids     A vector of room IDs to get data for.
#' @param since        Stop paginating when reaching this time.
#' @param initial_sync Result of a prior call to [sync()]. This should be an
#'   initial sync meaning that the `since` parameter was not provided. If this
#'   is not provided, it will be called automatically.
#'
#' @return An object of class `ChatStat_rooms`.
#'
#' @export
get_rooms <- function(room_ids, since, initial_sync = NULL) {
  since <- lubridate::as_datetime(since)

  # TODO: Add better configuration.
  token <- Sys.getenv("token")

  # Perform an initial sync if required.
  if (is.null(initial_sync)) {
    rlog::log_info("Performing initial sync.")
    initial_sync <- sync()
  }

  # Buffers for all events and members across rooms.
  events <- empty_events()
  members <- empty_members()

  # Iterate through the room IDs and retrieve the rooms separately.
  for (room_id in room_ids) {
    rlog::log_info(glue::glue("Processing room {room_id}."))

    timeline <- initial_sync$rooms$join[[room_id]]$timeline
    from <- timeline$prev_batch

    room_events <- process_events(room_id, timeline$events)
    rlog::log_debug(glue::glue("Initial sync yielded {nrow(events)} events."))

    # Paginate backwards to retrieve older events that were not included in the
    # initial sync.
    while (TRUE) {
      oldest_time <- room_events |>
        dplyr::arrange(time) |>
        dplyr::slice(1) |>
        dplyr::pull(time)

      if (oldest_time < since) {
        rlog::log_info("Specified time has been reached. Stopping.")
        break
      }

      rlog::log_debug(glue::glue("Oldest message is from {oldest_time}."))

      messages <- get_messages(room_id, from)
      new_event_count <- length(messages$chunk)

      if (new_event_count <= 0) {
        rlog::log_info("No more events. Stopping early.")
        break
      }

      rlog::log_debug(glue::glue("Received {new_event_count} more events."))

      new_events <- process_events(room_id, messages$chunk)
      room_events <- room_events |> tibble::add_row(new_events, .before = 0)
      from <- messages$end
    }

    room_events <- room_events |>
      dplyr::filter(time >= since)

    events <- events |> tibble::add_row(room_events)

    # Also retrieve the current room members and add them to the buffer.
    room_members_raw <- get_members(room_id)
    room_members <- process_members(room_id, room_members_raw$chunk)
    members <- members |> tibble::add_row(room_members)
  }

  rooms(
    id = room_ids,
    since = since,
    events = normalize_events(events),
    members = members,
    next_token = initial_sync$next_batch
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
      "API. Use ChatStat::get_rooms() to get an updatable rooms object."
    )
  }

  if (is.null(sync)) {
    sync <- sync(since = rooms$next_token)
  }

  events <- rooms$events
  next_batch_token <- rooms$next_token

  # Buffer for all members across rooms. They will be refetched in total for
  # each update.
  members <- empty_members()

  # Iterate through the rooms and update their state separately.
  for (room_id in rooms$id) {
    room_timeline <- sync$rooms$join[[room_id]]$timeline
    events_from_sync <- room_timeline$events
    prev_batch_token <- room_timeline$prev_batch

    rlog::log_info(glue::glue("Updating room {room_id}."))
    rlog::log_debug(
      glue::glue("Sync contains {length(events_from_sync)} events.")
    )

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
      n_new_events <- length(new_events)

      if (n_new_events < 1) {
        rlog::log_debug("No more events for room. Stopping.")
        break
      }

      rlog::log_debug(glue::glue("Received {n_new_events} more events."))

      events <- events |>
        tibble::add_row(process_events(room_id, messages$chunk))

      next_batch_token <- messages$end
    }

    # Append the messages from the new sync.
    events <- events |>
      tibble::add_row(process_events(room_id, events_from_sync))

    # Also retrieve the current room members and add them to the buffer.
    room_members_raw <- get_members(room_id)
    room_members <- process_members(room_id, room_members_raw$chunk)
    members <- members |> tibble::add_row(room_members)
  }

  # Remove events older than since that may have been added by the calls to the
  # messages API.
  events <- events |>
    dplyr::filter(time >= rooms$since)

  rooms(
    id = rooms$id,
    since = rooms$since,
    events = normalize_events(events),
    members = members,
    next_token = sync$next_batch
  )
}
