#' Get the list of room events for a given room ID.
#'
#' @param room_id The room to get data for.
#' @param since  Stop paginating when reaching this time.
#'
#' @return A list of events.
#'
#' @export
room_history <- function(room_id, since = NULL) {
  # Documentation:
  # https://spec.matrix.org/v1.1/client-server-api/#get_matrixclientv3roomsroomidmessages

  # The room_id will have a "!" at the start which needs to be encoded as %21.
  room_id <- sub("^\\!", "%21", room_id)

  # Get the initial sync (i.e *newest* messages)
  # TODO check that actually worked
  initial <- initial_sync()

  # Build a pagination loop
  continue <- TRUE
  result <- list()
  from <- initial$next_batch
  token <- Sys.getenv("token")

  while (continue) {
    response <- httr::GET(
      url = api_url(glue::glue("/_matrix/client/r0/rooms/{roomid}/messages")),
      query = list(
        access_token = token,
        dir = "b",
        limit = 100,
        from = from
      )
    )

    dat <- httr::content(response)

    result <- c(result, dat$chunk)
    from <- dat$end

    # We expect since to be a POSIX datetime, but could be a string.
    since <- lubridate::as_datetime(since)

    # Find the earliest event in the results so far, and see if we've gone
    # back far enough yet.
    # NOTE "age" is the time since the event, so *max* is needed to get the
    # earliest event, not min.
    max_age <- max(sapply(result, function(event) {
      event$age
    }))

    min_time <- lubridate::as_datetime(Sys.time() - max_age / 1000)

    print(min_time)
    continue <- (since < min_time)
  }

  # TODO actually filter these to the events after `since`
  result
}
