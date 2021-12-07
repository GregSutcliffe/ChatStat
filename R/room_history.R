#' Get the list of room events for a given roomID
#'
#' @param roomid the room to get data for
#' @param since  stop paginating when reaching this time
#'
#' @return A list of events
#' @export
room_history <- function(roomid, since=NULL) {
  # https://matrix.org/docs/api/client-server/#get-/_matrix/client/r0/rooms/-roomId-/messages

  # RoomID will have a "!" at the start which needs to be encoded as %21
  roomid <- sub('^\\!','%21',roomid)

  # Get the initial sync (i.e *newest* messages)
  initial <- sync()
  # TODO check that actually worked

  # Build a pagination loop
  continue <- TRUE
  result   <- list()
  from     <- initial$next_batch
  token    <- Sys.getenv('token')
  while (continue) {
    resp <- httr::GET(
      url   = api_url(glue::glue('/_matrix/client/r0/rooms/{roomid}/messages')),
      query = list(access_token = token,
                   dir = 'b',
                   limit = 100,
                   from = from)
    )
    dat <- httr::content(resp)

    result   <- c(result, dat$chunk)
    from     <- dat$end

    # We expect since to be a POSIX datetime, but could be a string
    since <- lubridate::as_datetime(since)

    # Find the earliest event in the results so far, and see if we've gone
    # back far enough yet
    # NOTE "age" is the time since the event, so *max* is needed to get the
    # earliest event, not min
    max_age  <- max(sapply(result, function(.x){.x$age}))
    min_time <- lubridate::as_datetime(Sys.time() - max_age/1000)

    print(min_time)
    continue <- (since < min_time)
  }

  # TODO actually filter these to the events after `since`
  result
}
