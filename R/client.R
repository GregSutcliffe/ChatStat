#' Perform an initial sync using the Matrix Client-Server API.
#'
#' @param since Optional parameter to continue from a previous sync.
#'
#' @return A list object from the Matrix API.
#'
#' @export
sync <- function(since = NULL) {
  # Documentation:
  # https://spec.matrix.org/v1.1/client-server-api/#get_matrixclientv3sync

  # TODO we can do this better I think
  token <- Sys.getenv("token")

  # TODO use Filtering here
  # (https://spec.matrix.org/v1.1/client-server-api/#api-endpoints)
  response <- httr::GET(
    url   = api_url("/_matrix/client/r0/sync"),
    query = list(
      access_token = token,
      set_presence = "offline", # The user is not really logging in
      since        = since      # optional
    )
  )
  # TODO check error code properly
  # print(httr::status_code(resp))

  # Return the list
  httr::content(response)
}

#' Retrieve messages using the Matrix-Client-Server API.
#'
#' @param room_id The room whose messages will be received.
#' @param from The start token within the history.
#' @param dir Direction to receive messages in. "b" for backwards in time, "f"
#'   for forwards in time.
#' @param to The end token to stop receiving messages at.
#'
#' @return A list object from the Matrix API.
#'
#' @export
get_messages <- function(room_id, from, dir = "b", to = NULL) {
  # Documentation:
  # https://spec.matrix.org/v1.1/client-server-api/#get_matrixclientv3roomsroomidmessages

  # TODO: Better configuration.
  token <- Sys.getenv("token")

  response <- httr::GET(
    url = api_url(
      glue::glue("/_matrix/client/r0/rooms/{encode_room_id(room_id)}/messages")
    ),
    query = list(
      access_token = token,
      dir          = dir,
      from         = from,
      limit        = 100,
      to           = to
    )
  )

  # TODO: Handle errors.
  httr::content(response)
}

#' Encode a Matrix room ID for use within an URL.
#'
#' Matrix supports room aliases which are human readable
#' but requires the use of the room ID for the API. This
#' function will take either and make it safe for use in
#' later API requests
#'
#' @param room_id The room ID to be encoded.
#' @return The encoded room ID.
#'
#' @noRd
encode_room_id <- function(room_id) {
  if (stringi::stri_startswith_fixed(room_id,'!')) {
    # Raw room ID, just encode it
    return(curl::curl_escape(room_id))
  }

  # Else assume an alias of some kind

  # Documentation:
  # https://spec.matrix.org/v1.1/client-server-api/#get_matrixclientv3directoryroomroomalias
  token <- Sys.getenv("token")
  room_id = curl::curl_escape(room_id)

  response <- httr::GET(
    url = api_url(
      glue::glue("/_matrix/client/r0/directory/room/{room_id}")
    ),
    query = list(access_token = token)
  )

  # TODO handle errors
  httr::content(response)$room_id |>
    curl::curl_escape()
}

#' Get the primary alias for a Matrix room
#'
#' Get the canonical alias of a room from the sync
#' object - this is quicker and safer than using the API
#' as the data is already on disk, and the API can return
#' unofficial aliases
#'
#' For more details, see note at
#' https://spec.matrix.org/v1.1/client-server-api/#get_matrixclientv3roomsroomidaliases
#'
#' @param room The room ID to be encoded.
#' @param sync The initial sync state from sync()
#' @return The room ID.
#'
#' @noRd
get_room_alias <- function(room_id, sync) {
  # Documentation:
  # https://spec.matrix.org/v1.1/client-server-api/#mroomcanonical_alias

  # We need to filter the canonical_alias state event from the sync tree
  room_events <- sync$rooms$join[[room_id]]$state$events
  filter      <- Filter(function(x) {x$type == 'm.room.canonical_alias'},
                       room_events)
  if (length(filter) == 0) {
    # No alias, return the ID
    return(room_id)
  } else(
    return(filter[[1]]$content$alias)
  )
}


