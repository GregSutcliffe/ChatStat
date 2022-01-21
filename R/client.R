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
      limit        = 100,
      from         = from,
      to           = to
    )
  )

  # TODO: Handle errors.
  httr::content(response)
}

#' Encode a Matrix room ID for use within an URL.
#'
#' Room IDs will have a "!" at the start which needs to be encoded as %21
#' within URLs.
#'
#' @param room_id The room ID to be encoded.
#' @return The encoded room ID.
#'
#' @noRd
encode_room_id <- function(room_id) {
  sub("^\\!", "%21", room_id)
}
