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
    url = api_url("/_matrix/client/r0/sync"),
    query = list(
      access_token = token,
      set_presence = "offline", # The user is not really logging in
      since = since # optional
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
#'
#' @return A list object from the Matrix API.
#'
#' @export
get_messages <- function(room_id, from) {
  # Documentation:
  # https://spec.matrix.org/v1.1/client-server-api/#get_matrixclientv3roomsroomidmessages

  # The room_id will have a "!" at the start which needs to be encoded as %21.
  room_id_encoded <- sub("^\\!", "%21", room_id)

  # TODO: Better configuration.
  token <- Sys.getenv("token")

  response <- httr::GET(
    url = api_url(
      glue::glue("/_matrix/client/r0/rooms/{room_id_encoded}/messages")
    ),
    query = list(
      access_token = token,
      dir = "b",
      from = from,
      limit = 100
    )
  )

  # TODO: Handle errors.
  httr::content(response)
}
