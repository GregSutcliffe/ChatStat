#' Perform an initial sync using the Matrix Client-Server API.
#'
#' @param since Optional parameter to continue from a previous sync.
#'
#' @return A list object from the Matrix API.
#'
#' @export
sync <- function(since = NULL) {
  # Documentation:
  # https://spec.matrix.org/v1.9/client-server-api/#get_matrixclientv3sync

  # TODO we can do this better I think
  token <- Sys.getenv("token")

  # TODO use Filtering here
  # (https://spec.matrix.org/v1.9/client-server-api/#api-endpoints)
  req <- httr2::request(api_url("/_matrix/client/v3/sync")) |>
    httr2::req_headers(Authorization = glue::glue("Bearer {token}")) |>
    httr2::req_url_query(set_presence = "offline", # The user is not really logging in
                         since        = since) |>  # optional
    httr2::req_throttle(30 / 60) # 30 requests per 60 seconds

  response <- req |> httr2::req_perform() |> httr2::resp_check_status()


  try(httr2::resp_check_content_type(response, "application/json"))

  # TODO check error code properly
  # Return the list
  response |> httr2::resp_body_json()
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
get_messages <- function(room_id,
                         from,
                         dir = "b",
                         to = NULL) {
  # Documentation:
  # https://spec.matrix.org/v1.9/client-server-api/#get_matrixclientv3roomsroomidmessages

  # TODO: Better configuration.
  token <- Sys.getenv("token")

  req <- httr2::request(api_url(
    glue::glue(
      "/_matrix/client/v3/rooms/{room_id}/messages"
    )
  ))  |>
    httr2::req_headers(Authorization = glue::glue("Bearer {token}")) |>
    httr2::req_url_query(
      dir          = dir,
      limit        = 100,
      from         = from,
      to           = to
    ) |>
    httr2::req_throttle(30 / 60) # 30 requests per 60 seconds

  response <- req |> httr2::req_perform() |> httr2::resp_check_status()
  try(httr2::resp_check_content_type(response, "application/json"))

  # TODO: Handle errors.
  response |> httr2::resp_body_json()
}

#' Retrieve a list of memberships for one Matrix room.
#'
#' This will fetch all memberships for a given room at the current point in
#' time. Note that this will include currently joined users as well as all users
#' that have been in the room historically. They can be differentiated by their
#' `membership` field.
#'
#' @param room_id The room to receive members for.
#'
#' @return A list object from the Matrix API.
#'
#' @export
get_members <- function(room_id) {
  # Documentation:
  # https://spec.matrix.org/v1.9/client-server-api/#get_matrixclientv3roomsroomidmembers

  # TODO: Better configuration.
  token <- Sys.getenv("token")

  req <- httr2::request(api_url(
    glue::glue(
      "/_matrix/client/v3/rooms/{room_id}/members"
    )
  )) |>
    httr2::req_headers(Authorization = glue::glue("Bearer {token}")) |>
    httr2::req_throttle(30 / 60) # 30 requests per 60 seconds

  # TODO: Handle errors.
  response <- req |> httr2::req_perform() |> httr2::resp_check_status()
  try(httr2::resp_check_content_type(response, "application/json"))

  response |> httr2::resp_body_json()
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
