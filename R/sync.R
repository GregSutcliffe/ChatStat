#' Perform an initial sync using the Matrix Client-Server API.
#'
#' @param since Optional parameter to continue from a previous sync.
#'
#' @return A list object from the Matrix API.
#'
#' @export
initial_sync <- function(since = NULL) {
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
