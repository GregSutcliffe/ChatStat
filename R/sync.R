#' Call the initial sync from the Matrix Client-Server API
#'
#' @param since Optional parameter to continue from a previous sync
#'
#' @return A list object from the Matrix API
#' @export
sync <- function(since = NULL) {
  # Docs: https://spec.matrix.org/v1.1/client-server-api/#get_matrixclientv3sync
  endpoint <- '/_matrix/client/r0/sync'
  token    <- Sys.getenv('token') # TODO we can do this better I think


  # TODO use Filtering here (https://spec.matrix.org/v1.1/client-server-api/#api-endpoints)
  resp <- httr::GET(
    url   = api_url(endpoint),
    query = list(access_token = token,
                 set_presence = 'offline', # The user is not really logging in
                 since        = since      # optional
    )
  )
  # TODO check error code properly
  # print(httr::status_code(resp))

  # Return the list
  httr::content(resp)
}
