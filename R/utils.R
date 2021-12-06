api_url <- function(url, host = NULL, port = NULL) {
  if (is.null(host)) {
    host = Sys.getenv("host")
  }

  if (!is.null(port)) {
    port = glue::glue(':{port}')
  } else {
    port = ""
  }

  glue::glue('https://{host}{port}{url}')
}
