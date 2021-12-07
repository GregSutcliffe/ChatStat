api_url <- function(url, host = NULL, port = NULL) {
  if (is.null(host)) {
    host <- Sys.getenv("host")
  }

  port <- if (!is.null(port)) {
    glue::glue(":{port}")
  } else {
    ""
  }

  glue::glue("https://{host}{port}{url}")
}
