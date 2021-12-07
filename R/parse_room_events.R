#' Parse the JSON list for a room into a rectangular format
#'
#' @param event_list the list of events from the Matrix API
#'
#' @return A table of event data
#' @export
parse_room_events <- function(event_list) {
  tibble::tibble(
    roomid     = purrr::map_chr(event_list,'room_id'),
    userid     = purrr::map(event_list,'sender') |> purrr::map_chr(`[[`,1),
    name       = purrr::map(event_list,'content') |>
                   purrr::map_chr('displayname', .default=NA),
    event_type = purrr::map(event_list,'type') |> purrr::map_chr(`[[`,1),
    msgtype    = purrr::map(event_list,'content') |>
                   purrr::map_chr('msgtype', .default=NA),
    body       = purrr::map(event_list,'content') |>
                   purrr::map_chr('body', .default=NA),
    age        = purrr::map(event_list,'age') |> purrr::map_dbl(`[[`,1),
  ) |>
    # TODO Sys.time() is not necessarily right here
    # We really need to log the API query time in the results object ...
    dplyr::mutate(time = Sys.time() - (age/1000),
                  date = lubridate::as_datetime(time)) |>
    dplyr::arrange(date)
}
