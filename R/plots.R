#' Create a plot showing the daily activity across the provided rooms.
#'
#' @param rooms Rooms object to use. See [rooms()].
#' @return A `ggplot2` object for plotting.
#'
#' @export
plot_daily_activity <- function(rooms) {
    rooms$events |>
        dplyr::filter(type %in% c("m.room.message", "m.reaction")) |>
        dplyr::mutate(
            day = lubridate::date(time),
            hour = lubridate::hour(time)
        ) |>
        dplyr::count(day) |>
        ggplot2::ggplot(ggplot2::aes(x = day, y = n)) +
        ggplot2::geom_col(fill = "lightblue") +
        ggplot2::geom_smooth() +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Date", y = "Messages")
}

#' Create a plot showing the activity across the provided rooms by time-of-day.
#'
#' @param rooms Rooms object to use. See [rooms()].
#' @return A `ggplot2` object for plotting.
#'
#' @export
plot_time_of_day_activity <- function(rooms) {
    rooms$events |>
        dplyr::filter(type %in% c("m.room.message", "m.reaction")) |>
        dplyr::mutate(
            day = lubridate::date(time),
            hour = lubridate::hour(time)
        ) |>
        dplyr::mutate(colour = dplyr::case_when(
            hour %in% 0:5 ~ "0-5",
            hour %in% 6:11 ~ "6-11",
            hour %in% 12:17 ~ "12-17",
            TRUE ~ "18-23"
        )) |>
        dplyr::mutate(colour = factor(
            colour,
            levels = rev(c("0-5", "6-11", "12-17", "18-23"))
        )) |>
        dplyr::count(day, colour) |>
        dplyr::add_count(day, wt = n, name = "total") |>
        ggplot2::ggplot(ggplot2::aes(x = day, y = n, fill = colour)) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::geom_text(
            ggplot2::aes(y = total, label = total, fill = NULL),
            nudge_y = 30
        ) +
        ggplot2::scale_fill_manual(
            name = "Hour",
            values = c("blue", "green", "yellow", "red")
        ) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Date", y = "Messages")
}

#' Create a plot showing the most active hours across the provided rooms.
#'
#' @param rooms Rooms object to use. See [rooms()].
#' @return A `ggplot2` object for plotting.
#'
#' @export
plot_active_times <- function(rooms) {
    rooms$events |>
        dplyr::filter(type %in% c("m.room.message", "m.reaction")) |>
        dplyr::mutate(hour = lubridate::hour(time)) |>
        dplyr::count(hour) |>
        dplyr::mutate(p = n / sum(n)) |>
        dplyr::mutate(colour = dplyr::case_when(
            hour %in% 0:5 ~ "0-5",
            hour %in% 6:11 ~ "6-11",
            hour %in% 12:17 ~ "12-17",
            TRUE ~ "18-23"
        )) |>
        dplyr::mutate(colour = factor(
            colour,
            levels = rev(c("0-5", "6-11", "12-17", "18-23"))
        )) |>
        ggplot2::ggplot(ggplot2::aes(x = hour, y = p, fill = colour)) +
        ggplot2::geom_col() +
        ggplot2::geom_text(
            ggplot2::aes(
                y = p,
                label = scales::percent(p, accuracy = 0.1)
            ),
            size = 3,
            vjust = -.5
        ) +
        ggplot2::scale_fill_manual(
            name = "Hour",
            values = c("blue", "green", "yellow", "red")
        ) +
        ggplot2::scale_x_continuous(breaks = 0:23) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()
        ) +
        ggplot2::labs(x = "Hour", y = "")
}
