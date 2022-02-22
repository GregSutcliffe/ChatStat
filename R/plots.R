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

#' Create a network plot displaying the adjacency of users.
#'
#' The network graph is constructed by putting users into close proximity based
#' on their shared joined rooms.
#'
#' @param rooms Rooms object to use. See [rooms()].
#' @param joined_threshold Minimum number of joined rooms for including a user.
#' @param user_blacklist User IDs that should be excluded.
#' @param shared_threshold Minimum number of shared rooms for including a
#'   connection between two users. The default value is 2 to accomodate the
#'   fact, that all users share a room with the user doing the request.
#' @param show_labels Whether to show labels. This parameter does not apply to
#'   the users on the `label_whitelist`.
#' @param label_threshold Minimum number of joined rooms for labeling a user.
#' @param label_whitelist User IDs that should be labeled.
#'
#' @return An `igraph` object for plotting. See [igraph::igraph.plotting] for
#'   information on tweaking the visuals.
#'
#' @export
#'
#' @examples
#'
#' # Retrieve some rooms (in this case all joined rooms):
#'
#' initial_sync <- sync()
#'
#' rooms <- get_rooms(
#'   all_rooms(initial_sync),
#'   since = lubridate::today(),
#'   initial_sync
#' )
#'
#' # Create the network graph:
#'
#' graph <- plot_user_network(rooms)
#'
#' # Plot the graph tweaking some of the visuals:
#'
#' plot(
#'   graph,
#'   edge.color = "#BBBBBB1A",
#'   vertex.frame.color = NA,
#'   vertex.color = "#303030AA"
#' )
#'
plot_user_network <- function(rooms,
                              joined_threshold = 2,
                              user_blacklist = NULL,
                              shared_threshold = 2,
                              show_labels = TRUE,
                              label_threshold = 5,
                              label_whitelist = NULL) {
    members_filtered <- rooms$members |>
        dplyr::filter(!user_id %in% user_blacklist &
            membership_state == "join") |>
        dplyr::group_by(user_id) |>
        dplyr::add_count() |>
        dplyr::ungroup() |>
        dplyr::rename(n_joined = n) |>
        dplyr::filter(n_joined >= joined_threshold)

    nodes <- members_filtered |>
        dplyr::rename(id = user_id, label = display_name) |>
        dplyr::mutate(label = dplyr::if_else(
            (show_labels & n_joined >= label_threshold) |
                id %in% label_whitelist,
            label,
            ""
        )) |>
        dplyr::mutate(size = 10 * n_joined / max(n_joined)) |>
        dplyr::select(id, label, size) |>
        dplyr::distinct(id, .keep_all = TRUE)

    edges <- members_filtered |>
        widyr::pairwise_count(user_id, room, upper = FALSE) |>
        dplyr::rename(n_shared = n) |>
        dplyr::arrange(-n_shared) |>
        dplyr::filter(n_shared >= shared_threshold) |>
        dplyr::mutate(weight = n_shared / max(n_shared)) |>
        dplyr::select(item1, item2, weight)

    graph <- igraph::graph_from_data_frame(
        edges,
        vertices = nodes,
        directed = FALSE
    )

    graph
}
