% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rooms.R
\name{rooms}
\alias{rooms}
\title{Create the representation of matrix rooms.}
\usage{
rooms(
  id,
  since = lubridate::now(),
  events = tibble::tibble(),
  members = tibble::tibble(),
  next_token = character()
)
}
\arguments{
\item{id}{Matrix API identifiers of the rooms.}

\item{since}{Date and time of the beginning of the included events.}

\item{events}{One \code{data.frame} containing the events for all rooms.}

\item{members}{A \code{data.frame} of current room memberships.}

\item{next_token}{Token for the next sync. This should be set from the
\code{next_batch} item from the last \code{sync()} that was used to update the
rooms.}
}
\value{
An object of class \code{ChatStat_rooms} that stores the parameters as
named list items.
}
\description{
Create the representation of matrix rooms.
}
\seealso{
\code{\link[=get_rooms]{get_rooms()}}
}
