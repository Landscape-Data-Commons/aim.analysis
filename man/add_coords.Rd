% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial_manipulation.R
\name{add_coords}
\alias{add_coords}
\title{Adding coordinate variables to an sf points object}
\usage{
add_coords(
  points,
  coord_names = c("x_coord", "y_coord"),
  nad83 = FALSE,
  albers = FALSE
)
}
\arguments{
\item{points}{A points sf object. The coordinates for the points will be added for at least the current projection.}

\item{coord_names}{Character vector. The names for the coordinate variables using the CRS of \code{points}. Format is \code{c("name for x coord", "name for y coord")}. Defaults to \code{c("x_coord", "y_coord")}.}

\item{nad83}{Logical. If \code{TRUE} Then the variables \code{LONGITUDE_NAD83} and \code{LATITUDE_NAD83} will be added using NAD83 as the CRS. Defaults to \code{FALSE}.}

\item{albers}{Logical. If \code{TRUE} Then the variables \code{X_METERS_AL} and \code{Y_METERS_AL} will be added using Albers Equal Area as the CRS. Defaults to \code{FALSE}.}
}
\value{
\code{points} with coordinate fields added to the data frame as requested.
}
\description{
Adds one or more of the following: the coordinates from the current coordinate reference system; coordinates in NAD83; and coordinates in Albers Equal Area. This does not change the projection of the points.
}
