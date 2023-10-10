#' Adding coordinate variables to the data frame of a SpatialPointsDataFrame
#'
#' @description Adds one or more of the following: the coordinates from the current coordinate refrence system; coordinates in NAD83; and coordinates in Albers Equal Area. This does not change the projection of the SPDF.
#' @param spdf A SpatialPointsDataFrame to add the coordinates to
#' @param current.proj Logical. If \code{TRUE} Then the columns \code{coords.x1} and \code{coords.x2} will be added using the current projection. Defaults to \code{TRUE}.
#' @param xynames Optional vector of two character strings to rename the coordinate variables from the current projection. Format is \code{c("replacement for coords.x1", "replacement for coords.x2")}.
#' @param nad83 Logical. If \code{TRUE} Then the columns \code{LONGITUDE.NAD83} and \code{LATITUDE.NAD83} will be added using NAD83. Defaults to \code{FALSE}.
#' @param albers Logical. If \code{TRUE} Then the columns \code{X.METERS.AL} and \code{Y.METERS.AL} will be added using Albers Equal Area. Defaults to \code{FALSE}.
#' @return \code{spdf} with fields added to the data frame as requested.
#' @export
add_coords <- function(spdf,
                       current.proj = TRUE,
                       xynames = NULL,
                       nad83 = FALSE,
                       albers = FALSE){
  projNAD83 <- sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
  projAL <- sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  if (current.proj) {
    coords <- as.data.frame(spdf@coords)
    if(!is.null(xynames)) {
      names(coords) <- xynames
    }
    spdf@data <- cbind(spdf@data, coords)
  }
  if (nad83) {
    coords <-  sp::spTransform(spdf, CRSobj = projNAD83)@coords
    names(coords) <- c("LONGITUDE.NAD83", "LATITUDE.NAD83")
    spdf@data <- cbind(spdf@data, coords)
  }
  if (albers) {
    coords <- sp::spTransform(spdf, CRSobj = projAL)@coords
    names(coords) <- c("X.METERS.AL", "Y.METERS.AL")
    spdf@data <- cbind(spdf@data, coords)
  }
  return(spdf)
}
