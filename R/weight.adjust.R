#' Adjusting Weights Calculated from AIM Sample Designs
#'
#' This function takes the point weights data frame output from the function \code{weight()} and a SpatialPolygonsDataFrame defining the weight categories. Returns the data frame supplied as points with the new column \code{ADJWGT} containing the adjusted weights.
#' @param points Data frame output from \code{weight()}, equivalent to \code{weight()[["point.weights"]]} or \code{weight()[[2]]}.
#' @param wgtcat.spdf SpatialPolygonsDataFrame describing the weight categories for adjusting the weights. Use the output from \code{intersect()}
#' @param spdf.area.field Character string defining the field name in \code{wgtcat@data} that contains the areas for the weight categories. Defaults to \code{"AREA.HA.UNIT.SUM"}.
#' @param spdf.wgtcat.field Character string defining the field name in \code{wgtcat@data} that contains the unique identification for the weight categories. Defaults to \code{"UNIQUE.IDENTIFIER"}.
#' @param projection \code{sp::CRS()} argument. Defaults to NAD83 with \code{sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")}. Is used to reproject all SPDFs in order to perform spatial manipulations
#' @keywords weights
#' @examples
#' weight.adjuster()
#' @export

weight.adjust <- function(points, ## The weighted output from weighter(), so weighter()["point.weights"] | weighter()[2] IF YOU RESTRICTED THE SDD INPUT BY THE REPORTING UNIT POLYGON
                          wgtcat.spdf, ## The SPDF that's represents all the weird possible combinations of the reporting unit and strata
                          spdf.area.field = "AREA.HA.UNIT.SUM", ## The name of the field in the SPDF that contains the areas of the weight categories
                          spdf.wgtcat.field = "UNIQUE.IDENTIFIER", ## The name of the field in the SPDF that contains the identifiers for weight categories
                          projection = sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs") ## NAD83, standard issue as always
){
  ## Sanitization
  names(points) <- stringr::str_to_upper(names(points))
  names(wgtcat.spdf@data) <- stringr::str_to_upper(names(wgtcat.spdf@data))
  spdf.area.field <- stringr::str_to_upper(spdf.area.field)
  spdf.wgtcat.field <- stringr::str_to_upper(spdf.wgtcat.field)

  ## Convert points to an SPDF
  points.spdf <- SpatialPointsDataFrame(coords = points[, c("LONGITUDE", "LATITUDE")],
                                        data = points,
                                        proj4string = projection)

  ## Attribute the points.spdf with the wgtcat identities from wgtcat.spdf
  points.spdf <- attribute.shapefile(spdf1 = points.spdf,
                                     spdf2 = wgtcat.spdf,
                                     attributefield = spdf.wgtcat.field,
                                     newfield = spdf.wgtcat.field)

  ## Add the areas in using the unique identifier
  data.current <- merge(x = points.spdf@data,
                        y = distinct(wgtcat.spdf@data[, c(spdf.wgtcat.field, spdf.area.field)]))

  ## The weighted points attributed by the combination of reporting units and strata
  ## We first restrict to the points that inherited identities (this should've already happened in the previous step, but just to be safe)
  # data.current <- data.attributed[!is.na(data.attributed[, points.wgtcat.field]),]

  ## We want to include all the points. So we make a logical vector of just T with a length equal to the number of plots
  sites.current <- (rep(T, nrow(data.current)))

  ## Grab the current weights from those points as its own vector
  wgt.current <- data.current$WGT

  ## NB: The identity inherited from the shapefile needs to match the field used for name in framesize
  wtcat.current <- data.current[, spdf.wgtcat.field]

  ## The framesize information about each of the unique wgtcat identities
  ## I currently have this as an area, but I think it needs to be the inverse of the proportion of the area of the reporting unit that each identity represents
  ## so the framesize value for a particular wgtcat = [area of the whole spdf]/[area of particular wgtcat]
  framesize.current <- wgtcat.spdf@data[, spdf.area.field]
  names(framesize.current) <- wgtcat.spdf@data[, spdf.wgtcat.field]

  ## Run the weight adjustment
  data.current$ADJWGT <- spsurvey::adjwgt(sites.current, wgt.current, wtcat.current, framesize.current)

  return(data.current)
}
