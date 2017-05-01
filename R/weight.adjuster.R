#' Adjusting Weights Calculated from AIM Sample Designs
#'
#' This function takes the point weights data frame output from the function weighter() and a SpatialPolygonsDataFrame defining the weight categories. Returns the data frame supplied as points with the new column ADJWGT containing the adjusted weights.
#' @param points Data frame output from weighter(), equivalent to weighter()[["point.weights"]] or weighter()[[2]].
#' @param wgtcat.spdf SpatialPolygonsDataFrame describing the weight categories for adjusting the weights. Use the output from intersector()
#' @param spdf.area.field Character string defining the field name in wgtcat@data that contains the areas for the weight categories. Defaults to "AREA.HA.UNIT.SUM"
#' @param spdf.wgtcat.field Character string defining the field name in wgtcat@data that contains the unique identification for the weight categories. Defaults to "UNIQUE.IDENTIFIER"
#' @param projection CRS string. Defaults to NAD83 CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"). Is used to reproject all SPDFs in order to perform spatial manipulations
#' @keywords weights
#' @examples
#' weight.adjuster()

weight.adjuster <- function(points, ## The weighted output from weighter(), so weighter()["point.weights"] | weighter()[2] IF YOU RESTRICTED THE SDD INPUT BY THE REPORTING UNIT POLYGON
                            wgtcat.spdf, ## The SPDF that's represents all the weird possible combinations of the reporting unit and strata
                            spdf.area.field = "AREA.HA.UNIT.SUM", ## The name of the field in the SPDF that contains the areas of the weight categories
                            spdf.wgtcat.field = "UNIQUE.IDENTIFIER", ## The name of the field in the SPDF that contains the identifiers for weight categories
                            projection = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs") ## NAD83, standard issue as always
){
  ## Sanitization
  names(points) <- str_to_upper(names(points))
  names(wgtcat.spdf@data) <- str_to_upper(names(wgtcat.spdf@data))
  spdf.area.field <- str_to_upper(spdf.area.field)
  spdf.wgtcat.field <- str_to_upper(spdf.wgtcat.field)

  ## Convert points to an SPDF
  points.spdf <- SpatialPointsDataFrame(coords = points[, c("LONGITUDE", "LATITUDE")],
                                        data = points,
                                        proj4string = projection)

  ## Attribute the points.spdf with the wgtcat identities from wgtcat.spdf
  points.spdf <- attribute.shapefile(shape1 = points.spdf,
                                     shape2 = wgtcat.spdf,
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
  data.current$ADJWGT <- adjwgt(sites.current, wgt.current, wtcat.current, framesize.current)

  return(data.current)
}
