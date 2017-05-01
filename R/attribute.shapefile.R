#' Attributing a Spatial Data Frame using another Spatial Data Frame
#'
#' This function will take a Spatial Points/Polygons Data Frame and add one attribute fields from a second Spatial Points/Polygons Data Frame
#' @param shape1 A Spatial Points/Polygons Data Frame containing the geometry to add an attribute to
#' @param shape2 A Spatial Points/Polygons Data Frame containing the geometry to add an attribute from
#' @param attributefield The name of the field in \code{shape2} as a string containing the values to add to \code{shape1}
#' @param newfield The name of the field in \code{shape1} as a string to add the values from \code{shape2$attributefield} to. If NULL, the field will use \code{attributefield}. Defaults to NULL.
#' @param projection An \code{sp::CRS()} argument to apply in the event that \code{shape1} and \code{shape2} have different projections. Defaults to \code{CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")}


attribute.shapefile <- function(shape1,
                                shape2,
                                attributefield = NULL,
                                newfield = NULL,
                                projection = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
                                ){
  if (is.null(attributefield) | !(attributefield %in% names(shape2@data))) {
    stop("attributefield must be a field name found in shape2")
  }

  if (is.null(newfield)) {
    newfield <- attributefield
  }

  if (shape1@proj4string@projargs != shape2@proj4string@projargs) {
    ## Make sure that the points also adhere to the same projection
    shape1 <- shape1 %>% spTransform(projection)
    shape2 <- shape2 %>% spTransform(projection)
  }

  ## Initialize list for attributed SPDFs
  attributed.spdfs <- list()

  ## We'll check each attribute field value independently
  for (n in unique(shape2@data[, attributefield])) {
    ## Create a copy of the points to work with on this loop
    current.shape1 <- shape1
    ## Get the data frame from over()
    over.result <- over(current.shape1,
                        shape2[shape2@data[, attributefield] == n, ])
    ## Add the values to the newfield column
    current.shape1@data[, newfield] <- over.result[, attributefield]
    ## Make sure that the polygons have unique IDs
    if (class(current.shape1) == "SpatialPolygonsDataFrame") {
      current.shape1 <- spChFIDs(current.shape1,
                                 paste(runif(n = 1, min = 0, max = 666666666),
                                       row.names(current.shape1),
                                       sep = "."))
    }

    ## Only if the number of coordinates is greater than 0!
    print(nrow(current.shape1[!is.na(current.shape1@data[, newfield]), ]))
    if (nrow(current.shape1[!is.na(current.shape1@data[, newfield]), ]) > 0) {
      attributed.spdfs <- list(attributed.spdfs,
                               current.shape1[!is.na(current.shape1@data[, newfield]), ])
    }
  }

  if (length(attributed.spdfs) > 0) {
    if (length(attributed.spdfs) == 1) {
      output <- get(attributed.spdfs[1])
    } else {
      output <- eval(parse(text = paste0("rbind(`",
                                         paste(attributed.spdfs, collapse = "`,`"),
                                         "`)")))
    }
  } else {
    output <- NULL
  }
  return(output)
}
