#' Attributing a Spatial Data Frame using another Spatial Data Frame
#'
#' This function will take a Spatial Points/Polygons Data Frame and add one attribute fields from a second Spatial Points/Polygons Data Frame
#' @param spdf1 A Spatial Points/Polygons Data Frame containing the geometry to add an attribute to
#' @param spdf2 A Spatial Points/Polygons Data Frame containing the geometry to add an attribute from
#' @param attributefield The name of the field in \code{spdf2} as a string containing the values to add to \code{spdf1}
#' @param newfield The name of the field in \code{spdf1} as a string to add the values from \code{spdf2$attributefield} to. If NULL, the field will use \code{attributefield}. Defaults to NULL.
#' @param projection An \code{sp::CRS()} argument to apply in the event that \code{spdf1} and \code{spdf2} have different projections. Defaults to \code{CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")}
#' @return The original SPDF spdf1 with the new field containing the values inherited from spdf2.
#' @examples
#' attribute.shapefile()
#' @export


attribute.shapefile <- function(spdf1,
                                spdf2,
                                attributefield = NULL,
                                newfield = NULL,
                                projection = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
                                ){
  if (is.null(attributefield) | !(attributefield %in% names(spdf2@data))) {
    stop("attributefield must be a field name found in spdf2")
  }

  if (is.null(newfield)) {
    newfield <- attributefield
  }

  remove.coords <- F
  coord.names <- spdf1@coords %>% colnames()

  if (spdf1@proj4string@projargs != spdf2@proj4string@projargs) {
    ## Make sure that the points also adhere to the same projection
    spdf1 <- spdf1 %>% sp::spTransform(projection)
    spdf2 <- spdf2 %>% sp::spTransform(projection)
  } else {
    projection <- spdf1@proj4string
  }

  ## Initialize list for attributed SPDFs
  attributed.dfs <- list()

  ## We'll check each attribute field value independently
  for (n in unique(spdf2@data[, attributefield])) {
    ## Create a copy of the points to work with on this loop
    current.spdf <- spdf1
    ## Get the data frame from over()
    over.result <- sp::over(current.spdf,
                        spdf2[spdf2@data[, attributefield] == n, ])
    ## Add the values to the newfield column
    current.spdf@data[, newfield] <- over.result[, attributefield]
    if (!(coord.names[1] %in% names(current.spdf@data)) & !(coord.names[2] %in% names(current.spdf@data))){
      current.spdf@data <- cbind(current.spdf@data, current.spdf@coords)
      remove.coords <- T
    }
    ## Make sure that the polygons have unique IDs
    if (class(current.spdf) == "SpatialPolygonsDataFrame") {
      current.spdf <- sp::spChFIDs(current.spdf,
                                 paste(runif(n = 1, min = 0, max = 666666666),
                                       row.names(current.spdf),
                                       sep = "."))
    }
    current.df <- current.spdf@data

    ## Only if the number of coordinates is greater than 0!
    print(nrow(current.df[!is.na(current.df[, newfield]), ]))
    if (nrow(current.df[!is.na(current.df[, newfield]), ]) > 0) {
      attributed.dfs[[paste(n)]] <- current.df[!is.na(current.df[, newfield]), ]
    }
  }

  if (length(attributed.dfs) > 0) {
    if (length(attributed.dfs) == 1) {
      output <- attributed.dfs[[1]] %>%
        sp::SpatialPointsDataFrame(data = .,
                                  coords = .[, coord.names],
                                  proj4string = projection)
    } else {
      output <- dplyr::bind_rows(attributed.dfs) %>%
        sp::SpatialPointsDataFrame(data = .,
                                   coords = .[, coord.names],
                                   proj4string = projection)
    }
    if (remove.coords) {
      output <- output[, names(output)[!(names(output) %in% coord.names)]]
    }
  } else {
    output <- NULL
  }
  return(output)
}
