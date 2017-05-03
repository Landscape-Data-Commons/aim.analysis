#' Create a SpatialPolygonsDataFrame from the intersection of two SpatialPolygonsDataFrames
#'
#' Basically a wrapping of \code{rgeos::gIntersection()} and \code{raster::intersect()} with additional opportunity to call area.add() and automatically added unique identifiers for each resultant polygon
#' @param spdf1 A SpatialPolygonsDataFrame to be intersected
#' @param spdf1.attributefieldname Name of the field in \code{spdf1} unique to the unit groups or units to take values from.
#' @param spdf1.attributefieldname.output Optional string for the name of the field in the output SPDF to duplicate values from spdf1.attributefieldname into. If unspecified, then \code{spdf1.attributefieldname} will be used. Defaults to \code{NULL}.
#' @param spdf2 A SpatialPolygonsDataFrame to be intersected
#' @param spdf2.attributefieldname Name of the field in \code{spdf2} unique to the unit groups or units to take values from.
#' @param spdf2.attributefieldname.output Optional string for the name of the field in the output SPDF to duplicate values from spdf2.attributefieldname into. If unspecified, then \code{spdf2.attributefieldname} will be used. Defaults to \code{NULL}.
#' @param method A string specifying which function to use for the interscting step: either \code{rgeos::gIntersection()} or \code{raster::intersect()}. Valid options are \code{"gintersection"} and \code{"intersect"}. Defaults to \code{"gintersection"}.
#' @param area.ha Logical. If \code{T}, areas will be calculated and added in hectares. Default is \code{T}.
#' @param area.sqkm Logical. If \code{T}, areas will be calculated and added in square kilometers. Default is \code{T}.
#' @param projection An \code{sp::CRS()} argument. The final output will be reprojected using this. Defaults to \code{CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")}
#' @return A SpatialPolygonsDataFrame with the attributes inherited from \code{spdf1} and \code{spdf2}, areas as appropriate, and a unique identifier.
#' @example
#' intersect()

intersect <- function(spdf1, ## A SpatialPolygonsShapefile
                      spdf1.attributefieldname, ## Name of the field in SPDF1 unique to the unit groups or units to take values from
                      spdf1.attributefieldname.output = NULL, ## Optional name of the field in the output SPDF to duplicate values from spdf1.attributefieldname in.
                      spdf2, ## A SpatialPolygonsShapefile
                      spdf2.attributefieldname, ## Name of the field in SPDF2 unique to the unit groups or units to take values from
                      spdf2.attributefieldname.output = NULL,  ## Optional name of the field in the output SPDF to duplicate values from spdf2.attributefieldname in
                      method = "gintersection", ## Which function to use, gIntersection() or intersect()
                      area.ha = T, ## Add fields for area in hectares for individual polygons and the sum of those within unique combinations of the input attribute fields
                      area.sqkm = T, ## Add fields for area in square kilometers for individual polygons and the sum of those within unique combinations of the input attribute fields
                      projection = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs") ## Standard NAD83
){
  ## Sanitization
  if (spdf1@proj4string@projargs != spdf2@proj4string@projargs) {
    ## Make sure that the points also adhere to the same projection
    spdf1 <- spdf1 %>% spTransform(projection)
    spdf2 <- spdf2 %>% spTransform(projection)
  }
  method <- stringr::str_to_upper(method)
  names(spdf1@data) <- stringr::str_to_upper(names(spdf1@data))
  names(spdf2@data) <- stringr::str_to_upper(names(spdf2@data))
  spdf1.attributefieldname <- stringr::str_to_upper(spdf1.attributefieldname)
  spdf2.attributefieldname <- stringr::str_to_upper(spdf2.attributefieldname)
  ## Create new columns that we can drop later. This is in case the field names were the same in both SPDFs
  spdf1@data[, paste0(spdf1.attributefieldname, ".spdf1")] <- spdf1@data[, spdf1.attributefieldname]
  spdf2@data[, paste0(spdf2.attributefieldname, ".spdf2")] <- spdf2@data[, spdf2.attributefieldname]

  switch(method,
         "GINTERSECTION" = {
           intersect.sp.attribute <- rgeos::gIntersection(spdf1,
                                                          spdf2,
                                                          byid = T,
                                                          drop_lower_td = T)

           ## Now we need to build the data frame that goes back into this. It's a pain
           ## Get the rownames from the polygons. This will consist of the two row names from spdf1 and spdf2 separated by a " "
           intersection.rownames <- intersect.sp.attribute %>% row.names() %>% strsplit(split = " ")

           ## Create an empty data frame that we can add the constructed rows to
           intersection.dataframe <- data.frame()
           ## For each of the intersection polygons, create a row with the attributes from the source polygons
           for (row in 1:length(intersection.rownames)) {
             intersection.dataframe <- rbind(intersection.dataframe,
                                             cbind(
                                               spdf1@data[intersection.rownames[[row]][1],],
                                               spdf2@data[intersection.rownames[[row]][2],]
                                             ))
           }
           rownames(intersection.dataframe) <- row.names(intersect.sp.attribute)

           intersect.spdf.attribute <- sp::SpatialPolygonsDataFrame(Sr = intersect.sp.attribute,
                                                                    data = intersection.dataframe)
         },
         "INTERSECT" = {
           current.drop <- get_RGEOS_dropSlivers()
           current.warn <- get_RGEOS_warnSlivers()
           current.tol <- get_RGEOS_polyThreshold()

           ## Find the intersection of the two SPDFs
           intersect.spdf.attribute <- tryCatch(
             expr = {
               raster::intersect(x = spdf1, y = spdf2)
             },
             error = function(e){
               if (grepl(x = e, pattern = "few points in geometry")) {
                 set_RGEOS_dropSlivers(T)
                 set_RGEOS_warnSlivers(T)
                 set_RGEOS_polyThreshold(0.01)
               } else if (grepl(x = e, pattern = "SET_VECTOR_ELT")) {
                 set_RGEOS_dropSlivers(F)
                 set_RGEOS_warnSlivers(F)
                 set_RGEOS_polyThreshold(0)
               }

               raster::intersect(x = spdf1, y = spdf2)
             }
           )

           set_RGEOS_dropSlivers(current.drop)
           set_RGEOS_warnSlivers(current.warn)
           set_RGEOS_polyThreshold(current.tol)
         })


  ## Create a single field to serve as a unique identifier to dissolve the polygons by.
  for (n in 1:nrow(intersect.spdf.attribute@data)) {
    intersect.spdf.attribute@data$UNIQUE.IDENTIFIER[n] <- digest::sha1(x = paste0(intersect.spdf.attribute@data[n, paste0(spdf1.attributefieldname, ".spdf1")],
                                                                          intersect.spdf.attribute@data[n, paste0(spdf2.attributefieldname, ".spdf2")]),
                                                               digits = 14)
  }

  ## Remove those two columns we made that were just duplicates of existing columns but with .spdf1 or .spdf2 appended
  intersect.spdf.attribute@data <- intersect.spdf.attribute@data[, names(intersect.spdf.attribute@data)[!(intersect.spdf.attribute@data %in% c(paste0(spdf1.attributefieldname, ".spdf1"), paste0(spdf2.attributefieldname, ".spdf2")))]]

  ## If we're adding areas then:
  if (area.ha | area.sqkm) {
    ## Add the areas in hectares and square kilometers for each as called for
    intersect.spdf.attribute <- area.add(spdf = intersect.spdf.attribute,
                                         area.ha = area.ha,
                                         area.sqkm = area.sqkm)
    ## Now we summarize() the areas by unique identifier and then merge that with the original data frame and use it to overwrite the original data frame
    ## The arguments to summarize() are specific to what columns exist and what columns are therefore being added, so there are three alternatives
    if (area.ha & area.sqkm) {
      ## When there are both units represented
      ## group_by_() is used instead of group_by() so that we can provide strings as arguments to let us programmatically use the attributefieldname.output values
      intersect.spdf.attribute@data <- group_by(intersect.spdf.attribute@data, UNIQUE.IDENTIFIER) %>%
        summarize(AREA.HA.UNIT.SUM = sum(AREA.HA), AREA.SQKM.UNIT.SUM = sum(AREA.SQKM)) %>%
        merge(x = intersect.spdf.attribute@data, y = .)
    } else if (!(area.ha) & area.sqkm) {
      ## When there's no area.ha
      intersect.spdf.attribute@data <- group_by_(intersect.spdf.attribute@data, UNIQUE.IDENTIFIER) %>%
        summarize(AREA.SQKM.UNIT.SUM = sum(AREA.SQKM)) %>%
        merge(x = intersect.spdf.attribute@data, y = .)
    } else if (area.ha & !(area.sqkm)) {
      ## When there's no area.sqkm
      intersect.spdf.attribute@data <- group_by_(intersect.spdf.attribute@data, UNIQUE.IDENTIFIER) %>%
        summarize(AREA.HA.UNIT.SUM = sum(AREA.HA)) %>%
        merge(x = intersect.spdf.attribute@data, y = .)
    }
  }

  ## Create the fields requested if they're specified
  if (!is.null(spdf1.attributefieldname.output)) {
    intersect.spdf.attribute@data[, spdf1.attributefieldname.output] <- intersect.spdf.attribute@data[, spdf1.attributefieldname]
  }
  if (!is.null(spdf2.attributefieldname.output)) {
    intersect.spdf.attribute@data[, spdf2.attributefieldname.output] <- intersect.spdf.attribute@data[, spdf2.attributefieldname]
  }

  ## Return the final SPDF, making sure to project it into NAD83 (or whatever projection was provided to override the default)
  return(intersect.spdf.attribute %>% spTransform(projection))
}
