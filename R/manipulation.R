#' Attributing a spatial data frame using another spatial data frame
#'
#' This function will take a SpatialPoints/PolygonsDataFrame and add one attribute fields from a second SpatialPoints/PolygonsDataFrame
#' @param spdf1 A SpatialPoints/PolygonsDataFrame containing the geometry to add an attribute to
#' @param spdf2 A SpatialPoints/PolygonsDataFrame containing the geometry to add an attribute from
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
#' @examples
#' intersect()
#' @export

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


#' Add areas to a Spatial Polygons Data Frame
#'
#' This function takes a Spatial Polygons Data Frame and calculates and adds area fields to the data frame. Areas can be calculated either treating the whole SPDF as a unit or for each polygon individually.
#' @param spdf Spatial Polygons Data Frame to calculate areas for
#' @param area.ha Logical. If \code{T}, areas will be calculated and added in hectares. Default is \code{T}.
#' @param area.sqkm Logical. If \code{T}, areas will be calculated and added in square kilometers. Default is \code{T}.
#' @param byid Logical. If \code{T}, areas will be calculated and added for each polygon by ID. If \code{F} the area of the whole SPDF will be calculated and added, so every value for that field will be the same, regardless of polygon ID. Default is \code{T}.
#' @return The original Spatial Polygons Data Frame with an additional field for each area unit calculated.
#' @keywords area
#' @examples
#' area.add()
#' @export

area.add <- function(spdf, ## SpatialPolygonsDataFrame to add area values to
                     area.ha = T, ## Add area in hectares?
                     area.sqkm = T, ## Add area in square kilometers?
                     byid = T ## Do it for the whole SPDF or on a per-polygon basis? Generally don't want to toggle this
){
  original.proj <- spdf@proj4string
  ## Make sure the SPDF is in Albers equal area projection
  spdf <- sp::spTransform(x = spdf, CRSobj = CRS("+proj=aea"))

  ## Add the area in hectares, stripping the IDs from gArea() output
  spdf@data$AREA.HA <- rgeos::gArea(spdf, byid = byid) * 0.0001 %>% unname()
  ## Add the area in square kilometers, converting from hectares
  spdf@data$AREA.SQKM <- spdf@data$AREA.HA * 0.01

  if (!(area.ha)) {
    spdf@data$AREA.HA <- NULL
  }
  if (!(area.sqkm)) {
    spdf@data$AREA.SQKM <- NULL
  }
  return(sp::spTransform(spdf, original.proj))
}



#' Restricting imported TerrADat data to the boundaries of the imported design databases
#' @export
restrict.tdat <- function(dd.raw, tdat.spdf){
  ## NAD83 sp::CRS()
  nad83.prj <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
  tdat.prj <- tdat.spdf@proj4string
  for (design in names(dd.raw$sf)) {
    assign(value = attribute.shapefile(spdf1 = tdat.spdf,
                                       spdf2 = dd.raw$sf[[design]],
                                       attributefield = "TERRA_SAMPLE_FRAME_ID",
                                       newfield = "SampleFrame")  %>% spTransform(nad83.prj),
           x = paste0("temp", ".", grep(x = names(dd.raw$sf), pattern = paste0("^", design, "$")))
    )
  }
  if (length(ls()[grepl(x = ls(), pattern = "^temp.\\d{1,3}$")]) > 1) {
    tdat.spdf.restricted <- subset(tdat.spdf %>% spTransform(nad83.prj), "SiteID" == "Nothing")
    for (spdf in ls()[grepl(x = ls(), pattern = "^temp.\\d{1,3}$")]) {
      tdat.spdf.restricted <- rbind(tdat.spdf.restricted, get(spdf))
    }
  } else {
    tdat.spdf.restricted <- get(ls()[grepl(x = ls(), pattern = "^temp.\\d{1,3}$")]) %>% spTransform(nad83.prj)
  }
  tdat.spdf.restricted@data <- tdat.spdf.restricted@data[, names(tdat.spdf.restricted@data)[!(names(tdat.spdf.restricted@data) %in% "SampleFrame")]]
  tdat.spdf.restricted <- tdat.spdf.restricted@data %>% dplyr::distinct() %>%
    SpatialPointsDataFrame(data = .,
                           coords = .[, c("coords.x1", "coords.x2")],
                           proj4string = tdat.prj)
  tdat.spdf.restricted <- tdat.spdf.restricted %>% spTransform(nad83.prj)
  return(tdat.spdf.restricted)
}

#' Adding coordinate variables to the data frame of a SpatialPointsDataFrame
#'
#' @description Adds one or more of the following: the coordinates from the current coordinate refrence system; coordinates in NAD83; and coordinates in Albers Equal Area. This does not change the projection of the SPDF.
#' @param spdf A SpatialPointsDataFrame to add the coordinates to
#' @param current.proj Logical. If \code{T} Then the columns \code{coords.x1} and \code{coords.x2} will be added using the current projection. Defaults to \code{T}.
#' @param xynames Optional vector of two character strings to rename the coordinate variables from the current projection. Format is \code{c("replacement for coords.x1", "replacement for coords.x2")}.
#' @param nad83 Logical. If \code{T} Then the columns \code{LONGITUDE.NAD83} and \code{LATITUDE.NAD83} will be added using NAD83. Defaults to \code{F}.
#' @param albers Logical. If \code{T} Then the columns \code{X.METERS.AL} and \code{Y.METERS.AL} will be added using Albers Equal Area. Defaults to \code{F}.
#' @export
add.coords <- function(spdf,
                       current.proj = T,
                       xynames = NULL,
                       nad83 = F,
                       albers = F){
  projNAD83 <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
  projAL <- CRS("+proj=aea")
  if (current.proj) {
    coords <- spdf@coords %>% as.data.frame()
    if(!is.null(xynames)) {
      names(coords) <- xynames
    }
    spdf@data <- cbind(spdf@data, coords)
  }
  if (nad83) {
    coords <- spdf %>% spTransform(projNAD83) %>% .@coords
    names(coords) <- c("LONGITUDE.NAD83", "LATITUDE.NAD83")
    spdf@data <- cbind(spdf@data, coords)
  }
  if (albers) {
    coords <- spdf %>% spTransform(projAL) %>% .@coords
    names(coords) <- c("X.METERS.AL", "Y.METERS.AL")
    spdf@data <- cbind(spdf@data, coords)
  }
  return(spdf)
}

#' Adding sampling years based on date of visit and panel
#'
#' @description For the given SPDF, this adds the year extracted from the values in PANEL, then overwrites that with the year extracted from DT_VST if possible, then for all remaining observations still missing a YEAR value it makes a best guess based on the YEAR values in the other observations with the same PANEL value.
#' @param pts The SPDF to add YEAR to.
add.dates <- function(pts){
  if (!("PANEL" %in% names(pts))) {
    stop("Variable 'PANEL' is missing from data frame.")
  }
  ## Check to see if the panel names contain the intended year (either at the beginning or end of the panel name) and use those to populate the YEAR
  pts$YEAR[grepl(x = pts$PANEL, pattern = "\\d{4}$")] <- pts$PANEL %>%
    str_extract(string = ., pattern = "\\d{4}$") %>% na.omit() %>% as.numeric()
  pts$YEAR[grepl(x = pts$PANEL, pattern = "^\\d{4}")] <- pts$PANEL %>%
    str_extract(string = ., pattern = "^\\d{4}") %>% na.omit() %>% as.numeric()

  ## Use the sampling date if we can. This obviously only works for points that were sampled. It overwrites an existing YEAR value from the panel name if it exists
  pts$YEAR[!is.na(pts$DT_VST)] <- pts$DT_VST[!is.na(pts$DT_VST)]%>% lubridate::as_date() %>% format("%Y") %>% as.numeric()

  ## For some extremely mysterious reasons, sometimes there are duplicate fields here. This will remove them
  if (length(grep(x = names(pts), pattern = ".1$")) > 0) {
    pts <- pts[, (1:length(names(pts)))] %>% dplyr::select(-ends_with(match = ".1"))
  }


  ## To create a lookup table in the case that we're working solely from sampling dates. Let's get the most common sampling year for each panel
  if (grepl(class(pts)[[1]], pattern = "^Spatial")) {
    panel.years <- pts@data %>% dplyr::group_by(PANEL) %>%
      dplyr::summarize(YEAR = names(sort(summary(as.factor(YEAR)), decreasing = T)[1]))
  } else {
    panel.years <- pts %>% dplyr::group_by(PANEL) %>%
      dplyr::summarize(YEAR = names(sort(summary(as.factor(YEAR)), decreasing = T)[1]))
  }


  ## If we still have points without dates at this juncture, we can use that lookup table to make a good guess at what year they belong to
  for (p in panel.years$PANEL) {
    pts$YEAR[is.na(pts$YEAR) & pts$PANEL == p] <- panel.years$YEAR[panel.years$PANEL == p]
  }

  pts$YEAR <- pts$YEAR %>% as.numeric()
  return(pts)
}

