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
                                projection = sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
){
  if (!grepl(class(spdf1), pattern = "^Spatial((Points)|(Polygons))DataFrame$")) {
    stop("spdf1 must be either a Spatial Points or Spatial Polygons Data Frame")
  }
  if (class(spdf2) != "SpatialPolygonsDataFrame") {
    stop("spdf2 must be a Spatial Polygons Data Frame.")
  }
  if (is.null(attributefield) | !(attributefield %in% names(spdf2@data))) {
    stop("attributefield must be a field name found in spdf2")
  }

  if (any(is.na(spdf2@data[[attributefield]]))) {
    message(paste0("Removing geometry from spdf2 with NA in the field ", attributefield))
    spdf2 <- spdf2[!is.na(spdf2@data[[attributefield]]),]
  }

  if (is.null(newfield)) {
    newfield <- attributefield
  }

  if (newfield %in% names(spdf1@data)) {
    message(paste0("The variable ", newfield, " is already in spdf1 and will be overwritten."))
  }

  remove.coords <- FALSE
  coord.names <- colnames(spdf1@coords)

  if (spdf1@proj4string@projargs != spdf2@proj4string@projargs) {
    ## Make sure that the points also adhere to the same projection
    spdf2 <- sp::spTransform(spdf2, CRSobj = spdf1@proj4string)
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
      remove.coords <- TRUE
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
    print(nrow(current.df[current.df[, newfield] == n, ]))
    if (nrow(current.df[current.df[, newfield] == n, ]) > 0) {
      attributed.dfs[[paste(n)]] <- current.df[current.df[, newfield] == n, ]
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

#' Creating the Intersection of Two Spatial Polygons Data Frames
#' @description This uses \code{rgeos::ginstersect} and assumes that the row names in both \code{spdf1} and \code{spdf2} DO NOT HAVE SPACES.
#' @param spdf1 Spatial polygons data frame. One of the two set of polygons to intersect. If \code{crs} is \code{NULL} and \code{spdf1} and \code{spdf2} have different CRSes, then the CRS from \code{spdf1} will be use to reproject \code{spdf2}.
#' @param spdf2 Spatial polygons data frame.
#' @param crs Optional \code{sp::CRS()} call. Used to reproject both \code{spdf1} and \code{spdf2} using \code{sp::spTransform()}. Defaults to \code{NULL}.
#' @export
rgeos.intersect <- function(spdf1,
                        spdf2,
                        crs = NULL){

  if (class(spdf1)[1] != "SpatialPolygonsDataFrame") {
    stop("spdf1 must be a spatial polygons data frame")
  }
  if (class(spdf2)[1] != "SpatialPolygonsDataFrame") {
    stop("spdf2 must be a spatial polygons data frame")
  }


  if (!is.null(crs)) {
    spdf1 <- sp::spTransform(spdf1, CRSobj = crs)
    spdf2 <- sp::spTransform(spdf2, CRSobj = crs)
  }

  if (spdf1@proj4string@projargs != spdf2@proj4string@projargs) {
    spdf2 <- sp::spTransform(spdf2, CRSobj = spdf1@proj4string)
  }
  # First off, just do the intersect with rgeos::gintersect()
  intersect.sp <- rgeos::gIntersection(spdf1,
                                       spdf2,
                                       byid = TRUE,
                                       drop_lower_td = TRUE)

  ## Now we need to build the data frame that goes back into this. It's a pain
  ## Get the place in the rownames from the polygons where a " " occurs
  intersection.rownames.splitpoints <-  sapply(row.names(intersect.sp),
                                               function(X){gregexpr(X, pattern = " ")[[1]][1]})

  # Usng thse split information, get the rows from the source data that match each and cbind() them into a data frame
  intersection.dataframe <- cbind(spdf1@data[mapply(X = row.names(intersect.sp),
                                                    Y = intersection.rownames.splitpoints,
                                                    rownames = rownames(spdf1@data),
                                                    FUN = function(X, Y, rownames){
                                                      rowname <- substr(X, start = 1, stop = Y-1)
                                                      row.index <- grep(rownames, pattern = paste0("^", rowname, "$"))
                                                      return(row.index)}),],
                                  spdf2@data[mapply(X = row.names(intersect.sp),
                                                    Y = intersection.rownames.splitpoints,
                                                    rownames = rownames(spdf2@data),
                                                    FUN = function(X, Y, rownames){
                                                      rowname <- substr(X, start = Y + 1, stop = nchar(X))
                                                      row.index <- grep(rownames, pattern = paste0("^", rowname, "$"))
                                                      return(row.index)}),])

  # Set the rownames to be the same as the intersection SP
  rownames(intersection.dataframe) <- row.names(intersect.sp)

  # Make it an SPDF
  intersect.spdf <- sp::SpatialPolygonsDataFrame(Sr = intersect.sp,
                                                           data = intersection.dataframe)

  return(intersect.spdf)
}

#' Create a SpatialPolygonsDataFrame from the intersection of two SpatialPolygonsDataFrames
#'
#' Basically a wrapping of \code{rgeos::gIntersection()} and \code{raster::intersect()} with additional opportunity to call add.area() and automatically added unique identifiers for each resultant polygon
#' @param spdf1 A SpatialPolygonsDataFrame to be intersected
#' @param spdf1.attributefieldname Name of the field in \code{spdf1} unique to the unit groups or units to take values from.
#' @param spdf1.attributefieldname.output Optional string for the name of the field in the output SPDF to duplicate values from spdf1.attributefieldname into. If unspecified, then \code{spdf1.attributefieldname} will be used.
#' @param spdf2 A SpatialPolygonsDataFrame to be intersected
#' @param spdf2.attributefieldname Name of the field in \code{spdf2} unique to the unit groups or units to take values from.
#' @param spdf2.attributefieldname.output Optional string for the name of the field in the output SPDF to duplicate values from spdf2.attributefieldname into. If unspecified, then \code{spdf2.attributefieldname} will be used.
#' @param method A string specifying which function to use for the intersecting step: either \code{rgeos::gIntersection()} or \code{raster::intersect()}. Valid options are \code{"rgeos"} which calls \code{rgeos.intersect()} and \code{"raster"}. Defaults to \code{"rgeos"}.
#' @param area.ha Logical. If \code{TRUE}, areas will be calculated and added in hectares. Default is \code{FALSE}.
#' @param area.sqkm Logical. If \code{TRUE}, areas will be calculated and added in square kilometers. Default is \code{FALSE}.
#' @param projection An \code{sp::CRS()} call. The final output will be reprojected using this, unless it's \code{NULL}. Defaults to \code{NULL} (but for many uses you'll want NAD83: \code{CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")}).
#' @return A SpatialPolygonsDataFrame with the attributes inherited from \code{spdf1} and \code{spdf2}, areas as appropriate, and a unique identifier.
#' @examples
#' flex.intersect()
#' @export

flex.intersect <- function(spdf1,
                           spdf1.attributefieldname,
                           spdf1.attributefieldname.output = NULL,
                           spdf2,
                           spdf2.attributefieldname,
                           spdf2.attributefieldname.output = NULL,
                           method = "rgeos",
                           area.ha = FALSE,
                           area.sqkm = FALSE,
                           projection = NULL
){
  ## Sanitization
  if (spdf1@proj4string@projargs != spdf2@proj4string@projargs) {
    ## Make sure that the points also adhere to the same projection
    spdf2 <- sp::spTransform(spdf2, CRSobj = spdf1@proj4string)
  }
  if (!(toupper(method) %in% c("RGEOS", "RASTER"))) {
    stop(paste0("method must be either 'gintersection' or 'intersect' but is currently '", method, "'."))
  }

  names(spdf1@data) <- toupper(names(spdf1@data))
  names(spdf2@data) <- toupper(names(spdf2@data))
  spdf1.attributefieldname <- toupper(spdf1.attributefieldname)
  spdf2.attributefieldname <- toupper(spdf2.attributefieldname)
  ## Create new columns that we can drop later. This is in case the field names were the same in both SPDFs
  spdf1@data[, paste0(spdf1.attributefieldname, ".spdf1")] <- spdf1@data[, spdf1.attributefieldname]
  spdf2@data[, paste0(spdf2.attributefieldname, ".spdf2")] <- spdf2@data[, spdf2.attributefieldname]

  switch(toupper(method),
         "RGEOS" = {
           intersect.spdf.attribute <- rgeos.intersect(spdf1,
                                                       spdf2)
         },
         "RASTER" = {
           current.drop <- rgeos::get_RGEOS_dropSlivers()
           current.warn <- rgeos::get_RGEOS_warnSlivers()
           current.tol <- rgeos::get_RGEOS_polyThreshold()

           ## Find the intersection of the two SPDFs
           intersect.spdf.attribute <- tryCatch(
             expr = {
               raster::intersect(x = spdf1, y = spdf2)
             },
             error = function(e){
               if (grepl(x = e, pattern = "few points in geometry")) {
                 rgeos::set_RGEOS_dropSlivers(TRUE)
                 rgeos::set_RGEOS_warnSlivers(TRUE)
                 rgeos::set_RGEOS_polyThreshold(0.01)
               } else if (grepl(x = e, pattern = "SET_VECTOR_ELT")) {
                 rgeos::set_RGEOS_dropSlivers(FALSE)
                 rgeos::set_RGEOS_warnSlivers(FALSE)
                 rgeos::set_RGEOS_polyThreshold(0)
               }

               raster::intersect(x = spdf1, y = spdf2)
             }
           )

           rgeos::set_RGEOS_dropSlivers(current.drop)
           rgeos::set_RGEOS_warnSlivers(current.warn)
           rgeos::set_RGEOS_polyThreshold(current.tol)
         })


  ## Create a single field to serve as a unique identifier to dissolve the polygons by.
  intersect.spdf.attribute@data$UNIQUE.IDENTIFIER <- runif(n = nrow(intersect.spdf.attribute@data),
                                                           min = 10000000000,
                                                           max = 99999999999)
  ## Just to make sure that they're unique, although the odds of it being otherwise are low
  while(length(unique(intersect.spdf.attribute@data$UNIQUE.IDENTIFIER)) < nrow(intersect.spdf.attribute@data)) {
    intersect.spdf.attribute@data$UNIQUE.IDENTIFIER <- runif(n = nrow(intersect.spdf.attribute@data),
                                                             min = 10000000000,
                                                             max = 99999999999)
  }

  ## Remove those two columns we made that were just duplicates of existing columns but with .spdf1 or .spdf2 appended
  intersect.spdf.attribute@data <- intersect.spdf.attribute@data[, names(intersect.spdf.attribute@data)[!(intersect.spdf.attribute@data %in% c(paste0(spdf1.attributefieldname, ".spdf1"), paste0(spdf2.attributefieldname, ".spdf2")))]]

  ## If we're adding areas then:
  if (area.ha | area.sqkm) {
    ## Add the areas in hectares and square kilometers for each as called for
    intersect.spdf.attribute <- add.area(spdf = intersect.spdf.attribute,
                                         area.ha = area.ha,
                                         area.sqkm = area.sqkm)
    ## Now we summarize() the areas by unique identifier and then merge that with the original data frame and use it to overwrite the original data frame
    ## The arguments to summarize() are specific to what columns exist and what columns are therefore being added, so there are three alternatives
    if (area.ha & area.sqkm) {
      ## When there are both units represented
      intersect.spdf.attribute@data <- merge(x = intersect.spdf.attribute@data,
                                             y = dplyr::summarize(.data = dplyr::group_by(.data = intersect.spdf.attribute@data,
                                                                                          UNIQUE.IDENTIFIER),
                                                                  AREA.HA.UNIT.SUM = sum(AREA.HA),
                                                                  AREA.SQKM.UNIT.SUM = sum(AREA.SQKM)))
    } else if (!(area.ha) & area.sqkm) {
      ## When there's no area.ha
      intersect.spdf.attribute@data <- merge(x = intersect.spdf.attribute@data,
                                             y = dplyr::summarize(.data = dplyr::group_by(intersect.spdf.attribute@data,
                                                                                          UNIQUE.IDENTIFIER),
                                                                  AREA.SQKM.UNIT.SUM = sum(AREA.SQKM)))
    } else if (area.ha & !(area.sqkm)) {
      ## When there's no area.sqkm
      intersect.spdf.attribute@data <- merge(x = intersect.spdf.attribute@data,
                                             y = dplyr::summarize(.data = dplyr::group_by(.data = intersect.spdf.attribute@data,
                                                                                          UNIQUE.IDENTIFIER),
                                                                  AREA.HA.UNIT.SUM = sum(AREA.HA)))
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
  if (is.null(projection)) {
    output <- intersect.spdf.attribute
  } else {
    output <- sp::spTransform(intersect.spdf.attribute, CRSobj = projection)
  }
  return(output)
}


#' Add areas to a Spatial Polygons Data Frame
#'
#' This function takes a Spatial Polygons Data Frame and calculates and adds area fields to the data frame. Areas can be calculated either treating the whole SPDF as a unit or for each polygon individually.
#' @param spdf Spatial Polygons Data Frame to calculate areas for.
#' @param area.ha Logical. If \code{TRUE}, areas will be calculated and added in hectares. Default is \code{TRUE}.
#' @param area.sqkm Logical. If \code{TRUE}, areas will be calculated and added in square kilometers. Default is \code{TRUE}.
#' @param byid Logical. If \code{TRUE}, areas will be calculated and added for each polygon by ID. If \code{FALSE} the area of the whole SPDF will be calculated and added, so every value for that field will be the same, regardless of polygon ID. Default is \code{TRUE}.
#' @return The original Spatial Polygons Data Frame with an additional field for each area unit calculated.
#' @keywords area
#' @examples
#' add.area()
#' @export

add.area <- function(spdf,
                     area.ha = TRUE,
                     area.sqkm = TRUE,
                     byid = TRUE
){
  ## Create a version in Albers Equal Area
  spdf.albers <- sp::spTransform(x = spdf, CRSobj = sp::CRS("+proj=aea"))

  ## Add the area in hectares, stripping the IDs from gArea() output
  spdf@data$AREA.HA <- unname(rgeos::gArea(spdf.albers, byid = byid) * 0.0001)
  ## Add the area in square kilometers, converting from hectares
  spdf@data$AREA.SQKM <- spdf@data$AREA.HA * 0.01

  if (!(area.ha)) {
    spdf@data$AREA.HA <- NULL
  }
  if (!(area.sqkm)) {
    spdf@data$AREA.SQKM <- NULL
  }
  return(spdf)
}



#' Restricting imported TerrADat data to the boundaries of the imported design databases
#' @param dd.raw The output from \code{read.dd()}. The function uses the spatial polygons data frames found in the \code{sf} list
#' @param tdat.spdf The output from \code{read.tdat()}.
#' @return The contents of \code{read.tdat} that overlap spatially with the sample frames in \code{dd.raw}
#' @export
restrict.tdat <- function(dd.raw, tdat.spdf){
  ## NAD83 sp::CRS()
  nad83.prj <- sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
  ## Save this for later
  tdat.prj <- tdat.spdf@proj4string
  ## Create a list to dump restricted SPDFs into
  restrictions <- list()
  ## Restrict!
  for (design in names(dd.raw$sf)) {
    restrictions[[design]] <-sp::spTransform(attribute.shapefile(spdf1 = tdat.spdf,
                                                                 spdf2 = dd.raw$sf[[design]],
                                                                 attributefield = "TERRA_SAMPLE_FRAME_ID",
                                                                 newfield = "SampleFrame"),
                                             CRSobj = nad83.prj)
  }
  ## If there was more than one SPDF
  if (length(restrictions) > 1) {
    ## Prime this with an empty data frame made from an empty subset. Ridiculous but necessary
    tdat.spdf.restricted <- subset(sp::spTransform(tdat.spdf, nad83.prj), "SiteID" == "Nothing")
    ## Mash together all the SPDFs because beautiful dplyr::bind_rows() doesn't work on SPDFs
    for (n in seq_along(restrictions)) {
      tdat.spdf.restricted <- rbind(tdat.spdf.restricted, restrictions[[n]])
    }
    return(tdat.spdf.restricted)
  } else {
    tdat.spdf.restricted <- sp::spTransform(restrictions[[1]], nad83.prj)
  }

  ## Drop that SampleFrame variable
  tdat.spdf.restricted@data <- tdat.spdf.restricted@data[, names(tdat.spdf.restricted@data)[!(names(tdat.spdf.restricted@data) %in% "SampleFrame")]]
  ## Build a new SPDF? I must've done this for a reason?
  tdat.spdf.restricted <- sp::SpatialPointsDataFrame(data = dplyr::distinct(tdat.spdf.restricted@data),
                                                     coords = dplyr::distinct(tdat.spdf.restricted@data)[, c("coords.x1", "coords.x2")],
                                                     proj4string = tdat.prj)
  tdat.spdf.restricted <- sp::spTransform(tdat.spdf.restricted, CRSobj = nad83.prj)
  return(tdat.spdf.restricted)
}

#' Erase geometry from a Spatial Polygons/Points Data Frame using a Spatial Polygons Data Frame with ArcPy
#' @description Spatial manipulations in R can get, for lack of a better term, squirrelly. This removes the geometry of one SPDF from another via a system call to \code{Python} using the library \code{ArcPy}.
#' @param spdf A Spatial Polygons or Points Data Frame to remove FROM.
#' @param spdf.erase  A Spatial Polygons Data Frame to remove geometry from \code{spdf} WITH.
#' @param temp.path Optional character string. This must be the path to a folder that R has write permissions to so that a subfolder called arcpy_temp can be created and used for ArcPy erasure steps. Defaults to the output from a \code{tempdir()} call.
#' @param python.search.path Optional character string. The filepath for the folder containing \code{pythonw.exe}. Defaults to \code{"C:/Python27"}.
#' @return The remaining geometry and data in \code{spdf} after any parts overlapping \code{spdf.erase} has been removed from it.
#' @export
erase.arcpy <- function(spdf,
                        spdf.erase,
                        temp.path = NULL,
                        python.search.path = "C:/Python27"){
  if (class(spdf) != "SpatialPolygonsDataFrame") {
    stop("spdf must be a valid Spatial Polygons Data Frame")
  }
  if (class(spdf.erase) != "SpatialPolygonsDataFrame") {
    stop("spdf.erase must be a valid Spatial Polygons Data Frame")
  }
  if (!file.exists(python.search.path)) {
    stop("python.search.path must be a valid, pre-existing filepath.")
  }
  if (spdf@proj4string@projargs != spdf.erase@proj4string@projargs) {
    spdf.erase <- sp::spTransform(spdf.erase, CRSobj = spdf@proj4string)
  }
  ## Create a temp directory
  if (is.null(temp.path)) {
    temp.path <- tempdir()
    delete <- FALSE
  } else {
    if (!file.exists(temp.path)) {
      stop("temp.path must be a valid, pre-existing filepath.")
    }
    temp.path <- paste0(temp.path, "/arcpy_temp")
    dir.create(temp.path, showWarnings = FALSE)
    delete <- TRUE
  }

  ## Write out the two current frames
  rgdal::writeOGR(obj = spdf, dsn = temp.path, layer = "inshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  rgdal::writeOGR(obj = spdf.erase, dsn = temp.path, layer = "eraseshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)

  ## Construct a quick python script to erase frame.spdf from frame.spdf.temp
  arcpy.script <- c("import arcpy",
                    "from arcpy import env",
                    paste0("env.workspace = '", temp.path, "'"),
                    "in_features = 'inshape.shp'",
                    "erase_features = 'eraseshape.shp'",
                    "out_feature_class = 'eraseresults.shp'",
                    "xy_tolerance = ''",
                    "arcpy.Erase_analysis(in_features, erase_features, out_feature_class)"
  )
  ## Write the constructed script out
  cat(arcpy.script, file = paste0(temp.path, "/erase.py"), sep = "\n", append = F)

  ## Find the local machine's copy of pythonw.exe in C:/Python27. There are no failsafes for if this isn't where to find it
  python.path <- paste0(python.search.path, "/", list.files(path = python.search.path, pattern = "pythonw.exe", recursive = TRUE))
  if (length(python.path) < 1) {
    stop(paste0("Unable to find pythonw.exe in the folder or subfolders of ", python.search.path))
  } else {
    python.path <- python.path[1]
  }

  ## Execute the Python script
  system(paste(python.path, stringr::str_replace_all(paste0(temp.path, "/erase.py"), pattern = "/", replacement = "\\\\")))

  ## Read in the results and rename the attributes because rgdal::writeOGR() truncated them
  erase.results <- rgdal::readOGR(dsn = temp.path, layer = "eraseresults", stringsAsFactors = FALSE)
  names(erase.results@data) <- names(spdf@data)

  if (erase.results@proj4string@projargs != spdf@proj4string@projargs) {
    output <-sp::spTransform(erase.results, CRSobj = spdf@proj4string)
  } else {
    output <- erase.results
  }
  ## Remove the temp folder and files if we didn't use tempdir()
  if (delete) {
    system(paste("cmd /c rmdir", stringr::str_replace_all(temp.path, pattern = "/", replacement = "\\\\"), "/s /q"))
  }
}

#' Erase geometry from a Spatial Polygons/Points Data Frame using a Spatial Polygons Data Frame with rgeos
#' @param spdf A Spatial Polygons Data Frame to remove FROM.
#' @param spdf.erase  A Spatial Polygons Data Frame to remove geometry from \code{spdf} WITH.
#' @param sliverdrop Optional logical value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_dropSlivers()} to temporarily set the environment during the erasure attempt. Defaults to \code{TRUE}.
#' @param sliverwarn Optional logical value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_warnSlivers()} to temporarily set the environment during the erasure attempt. Defaults to \code{TRUE}.
#' @param sliverdrop Optional numeric value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_polyThreshold()} to temporarily set the environment during the erasure attempt. Defaults to \code{0.01}.
#' @return The remaining geometry and data in \code{spdf} after any parts overlapping \code{spdf.erase} has been removed from it.
#' @export
erase.rgeos <- function(spdf,
                        spdf.erase,
                        sliverdrop = TRUE,
                        sliverwarn = TRUE,
                        sliverthreshold = 0.01){
  if (spdf@proj4string@projargs != spdf.erase@proj4string@projargs) {
    spdf.erase <- sp::spTransform(spdf.erase, CRSobj = spdf@proj4string)
  }

  current.drop <- rgeos::get_RGEOS_dropSlivers()
  current.warn <- rgeos::get_RGEOS_warnSlivers()
  current.tol <- rgeos::get_RGEOS_polyThreshold()

  rgeos::set_RGEOS_dropSlivers(sliverdrop)
  rgeos::set_RGEOS_warnSlivers(sliverwarn)
  rgeos::set_RGEOS_polyThreshold(sliverthreshold)
  message(paste0("Attempting using rgeos::set_RGEOS_dropslivers(", sliverdrop, ") and rgeos::set_RGEOS_warnslivers(", sliverwarn, ") and set_REGOS_polyThreshold(", sliverthreshold, ")"))
  ## Making this Albers for right now for gBuffer()
  ## The gbuffer() is a common hack to deal with ring self-intersections, which it seems to do just fine here?
  sp.temp <- rgeos::gDifference(spgeom1 = rgeos::gBuffer(sp::spTransform(spdf, sp::CRS("+proj=aea")),
                                                         byid = TRUE,
                                                         width = 0.1),
                                spgeom2 = rgeos::gBuffer(sp::spTransform(spdf.erase,
                                                                         sp::CRS("+proj=aea")),
                                                         byid = TRUE,
                                                         width = 0.1),
                                drop_lower_td = TRUE)

  rgeos::set_RGEOS_dropSlivers(current.drop)
  rgeos::set_RGEOS_warnSlivers(current.warn)
  rgeos::set_RGEOS_polyThreshold(current.tol)

  if (!is.null(frame.sp.temp)) {
    output <- sp::spTransform(sp::SpatialPolygonsDataFrame(sp.temp,
                                                           data = spdf@data[1:length(sp.temp@polygons),]),
                              CRSobj = spdf@proj4string)
  } else {
    output <- NULL
  }

  return(output)
}

#' Erase a Spatial Polygons Data Frame from another, either with rgeos or ArcPy
#' @description Spatial manipulations in R can get, for lack of a better term, squirrelly. Even with multiple failsafes in place to try to compensate for sliver geometry, \code{rgeos::gDifference()} still isn't as robust as one might hope. This wrapper removes the geometry of one SPDF from another via a system call to \code{Python} using the library \code{ArcPy} or via \code{rgeos::gDifference()}.
#' @param spdf A Spatial Polygons Data Frame to remove FROM.
#' @param spdf.erase  A Spatial Polygons Data Frame to remove geometry from \code{spdf} WITH.
#' @param method Character string. This must either be \code{"arcpy"} or \code{"rgeos"} and determines which approach will be used to frames from one another if \code{combine} is \code{TRUE}. If \code{"arcpy"} is used, then R must have write permissions to the folder \code{temp.path} and a valid install of ArcPy. This is preferable to \code{"rgeos"} because the functions involved tend to crash at random when handling very small remainder geometries. Case insensitive. Defaults to \code{"arcpy"}.
#' @param temp.path Optional character string. If \code{erase} is \code{"arcpy"} this must be the path to a folder that R has write permissions to so that a subfolder called arcpy_temp can be created and used for ArcPy erasure steps. Defaults to the result of a \code{tempdir()} call.
#' @param python.search.path Character string. The filepath for the folder containing \code{pythonw.exe}. Defaults to \code{"C:/Python27"}.
#' @param sliverdrop Optional logical value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_dropSlivers()} to temporarily set the environment during the erasure attempt. Defaults to \code{TRUE}.
#' @param sliverwarn Optional logical value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_warnSlivers()} to temporarily set the environment during the erasure attempt. Defaults to \code{TRUE}.
#' @param sliverdrop Optional numeric value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_polyThreshold()} to temporarily set the environment during the erasure attempt. Defaults to \code{0.01}.
#' @return The remaining geometry and data in \code{spdf} after any parts overlapping \code{spdf.erase} has been removed from it.
#' @export

flex.erase <- function(spdf,
                       spdf.erase,
                       method = "arcpy",
                       temp.path = NULL,
                       python.search.path = "C:/Python27",
                       sliverdrop = TRUE,
                       sliverwarn = TRUE,
                       sliverthreshold = 0.01
){
  if (class(spdf) != "SpatialPolygonsDataFrame") {
    stop("spdf must be a valid Spatial Polygons Data Frame")
  }
  if (class(spdf.erase) != "SpatialPolygonsDataFrame") {
    stop("spdf.erase must be a valid Spatial Polygons Data Frame")
  }
  if (!(tolower(method) %in% c("arcpy", "rgeos"))) {
    stop("method must be either 'arcpy' or 'rgeos'.")
  }
  if (!file.exists(python.search.path)) {
    stop("python.search.path must be a valid, pre-existing filepath.")
  }
  if (!file.exists(temp.path)) {
    stop("temp.path must be a valid, pre-existing filepath.")
  }

  switch(tolower(method),
         "arcpy" = {
           output <- erase.arcpy(spdf = spdf,
                                 spdf.erase = spdf.erase)},
         "rgeos" = {
           output <- erase.rgeos(spdf = spdf,
                                 spdf.erase = spdf.erase,
                                 sliverdrop = sliverdrop,
                                 sliverwarn = sliverwarn,
                                 sliverthreshold = sliverthreshold)
         }
  )
  return(output)
}

#' Clip a Spatial Polygon/Points Data Frame to a Spatial Polygons Data Frame with ArcPy
#' @param spdf A Spatial Point or Spatial Polygons Data Frame to be clipped
#' @param spdf.clip A Spatial Polygons Data Frame to clip \code{spdf} by
#' @param temp.path Optional character string. A pre-existing filepath to use as a temporary folder to write files to. Defaults to a temporary directory from \code{tempdir()}.
#' @param python.search.path Optional character string. The filepath to search for pythonw.exe in. Defaults to "C:/Python27".
#' @return The remaining geometry and data in \code{spdf} after being restricted to only overlap with \code{spdf.clip}.
#' @export
clip.arcpy <- function(spdf,
                       spdf.clip,
                       temp.path = NULL,
                       python.search.path = "C:/Python27"
){
  if (!(class(spdf) %in% c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame"))) {
    stop("spdf must be a valid Spatial Polygons or Spatial Points Data Frame")
  }
  if (class(spdf.clip) != "SpatialPolygonsDataFrame") {
    stop("spdf.clip must be a valid Spatial Polygons Data Frame")
  }
  if (!file.exists(python.search.path)) {
    stop("python.search.path must be a valid, pre-existing filepath.")
  }
  ## Create a temp directory
  if (is.null(temp.path)) {
    temp.path <- tempdir()
    delete <- FALSE
  } else {
    if (!file.exists(temp.path)) {
      stop("temp.path must be a valid, pre-existing filepath.")
    }
    temp.path <- paste0(temp.path, "/arcpy_temp")
    dir.create(temp.path, showWarnings = FALSE)
    delete <- TRUE
  }

  # Conform the clipping frame to the SPDF to be clipped
  if (spdf@proj4string@projargs != spdf.clip@proj4string@projargs) {
    spdf.clip <- sp::spTransform(spdf.clip, CRSobj = spdf@proj4string)
  }

  ## Write out the two current frames
  rgdal::writeOGR(obj = spdf,
                  dsn = temp.path,
                  layer = "inshape",
                  driver = "ESRI Shapefile",
                  overwrite_layer = TRUE)
  rgdal::writeOGR(obj = spdf.clip,
                  dsn = temp.directory,
                  layer = "clipshape",
                  driver = "ESRI Shapefile",
                  overwrite_layer = TRUE)

  ## Construct a quick python script to clip spdf by spdf.clip
  arcpy.script <- c("import arcpy",
                    "from arcpy import env",
                    paste0("env.workspace = '", temp.path, "'"),
                    "in_features = 'inshape.shp'",
                    "clip_features = 'clipshape.shp'",
                    "out_feature_class = 'clipresults.shp'",
                    "xy_tolerance = ''",
                    "arcpy.Clip_analysis(in_features, clip_features, out_feature_class,xy_tolerance)"
  )


  ## Write the constructed script out
  cat(arcpy.script, file = paste0(temp.path, "/clip.py"), sep = "\n", append = FALSE)

  ## Find the local machine's copy of pythonw.exe in python.search.path. There are no failsafes for if this isn't where to find it
  python.path <- paste0(python.search.path, "/", list.files(path = python.search.path, pattern = "pythonw.exe", recursive = TRUE))
  if (length(python.path) < 1) {
    stop(paste0("Unable to find pythonw.exe in the folder or subfolders of ", python.search.path))
  } else {
    python.path <- python.path[1]
  }

  ## Execute the Python script
  system(paste(python.path, stringr::str_replace_all(paste0(temp.directory, "/clip.py"), pattern = "/", replacement = "\\\\")))

  ## Read in the results and rename the attributes because rgdal::writeOGR() almost certainly truncated them
  clip.results <- rgdal::readOGR(dsn = temp.directory, layer = "clipresults", stringsAsFactors = FALSE)
  names(clip.results@data) <- names(spdf@data)

  ## Make sure that the results are in the same projection as the original source SPDF
  if (clip.results@proj4string@projargs != spdf@proj4string@projargs) {
    output <-sp::spTransform(clip.results, CRSobj = spdf@proj4string)
  } else {
    output <- clip.results
  }

  ## Remove the temp folder and files if we didn't use tempdir()
  if (delete) {
    system(paste("cmd /c rmdir", stringr::str_replace_all(temp.path, pattern = "/", replacement = "\\\\"), "/s /q"))
  }

  return(output)
}

#' Find the intersection of two Spatial Polygon/Points Data Frames with ArcPy
#' @param spdf A Spatial Polygons Data Frame
#' @param spdf.intersect A Spatial Polygons Data Frame to intersect with \code{spdf}.
#' @param temp.path Optional character string. A pre-existing filepath to use as a temporary folder to write files to. Defaults to a temporary directory from \code{tempdir()}.
#' @param python.search.path Optional character string. The filepath to search for pythonw.exe in. Defaults to "C:/Python27".
#' @export
intersect.arcpy <- function(spdf,
                            spdf.intersect,
                            temp.path = NULL,
                            python.search.path = "C:/Python27"
){
  if (!(class(spdf) %in% c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame"))) {
    stop("spdf must be a valid Spatial Polygons or Spatial Points Data Frame")
  }
  if (class(spdf.intersect) != "SpatialPolygonsDataFrame") {
    stop("spdf.intersect must be a valid Spatial Polygons Data Frame")
  }
  if (!file.exists(python.search.path)) {
    stop("python.search.path must be a valid, pre-existing filepath.")
  }
  ## Create a temp directory
  if (is.null(temp.path)) {
    temp.path <- tempdir()
    delete <- FALSE
  } else {
    if (!file.exists(temp.path)) {
      stop("temp.path must be a valid, pre-existing filepath.")
    }
    temp.path <- paste0(temp.path, "/arcpy_temp")
    dir.create(temp.path, showWarnings = FALSE)
    delete <- TRUE
  }

  # Conform spdf.intersect to spdf
  if (spdf@proj4string@projargs != spdf.intersect@proj4string@projargs) {
    spdf.intersect <- sp::spTransform(spdf.intersect, CRSobj = spdf@proj4string)
  }

  ## Write out the two current frames
  rgdal::writeOGR(obj = spdf,
                  dsn = temp.path,
                  layer = "inshape",
                  driver = "ESRI Shapefile",
                  overwrite_layer = TRUE)
  rgdal::writeOGR(obj = spdf.intersect,
                  dsn = temp.path,
                  layer = "intersectshape",
                  driver = "ESRI Shapefile",
                  overwrite_layer = TRUE)

  ## Construct a quick python script to intersect spdf with spdf.intersect
  arcpy.script <- c("import arcpy",
                    "from arcpy import env",
                    paste0("env.workspace = '", temp.path, "'"),
                    "infeatures = ['inshape.shp', 'intersectshape.shp']",
                    "intersectOutput = 'intersectresults.shp'",
                    "clusterTolerance = 1.5",
                    "arcpy.Intersect_analysis(infeatures, intersectOutput, '', '', 'INPUT')"
  )


  ## Write the constructed script out
  cat(arcpy.script, file = paste0(temp.path, "/intersect.py"), sep = "\n", append = FALSE)

  ## Find the local machine's copy of pythonw.exe in python.search.path. There are no failsafes for if this isn't where to find it
  python.path <- paste0(python.search.path, "/", list.files(path = python.search.path, pattern = "pythonw.exe", recursive = TRUE))
  if (length(python.path) < 1) {
    stop(paste0("Unable to find pythonw.exe in the folder or subfolders of ", python.search.path))
  } else {
    python.path <- python.path[1]
  }

  ## Execute the Python script
  system(paste(python.path, stringr::str_replace_all(paste0(temp.path, "/intersect.py"), pattern = "/", replacement = "\\\\")))

  ## Read in the results and rename the attributes because rgdal::writeOGR() almost certainly truncated them
  intersect.results <- rgdal::readOGR(dsn = temp.path, layer = "intersectresults", stringsAsFactors = FALSE)
  names(intersect.results@data) <- c("FID1", names(spdf@data), "FID2", names(spdf.intersect@data))

  ## Make sure that the results are in the same projection as the original source SPDF
  if (intersect.results@proj4string@projargs != spdf@proj4string@projargs) {
    output <-sp::spTransform(intersect.results, CRSobj = spdf@proj4string)
  } else {
    output <- intersect.results
  }

  ## Remove the temp folder and files if we didn't use tempdir()
  if (delete) {
    system(paste("cmd /c rmdir", stringr::str_replace_all(temp.path, pattern = "/", replacement = "\\\\"), "/s /q"))
  }

  return(output)
}
