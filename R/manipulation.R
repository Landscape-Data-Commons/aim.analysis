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
  if (!grepl(class(spdf1), pattern = "^Spatial((Points)|(Polygons))DataFrame$")) {
    stop("spdf1 must be either a Spatial Points or Spatial Polygons Data Frame")
  }
  if (class(spdf2) != "SpatialPolygonsDataFrame") {
    stop("spdf2 must be a Spatial Polygons Data Frame.")
  }
  if (is.null(attributefield) | !(attributefield %in% names(spdf2@data))) {
    stop("attributefield must be a field name found in spdf2")
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
#' @param area.ha Logical. If \code{TRUE}, areas will be calculated and added in hectares. Default is \code{TRUE}.
#' @param area.sqkm Logical. If \code{TRUE}, areas will be calculated and added in square kilometers. Default is \code{TRUE}.
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
                      area.ha = TRUE, ## Add fields for area in hectares for individual polygons and the sum of those within unique combinations of the input attribute fields
                      area.sqkm = TRUE, ## Add fields for area in square kilometers for individual polygons and the sum of those within unique combinations of the input attribute fields
                      projection = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs") ## Standard NAD83
){
  ## Sanitization
  if (spdf1@proj4string@projargs != spdf2@proj4string@projargs) {
    ## Make sure that the points also adhere to the same projection
    spdf2 <- spTransform(spdf2, CRSobj = spdf1@proj4string)
  }
  if (!(stringr::str_to_upper(method) %in% c("GINTERSECTION", "INTERSECT"))) {
    stop(paste0("method must be either 'gintersection' or 'intersect' but is currently '", method, "'."))
  }

  names(spdf1@data) <- stringr::str_to_upper(names(spdf1@data))
  names(spdf2@data) <- stringr::str_to_upper(names(spdf2@data))
  spdf1.attributefieldname <- stringr::str_to_upper(spdf1.attributefieldname)
  spdf2.attributefieldname <- stringr::str_to_upper(spdf2.attributefieldname)
  ## Create new columns that we can drop later. This is in case the field names were the same in both SPDFs
  spdf1@data[, paste0(spdf1.attributefieldname, ".spdf1")] <- spdf1@data[, spdf1.attributefieldname]
  spdf2@data[, paste0(spdf2.attributefieldname, ".spdf2")] <- spdf2@data[, spdf2.attributefieldname]

  switch(stringr::str_to_upper(method),
         "GINTERSECTION" = {
           intersect.sp.attribute <- rgeos::gIntersection(spdf1,
                                                          spdf2,
                                                          byid = TRUE,
                                                          drop_lower_td = TRUE)

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
  return(intersect.spdf.attribute %>% spTransform(projection))
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
#' area.add()
#' @export

area.add <- function(spdf, ## SpatialPolygonsDataFrame to add area values to
                     area.ha = TRUE, ## Add area in hectares?
                     area.sqkm = TRUE, ## Add area in square kilometers?
                     byid = TRUE ## Do it for the whole SPDF or on a per-polygon basis? Generally don't want to toggle this
){
  ## Create a version in Albers Equal Area
  spdf.albers <- sp::spTransform(x = spdf, CRSobj = CRS("+proj=aea"))

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
#' @export
restrict.tdat <- function(dd.raw, tdat.spdf){
  ## NAD83 sp::CRS()
  nad83.prj <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
  ## Save this for later
  tdat.prj <- tdat.spdf@proj4string
  ## Create a list to dump restricted SPDFs into
  restrictions <- list()
  ## Restrict!
  for (design in names(dd.raw$sf)) {
    restrictions[[design]] <- spTransform(attribute.shapefile(spdf1 = tdat.spdf,
                                                              spdf2 = dd.raw$sf[[design]],
                                                              attributefield = "TERRA_SAMPLE_FRAME_ID",
                                                              newfield = "SampleFrame"),
                                          CRSobj = nad83.prj)
  }
  ## If there was more than one SPDF
  if (length(restrictions) > 1) {
    ## Prime this with an empty data frame made from an empty subset. Ridiculous but necessary
    tdat.spdf.restricted <- subset(sp::spTransform(tdat.spdf, nad83.prj), "SiteID" == "Nothing")
    ## Mash together all the SPDFs
    tdat.spdf.restricted <- dplyr::bind_rows(restrictions)
  } else {
    tdat.spdf.restricted <- spTransform(restrictions[[1]], nad83.prj)
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
add.coords <- function(spdf,
                       current.proj = TRUE,
                       xynames = NULL,
                       nad83 = FALSE,
                       albers = FALSE){
  projNAD83 <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
  projAL <- CRS("+proj=aea")
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

#' Adding sampling years based on date of visit and panel
#'
#' @description For the given SPDF, this adds the year extracted from the values in \code{source.field}, then overwrites that with the year extracted from \code{date.field} if possible, then for all remaining observations still missing a YEAR value it makes a best guess based on the YEAR values in the other observations with the same \code{source.field} value. Note that strings in \code{source.field} must begin or end with a four-digit year in order for this to work.
#' @param pts The SPDF or data frame to add the field \code{YEAR} to.
#' @param date.field Character string. The name of the field in \code{pts} that contains the dates to reference.
#' @param source.field Character string. The name of the field in \code{pts} that contains the strings starting or ending with the year.
#' @export

year.add <- function(pts,
                     date.field = NULL,
                     source.field = NULL){
  if (!(class(pts) != "data.frame") & grepl(class(pts),  pattern = "^Spatial.{4,15}DataFrame$")) {
    stop("pts must either be a data frame or a spatial points data frame.")
  }
  if (grepl(class(pts), pattern = "^Spatial.{4,15}DataFrame$")){
    pts.df <- pts@data
  } else {
    pts.df <- pts
  }
  if (is.null(date.field) & is.null(source.field)) {
    stop("At least one of either date.field and source.field must be a string corresponding to a variable name in pts.")
  }
  if (!is.null(date.field)) {
    if (!(date.field %in% names(pts.df))) {
      stop(paste0("Variable ", date.field, " is missing from pts"))
    }
  }
  if (!is.null(source.field)) {
    if (!(source.field %in% names(pts.df))) {
      stop(paste0("Variable ", source.field, " is missing from pts"))
    }
  }

  ## Check to see if the values in source.field contain the intended year (either at the beginning or end of the panel name) and use those to populate the YEAR
  if (!is.null(source.field)) {
    if (any(grepl(x = pts.df[[source.field]], pattern = "\\d{4}$"))) {
      pts.df$YEAR[grepl(x = pts.df[[source.field]], pattern = "\\d{4}$")] <- stringr::str_extract(string = pts.df[[source.field]],
                                                                                                  pattern = "\\d{4}$") %>% na.omit() %>% as.numeric()
    }
    if (any(grepl(x = pts.df[[source.field]], pattern = "^\\d{4}"))) {
      pts.df$YEAR[grepl(x = pts.df[[source.field]], pattern = "^\\d{4}")] <- stringr::str_extract(string = pts.df[[source.field]],
                                                                                                  pattern = "^\\d{4}") %>% na.omit() %>% as.numeric()
    }
  }

  ## Use the sampling date if we can. This obviously only works for points that were sampled. It overwrites an existing YEAR value from the panel name if it exists
  if (!is.null(date.field)) {
    pts.df$YEAR[!is.na(pts.df[[date.field]])] <- lubridate::as_date(pts.df[[date.field]][!is.na(pts.df[[date.field]])]) %>% format("%Y") %>% as.numeric()
  }

  ## For some extremely mysterious reasons, sometimes there are duplicate fields here. This will remove them
  if (length(grep(x = names(pts.df), pattern = ".1$")) > 0) {
    pts.df <- pts.df[, (1:length(names(pts.df)))] %>% dplyr::select(-ends_with(match = ".1"))
  }


  ## To create a lookup table in the case that we're working solely from sampling dates. Let's get the most common sampling year for each panel
  panel.years <- dplyr::group_by_(.data = pts.df,
                                  source.field) %>% dplyr::summarize(YEAR = names(sort(summary(as.factor(YEAR)),
                                                                                       decreasing = TRUE)[1]))

  ## If we still have points without dates at this juncture, we can use that lookup table to make a good guess at what year they belong to
  for (p in panel.years[[source.field]]) {
    pts.df$YEAR[is.na(pts.df$YEAR) & pts.df$PANEL == p] <- panel.years$YEAR[panel.years[[source.field]] == p]
  }

  pts$YEAR <- as.numeric(pts.df$YEAR)
  return(pts)
}

#' Apply project tracking Excel files to imported Design Databases.
#' @description Imports plot tracking worksheets used during an AIM project and uses them to assigns statuses to imported Design Databases.
#' @return Returns \code{dd.list} with sampling dates and statuses added from the plot tracking Excel files.
#' @param filenames A character vector of the filenames (including extension) of the project tracking Excel files to import. If not using the \code{path} argument, the filename should include the entire filepath.
#' @param path Optional string specifying a common filepath containing the project tracking sheets to read in. This will be prepended to the values in \code{filenames}. If the tracking sheets are in different folder paths, do not provide this.
#' @param dd.list Output from \code{read.dd()}.
#' @param dd.names An optional character string vector of Design Database names from \code{dd.list} to compare against the plot tracking Excel files. If not provided, all of the Design Databases represented in \code{dd.list} will be compared and updated.
#' @param tdat Output from \code{read.tdat()}.
#' @param target.values Character string or character vector. This defines what values in the point fate field count as target points. The function always looks for "Target Sampled" and "TS", so this argument is only necessary if there are additional values in the sample design databases. This is case insensitive
#' @param deleteoverdraw Logical. If \code{TRUE} then unsampled overdraw points will be dropped. Defaults to \code{TRUE}.
#' @export
apply.tracking <- function(filenames,
                           path = "",
                           dd.list,
                           dd.names = c(""),
                           tdat,
                           target.values = c("Target Sampled",
                                             "TS"),
                           deleteoverdraw = T
) {

  ## If the provided TerrADat is an SPDF, just take the data frame
  if (class(tdat) == "SpatialPointsDataFrame") {
    tdat <- tdat@data
  }
  names(tdat) <- stringr::str_to_upper(names(tdat))

  ## Add the ending / to a filepath if provided
  if (path != "") {
    path <- paste0(path, "/")
  }

  ## Specify that all the DD names will be operated on if none are provided
  if (dd.names == "") {
    dd.names <- names(dd.list)
  }

  ## Sanitize and add the target values
  target.values <- c(target.values,
                     "Target Sampled",
                     "TS") %>% stringr::str_to_upper() %>% unique()

  ## Read in the plot tracking Excel files, renaming variables, and restricting to needed variables and combine them
  tracking <- lapply(filenames,
                     FUN = function(X, path){
                       read.tracking(filename = X, path = path) %>%
                         dplyr::select(starts_with(match  = "Plot ID"), Panel, starts_with(match = "Plot Status")) %>%
                         setNames(c("PLOTID", "PANEL", "PLOTSTATUS"))
                     },
                     path = path) %>% dplyr::bind_rows()

  if (nrow(tracking) != nrow(distinct(tracking))) {
    stop("Not all plot IDs are unique across the imported combined plot tracking Excel files. Plot IDs must be unique.")
  }



  ################  Locate the plotid in terradata then store the plot key, primary key, and date visited.  If plotid not in terradata, then
  ##                the plot was not sampled (but there are subsequent checks to make sure the plotid's aren't misspelled) but
  ##                frame data ID, PLKEY, PRKEY, and DV need to be set to NA.

  ## Plot ID, plot key, primary key, and date visited for all plots in the tracking form with IDs that matched TerrADat
  tracking.tdat.in <- merge(x = tracking,
                            y = tdat[, c("PLOTID", "PLOTKEY", "PRIMARYKEY", "DATEVISITED")],
                            by = "PLOTID") %>%
    dplyr::select(PLOTID, PLOTKEY, PRIMARYKEY, DATEVISITED)
  ## Plot ID and NAs for plot key, primary key, and date visited for all plots in the tracking form in tracking.tdat.in
  tracking.tdat.out <- tracking %>% dplyr::filter(!(PLOTID %in% tracking.tdat.in$PLOTID)) %>%
    dplyr::mutate(PLOTKEY = NA, PRIMARYKEY = NA, DATEVISITED = NA) %>%
    dplyr::select(PLOTID, PLOTKEY, PRIMARYKEY, DATEVISITED)

  ## Combine and format dates as dates
  tracking.tdat <- rbind(tracking.tdat.in, tracking.tdat.out)
  tracking.tdat$DATEVISITED <- lubridate::as_date(tracking.tdat$DATEVISITED)
  ## Add the plot IDs as rownames. This will later make reordering to reflect the contents of dd$pts much, much simpler
  rownames(tracking.tdat) <- tracking.tdat$PLOTID

  ## Update PLOTSTATUS for base points that are currently flagged as NA
  tracking.tdat$PLOTSTATUS[is.na(tracking.tdat$PLOTSTATUS) & !grepl(x = tracking.tdat$PANEL, pattern = "over", ignore.case = TRUE)] <- "Unknown"

  ## Stop if these don't sum!
  if (nrow(tracking.tdat != nrow(tracking))) {
    stop(paste0("The number of rows in the imported plot tracking information (", nrow(tracking), ") somehow doesn't match the number of rows after comparing that to TerrADat (", nrow(tracking.tdat), ")."))
  }

  ## Warn if there are target sampled plots that have no match by plot ID
  if (tracking.tdat %>% filter(stringr::str_to_upper(PLOTSTATUS) %in% target.values, is.na(PLOTKEY)) %>% nrow() > 0) {
    message(paste0("The following plots in the plot tracking information are flagged as 'target sampled' but did not match a plot ID in TerrADat: ",
                   paste(tracking.tdat$PLOTID[is.na(tracking.tdat$PLOTKEY) & tracking.tdat$PLOTSTATUS %in% target.values],
                         collapse = ", ")
    )
    )
  }

  ## For each DD in dd.names, check and update the pts SPDF
  for (dd in dd.names) {
    pts <- dd.list$pts[[dd]]

    ####################### Compare plot tracking panel with dd point draw.
    mismatches <- merge(x = pts,
                        y = tracking.tdat %>% dplyr::filter(grepl(x = PANEL, pattern="over", ignore.case = TRUE)),
                        by.x = "PLOT_NM",
                        by.y = "PLOTID") %>% dplyr::filter(!grepl(x = PT_DRAW, pattern = "over", ignore.case = TRUE))
    if (nrow(mismatches) > 0) {
      message(paste0("There is a disagreement in the following plots between the Design Database (", dd, ") and plot tracking as to whether they were oversample points or not."))
      message(print(mismatches %>% dplyr::select(PLOTID, PANEL, PT_DRAW)))
    }

    ## Restrict tracking.tdat to plots where the plot ID occurs in the current pts SPDF and reorder the result to match the order of pts using the row names
    tracking.tdat.current <- tracking.tdat[pts@data$PLOT_NM[pts@data$PLOT_NM %in% tracking.tdat$PLOTID],]
    if (nrow(tracking.tdat) != nrow(tracking.tdat.current)) {
      message(paste0("The following plot IDs were found in the plot tracking information and TerrADat but not the Design Database (", dd, "):",
                     paste(tracking.tdat$PLOTID[!(tracking.tdat$PLOTID %in% tracking.tdat.current$PLOTID)], collapse = ", ")
      )
      )
    }

    ## Write in the plot keys from tracking.tdat.current where they exist
    pts@data$PLOT_KEY[pts@data$PLOT_NM %in% tracking.tdat.current$PLOTID[!is.na(tracking.tdat.current$PLOTKEY)]] <- tracking.tdat.current$PLOTKEY[!is.na(tracking.tdat.current$PLOTKEY)]
    ## Write in the primary keys from tracking.tdat.current where they exist
    pts@data$TERRA_TERRADAT_ID[pts@data$PLOT_NM %in% tracking.tdat.current$PLOTID[!is.na(tracking.tdat.current$PRIMARYKEY)]] <- tracking.tdat.current$PRIMARYKEY[!is.na(tracking.tdat.current$PRIMARYKEY)]
    ## Write in the plot statuses/fates from tracking.tdat.current where they exist
    pts@data$FINAL_DESIG[pts@data$PLOT_NM %in% tracking.tdat.current$PLOTID[!is.na(tracking.tdat.current$PLOTSTATUS)]] <- tracking.tdat.current$PLOTSTATUS[!is.na(tracking.tdat.current$PLOTSTATUS)]
    ## Write in the plot keys from tracking.tdat.current where they exist
    pts@data$DT_VST[pts@data$PLOT_NM %in% tracking.tdat.current$PLOTID[!is.na(tracking.tdat.current$DATEVISITED)]] <- tracking.tdat.current$DATEVISITED[!is.na(tracking.tdat.current$DATEVISITED)]


    ####################  If specified, delete overdraw pts that lack a final designation (i.e., not used).  This does not eliminate all NAs, just overdraw NAs..
    if(deleteoverdraw) {
      pts <- pts %>% drop.values(variable = "PANEL", dropvalue = "over", ignore.case = TRUE)
    }
    ####################  Store final pts file
    dd.list$pts[[dd]] <- pts
  } ## End of loop through dd.names

  ## Return the modified dd pts file(s) contained in the named list
  return(dd.list)
}

#' Quickly drop observations from a data frame based on the values of a single variable
#' @description This wrapper for \code{dplyr::filter()} will take a data frame (or Spatial Data Frame if the package \code{spdplyr} is installed) and remove all observations where the given variable meets the value of the argument \code{dropvalue}.
#' @param df A data frame or, if \code{spdplyr} is installed, spatial data frame to manipulate.
#' @param variable A character string specifying the name of the variable to base the filtering on.
#' @param dropvalue The value to drop observations based on. Can be either a regular expression as a character string to be passed to \code{grepl()} or \code{NA}. All observations where the variable \code{variable} return \code{TRUE} when checked against this will be dropped. Defaults to \code{NA}.
#' @param ignore.case Logical. If \code{dropvalue} is a regular expression, then this argument is passed to \code{grepl()} to decide if the search is case sensitive or not. Defaults to \code{TRUE}.
#' @return The data frame \code{df} without the observations where \code{variable} matched \code{dropvalue}.
#' @export
drop.values <- function(df, variable = "", dropvalue = NA, ignore.case = TRUE) {
  if (!grepl(x = class(df), pattern = "(data.frame)|(DataFrame)$")) {
    stop("Please provide a valid data frame.")
  }
  if (variable == "" | length(names(df)[grepl(x = names(df), pattern = "variable")] != 1)) {
    stop("Please provide a valid variable name.")
  }
  if (is.na(dropvalue)) {
    filtered <- eval(parse(text = paste0("df %>% dplyr::filter(!is.na(", variable, "))")))
  } else {
    filtered <- eval(parse(text = paste0("df %>% dplyr::filter(!grepl(x = ", variable, ", pattern = '", dropvalue, "', ignore.case = ", ignore.case(), "))")))
  }

  return(filtered)
}

#' Erase a Spatial Polygons Data Frame from another, either with rgeos or ArcPy
#' @description Spatial manipulations in R can get, for lack of a better term, squirrelly. Even with multiple failsafes in place to try to compensate for sliver geometry, \code{rgeos::gDifference()} still isn't as robust as one might hope. This wrapper removes the geometry of one SPDF from another via a system call to \code{Python} using the library \code{ArcPy} or via \code{rgeos::gDifference()}.
#' @param spdf A Spatial Polygons Data Frame to remove FROM.
#' @param spdf.erase  A Spatial Polygons Data Frame to remove geometry from \code{spdf} WITH.
#' @param method Character string. This must either be \code{"arcpy"} or \code{"rgeos"} and determines which approach will be used to frames from one another if \code{combine} is \code{TRUE}. If \code{"arcpy"} is used, then R must have write permissions to the folder \code{temp.path} and a valid install of ArcPy. This is preferable to \code{"rgeos"} because the functions involved tend to crash at random when handling very small remainder geometries. Case insensitive. Defaults to \code{"arcpy"}.
#' @param temp.path Optional character string. If \code{erase} is \code{"arcpy"} this must be the path to a folder that R has write permissions to so that a subfolder called arcpy_temp can be created and used for ArcPy erasure steps.
#' @param python.search.path Character string. The filepath for the folder containing \code{pythonw.exe}. Defaults to \code{"C:/Python27"}.
#' @param sliverdrop Optional logical value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_dropSlivers()} to temporarily set the environment during the erasure attempt. Defaults to \code{TRUE}.
#' @param sliverwarn Optional logical value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_warnSlivers()} to temporarily set the environment during the erasure attempt. Defaults to \code{TRUE}.
#' @param sliverdrop Optional numeric value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_polyThreshold()} to temporarily set the environment during the erasure attempt. Defaults to \code{0.01}.
#' @return The remaining geometry and data in \code{spdf} after \code{spdf.erase} has been removed from it.
#' @export

flex.erase <- function(spdf,
                       spdf.erase,
                       method = "arcpy",
                       temp.path = "",
                       python.search.path = "C:/Python27",
                       sliverdrop = T,
                       sliverwarn = T,
                       sliverthreshold = 0.01
){
  if (class(spdf) != "SpatialPolygonsDataFrame") {
    stop("spdf must be a valid Spatial Polygons Data Frame")
  }
  if (class(spdf.erase) != "SpatialPolygonsDataFrame") {
    stop("spdf.erase must be a valid Spatial Polygons Data Frame")
  }
  if (!(stringr::str_to_upper(method) %in% c("ARCPY", "RGEOS"))) {
    stop("method must be either 'arcpy' or 'rgeos'.")
  }
  if (!file.exists(python.search.path)) {
    stop("python.search.path must be a valid, pre-existing filepath.")
  }
  if (!file.exists(temp.path)) {
    stop("temp.path must be a valid, pre-existing filepath.")
  }

  if (spdf@proj4string@projargs != spdf.erase@proj4string@projargs) {
    spdf.erase <- sp::spTransform(spdf.erase, CRSobj = spdf@proj4string)
  }
  switch(stringr::str_to_upper(method),
         "ARCPY" = {
           ## Create a temp directory
           temp.directory <- paste0(temp.path, "/arcpy_temp")
           dir.create(temp.directory, showWarnings = FALSE)

           ## Write out the two current frames
           rgdal::writeOGR(obj = spdf, dsn = temp.directory, layer = "inshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)
           rgdal::writeOGR(obj = spdf.erase, dsn = temp.directory, layer = "eraseshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)

           ## Construct a quick python script to erase frame.spdf from frame.spdf.temp
           arcpy.script <- c("import arcpy",
                             "from arcpy import env",
                             paste0("env.workspace = '", temp.directory, "'"),
                             "in_features = 'inshape.shp'",
                             "erase_features = 'eraseshape.shp'",
                             "out_feature_class = 'eraseresults.shp'",
                             "xy_tolerance = ''",
                             "arcpy.Erase_analysis(in_features, erase_features, out_feature_class)"
           )
           ## Write the constructed script out
           cat(arcpy.script, file = paste0(temp.directory, "/erase.py"), sep = "\n", append = F)

           ## Find the local machine's copy of pythonw.exe in C:/Python27. There are no failsafes for if this isn't where to find it
           python.path <- paste0(python.search.path, "/", list.files(path = python.search.path, pattern = "pythonw.exe", recursive = TRUE))
           if (length(python.path) < 1) {
             stop(paste0("Unable to find pythonw.exe in the folder or subfolders of ", python.search.path))
           } else {
             python.path <- python.path[1]
           }

           ## Execute the Python script
           system(paste(python.path, stringr::str_replace_all(paste0(temp.directory, "/erase.py"), pattern = "/", replacement = "\\\\")))

           ## Read in the results and rename the attributes because rgdal::writeOGR() truncated them
           erase.results <- rgdal::readOGR(dsn = temp.directory, layer = "eraseresults", stringsAsFactors = FALSE)
           names(erase.results@data) <- names(frame.spdf.temp@data)

           if (erase.results@proj4string@projargs != spdf@proj4string@projargs) {
             output <- spTransform(erase.results, CRSobj = spdf@proj4string)
           } else {
             output <- erase.results
           }
           ## Remove the temp folder and files
           if (grepl(method, pattern = "arcpy", ignore.case = TRUE)) {
             system(paste("cmd /c rmdir", stringr::str_replace_all(temp.directory, pattern = "/", replacement = "\\\\"), "/s /q"))
           }
         }, "RGEOS" = {
           ## This lets rgeos deal with tiny fragments of polygons without crashing
           ## This and the following tryCatch() may be unnecessary since the argument drop_lower_td = TRUE was added, but it works so I'm leaving it
           current.drop <- rgeos::get_RGEOS_dropSlivers()
           current.warn <- rgeos::get_RGEOS_warnSlivers()
           current.tol <- rgeos::get_RGEOS_polyThreshold()

           rgeos::set_RGEOS_dropSlivers(sliverdrop)
           rgeos::set_RGEOS_warnSlivers(sliverwarn)
           rgeos::set_RGEOS_polyThreshold(sliverthreshold)
           message(paste0("Attempting using rgeos::set_RGEOS_dropslivers(", sliverdrop, ") and rgeos::set_RGEOS_warnslivers(", sliverwarn, ") and set_REGOS_polyThreshold(", sliverthreshold, ")"))
           ## Making this Albers for right now for gBuffer()
           ## The gbuffer() is a common hack to deal with ring self-intersections, which it seems to do just fine here?
           sp.temp <- rgeos::gDifference(spgeom1 = rgeos::gBuffer(sp::spTransform(spdf, CRS("+proj=aea")),
                                                                  byid = TRUE,
                                                                  width = 0.1),
                                         spgeom2 = rgeos::gBuffer(sp::spTransform(spdf.erase,
                                                                                  CRS("+proj=aea")),
                                                                  byid = TRUE,
                                                                  width = 0.1),
                                         drop_lower_td = TRUE)
           if (!is.null(frame.sp.temp)) {
             output <- sp::spTransform(sp::SpatialPolygonsDataFrame(sp.temp,
                                                                    data = spdf@data[1:length(sp.temp@polygons),]),
                                       CRSobj = spdf@proj4string)
           } else {
             output <- NULL
           }

           rgeos::set_RGEOS_dropSlivers(current.drop)
           rgeos::set_RGEOS_warnSlivers(current.warn)
           rgeos::set_RGEOS_polyThreshold(current.tol)

         }
  )
  return(output)
}
