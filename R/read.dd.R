#' Importing Sample Design Databases for AIM Sample Designs
#'
#' This function imports one or more Sample Design Database[s] and returns a list of the named lists sf, pts, and strata. The named lists contain SpatialPoints/PolygonsDataFrames of the sample frame, points, and strata from each of the geodatabases. The SPDFs are named using the filename of the geodatabase source, so that each list has one SPDF named for each geodatabase imported and those names are identical between lists. If a Sample Design Database is missing any one of those features, a NULL value replaces the SPDF.
#' @param src Character string defining the filepath containing the sample design database[s].
#' @param sdd.src Character string or character vector containing the filenames of the geodatabases to import. Each filename should include the extension ".gdb"
#' @param func Character string. Defines whether to use the \code{rgdal::} or \code{arcgisbinding::} package to read in the geodatabases. Defaults to \code{"arcgisbinding"}. Valid values are \code{"arcgisbinding"} and \code{"readogr"}. This is not case sensitive
#' @param projection \code{sp::CRS()} argument. Defaults to NAD83 with \code{sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")}. Is used to reproject all SPDFs in order to perform spatial manipulations
#' @keywords weights
#' @examples
#' weighter()

## TODO: Should try to handle raster location/import either within sdd.reader() or as an independent function
## Reads in SDDs. Returns a named list of lists of SPDFs: sf, pts, strata.
## The index order is maintained as well, so output[1][1] and output[2][1] correspond to the same SDD source
## If a feature class couldn't be found, there will be a NULL instead of SPDF for that SDD in the list
read.dd <- function(src = "", ## A filepath as a string
                    sdd.src, ## A character string or vector of character strings with the filename[s] for the relevant .gdb in the filepath src
                    func = "arcgisbinding", ## This can be "readOGR" or "arcgisbinding" depending on which you prefer to or can use
                    validate.keys = T, ## Should the process also produce a data frame in the output of points in the design dtabases that have issues with final designations or TerrAdat primary keys?
                    target.values = c("Target Sampled",
                                      "TS"),
                    omitNAdesignations = F, ## Strip out plots with a final designation value of NA
                    projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83
){

  ## readOGR() wrapped in safely() so that it will return NULL instead of an error
  safe.readOGR <- safely(readOGR, otherwise = NULL)

  ## Sanitization
  func <- str_to_upper(func)
  target.values <- c(target.values,
                     "Target Sampled",
                     "TS") %>% unique() %>% str_to_upper()

  ## Checking that func is a valid value
  if (!(func %in% c("ARCGISBINDING", "READOGR"))) {
    print("The argument func needs to be 'arcgisbinding' or 'readOGR'")
  }

  ## Only keeping the SDD filenames that actually exist in the src filepath provided
  sdd.src.exist <- sdd.src[sdd.src %in% list.files(path = src)]

  ## Reporting the filenames it couldn't find in the folder
  if (length(sdd.src) != length(sdd.src.exist)) {
    print(paste0("Couldn't find the following .gdb[s]: ", paste(sdd.src[!(sdd.src %in% list.files(path = src))], collapse = ", ")))
  }

  switch(func,
         READOGR = {
           ## Looped so that it can execute across all the SDDs in the vector (if there are more than one)
           for (s in sdd.src.exist) {
             ## Read in the sample frame feature class inside the current SDD.
             sf <- safe.readOGR(dsn = paste(src, s, sep = "/"),
                                layer = "Terra_Sample_Frame",
                                stringsAsFactors = F)[[1]] ## The [[]] is to get the SPDF (or NULL) out of the list returned by the safely()
             # The spTransform() is just to be safe, but probably isn't necessary
             if (!is.null(sf)) {
               sf <- spTransform(sf, projection)
             }
             ## Sanitize the column names
             names(sf@data) <- str_to_upper(names(sf@data))
             ## Stores the current sf SPDF with the name sf.[SDD name]
             assign(x = paste("sf", s, sep = "."), value = sf)

             #Read in the Strata
             strata <- safe.readOGR(dsn = paste(src, s, sep = "/"),
                                    layer = "Terra_Strtfctn",
                                    stringsAsFactors = F)[[1]]
             if (!is.null(strata)) {
               strata <- spTransform(strata, projection)
             }
             names(strata@data) <- str_to_upper(names(strata@data))
             assign(x = paste("strata", s, sep = "."), value = strata)

             #Read in the Points
             points <- safe.readOGR(dsn = paste(src, s, sep = "/"),
                                    layer = "Terra_Sample_Points",
                                    stringsAsFactors = F)[[1]]
             if (!is.null(points)) {
               points <- spTransform(points, projection)
             }
             names(points@data) <- str_to_upper(names(points@data))
             ## Strip out points with an NA value in the FINAL_DESIG field if asked
             if (omitNAdesignations) {
               points <- points[!is.na(points@data$FINAL_DESIG)]
             }
             assign(x = paste("pts", s, sep = "."), value = points)
           }
         },
         ARCGISBINDING = {
           for (s in sdd.src.exist) {
             ## Identify/create the filepath to the sample frame feature class inside the current SDD
             sf <- paste(src, s, "Terra_Sample_Frame", sep = "/")
             ## Creates an SPDF with the name sf.[SDD name] using the filepath to that feature class
             assign(x = paste("sf", s, sep = "."),
                    value = sf %>% arc.open() %>% arc.select %>%
                      SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .) %>% spTransform(projection)
             )
             eval(parse(text = paste0("names(", paste("sf", s, sep = "."), ") <- str_to_upper(names(", paste("sf", s, sep = "."), "))")))

             #Read in the Strata
             #first check for strata
             ## Identify/create the filepath to the design stratification feature class inside the current SDD
             strata <- paste(src, s, "Terra_Strtfctn", sep = "/")
             #this loads enough of the feature class to tell if there are strata
             strata <- strata %>% arc.open() %>% arc.select
             #check for strata, if there are, then we will finish loading the file.
             if (nrow(strata) > 0) {
               ## Identify/create the filepath to the design stratification feature class inside the current SDD
               strata <- paste(src, s, "Terra_Strtfctn", sep = "/")
               ## Creates an SPDF with the name strat.[SDD name] using the filepath to that feature class
               assign(x = paste("strata", s, sep = "."),
                      value = strata %>% arc.open() %>% arc.select %>%
                        SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()},
                                                 data = .) %>% spTransform(projection))
               eval(parse(text = paste0("names(", paste("strata", s, sep = "."), ") <- str_to_upper(names(", paste("strata", s, sep = "."), "))")))

             } else {
               ## If the stratification feature class is empty, we'll just save ourselves some pain and store NULL
               assign(x = paste("strata", s, sep = "."),
                      value = NULL)
             }

             #Read in the Points
             ## Identify/create the filepath to the design points feature class inside the current SDD
             pts <- paste(src, s, "Terra_Sample_Points", sep = "/")
             ## Creates an SPDF with the name pts.[SDD name] using the filepath to that feature class
             assign(x = paste("pts", s, sep = "."),
                    value = pts %>% arc.open() %>% arc.select %>%
                      #read in the feature class, notice the difference between Polygons and points (different function with different arguments needs)
                      SpatialPointsDataFrame(coords = {arc.shape(.) %>% arc.shape2sp()}) %>% spTransform(projection))
             eval(parse(text = paste0("names(", paste("pts", s, sep = "."), ") <- str_to_upper(names(", paste("pts", s, sep = "."), "))")))
             ## Strip out points with an NA value in the FINAL_DESIG field if asked
             if (omitNAdesignation) {
               eval(parse(text = paste0(paste("pts", s, sep = "."), " <- ", paste("pts", s, sep = "."), "[!is.na(", paste("pts", s, sep = "."), "@data$FINAL_DESIG)]")))
             }
           }
         }
  )

  ## Create a list of the sample frame SPDFs.
  ## This programmatically create a string of the existing object names that start with "sf." separated by commas
  ## then wraps that in "list()" and runs the whole string through parse() and eval() to execute it, creating a list from those SPDFs
  sf.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^sf\\.") & !grepl(x = ls(), pattern = "^sf.list$")], collapse = "`, `"), "`)")))
  ## Rename them with the correct SDD name because they'll be in the same order that ls() returned them earlier. Also, we need to remove sf.list itself
  names(sf.list) <- ls()[grepl(x = ls(), pattern = "^sf\\.") & !grepl(x = ls(), pattern = "^sf.list$")] %>% str_replace(pattern = "^sf\\.", replacement = "")

  ## Creating the named list of all the pts SPDFs created by the loop
  pts.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")], collapse = "`, `"), "`)")))
  names(pts.list) <- ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")] %>% str_replace(pattern = "^pts\\.", replacement = "")

  ## Creating the named list of all the strata SPDFs created by the loop
  strata.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^strata\\.") & !grepl(x = ls(), pattern = "^strata.list$")], collapse = "`, `"), "`)")))
  names(strata.list) <- ls()[grepl(x = ls(), pattern = "^strata\\.") & !grepl(x = ls(), pattern = "^strata.list$")] %>% str_replace(pattern = "^strata\\.", replacement = "")

  output <- list(sf = sf.list, pts = pts.list, strata = strata.list)

  if (validate.keys) {
    ## Initialize the output data frame
    key.errors.df <- data.frame()

    ## Check each design database in turn
    for (sdd in names(output$pts)) {
      ## Get the @data slot from the SPDF for the points for this design
      pts.df <- sdd.list$pts[[sdd]] %>% .@data
      ## Sanitize the field names
      names(pts.df) <- str_to_upper(names(pts.df))

      ## Grab all the lines where the point was sampled, but there's not a correct PrimaryKey value
      errors.missing.tdat <- pts.df %>%
        filter(FINAL_DESIG %in% target.values,
               !grepl(x = TERRA_TERRADAT_ID, pattern = "^[0-9]{15,24}-[0-9]{1,3}-[0-9]{1,3}$")) %>% .[, c("TERRA_SAMPLE_FRAME_ID", "PLOT_NM", "TERRA_TERRADAT_ID")]
      ## If that turned up anything, then alert the user and add the information about all the points with that error to the output data frame
      if (nrow(errors.missing.tdat) > 0) {
        message(paste0("In ", sdd, ", ", nrow(errors.missing.tdat), " points were designated as 'target sampled' but missing a valid PrimaryKey value. See the data frame 'errors' in the output for details."))
        errors.missing.tdat$SDD <- sdd
        errors.missing.tdat$ERROR <- "Point is designated as 'target sampled' but is missing a valid PrimaryKey value"
        key.errors.df <- rbind(key.errors.df, errors.missing.tdat[, c("SDD", "ERROR", "TERRA_SAMPLE_FRAME_ID", "PLOT_NM", "TERRA_TERRADAT_ID")])
      }

      ## Grab all the lines where there's a proper PrimaryKey value but there's not a target designation
      errors.missing.desig <- pts.df %>%
        filter(!(FINAL_DESIG %in% target.values),
               grepl(x = TERRA_TERRADAT_ID, pattern = "^[0-9]{15,24}-[0-9]{1,3}-[0-9]{1,3}$")) %>% .[, c("TERRA_SAMPLE_FRAME_ID", "PLOT_NM", "TERRA_TERRADAT_ID")]
      if (nrow(errors.missing.desig) > 0) {
        message(paste0("In ", sdd, ", ", nrow(errors.missing.desig), " points have a valid PrimaryKey value but are not designated as 'target sampled.' See the data frame 'errors' in the output for details."))
        errors.missing.desig$SDD <- sdd
        errors.missing.desig$ERROR <- "Point has a valid PrimaryKey value but is not designated as 'target sampled'"
        key.errors.df <- rbind(key.errors.df, errors.missing.desig[, c("SDD", "ERROR", "TERRA_SAMPLE_FRAME_ID", "PLOT_NM", "TERRA_TERRADAT_ID")])
      }
    }

    if (nrow(key.errors.df) < 1) {
      message("No points designated as 'target sampled' were missing valid TerrADat primary key values and no points with valid primary keys were designated as anything but 'target sampled.'")
    }

    ## Append this to the output list
    output <- list(output, "errors" = key.errors.df)
  }

  return(output)
}
