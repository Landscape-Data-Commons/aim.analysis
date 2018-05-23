#' Reading in the benchmarks from the Data Explorer
#'
#' @param data.path Character string. The path to the folder containing the file \code{benchmarks.filename} with the benchmarks. Defaults to the working directory.
#' @param benchmarks.filename Character string. The filename, including filetype extension, of the .XLSX, .CSV, or .XLS containing the benchmarks. Expects to find columns with headers matching "Management Question", "Benchmark Source", "Benchmark Group", "Reporting Unit", "Lower Limit", "LL Relation", "Indicator", "UL Relation", "Upper Limit", "Unit", "Condition Category", "Proportion Relation", and "Required Proportion".
#' @param indicator.lut Data frame containing a variable specified in \code{indicator.lut.benchmarkfield} holding values matching those values in the benchmark file's column named "Indicator" and \code{"indicator.tdat"} with corresponding values for the indicators' names in the point data. Defaults to the output from \code{indicator.lut()}.
#' @param indicator.lut.benchmarkfield The name of the column in \code{indicator.lut} that holds values matching the "Indicator" column of the benchmark file. Defaults to \code{"indicator.name"}
#' @return A data frame of the benchmarks from the specified file with fields containing evaluation strings to use in testing indicator values against the benchmarks.
#' @examples
#' read.benchmarks()
#' @export

## TODO: Add capitalization sanitization stuff
read.benchmarks <- function(data.path = NULL,
                            benchmarks.filename = "",
                            sheet.name = "Monitoring Objectives",
                            indicator.lut = NULL,
                            indicator.lut.benchmarkfield = "indicator.name",
                            convert.l2r = TRUE
){
  ## Use the working directory if none is provided
  if (is.null(data.path)){
    data.path <- getwd()
  }

  ## Check for the file extension
  if (!grepl(benchmarks.filename, pattern = "\\.((xlsx)|(csv)|(xls))", ignore.case = TRUE)) {
    stop("The benchmark filename needs to have a valid file extension (xlsx, csv, or xls). The most likely extension is xlsx.")
  }

  # Make the full filepath
  filepath <- paste0(data.path, "/", benchmarks.filename)

  # Check to see if it exists
  if (file.exists(filepath)) {
    if (grepl(filepath, pattern = "\\.csv", ignore.case = TRUE)) {
      ## Import the spreadsheet from the workbook. Should work regardless of presence/absence of other spreadsheets as long as the name is the same
      benchmarks.raw <- read.csv(filepath, stringsAsFactors = FALSE)
    } else {
      ## Import the spreadsheet from the workbook. Should work regardless of presence/absence of other spreadsheets as long as the name is the same
      benchmarks.raw <- readxl::read_excel(path = filepath,
                                           sheet = sheet.name)
      # Check to make sure that there's not a weird row up top we need to skip
      if (!all(c("INDICATOR", "UNIT") %in% toupper(names(benchmarks.raw)))){
        benchmarks.raw <- readxl::read_excel(path = filepath,
                                             sheet = sheet.name,
                                             skip = 1)
      }
    }
    ## Change all the header names to use "." instead of " " for consistency
    names(benchmarks.raw) <- gsub(names(benchmarks.raw), pattern = " ", replacement = ".")

    if (!all(c("INDICATOR", "UNIT") %in% toupper(names(benchmarks.raw)))){
      stop("Can't find the expected column headers in the provided benchmark file. Check to make sure that there are no non-header rows before the headers.")
    }
  } else {
    stop(paste("Can't find the benchmark file at", filepath))
  }

  ## If there's no indicator lookup table provided, use the defaut one built into the package
  if (is.null(indicator.lut)) {
    indicator.lut <- indicator.lookup()
  }


  ## In case there's a "Classification" column where we'd prefer a "Category" column. This lets us maintain backwards compatibility with older iterations of the spreadsheet
  names(benchmarks.raw)[toupper(names(benchmarks.raw)) %in% c("CLASSIFICATION", "EVALUATION.CATEGORY")] <- "Condition.Category"

  ## Strip out the extraneous columns and rows, which includes if they left the example in there. The pattern to look for is "e.g"
  benchmarks <- benchmarks.raw[!grepl(x = benchmarks.raw$Management.Question, pattern = "^[Ee].g.") & !is.na(benchmarks.raw$Indicator), 1:12]

  ## In case the lower bound relationships have been inverted for whatever reason, this'll flip them to the expected
  if (convert.l2r) {
    benchmarks$LL.Relation[benchmarks$LL.Relation == ">="] <- "<"
    benchmarks$LL.Relation[benchmarks$LL.Relation == ">"] <- "<="
  }

  ## Create the evaluations for the upper and lower limits of each benchmark.
  ## The way the spreadsheet is configured, there should be no rows without both defined
  benchmarks$eval.string.lower <- paste(benchmarks$Lower.Limit, benchmarks$LL.Relation)
  benchmarks$eval.string.upper <- paste(benchmarks$UL.Relation, benchmarks$Upper.Limit)

  ## Assume that the upper limit is infinity for places where there's a lower limit but not an upper
  benchmarks$eval.string.upper[!is.na(benchmarks$Lower.Limit) & !is.na(benchmarks$LL.Relation) & is.na(benchmarks$UL.Relation) & is.na(benchmarks$Upper.Limit)] <- "< Inf"
  ## Assume that the lower limit is negative infinity for places where there's an upper limit but not a lower
  benchmarks$eval.string.lower[is.na(benchmarks$Lower.Limit) & is.na(benchmarks$LL.Relation) & !is.na(benchmarks$UL.Relation) & !is.na(benchmarks$Upper.Limit)] <- "-Inf <"

  ## Create an evaluation string for future use with the required proportion and its relationship
  benchmarks$eval.string.proportion <- ""
  benchmarks$eval.string.proportion[!is.na(benchmarks$Required.Proportion)] <- paste(benchmarks$Proportion.Relation[!is.na(benchmarks$Required.Proportion)], benchmarks$Required.Proportion[!is.na(benchmarks$Required.Proportion)])

  ## For each benchmark add in the name of the field in TerrADat that corresponds
  benchmarks <- merge(x = benchmarks,
                      y = indicator.lut,
                      by.x = "Indicator",
                      by.y = indicator.lut.benchmarkfield)

  return(benchmarks)
}

#' Importing Sample Design Databases for AIM Sample Designs
#'
#' This function imports one or more Sample Design Database[s] and returns a list of the named lists sf, pts, and strata. The named lists contain SpatialPoints/PolygonsDataFrames of the sample frame, points, and strata from each of the geodatabases. The SPDFs are named using the filename of the geodatabase source, so that each list has one SPDF named for each geodatabase imported and those names are identical between lists. If a Sample Design Database is missing any one of those features, a NULL value replaces the SPDF.
#' @param src Character string defining the filepath containing the sample design database[s].
#' @param dd.src Character string or character vector containing the filenames of the geodatabases to import. Each filename should include the extension ".gdb"
#' @param func Character string. Defines whether to use the \code{rgdal::} or \code{arcgisbinding::} package to read in the geodatabases. Defaults to \code{"arcgisbinding"}. Valid values are \code{"arcgisbinding"} and \code{"readogr"}. This is not case sensitive
#' @param projection \code{sp::CRS()} argument. Defaults to NAD83 with \code{sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")}. Is used to reproject all SPDFs in order to perform spatial manipulations
#' @export

## TODO: Should try to handle raster location/import either within dd.reader() or as an independent function
## Reads in DDs. Returns a named list of lists of SPDFs: sf, pts, strata.
## The index order is maintained as well, so output[1][1] and output[2][1] correspond to the same DD source
## If a feature class couldn't be found, there will be a NULL instead of SPDF for that DD in the list
read.dd <- function(src = "", ## A filepath as a string
                    dd.src, ## A character string or vector of character strings with the filename[s] for the relevant .gdb in the filepath src
                    func = "arcgisbinding", ## This can be "readOGR" or "arcgisbinding" depending on which you prefer to or can use
                    # validate.keys = T, ## Should the process also produce a data frame in the output of points in the design dtabases that have issues with final designations or TerrAdat primary keys?
                    target.values = c("Target Sampled",
                                      "TS"),
                    split = TRUE,
                    omitNAdesignations = FALSE, ## Strip out plots with a final designation value of NA
                    projection = sp::CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83
){

  ## readOGR() wrapped in safely() so that it will return NULL instead of an error
  safe.readOGR <- purrr::safely(rgdal::readOGR, otherwise = NULL)

  ## Sanitization
  func <- toupper(func)
  target.values <- c(target.values,
                     "Target Sampled",
                     "TS") %>% unique() %>% stringr::str_to_upper()

  ## Checking that func is a valid value
  if (!(func %in% c("ARCGISBINDING", "READOGR"))) {
    stop("The argument func needs to be 'arcgisbinding' or 'readOGR'")
  }

  ## Only keeping the DD filenames that actually exist in the src filepath provided
  filepaths <- paste(src, dd.src, sep = "/")
  dd.src.exist <- filepaths[file.exists(filepaths)]

  ## Reporting the filenames it couldn't find in the folder
  if (length(dd.src) != length(dd.src.exist)) {
    message(paste0("Couldn't find the following .gdb[s]: ", paste(dd.src[!(dd.src %in% list.files(path = src))], collapse = ", ")))
  }

  ## Create the lists to store stuff from the databases
  sf.list <- list()
  pts.list <- list()
  strata.list <- list()

  switch(func,
         READOGR = {
           ## Looped so that it can execute across all the DDs in the vector (if there are more than one)
           for (s in dd.src.exist) {
             ## Read in the sample frame feature class inside the current DD.
             sf <- safe.readOGR(dsn = s,
                                layer = "Terra_Sample_Frame",
                                stringsAsFactors = FALSE)[[1]] ## The [[]] is to get the SPDF (or NULL) out of the list returned by the safely()
             # The spTransform() is just to be safe, but probably isn't necessary
             if (!is.null(sf)) {
               sf <- sp::spTransform(sf, projection)
             }
             ## Sanitize the column names
             names(sf@data) <- toupper(names(sf@data))
             ## Stores the returned sf object with the name s in sf.list
             sf.list[[s]] <- sf

             #Read in the Strata
             strata <- safe.readOGR(dsn = s,
                                    layer = "Terra_Strtfctn",
                                    stringsAsFactors = FALSE)[[1]]
             if (!is.null(strata)) {
               strata <- sp::spTransform(strata, projection)
               names(strata@data) <- toupper(names(strata@data))
             }
             strata.list[[s]] <- strata

             #Read in the Points
             points <- safe.readOGR(dsn = s,
                                    layer = "Terra_Sample_Points",
                                    stringsAsFactors = FALSE)[[1]]
             if (!is.null(points)) {
               points <- sp::spTransform(points, projection)
             }
             names(points@data) <- toupper(names(points@data))
             ## Strip out points with an NA value in the FINAL_DESIG field if asked
             if (omitNAdesignations) {
               points <- points[!is.na(points@data$FINAL_DESIG)]
             }
             pts.list[[s]] <- points
           }
         },
         ARCGISBINDING = {
           for (s in dd.src.exist) {
             ## Identify/create the filepath to the sample frame feature class inside the current DD
             sf.path <- paste(s, "Terra_Sample_Frame", sep = "/")
             ## Creates an SPDF with the name sf.[DD name] using the filepath to that feature class
             sf <- read.arclayer(filepath = sf.path, projection = projection)
             ## Sanitize the column names
             names(sf@data) <- toupper(names(sf@data))
             ## Stores the returned sf object with the name s in sf.list
             sf.list[[s]] <- sf

             #Read in the Strata
             #first check for strata
             ## Identify/create the filepath to the design stratification feature class inside the current DD
             strata.path <- paste(s, "Terra_Strtfctn", sep = "/")
             #this loads enough of the feature class to tell if there are strata
             strata <- strata.path %>% arc.open() %>% arc.select
             #check for strata, if there are, then we will finish loading the file.
             if (nrow(strata) > 0) {
               ## Creates an SPDF with the name strat.[DD name] using the filepath to that feature class
               strata <- read.arclayer(strata.path, projection)
               names(strata@data) <- toupper(names(strata@data))
             } else {
               ## If the stratification feature class is empty, we'll just save ourselves some pain and store NULL
              strata <- NULL
             }
             strata.list[[s]] <- strata

             #Read in the Points
             ## Identify/create the filepath to the design points feature class inside the current DD
             points.path <- paste(s, "Terra_Sample_Points", sep = "/")
             ## Creates an SPDF with the name pts.[DD name] using the filepath to that feature class
             points <- read.arclayer(points.path, projection)
             names(points@data) <- toupper(names(points@data))
             ## Strip out points with an NA value in the FINAL_DESIG field if asked
             if (omitNAdesignation) {
               points <- points[!is.na(points@data$FINAL_DESIG),]
             }
             pts.list[[s]] <- points
           }
         }
  )

  output <- list(sf = sf.list, pts = pts.list, strata = strata.list)

  ## In case there're databases involved that had more than one design in them, split them
  if (split) {
    output <- dd.split(output)
  }

  return(output)
}

#' Reading in an exported TerrADat geodatabase
#'
#' @description Reads in the terrestrial and remote sensing indicator feature classes from an ESRI geodatabase exported from the BLM NOC TerrADat. Returns a single SpatialPointsDataFrame containing the data from both.
#' @param tdat.path A string of the folder path that contains the \code{.gdb}.
#' @param tdat.name A string of the filename of the geodatabase to import from
#' @export
read.tdat <- function(tdat.path,
                      tdat.name){
  tdat.name <- sanitize(tdat.name, "gdb")
  tdat.terrestrial.spdf <- rgdal::readOGR(dsn = paste0(tdat.path, "/", tdat.name), layer = "SV_IND_TERRESTRIALAIM", stringsAsFactors = FALSE)
  tdat.remote.spdf <- rgdal::readOGR(dsn = paste0(tdat.path, "/", tdat.name), layer = "SV_IND_REMOTESENSING", stringsAsFactors = FALSE)
  tdat.spdf <- sp::merge(tdat.terrestrial.spdf, tdat.remote.spdf)
  return(tdat.spdf)
}

#' Splitting apart sample design databases containing multiple designs
#' @description When a sample design database contains more than one design, as indicated by multiple sample frames, this function will (after importing with \code{read.dd()}) split the SPDFs from every multiple-design database into one SPDF per design and insert them at the indices where the original SPDF was. The naming format for these new SPDFs is "geodatabase filename_sample frame ID".
#' @param dd.list The output from \code{read.dd()}
#' @return The list \code{dd.list} with one SPDF per design in each of the lists \code{"sf"}, \code{"pts"}, and \code{"strata"}, maintaining the order of the data in those lists.
#' @export
dd.split <- function(dd.list) {
  # Which design databases has multiple designs?
  multiple.sf <- names(dd.list$sf)[lapply(dd.list$sf, nrow) > 1]

  # For each of the databases that had more than one design
  for (frame in multiple.sf) {
    # Create a list of designs, each as a list of sf, pts, and strata
    split.list <- lapply(X = unique(dd.list$sf[[frame]]$TERRA_SAMPLE_FRAME_ID),
                         FUN = function(X, dd.list, frame) {
                           sf <- dd.list$sf[[frame]][dd.list$sf[[frame]]$TERRA_SAMPLE_FRAME_ID == X,]
                           pts <- dd.list$pts[[frame]][dd.list$pts[[frame]]$TERRA_SAMPLE_FRAME_ID == X,]
                           strata <- dd.list$strata[[frame]]
                           # Sometimes strata is NULL, so only try to slice if it isn't
                           if (!is.null(strata)) {
                             strata <- strata[strata$TERRA_STRTM_ID %in% pts$TERRA_STRTM_ID,]
                           }

                           # Make the output list with the names of the data types
                           output <- setNames(list(sf, pts, strata), c("sf", "pts", "strata"))
                           return(output)
                         },
                         dd.list = dd.list,
                         frame = frame)

    # Where in the original imported data did that design database fall?
    # We want to be able to keep the order consistent (in case the user cares)
    frame.index <- grep(names(dd.list$sf), pattern = frame)

    # For each of the data sets
    for (df in c("sf", "pts", "strata")) {
      # Get the data that came before the current database's stuff
      if (frame.index > 1) {
        head <- dd.list[[df]][1:(frame.index-1)]
      } else {
        head <- NULL
      }
      # and the data that came after
      if (frame.index < length(dd.list[[df]])) {
        tail <- dd.list[[df]][(frame.index+1):length(dd.list[[df]])]
      } else {
        tail <- NULL
      }

      # Get all of this database's stuff
      subset <- lapply(X = split.list,
                       FUN = function(X, df) {
                         X[[df]]
                       },
                       df = df) %>% setNames(paste0(frame, "_", unique(dd.list$sf[[frame]]$TERRA_SAMPLE_FRAME_ID)))

      # And mash it together!
      dd.list[[df]] <- c(head, subset, tail)

      # I don't know why the naming only works for the sample frames, so we'll just name everything else from that list
      if (df != "sf") {
        names(dd.list[[df]]) <- names(dd.list[["sf"]])
      }
    }
  }
  return(dd.list)
}

#' Read in plot tracking Excel files
#' @description Imports plot tracking Excel files.
#' @param filename A character vector of the filename (including extension) of the project tracking Excel file to import. If not using the \code{path} argument, the filename should include the entire filepath.
#' @param path Optional string specifying a the project tracking Excel file in \code{filename} to read in. This will be prepended to the value in \code{filename}. If the filepath is included in the string \code{filename}, do not provide this.
#' @export
read.tracking <- function(filename = "",
                          path = "") {
  if (path != "") {
    path <- paste0(path, "/")
  }
  tracking <- readxl::read_excel(path = paste0(path, filename),
                                 sheet = 1,
                                 col_types = c("text"),
                                 # col_types = c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "date", "text", "text", "date", "numeric", "numeric"),
                                 skip = 1
  )

  for (field in names(tracking)[grepl(x = names(tracking), pattern = "date", ignore.case = TRUE) & !grepl(x = names(tracking), pattern = " and ", ignore.case = TRUE)]) {
    tracking[, field] <- lubridate::as_date(as.character(tracking[[field]]))
  }

  return(tracking)
}

# Read in the default indicator lookup table
indicator.lookup <- function(){
  return(read.csv(paste0(path.package("aim.analysis"), "/defaults/indicator_lut.csv"), stringsAsFactors = FALSE))
}

# Read in the default point fate lookup table
fate.lookup <- function(){
  return(read.csv(paste0(path.package("aim.analysis"), "/defaults/fates.csv"), stringsAsFactors = FALSE))
}

