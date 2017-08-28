#' Reading in the benchmarks from the Data Explorer
#'
#' @param data.path A string specifying the path to the folder containing the .XLSX with the benchmarks
#' @param benchmarks.filename A string specifying the filename of the .XLSX containing the benchmarks
#' @param indicator.lut A data frame with the column \code{"indicator.name"} matching the values in the Data Explorer "Indicator" field and one called \code{"indicator.tdat"} with corresponding value for the indicators' names in TerrADat.
#' @param indicator.lut.benchmarkfield The name of the column in \code{indicator.lut} that matches the "Indicator column of Data Explorer. Defaults to \code{"indicator.name"}
#' @return A data frame of the benchmarks from the Data Explorer with a field containing an evaluation string to use in testing indicator values against the benchmarks.
#' @examples
#' read.benchmarks()
#' @export

## TODO: Add capitalization sanitization stuff
read.benchmarks <- function(data.path = "",
                            benchmarks.filename = "",
                            sheet.name = "Monitoring Objectives",
                            indicator.lut = NULL,
                            indicator.lut.benchmarkfield = "indicator.name",
                            convert.l2r = TRUE
){
  ## Sanitizing inputs because users can't be trusted
  benchmarks.filename <- sanitize(benchmarks.filename, "xlsx")

  if (is.null(indicator.lut)) {
    indicator.lut <- indicator.lookup()
  }

  ## Import the spreadsheet from the workbook. Should work regardless of presence/absence of other spreadsheets as long as the name is the same
  benchmarks.raw <- readxl::read_excel(path = paste0(data.path, "/", benchmarks.filename),
                                       sheet = sheet.name)

  names(benchmarks.raw) <- names(benchmarks.raw) %>% stringr::str_replace_all(pattern = " ", replacement = "\\.")

  ## In case there's a "Classification" column where we'd prefer a "Category" column. This lets us maintain backwards compatibility with older iterations of the spreadsheet
  names(benchmarks.raw)[names(benchmarks.raw) %in% c("Classification")] <- "Evaluation.Category"

  ## Strip out the extraneous columns and rows, which includes if they left the example in there
  benchmarks <- benchmarks.raw[!grepl(x = benchmarks.raw$Management.Question, pattern = "^[Ee].g.") & !is.na(benchmarks.raw$Indicator), 1:12]

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
  benchmarks <- merge(x = benchmarks, y = indicator.lut, by.x = "Indicator", by.y = indicator.lut.benchmarkfield)

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
                    omitNAdesignations = FALSE, ## Strip out plots with a final designation value of NA
                    projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83
){

  ## readOGR() wrapped in safely() so that it will return NULL instead of an error
  safe.readOGR <- safely(rgdal::readOGR, otherwise = NULL)

  ## Sanitization
  func <- stringr::str_to_upper(func)
  target.values <- c(target.values,
                     "Target Sampled",
                     "TS") %>% unique() %>% stringr::str_to_upper()

  ## Checking that func is a valid value
  if (!(func %in% c("ARCGISBINDING", "READOGR"))) {
    print("The argument func needs to be 'arcgisbinding' or 'readOGR'")
  }

  ## Only keeping the DD filenames that actually exist in the src filepath provided
  dd.src.exist <- dd.src[dd.src %in% list.files(path = src)]

  ## Reporting the filenames it couldn't find in the folder
  if (length(dd.src) != length(dd.src.exist)) {
    print(paste0("Couldn't find the following .gdb[s]: ", paste(dd.src[!(dd.src %in% list.files(path = src))], collapse = ", ")))
  }

  switch(func,
         READOGR = {
           ## Looped so that it can execute across all the DDs in the vector (if there are more than one)
           for (s in dd.src.exist) {
             ## Read in the sample frame feature class inside the current DD.
             sf <- safe.readOGR(dsn = paste(src, s, sep = "/"),
                                layer = "Terra_Sample_Frame",
                                stringsAsFactors = FALSE)[[1]] ## The [[]] is to get the SPDF (or NULL) out of the list returned by the safely()
             # The spTransform() is just to be safe, but probably isn't necessary
             if (!is.null(sf)) {
               sf <- sp::spTransform(sf, projection)
             }
             ## Sanitize the column names
             names(sf@data) <- stringr::str_to_upper(names(sf@data))
             ## Stores the current sf SPDF with the name sf.[DD name]
             assign(x = paste("sf", s, sep = "."), value = sf)

             #Read in the Strata
             strata <- safe.readOGR(dsn = paste(src, s, sep = "/"),
                                    layer = "Terra_Strtfctn",
                                    stringsAsFactors = FALSE)[[1]]
             if (!is.null(strata)) {
               strata <- spTransform(strata, projection)
               names(strata@data) <- stringr::str_to_upper(names(strata@data))
             }
             assign(x = paste("strata", s, sep = "."), value = strata)

             #Read in the Points
             points <- safe.readOGR(dsn = paste(src, s, sep = "/"),
                                    layer = "Terra_Sample_Points",
                                    stringsAsFactors = FALSE)[[1]]
             if (!is.null(points)) {
               points <- spTransform(points, projection)
             }
             names(points@data) <- stringr::str_to_upper(names(points@data))
             ## Strip out points with an NA value in the FINAL_DESIG field if asked
             if (omitNAdesignations) {
               points <- points[!is.na(points@data$FINAL_DESIG)]
             }
             assign(x = paste("pts", s, sep = "."), value = points)
           }
         },
         ARCGISBINDING = {
           for (s in dd.src.exist) {
             ## Identify/create the filepath to the sample frame feature class inside the current DD
             sf <- paste(src, s, "Terra_Sample_Frame", sep = "/")
             ## Creates an SPDF with the name sf.[DD name] using the filepath to that feature class
             assign(x = paste("sf", s, sep = "."),
                    value = sf %>% arc.open() %>% arc.select %>%
                      SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .) %>% spTransform(projection)
             )
             eval(parse(text = paste0("names(", paste("sf", s, sep = "."), ") <- stringr::str_to_upper(names(", paste("sf", s, sep = "."), "))")))

             #Read in the Strata
             #first check for strata
             ## Identify/create the filepath to the design stratification feature class inside the current DD
             strata <- paste(src, s, "Terra_Strtfctn", sep = "/")
             #this loads enough of the feature class to tell if there are strata
             strata <- strata %>% arc.open() %>% arc.select
             #check for strata, if there are, then we will finish loading the file.
             if (nrow(strata) > 0) {
               ## Identify/create the filepath to the design stratification feature class inside the current DD
               strata <- paste(src, s, "Terra_Strtfctn", sep = "/")
               ## Creates an SPDF with the name strat.[DD name] using the filepath to that feature class
               assign(x = paste("strata", s, sep = "."),
                      value = strata %>% arc.open() %>% arc.select %>%
                        SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()},
                                                 data = .) %>% spTransform(projection))
               eval(parse(text = paste0("names(", paste("strata", s, sep = "."), ") <- stringr::str_to_upper(names(", paste("strata", s, sep = "."), "))")))

             } else {
               ## If the stratification feature class is empty, we'll just save ourselves some pain and store NULL
               assign(x = paste("strata", s, sep = "."),
                      value = NULL)
             }

             #Read in the Points
             ## Identify/create the filepath to the design points feature class inside the current DD
             pts <- paste(src, s, "Terra_Sample_Points", sep = "/")
             ## Creates an SPDF with the name pts.[DD name] using the filepath to that feature class
             assign(x = paste("pts", s, sep = "."),
                    value = pts %>% arc.open() %>% arc.select %>%
                      #read in the feature class, notice the difference between Polygons and points (different function with different arguments needs)
                      SpatialPointsDataFrame(coords = {arc.shape(.) %>% arc.shape2sp()}) %>% spTransform(projection))
             eval(parse(text = paste0("names(", paste("pts", s, sep = "."), ") <- stringr::str_to_upper(names(", paste("pts", s, sep = "."), "))")))
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
  ## Rename them with the correct DD name because they'll be in the same order that ls() returned them earlier. Also, we need to remove sf.list itself
  names(sf.list) <- ls()[grepl(x = ls(), pattern = "^sf\\.") & !grepl(x = ls(), pattern = "^sf.list$")] %>% stringr::str_replace(pattern = "^sf\\.", replacement = "")

  ## Creating the named list of all the pts SPDFs created by the loop
  pts.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")], collapse = "`, `"), "`)")))
  names(pts.list) <- ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")] %>% stringr::str_replace(pattern = "^pts\\.", replacement = "")

  ## Creating the named list of all the strata SPDFs created by the loop
  strata.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^strata\\.") & !grepl(x = ls(), pattern = "^strata.list$")], collapse = "`, `"), "`)")))
  names(strata.list) <- ls()[grepl(x = ls(), pattern = "^strata\\.") & !grepl(x = ls(), pattern = "^strata.list$")] %>% stringr::str_replace(pattern = "^strata\\.", replacement = "")

  output <- list(sf = sf.list, pts = pts.list, strata = strata.list)

  # if (validate.keys) {
  #   key.errors.df <- validate.keys(output)
  #
  #   ## Append this to the output list
  #   output <- list(output, "errors" = key.errors.df)
  # }

  return(output)
}

#' Reading in an exported TerrADat geodatabase
#'
#' @description Reads in the terrestrial and remote sensing indicator feature classes from an ESRI geodatabase exported from the BLM NOC TerrADat. Returns a single SpatialPointsDataFrame containing the data from both.
#' @param tdat.path A string of the folder path that contains the \code{.gdb}.
#' @param tdat.name A string of the filename of the geodatabase to import from
#' @export
read.tdat <- function(tdat.path, tdat.name){
  tdat.name <- sanitize(tdat.name, "gdb")
  tdat.terrestrial.spdf <- rgdal::readOGR(dsn = paste0(tdat.path, "/", tdat.name), layer = "SV_IND_TERRESTRIALAIM", stringsAsFactors = FALSE)
  tdat.remote.spdf <- rgdal::readOGR(dsn = paste0(tdat.path, "/", tdat.name), layer = "SV_IND_REMOTESENSING", stringsAsFactors = FALSE)
  tdat.spdf <- sp::merge(tdat.terrestrial.spdf, tdat.remote.spdf)
  return(tdat.spdf)
}

#' Splitting apart sample design databases containing multiple designs
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
                           if (!is.null(strata)) {
                             strata <- strata[strata$TERRA_STRTM_ID %in% pts$TERRA_STRTM_ID,]
                           }

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
        tail <- dd.list[[df]][(frame.index+1):length(dd.list)]
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
    }
  }
  return(dd.list)
}

#' Read in plot tracking Excel files
#' @description Imports plot tracking Excel files.
#' @param filename A character vector of the filename (including extension) of the project tracking Excel file to import. If not using the \code{path} argument, the filename should include the entire filepath.
#' @param path Optional string specifying a the project tracking Excel file in \code{filename} to read in. This will be prepended to the value in \code{filename}. If the filepath is included in the string \code{filename}, do not provide this.
#' @export
read.tracking <- function(filename = "", path = "") {
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
