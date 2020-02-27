#' Reading in the benchmarks from the Data Explorer
#'
#' @param filename Character string. The filename, including filetype extension, of the .XLSX, .CSV, .XLSM, or .XLS containing the benchmarks. Expects to find columns with headers matching "Management Question", "Benchmark Source", "Benchmark Group", "Reporting Unit", "Lower Limit", "LL Relation", "Indicator", "UL Relation", "Upper Limit", "Unit", "Condition Category", "Proportion Relation", and "Required Proportion".
#' @param filepath Optional character string. The filepath to the location where the file matching \code{filename} is stored. Used to locate the file by combining with the filename. Will be ignored if \code{NULL}. Defaults to \code{NULL}.
#' @param sheet_name Optional character string. The sheet name of the spreadsheet in the Excel workbook specified by \code{filename}. Only used if \code{filename} is an Excel workbook. Defaults to \code{"Monitoring Objectives"}.
#' @param eval_strings Optional list of character vectors. If \code{NULL}, nothing will be done. Otherwise, each character vector should contain one or more variable/column names from the benchmarks. The only other string allowed is \code{"x"} which can be used as a placeholder for indicator values. As an example, \code{list(c("lower.relationship", "lower.limit", "x"))} would add a column to the output called \code{"evalstring1"} that contains the results of \code{paste(benchmarks$lower.limit, benchmarks$lower.relationship, "x")}. Defaults to \code{list(c("Lower.Limit", "LL.Relation", "x"), c("x", "UL.Relation", "Upper.Limit"), c("x", "Required.Relation", "Required.Proportion"))}, appropriate for use with the AIM "Benchmark Tool".
#' @return A data frame of the benchmarks from the specified file with fields containing evaluation strings to use in testing indicator values against the benchmarks.
#' @examples
#' read_benchmarks()
#' @export

## TODO: Add capitalization sanitization stuff
read_benchmarks <- function(filename = "",
                            filepath = NULL,
                            sheet_name = "Monitoring Objectives",
                            eval_strings = list(c("Lower.Limit", "LL.Relation", "x"),
                                                c("x", "UL.Relation", "Upper.Limit"),
                                                c("x", "Proportion.Relation", "Required.Proportion"))){
  ## Check for the file extension
  if (!grepl(filename, pattern = "\\.((xlsx)|(csv)|(xls)|(xlsm))$", ignore.case = TRUE)) {
    stop("The benchmark filename needs to have a valid file extension (xlsx, csv, xls, or xlsm). The most likely extension is xlsx.")
  }

  # Make the full filepath
  if (is.null(filepath)) {
    filepath <- filename
  } else {
    filepath <- paste(filepath, filename, sep = "/")
  }

  # Check to see if it exists
  if (file.exists(filepath)) {
    if (grepl(filepath, pattern = "\\.csv", ignore.case = TRUE)) {
      benchmarks_raw <- read.csv(filepath, stringsAsFactors = FALSE)
    } else {
      benchmarks_raw <- readxl::read_excel(path = filepath,
                                           sheet = sheet_name)
      # Check to make sure that there's not a weird row up top we need to skip (very possible with the way the workbooks get formatted)
      if (!all(c("INDICATOR", "UNIT") %in% toupper(names(benchmarks_raw)))){
        benchmarks_raw <- readxl::read_excel(path = filepath,
                                             sheet = sheet_name,
                                             skip = 1)
      }
    }
    ## Change all the header names to use "." instead of " " for consistency
    names(benchmarks_raw) <- gsub(names(benchmarks_raw), pattern = " ", replacement = ".")

    if (!all(c("INDICATOR", "UNIT") %in% toupper(names(benchmarks_raw)))){
      stop("Can't find the expected column headers in the provided benchmark file. Check to make sure that there are no non-header rows before the headers.")
    }
  } else {
    stop(paste("Can't find the benchmark file at", filepath))
  }

  ## Strip out the extraneous columns and rows, which includes if they left the example in there. The pattern to look for is "e.g"
  benchmarks <- benchmarks_raw[!grepl(x = benchmarks_raw$Management.Question, pattern = "^[Ee].g.") & !is.na(benchmarks_raw$Indicator), !grepl(names(benchmarks_raw), pattern = "__\\d+$")]

  ## Create the evaluations strings if asked to!
  if (!is.null(eval_strings)) {
    # Figure out if any are missing (the character "x" is fine though because it's the indicator stand-in)
    varnames <- unlist(eval_strings)[unlist(eval_strings) != "x"]
    missing.varnames <- varnames[!(varnames %in% names(benchmarks))]
    if (length(missing.varnames) > 0) {
      stop(paste("The following expected variables for constructing eval strings are missing:", paste0(missing.varnames, collapse = ", ")))
    }
    # If none were missing, rename each of the vectors with "evalstring" and a suffix number. This will be their variable names in the data frame
    names(eval_strings) <- paste0("evalstring", 1:length(eval_strings))
    # Construct the strings. For each vector in the list
    strings <- lapply(eval_strings, benchmarks = benchmarks, FUN = function(X, benchmarks){
      # For each character string in the vector, grab the values in the matching variable in the benchmarks (or just return the "x")
      vectors <- lapply(X, benchmarks = benchmarks, FUN = function(X, benchmarks){
        if (X %in% names(benchmarks)) {
          return(benchmarks[[X]])
        } else {
          return(X)
        }
      })
      # Paste them together. This gnarly eval(parse()) business is to do it regardless of how many components there are
      output <- eval(parse(text = paste0("paste(", paste0("vectors[[", 1:length(vectors), "]]", collapse = ", "), ")")))
      return(output)
    })
    # Add the strings as variables to the benchmarks data frame
    benchmarks <- cbind(benchmarks, data.frame(strings, stringsAsFactors = FALSE))

    names(benchmarks)[ncol(benchmarks)] <- "evalstring_threshhold"
  }

  return(benchmarks)
}

#' Reading in benchmark groups from the Data Explorer
#'
#' @description This will read in a table of benchmark group membership formatted like the Evaluation Information spreadsheet in the Data Explorer Excel workbook (wide, one variable per benchmark group with \code{"X"} designating membership and \code{NA} indicating no membership). Returns a tall version as a data frame with the unique identifier variables intact and
#' @param path Character string. The path to the folder containing the file \code{filename} OR the full filepath to the file itself if \code{filename = NULL}. Defaults to the working directory as retrieved by \code{getwd()}.
#' @param filename Character string. The full filename plus extension for the file containing the benchmark groups, if it is not part of the string provided as \code{path}. Normally this will be a \code{.xls}, \code{.xlsx}, or \code{.xlsm} file, although a \code{.csv} file will also work. Defaults to \code{NULL}.
#' @param id_var Vector of character strings. One or more character strings corresponding to the variable(s) that contain the unique identifiers for the data frame. Because the assumption is that this function will be used on the Data Explorer Excel workbook, defaults to \code{c("TerrADat Primary Key/LMF Plot Key")}.
#' @param sheet Optional character string. A character string corresponding to the name of the spreadsheet to read from an Excel workbook if that's the target filetype. Ignored if the target file is a \code{.csv}. Because the assumption is that this function will be used on the Data Explorer Excel workbook, defaults to \code{c("Evaluation Information")}.
#' @param ignore_var Vector of character strings. One or One or more character strings corresponding to the variable(s) that contain NEITHER the unique identifiers NOR benchmark group membership in the data frame. Because the assumption is that this function will be used on the Data Explorer Excel workbook, defaults to \code{c("GlobalID")}.
#' @export
read_benchmarkgroups <- function(path = getwd(),
                                 filename = NULL,
                                 id_var = c("TerrADat Primary Key/LMF Plot Key"),
                                 sheet = "Evaluation Information",
                                 ignore_var = c("GlobalID")){
  if (!is.null(filename)) {
    path <- paste0(path, "/", filename)
  }

  if (!file.exists(path)) {
    stop(paste("Unable to find the file", path))
  }

  if (grepl(path, pattern = "\\.csv$", ignore.case = TRUE)) {
    df <- read.csv(path, stringsAsFactors = FALSE)
  } else if (grepl(path, pattern = "\\.xls[xm]{0,1}$", ignore.case = TRUE)) {
    if (is.null(sheet)) {
      df <- readxl::read_excel(path)
    } else {
      df <- readxl::read_excel(path, sheet = sheet)
    }
  } else {
    stop ("The target file must include the file extension. Valid file extensions are .csv, .xls, .xlsx, and .xlsm")
  }

  if (!all(id_var %in% names(df))) {
    stop(paste("The following variable names in id_var were not found in the data:", paste(id_var[!(id_var %in% names(df))], collapse = ", ")))
  }

  # Each variable in the data frame that isn't an id field or explicitly ignored should be a benchmark group with "X" the rows with membership
  # The lapply makes a data frame for each of those variables
  # Then they're combined with rbind()
  df.tall <- do.call(rbind,
                     lapply(names(df)[!(names(df) %in% c(id_var, ignore_var))],
                            df = df,
                            id_var = id_var,
                            FUN = function(X, df, id_var){
                              group <- X
                              output.df <- df[!is.na(df[[group]]), id_var]
                              if(nrow(output.df) < 1) {
                                return(NULL)
                              }
                              output.df$benchmark.group <- group
                              return(output.df)
                            }))

  output <- df.tall

  names(output) <- c("PrimaryKey", "Benchmark.Group")

  return(output)
}

#' Importing Sample Design Databases for AIM Sample Designs
#'
#' This function imports one or more Sample Design Database[s] and returns a list of the named lists sf, pts, and strata. The named lists contain SpatialPoints/PolygonsDataFrames of the sample frame, points, and strata from each of the geodatabases. The SPDFs are named using the filename of the geodatabase source, so that each list has one SPDF named for each geodatabase imported and those names are identical between lists. If a Sample Design Database is missing any one of those features, a NULL value replaces the SPDF.
#' @param src Character string defining the filepath containing the sample design database[s].
#' @param dd.src Character string or character vector containing the filenames of the geodatabases to import. Each filename should include the extension ".gdb"
#' @param split Logical. If \code{TRUE} then databases containing multiple designs will be split so that they appear as separate designs in the output list rather than kept together due to coming from the same database. Defaults to \code{TRUE}.
#' @param omNAdesignations Logical. if \code{TRUE} then any plots with a final designation of \code{NA} are dropped after reading. Defaults to \code{FALSE}.
#' @param projection \code{sp::CRS()} call defining the coordinate reference system. Defaults to NAD83 with \code{sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")}. Is used to reproject all SPDFs in order to perform spatial manipulations.
#' @export

## TODO: Should try to handle raster location/import either within dd.reader() or as an independent function
## Reads in DDs. Returns a named list of lists of SPDFs: sf, pts, strata.
## The index order is maintained as well, so output[1][1] and output[2][1] correspond to the same DD source
## If a feature class couldn't be found, there will be a NULL instead of SPDF for that DD in the list
read.dd <- function(src = "", ## A filepath as a string
                    dd.src, ## A character string or vector of character strings with the filename[s] for the relevant .gdb in the filepath src
                    # validate.keys = T, ## Should the process also produce a data frame in the output of points in the design dtabases that have issues with final designations or TerrAdat primary keys?
                    split = TRUE,
                    omitNAdesignations = FALSE, ## Strip out plots with a final designation value of NA
                    projection = sp::CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83
){

  ## readOGR() wrapped in purrr::safely() so that it will return NULL instead of an error
  ## This means that if a shapefile is missing (most common with strata) the result will be a NULL instead of a showstopper
  safe.readOGR <- purrr::safely(rgdal::readOGR, otherwise = NULL)

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
#' @param tdat.path Character string. The folder path that contains the geodatabase specified in \code{tdat.name}. Defaults to the working directory.
#' @param tdat.name A string of the filename of the geodatabase to import from
#' @export
read.tdat <- function(tdat.path = NULL,
                      tdat.name){
  ## Default to the working directory
  if (is.null(tdat.path)) {
    tdat.path <- getwd()
  }

  ## Construct the full filepath
  filepath <- paste0(tdat.path, "/", tdat.name)
  ## Add the extension if necessary
  if (!grepl(filepath, pattern = "\\.gdb", ignore.case = TRUE)) {
    filepath <- paste0(filepath, ".gdb")
  }
  ## Check to make sure the geodatabase exists
  if (!file.exists(filepath)) {
    stop(paste("Can't find the geodatabase", filepath))
  }
  # Read in the data
  tdat.terrestrial.spdf <- rgdal::readOGR(dsn = filepath,
                                          layer = "SV_IND_TERRESTRIALAIM",
                                          stringsAsFactors = FALSE)
  tdat.remote.spdf <- rgdal::readOGR(dsn = filepath,
                                     layer = "SV_IND_REMOTESENSING",
                                     stringsAsFactors = FALSE)
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
      subset <- setNames(lapply(X = split.list,
                                FUN = function(X, df) {
                                  X[[df]]
                                },
                                df = df),
                         paste0(frame, "_", unique(dd.list$sf[[frame]]$TERRA_SAMPLE_FRAME_ID)))

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
                                 skip = 1
  )

  for (field in names(tracking)[grepl(x = names(tracking), pattern = "date", ignore.case = TRUE) & !grepl(x = names(tracking), pattern = " and ", ignore.case = TRUE)]) {
    tracking[, field] <- lubridate::as_date(as.character(tracking[[field]]))
  }

  return(tracking)
}

#' Return the default indicator lookup table
#' @description The default lookup table for combining the BLM Benchmark Tool table with the data from TerrADat
#' @param tdat_version Version of TerrADat used in the analysis. Defaults to \code{"2"} for AIM 2.0. Also accepts \code{"1"} for AIM 1.0.
#' @export
 
indicator.lookup <- function(tdat_version = "2"){
return(read.csv(paste0(path.package("aim.analysis"), "/defaults/indicator_lut", tdat_version, ".csv"), stringsAsFactors = FALSE)[,c(1,2)])
}

#' Return the default point fate lookup table
#' @description The default lookup table for final point fate designations in sample design databases.
#' @export
fate.lookup <- function(){
  return(read.csv(paste0(path.package("aim.analysis"), "/defaults/fates.csv"), stringsAsFactors = FALSE))
}

#' Read in a shapefile
#' @description Using \code{rgdal::readOGR()}, read in a shapefile.
#' @param filename Character string. The filename of the shapefile to read in. May be the full filepath to the shapefile, in which case use \code{filepath = NULL}. Does not need to include the file extension.
#' @param filepath Optional character string. The filepath to the location where the shapefile matching \code{filename} is stored. Used to locate the file by combining with the filename. Will be ignored if \code{NULL}. Defaults to \code{NULL}.
#' @param projection Optional CRS object. The projection to return the shapefile in as a CRS object, e.g. \code{sp::CRS("+proj=aea")}. Will be ignored if \code{NULL} or if it matches the shapefile's existing projection. Defaults to \code{NULL}.
#' @param stringsAsFactors Logical. To be passed to \code{rgdal::readOGR(stringsAsFactors)}. Defaults to \code{FALSE}.
#' @return Spatial data frame.
#' @export
read_shapefile <- function(filename,
                           filepath = NULL,
                           projection = NULL,
                           stringsAsFactors = FALSE) {
  if (!is.null(projection)) {
    if (length(projection) > 1) {
      stop("Projection must be a CRS object, e.g. sp::CRS('+proj=aea')")
    }
    if (class(projection) != "CRS") {
      stop("Projection must be a CRS object, e.g. sp::CRS('+proj=aea')")
    }
  }

  if (class(filename) != "character") {
    stop("The filename must be a single character string")
  }
  if (length(filename) > 1) {
    stop("The filename must be a single character string")
  }

  filename_has_suffix <- grepl(filename,
                               pattern = "\\.shp",
                               ignore.case = TRUE)

  if (is.null(filepath)) {
    if (filename_has_suffix) {
      filepath_full <- filename
    } else {
      filepath_full <- paste0(filename, ".shp")
    }
  } else {
    filepath <- gsub(filepath,
                     pattern = "/+$",
                     replacement = "")
    if (filename_has_suffix) {
      filepath_full <- paste(filepath, filename, sep = "/")
    } else {
      filepath_full <- paste(filepath, paste0(filename, ".shp"), sep = "/")
    }
  }

  # Can't ask if a layer inside a .gdb exists :/
  if (grepl(filepath_full, pattern = "\\.gdb/", ignore.case = TRUE)) {
    split <- stringr::str_split(filepath_full,
                                pattern = "\\.(gdb|GDB)/")[[1]]
    shapefile <- rgdal::readOGR(dsn = paste0(split[1], ".gdb"),
                                layer = gsub(split[2],
                                             pattern = "\\.shp",
                                             replacement = "",
                                             ignore.case = TRUE),
                                stringsAsFactors = stringsAsFactors)
  } else {
    if (file.exists(filepath_full)) {
      shapefile <- rgdal::readOGR(dsn = filepath_full,
                                  stringsAsFactors = stringsAsFactors)
    } else {
      stop(paste0("Cannot find the file ", filepath_full))
    }
  }


  if (!is.null(projection)) {
    if (!identical(projection, shapefile@proj4string)) {
      shapefile <- sp::spTransform(shapefile,
                                   CRSobj = projection)
    }
  }

  return(shapefile)
}

#' Read in multiple shapefiles
#' @description Using \code{rgdal::readOGR()}, read in one or more shapefiles.
#' @param filenames Character string or vector of character strings. The filename(s) of the shapefile(s) to read in. May be the full filepath(s) to the shapefile(s), in which case use \code{filepath = NULL}. Does not need to include the file extension.
#' @param filepaths Optional character string or vector of character strings. The filepath(s) to the location(s) where the shapefile(s) matching \code{filename} is/are stored. Used to locate the file(s) by combining with the filename(s). Must be \code{NULL} (will be ignored), a single character vector (if only reading one shapefile or if all shapefiles are in the same location), or the same length as \code{filenames} (if the filenames are stored in different locations; the filenames and filepaths are paired by index in \code{filenames} and \code{filepaths}). Defaults to \code{NULL}.
#' @param projection Optional CRS object. The projection to return the shapefile(s) in as a CRS object, e.g. \code{sp::CRS("+proj=aea")}. Will be ignored if \code{NULL} or if it matches the shapefile's existing projection. Defaults to \code{NULL}.
#' @param stringsAsFactors Logical. To be passed to \code{rgdal::readOGR(stringsAsFactors)}. Defaults to \code{FALSE}.
#' @return Spatial data frame.
#' @export
read_shapefiles <- function(filenames,
                            filepaths = NULL,
                            projection = NULL,
                            stringsAsFactors = FALSE) {

  if (is.null(filepaths)) {
    output <- lapply(X = filenames,
                     projection = projection,
                     stringsAsFactors = stringsAsFactors,
                     FUN = read_shapefile)
  } else {
    if (length(filepaths) == 1 | length(filenames) == length(filepaths)) {
      output <- lapply(X = paste(filepaths, filenames, sep = "/"),
                       projection = projection,
                       stringsAsFactors = stringsAsFactors,
                       FUN = read_shapefile)
    } else {
      stop("The number of filepaths provided must be none (if the filenames contain the full paths to the shapefiles), one (if all shapfiles are at the same filepath), or equal to the number of filenames (if one or more shapefile is at a different filepath).")
    }
  }

  return(output)
}
