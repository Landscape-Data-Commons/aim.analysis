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
add_coords <- function(spdf,
                       current.proj = TRUE,
                       xynames = NULL,
                       nad83 = FALSE,
                       albers = FALSE){
  projNAD83 <- sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
  projAL <- sp::CRS("+proj=aea")
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

#' Filtering data frames by dates
#' @description Given a date field and a lower date, an upper date, or both, filter data.
#' @param data Data frame. So long as \code{[]} will work on it, it's fine so spatial data frames are valid.
#' @param date.field Character string. The name of the column/variable in the data frame containing the date values. The date values must be interpretable by \code{lubridate::as_date()}.
#' @param after Optional character string expressing a date in the format \code{YYYY-MM-DD} (or \code{MM-DD} if \code{ignore.year = TRUE}). The earliest date (inclusive) to return data from. Rows containing earlier dates will not be returned.
#' @param before Optional character string expressing a date in the format \code{YYYY-MM-DD} (or \code{MM-DD} if \code{ignore.year = TRUE}). The latest date (inclusive) to return data from. Rows containing later dates will not be returned.
#' @param ignore.year Logical. If \code{TRUE} then years will be ignored in comparisons, e.g. If \code{after = "1986-10-30"} or \code{after = "10-30"} then all rows containing dates from October 30 to December 31 would be returned, regardless of the year. Defaults to \code{FALSE}.
#' @export filter.date
filter.date <- function(data,
                        date.field,
                        after = NULL,
                        before = NULL,
                        ignore.year = FALSE){
  # A few validity checks
  if (is.null(after) & is.null(before)) {
    stop("At least one bounding date must be provided.")
  }

  if (!ignore.year) {
    if (!is.null(after)) {
      if (grepl(after, pattern = "$[0-9]{4}-")) {
        stop("If ignore.year is FALSE then the year must be included in the after date.")
      }
    }
    if (!is.null(before)) {
      if (grepl(before, pattern = "$[0-9]{4}-")) {
        stop("If ignore.year is FALSE then the year must be included in the before date.")
      }
    }
  } else {
    if (!is.null(after)) {
      if (grepl(after, pattern = "^[0-9]{4}-")) {
        message("ignore.year is TRUE so the year will be ignored in the after date.")
      }
    }
    if (!is.null(before)) {
      if (grepl(before, pattern = "^[0-9]{4}-")) {
        message("ignore.year is TRUE so the year will be ignored in the before date.")
      }
    }
  }

  if (any(is.na(data[[date.field]]))) {
    message("Dropping all rows with NA in the date field.")
    data <- data[!is.na(data[[date.field]]),]
  }

  # If no bounds were provided, just set them to dates long before and after any sampling could occur
  if (is.null(after)) {
    after <- "0000-01-01"
  }
  if (is.null(before)) {
    before <- "9999-12-31"
  }

  # Whether years matter or not, we're doing this by year
  years <- unique(lubridate::year(lubridate::as_date(data[[date.field]])))

  # We only have to do it by year because those dang leap years changing the day number
  data.filtered <- lapply(X = years,
                          FUN = function(X,
                                         df,
                                         date.field,
                                         after,
                                         before,
                                         ignore.year){
                            # Get just this one year
                            df <- df[lubridate::year(lubridate::as_date(df[[date.field]])) == X,]

                            # If the year is ignored (e.g., we want the same date range from every year)
                            if (ignore.year) {
                              # Convert the after/before strings to the year being looked at
                              after <- paste0(X,
                                              "-",
                                              stringr::str_extract(after, pattern = "[0-9]{2}-[0-9]{2}$"))
                              before <- paste0(X,
                                               "-",
                                               stringr::str_extract(before, pattern = "[0-9]{2}-[0-9]{2}$"))
                            } else {
                              # Otherwise, filter the points from the wrong years
                              df <- df[lubridate::year(lubridate::as_date(df[[date.field]])) >= lubridate::year(lubridate::as_date(after)) & lubridate::year(lubridate::as_date(df[[date.field]])) <= lubridate::year(lubridate::as_date(before)),]

                              # If there's nothing left, then return the empty data frame
                              if (nrow(df) < 1) {
                                return(df)
                              }
                            }

                            # Filter using the day number.
                            # The check for the years matching is so that if ignore.year is FALSE and we have a situation where the year
                            # being filtered isn't in the year of the boundary, we don't cut it out e.g., after is in 2013, before is in 2018, and df is from 2015
                            if (lubridate::year(lubridate::as_date(df[[date.field]])) == lubridate::year(lubridate::as_date(after))) {
                              df <- df[lubridate::yday(lubridate::as_date(df[[date.field]])) >= lubridate::yday(lubridate::as_date(after)),]
                            }
                            if (lubridate::year(lubridate::as_date(df[[date.field]])) == lubridate::year(lubridate::as_date(before))) {
                              df <- df[lubridate::yday(lubridate::as_date(df[[date.field]])) <= lubridate::yday(lubridate::as_date(before)),]
                            }


                            return(df)
                          },
                          df = data,
                          date.field = date.field,
                          after = after,
                          before = before,
                          ignore.year = ignore.year)

  # Create the output, which involves combining data frames if more than one was made
  # Using rbind() instead of dplyr::bind_rows() in case these are SPDFs. Plus, a loop won't be terrible for so few data frames
  if (length(data.filtered) > 1) {
    output <- data.filtered[[1]]
    for (n in 2:length(data.filtered)) {
      if (nrow(data.filtered[[n]]) > 0) {
        output <- rbind(output, data.filtered[[n]])
      }
    }
  } else {
    output <- data.filtered[[1]]
  }

  return(output)
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
  names(tdat) <- toupper(names(tdat))

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
                     "TS") %>% toupper() %>% unique()

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
  if (tracking.tdat %>% filter(toupper(PLOTSTATUS) %in% target.values, is.na(PLOTKEY)) %>% nrow() > 0) {
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
