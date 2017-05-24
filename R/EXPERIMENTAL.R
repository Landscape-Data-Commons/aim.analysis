## 5/22/2017
## PlotTracking - uses the PlotTracking spreadsheet to populate the dd.  Used whenever the dd has not been updated
##                with the PlotTracking info.

## Checks to make sure that the PLOTID in the PlotTracking spreadsheet occurs in TerraDat (if FINAL_DESIG==TS, then PLOTID must occur in TerraDat), and that  all PLOTIDs of the spreadsheet  occurs in the dd.
## If not, the errors are printed out (but processing continues).  THERE is a stand-alone version of the following processing that performs these checks and allows one to fix the problems before running
## the weighting procedures.  HOWEVER, to ensure that fixes were in fact implemented properly, these checks are retained in this module.


##  path must contain the path(s) of the plot tracking spreadsheet & correspond to the order of dds in dd.src (which should be the same order as in
##                the named list called workinglist.

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

  for (field in names(tracking)[grepl(x = names(tracking), pattern = "date", ignore.case = T) & !grepl(x = names(tracking), pattern = " and ", ignore.case = T)]) {
    tracking[, field] <- lubridate::as_date(as.character(tracking[[field]]))
  }

  return(tracking)
}

#' Apply project tracking Excel files to imported Design Databases.
#' @description Imports plot tracking worksheets used during an AIM project and uses them to assigns statuses to imported Design Databases.
#' @return Returns \code{dd.list} with sampling dates and statuses added from the plot tracking Excel files.
#' @param filenames A character vector of the filenames (including extension) of the project tracking Excel files to import. If not using the \code{path} argument, the filename should include the entire filepath.
#' @param path Optional string specifying a common filepath containing the project tracking sheets to read in. This will be prepended to the values in \code{filenames}. If the tracking sheets are in different folder paths, do not provide this.
#' @param dd.list Output from \code{read.dd()}.
#' @param dd.names An optional character string vector of Design Database names from \code{dd.list} to compare against the plot tracking Excel files. If not provided, all of the Design Databases represented in \code{dd.list} will be compared and updated.
#' @param tdat Output from \code{read.tdat()}.
#' #' @param target.values Character string or character vector. This defines what values in the point fate field count as target points. The function always looks for "Target Sampled" and "TS", so this argument is only necessary if there are additional values in the sample design databases. This is case insensitive
#' @param deleteoverdraw Logical. If \code{T} then unsampled overdraw points will be dropped. Defaults to \code{T}.
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
  names(tdat) <- names(tdat) %>% stringr::str_to_upper()

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
                     "TS") %>% str_to_upper() %>% unique()

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
  tracking.tdat$PLOTSTATUS[is.na(tracking.tdat$PLOTSTATUS) & !grepl(x = tracking.tdat$PANEL, pattern = "over", ignore.case = T)] <- "Unknown"

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
                        y = tracking.tdat %>% dplyr::filter(grepl(x = PANEL, pattern="over", ignore.case = T)),
                        by.x = "PLOT_NM",
                        by.y = "PLOTID") %>% dplyr::filter(!grepl(x = PT_DRAW, pattern = "over", ignore.case = T))
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
      pts <- pts %>% drop.values(variable = "PANEL", dropvalue = "over", ignore.case = T)
    }
    ####################  Store final pts file
    dd.list$pts[[dd]] <- pts
  } ## End of loop through dd.names

  ## Return the modified dd pts file(s) contained in the named list
  return(dd.list)

} ## end of function

#' Quickly drop observations from a data frame based on the values of a single variable
#' @description This wrapper for \code{dplyr::filter()} will take a data frame (or Spatial Data Frame if the package \code{spdplyr} is installed) and remove all observations where the given variable meets the value of the argument \code{dropvalue}.
#' @param df A data frame or, if \code{spdplyr} is installed, spatial data frame to manipulate.
#' @param variable A character string specifying the name of the variable to base the filtering on.
#' @param dropvalue The value to drop observations based on. Can be either a regular expression as a character string to be passed to \code{grepl()} or \code{NA}. All observations where the variable \code{variable} return \code{T} when checked against this will be dropped. Defaults to \code{NA}.
#' @param ignore.case Logical. If \code{dropvalue} is a regular expression, then this argument is passed to \code{grepl()} to decide if the search is case sensitive or not. Defaults to \code{T}.
#' @return The data frame \code{df} without the observations where \code{variable} matched \code{dropvalue}.
#' @export
drop.values <- function(df, variable = "", dropvalue = NA, ignore.case = T) {
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
