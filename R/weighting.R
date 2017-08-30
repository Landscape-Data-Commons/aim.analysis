#' Calculate sample weights for points using design polygons
#' @description Calculate the weight for points in a sample design based on the sample frame or strata used to draw them and the point fate. The outputs are a data frame of the points and their weights, a data frame summary of the fates of the points (optionally by year), and a data frame summary of the strata (optionally by year) if strata were used. No spatial checks are done, so make sure that \code{pts} and \code{frame.spdf} are from the same design, limited to the extent of the original sample frame, and have complete overlap.
#' @param pts Data frame or spatial points data frame. This should be the points information. At minimum, it must contain a field matching the string \code{pts.fatefield} containing string values matching those in \code{target.values}, \code{unknown.values}, etc. If providing \code{pts.groupfield} it must also have a field matching that containing values matching values found in \code{frame.spdf$frame.groupfield}. If \code{date.field} or \code{year.source.field} is provided, then fields matching those must exist.
#' @param pts.fatefield Character string. This must exactly match the name of the field in \code{pts} that contains string values matching those in \code{target.values}, \code{unknown.values}, etc.
#' @param pts.groupfield Optional character string. This must exactly match the name of the field in \code{pts} that contains values matching those found in \code{frame.spdf$frame.groupfield}. This will most often be the field containing strata identities.
#' @param frame.spdf Spatial polygons data frame. This must be the design's sample frame or strata restricted to the sample frame extent. If providing \code{frame.groupfield} it must also have a field matching that containing values matching values found in \code{pts$pts.groupfield}.
#' @param frame.groupfield  Optional character string. This must exactly match the name of the field in \code{frame.spdf} that contains values matching those found in \code{pts$pts.groupfield}. This will most often be the field containing strata identities.
#' @param target.values Character string or character vector. This defines what values in the point fate field count as target points. When using AIM design databases, this should be at minimum \code{c("Target Sampled", "TS")}. This is case insensitive.
#' @param unknown.values Character string or character vector. This defines what values in the point fate field count as unknown points. When using AIM design databases, this should be at minimum \code{c("Unknown", "Unk")}. This is case insensitive.
#' @param nontarget.values Character string or character vector. This defines what values in the point fate field count as non-target points. When using AIM design databases, this should be at minimum \code{c("Non-Target", "NT", NA)}. This is case insensitive.
#' @param inaccessible.values Character string or character vector. This defines what values in the point fate field count as non-target points. When using AIM design databases, this should be at minimum \code{c("Inaccessible")}. This is case insensitive.
#' @param unneeded.values Character string or character vector. This defines what values in the point fate field count as not needed or unneeded points. When using AIM design databases, this should be at minimum \code{c("Not needed")}. This is case insensitive.
#' @param ... Optional character strings. These must exactly match the names of the field in \code{pts} and will be used to group the points beyond the identity/identities they share with the frame. When calculating \code{frame.stats} these will be passed to \code{dplyr::group_by_()}. They will have no impact on \code{frame.summary} or \code{point.weights}. \code{"YEAR"} would be a common string to pass here.
#' @return A list containing the named data frames \code{frame.stats}, \code{frame.summary} (if groupfield strings were provided), and \code{point.weights}.
#' @export
weight.gen <- function(pts,
                       pts.fatefield = NULL, #pts.fatefield
                       pts.groupfield = NULL, #"WEIGHT.ID"
                       frame.spdf,
                       frame.groupfield = NULL, #designstratumfield
                       target.values = NULL,
                       unknown.values = NULL,
                       nontarget.values = NULL,
                       inaccessible.values = NULL,
                       unneeded.values = NULL,
                       ...){
  ## Sanitize
  if (class(pts) == "SpatialPointsDataFrame") {
    working.pts <- pts.spdf@data
  } else {
    working.pts <- pts
  }
  if (class(working.pts) != "data.frame") {
    stop("pts must be either a data frame or a spatial points data frame.")
  }
  working.pts[[pts.fatefield]] <- stringr::str_to_upper(working.pts[[pts.fatefield]])

  if (!all(working.pts[[pts.fatefield]] %in% c(target.values, unknown.values, nontarget.values, inaccessible.values, unneeded.values))) {
    message("The following fate[s] need to be added to the appropriate fate argument[s] in your function call:")
    ## Take the vector of all the unique values in pts.spdf$final_desig (or another fate field) that aren't found in the fate vectors and collapse it into a single string, separated by ", "
    stop(paste(unique(working.pts[[pts.fatefield]][!(working.pts[[pts.fatefield]] %in% c(target.values, unknown.values, nontarget.values, inaccessible.values, unneeded.values))]), collapse = ", "))
  }

  additional.point.groups <- list(...)
  if (!any(additional.point.groups %in% names(working.pts))) {
    message("The following additional grouping fields were not found in pts:")
    stop(paste(additional.point.groups[!(additional.point.groups %in% names(working.pts))], collapse = ", "))
  }

  ## Add areas in hectares to the frame if they're not there already
  if (!("AREA.HA" %in% names(frame.spdf@data))) {
    frame.spdf <- area.add(frame.spdf)
  }

  ## Creating a table of the point counts by point type
  ## Start with adding the point types
  working.pts$key[working.pts[[pts.fatefield]] %in% target.values] <- "Observed.pts"
  working.pts$key[working.pts[[pts.fatefield]] %in% nontarget.values] <- "Unsampled.pts.nontarget"
  working.pts$key[working.pts[[pts.fatefield]] %in% inaccessible.values] <- "Unsampled.pts.inaccessible"
  working.pts$key[working.pts[[pts.fatefield]] %in% unneeded.values] <- "Unsampled.pts.unneeded"
  working.pts$key[working.pts[[pts.fatefield]] %in% unknown.values] <- "Unsampled.pts.unknown"


  ## Here's the summary by key and pts.groupfield and year.field as appropriate
  pts.summary.fields <- c(pts.groupfield[!is.null(pts.groupfield)], additional.point.groups)
  pts.summary <- eval(parse(text = paste0("working.pts %>% ungroup() %>% group_by(key,", paste(pts.summary.fields, collapse = ","), ") %>%
                                          dplyr::summarize(count = n())")))

  ## Spreading that
  pts.summary.wide <- tidyr::spread(data = pts.summary,
                                    key = key,
                                    value = count,
                                    fill = 0)

  ## What kinds of points might be tackled
  point.types <- c("Observed.pts", "Unsampled.pts.nontarget", "Unsampled.pts.inaccessible", "Unsampled.pts.unneeded", "Unsampled.pts.unknown")

  ## We need to know which of the types of points (target, non-target, etc.) are actually represented
  extant.counts <- point.types[point.types %in% names(pts.summary.wide)]

  ## Only asking for summarize() to operate on those columns that exist because if, for example, there's no Unsampled.pts.unneeded column and we call it here, the function will crash and burn
  # This should probably be converted to use !!!rlang::quos() but it works right now so don't sweat it
  frame.stats <- eval(parse(text = paste0("pts.summary.wide %>% group_by(", paste(pts.summary.fields, collapse = ","),") %>%",
                                          "dplyr::summarize(sum(", paste0(extant.counts, collapse = "), sum("), "))")))

  ## Fix the naming becaue it's easier to do it after the fact than write paste() so that it builds names in in the line above
  names(frame.stats) <- stringr::str_replace_all(string = names(frame.stats), pattern = "^sum\\(", replacement = "")
  names(frame.stats) <- stringr::str_replace_all(string = names(frame.stats), pattern = "\\)$", replacement = "")

  ## Add in the missing columns if some point categories weren't represented
  for (name in point.types[!(point.types %in% names(frame.stats))]) {
    frame.stats[, name] <- 0
  }

  frame.stats <- frame.stats[, names(frame.stats) != "NA"]

  ## TODO: Needs to handle a polygon OR a raster for the frame
  ## HERE WE FORK FOR IF THERE ARE STRATA OR NOT
  if (!is.null(frame.groupfield)) {
    ## Because we have strata, use the design stratum attribute
    ## Create a data frame to store the area values in hectares for strata. The as.data.frame() is because it was a tibble for some reason
    area.df <- group_by_(frame.spdf@data, frame.groupfield) %>% dplyr::summarize(AREA.HA.SUM = sum(AREA.HA)) %>% as.data.frame()


    ## Get the sums of point types
    frame.summary <- frame.stats %>% group_by_(pts.groupfield) %>%
      dplyr::summarize(Observed.pts = sum(Observed.pts),
                       Unsampled.pts.nontarget = sum(Unsampled.pts.nontarget),
                       Unsampled.pts.inaccessible = sum(Unsampled.pts.inaccessible),
                       Unsampled.pts.unneeded = sum(Unsampled.pts.unneeded),
                       Unsampled.pts.unknown = sum(Unsampled.pts.unknown))

    ## Add in the areas of the strata
    frame.summary <- merge(x = frame.summary,
                           y = area.df,
                           by.x = pts.groupfield,
                           by.y = frame.groupfield)

    ## Renaming is causing dplyr and tidyr to freak out, so we'll just copy the values into the fieldnames we want
    frame.summary$Stratum <- frame.summary[[pts.groupfield]]
    frame.summary$Area.HA <- frame.summary$AREA.HA.SUM

    ## Calculate the rest of the values
    frame.summary <- frame.summary %>% group_by(Stratum) %>%
      ## The total points, minus the unneeded so we don't penalize projects for them!
      mutate(Total.pts = sum(Observed.pts, Unsampled.pts.nontarget, Unsampled.pts.inaccessible, Unsampled.pts.unknown)) %>%
      ## The proportion of the total points in the stratum that were "target"
      mutate(Prop.dsgn.pts.obsrvd = Observed.pts/Total.pts) %>%
      ## The effective "sampled area" based on the proportion of points that were surveyed
      mutate(Sampled.area.HA = unlist(Area.HA * Prop.dsgn.pts.obsrvd)) %>%
      ## The weight for each point in the stratum is the effective sampled area divided by the number of points surveyed, unknown, and inaccessible in the stratum
      mutate(Weight = Sampled.area.HA/sum(Observed.pts, Unsampled.pts.inaccessible, Unsampled.pts.unknown)) %>% as.data.frame()

    ## When there are NaNs in the calculated fields, replace them with 0
    frame.summary <- tidyr::replace_na(frame.summary, replace = list(Prop.dsgn.pts.obsrvd = 0, Sampled.area.HA = 0, Weight = 0))

    ## Add the weights to the points, but only the observed ones
    for (stratum in frame.summary$Stratum) {
      working.pts$WGT[(working.pts[[pts.fatefield]] %in% target.values) & working.pts[[pts.groupfield]] == stratum] <- frame.summary$Weight[frame.summary$Stratum == stratum]
    }

    ## All the unassigned weights get converted to 0
    working.pts <- tidyr::replace_na(working.pts, replace = list(WGT = 0))

    point.weights <- working.pts

  } else if (is.null(frame.groupfield)) {

    ## Treat it as a single unit for lack of stratification
    area <- sum(frame.spdf@data$AREA.HA)

    ## derive weights
    proportion.observed <- 1 ## initialize - proportion of 1.0 means there were no nonresponses
    wgt <- 0 ## initialize wgt
    sample.area <- 0 ## initialize actual sampled area
    if (sum(frame.stats[, point.types]) > 0) {
      proportion.observed <- sum(frame.stats$Observed.pts)/sum(frame.stats[, c("Observed.pts", "Unsampled.pts.nontarget", "Unsampled.pts.inaccessible", "Unsampled.pts.unknown")]) ## realized proportion of the stratum that was sampled (observed/total no. of points that weren't "unneeded")
    }
    if (sum(frame.stats$Observed.pts) > 0) {
      ## Record the actual area(ha) sampled - (proportional reduction * stratum area)
      sample.area <- proportion.observed*area
      ## (The proportion of the total area that was sampled * total area [ha]) divided by the number of observed, inaccessible, and unknown points
      wgt <- (sample.area)/sum(frame.stats$Observed.pts, frame.stats$Inaccessible.pts, frame.stats$Unknown.pts)

    }

    ##Tabulate key information for this DD
    frame.summary <- data.frame(Stratum = "Frame",
                                Total.pts = sum(frame.stats[, point.types]),
                                Observed.pts = sum(frame.stats$Observed.pts),
                                Unsampled.pts.nontarget = sum(frame.stats$Unsampled.pts.nontarget),
                                Unsampled.pts.inaccessible = sum(frame.stats$Unsampled.pts.inaccessible),
                                Unsampled.pts.unneeded = sum(frame.stats$Unsampled.pts.unneeded),
                                Unsampled.pts.unknown = sum(frame.stats$Unsampled.pts.unknown),
                                Area.HA = area,
                                Prop.dsgn.pts.obsrvd = proportion.observed,
                                Sampled.area.HA = sample.area,
                                Weight = wgt,
                                stringsAsFactors = FALSE)

    point.weights <- working.pts

    ## If there are points to work with, do this
    if (nrow(point.weights) > 0) {
      ## If a point had a target fate, assign the calculates weight
      point.weights$WGT[point.weights[[pts.fatefield]] %in% target.values] <- wgt
      ## If a point had a non-target or unknown designation, assign 0 as the weight
      point.weights$WGT[point.weights[[pts.fatefield]] %in% c(nontarget.values, unknown.values, inaccessible.values, unneeded.values)] <- 0
    }
  }

  ## Make sure that this is in the order we want
  frame.summary <- frame.summary[, c("Stratum",
                                     "Total.pts",
                                     "Observed.pts",
                                     "Unsampled.pts.nontarget",
                                     "Unsampled.pts.inaccessible",
                                     "Unsampled.pts.unneeded",
                                     "Unsampled.pts.unknown",
                                     "Area.HA",
                                     "Prop.dsgn.pts.obsrvd",
                                     "Sampled.area.HA",
                                     "Weight")]

  names(point.weights)[names(point.weights) == "key"] <- "POINT.FATE"

  output <- list("frame.stats" = frame.stats, "frame.summary" = frame.summary, "point.weights" = point.weights)
  return(output[!is.null(output)])
}


#' Calculating Weights from AIM Sample Designs
#'
#' This function takes the output from the function \code{read.dd()} and optionally a SpatialPolygonsDataFrame defining the extent of reporting units. Returns a list of two data frames: strata.weights with the weighting information by stratum and point.weights with the weighting information by point.
#' @param dd.import Output from \code{read.dd()}. This can be the result of running \code{read.dd()} on one or many sample design databases
#' @param combine Logical. If provided multiple DDs, should those be combined as part of the weighting calculations? Otherwise the weights will be calculated on a per-DD basis. Defaults to \code{TRUE}.
#' @param reorder Logical. If provided multiple DDs, should those be reordered to reflect ascending area? If \code{FALSE}, the DDs are considered in the order they appear in dd.import. If combine is \code{FALSE}, then this only potentially affects the order of the output information. Defaults to \code{TRUE}.
#' @param erase Optional character string. If \code{combine} is \code{TRUE} this must either be \code{"arcpy"} or \code{"rgeos"} and determines which approach will be used to frames from one another if \code{combine} is \code{TRUE}. If \code{"arcpy"} is used, then R must have write permissions to the folder \code{temp.path} and a valid install of ArcPy. This is preferable to \code{"rgeos"} because the functions involved tend to crash at random when handling very small remainder geometries. Defaults to \code{"arcpy"}.
#' @param temp.path Optional character string. If \code{combine} is \code{TRUE} and \code{erase} is \code{"arcpy"} this must be the path to a folder that R has write permissions to so that a subfolder called arcpy_temp can be created and used for ArcPy erasure steps. Defulats to the current working directory.
#' @param reporting.units.spdf SpatialPolygonsDataFrame. Optional reporting units polygons to restrict the sample designs to. If provided, weights will be calculated appropriately based on the intersection of this SPDF and the design[s] Defaults to \code{NULL}.
#' @param reportingunitfield Character string. If passing a reporting unit SPDF, what field in it defines the reporting unit[s]?
#' @param target.values Character string or character vector. This defines what values in the point fate field count as target points. The function always looks for "Target Sampled" and "TS", so this argument is only necessary if there are additional values in the sample design databases. This is case insensitive.
#' @param unknown.values Character string or character vector. This defines what values in the point fate field count as unknown points. The function always looks for "Unknown" and "Unk", so this argument is only necessary if there are additional values in the sample design databases. This is case insensitive.
#' @param nontarget.values Character string or character vector. This defines what values in the point fate field count as non-target points. The function always looks for "Non-Target", "NT", and NA, so this argument is only necessary if there are additional values in the sample design databases. This is case insensitive.
#' @param inaccessible.values Character string or character vector. This defines what values in the point fate field count as non-target points. The function always looks for "Inaccessible", so this argument is only necessary if there are additional values in the sample design databases. This is case insensitive.
#' @param unneeded.values Character string or character vector. This defines what values in the point fate field count as not needed or unneeded points. The function always looks for "Not needed", so this argument is only necessary if there are additional values in the sample design databases. This is case insensitive.
#' @param daterange.max Optional character string. This must be interpretable by \code{lubridate::as_date()}, e.g. \code{"2016-04-20"}. Only sampling locations visited before this date will be considered. Currently only restricts to year.
#' @param daterange.min Optional character string. This must be interpretable by \code{lubridate::as_date()}, e.g. \code{"2016-04-20"}. Only sampling locations visited after this date will be considered. Currently only restricts to year.
#' @param fatefieldname Character string defining the field name in the points SPDF[s] in dd.import that contains the point fate. Defaults to \code{"final_desig"}.
#' @param pointstratumfieldname Character string defining the field name in the points SPDF[s] in dd.import that contains the design stratum. Defaults to \code{"dsgn_strtm_nm"}.
#' @param designstratumfield Character string defining the field name in the strata SPDF[s] in dd.import that contains the design stratum. Defaults to \code{"dmnt_strtm"}.
#' @param projection Character string \code{sp::CRS()} argument. Defaults to NAD83 with \code{sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")}. Is used to reproject all SPDFs in order to perform spatial manipulations.
#' @keywords weights
#' @examples
#' weighter()
#' @export

## TODO: Figure out what to do about DDs that have future points
weight <- function(dd.import,
                   combine = TRUE,
                   reorder = TRUE,
                   erase = "arcpy",
                   temp.path = getwd(),
                   reporting.units.spdf = NULL,
                   reportingunitfield = "REPORTING.UNIT",
                   ## Keywords for point fate—the values in the vectors unknown and nontarget are considered nonresponses.
                   target.values = NULL,
                   unknown.values = NULL,
                   nontarget.values = NULL,
                   inaccessible.values = NULL,
                   unneeded.values = NULL,
                   daterange.max = NULL,
                   daterange.min = NULL,
                   ## These shouldn't need to be changed from these defaults, but better to add that functionality now than regret not having it later
                   fatefieldname = "final_desig",
                   pointstratumfieldname = "dsgn_strtm_nm",
                   designstratumfield = "dmnnt_strtm",
                   projection = sp::CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"),
                   sliverdrop = TRUE,
                   sliverwarn = TRUE,
                   sliverthreshold = 0.01
){
  if (!(stringr::str_to_upper(erase) %in% c("ARCPY", "RGEOS"))) {
    stop("erase must be either 'arcpy' or 'rgeos'.")
  }
  if (erase == "arcpy" & (is.null(temp.path) | !is.character(temp.path))) {
    stop("If erase is 'arcpy' a valid filpath must be provided as temp.path.")
  }
  ## Sanitization
  if (!is.null(reporting.units.spdf)) {
    names(reporting.units.spdf@data) <- stringr::str_to_upper(names(reporting.units.spdf@data))
  }
  fatefieldname <- stringr::str_to_upper(fatefieldname)
  pointstratumfieldname <- stringr::str_to_upper(pointstratumfieldname)
  designstratumfield <- stringr::str_to_upper(designstratumfield)
  reportingunitfield <- stringr::str_to_upper(reportingunitfield)

  ## Initialize data frame for stratum info. The results from each loop end up bound to this
  master.df <- NULL
  ## Initialize data frame for point weight info. The results from each loop end up added to this
  ## In the end, these will all be joined to TerrADat and stripped down to the bare essentials to report out
  pointweights.df <- NULL
  ## Initialize data frame for stratum info, specifically the point counts per year per stratum per project
  stats.df <- NULL

  ## The fate values that we know about are brought from defaults/fates.csv with fate.lookup()
  fate.lut <- fate.lookup()
  fate.list <- lapply(unique(fate.lut$fate), function(X, df){
    df$fate.value[df$fate == X]
  }, df = fate.lut) %>% setNames(unique(fate.lut$fate))

  ## Whatever values are provided in the function arguments get concatenated and then we keep only the unique values from that result
  target.values <- c(target.values,
                     fate.list$'Target Sampled') %>% toupper() %>% unique()
  unknown.values <- c(unknown.values,
                      fate.list$'Unknown') %>% toupper() %>% unique()
  nontarget.values <- c(nontarget.values,
                        fate.list$'Non-target') %>% toupper() %>% unique()
  inaccessible.values <- c(inaccessible.values,
                           fate.list$'Inaccessible') %>% toupper() %>% unique()
  unneeded.values <- c(unneeded.values,
                       fate.list$'Unneeded') %>% toupper() %>% unique()

  ## In case the DDs are from different generations, we need to restrict them to only the shared fields
  if (length(dd.import$pts) > 1) {
    fieldnames.common <- lapply(dd.raw$pts, names) %>% unlist() %>%
      data.frame(fields = ., stringsAsFactors = FALSE) %>%
      dplyr::group_by(fields) %>% dplyr::summarize(count = n()) %>%
      dplyr::filter(count == length(dd.raw$pts)) %>% .$fields
  }



  ## We need to work our way up from the smallest-framed DD to the largest, so we need to establish that order starting with this initialized list to work from
  dd.order <- list()

  ## For each DD that was imported, bring in the points and the frame (strata if they exist, otherwise the sample frame) and clip them to reporting units if appropriate
  for (s in names(dd.import$sf)) {
    ## First, bring in the relevant SPDFs
    ## Get the pts file in dd.src that corresponds to s and call it pts.spdf, then create and init the WGT attribute
    pts.spdf <- dd.import$pts[[s]]
    ## Restrict only to the shared fields so there aren't rbind errors later
    if (length(dd.import$pts) > 1) {
      pts.spdf@data <- pts.spdf@data[, fieldnames.common]
    }
    pts.spdf@data[, fatefieldname] <- stringr::str_to_upper(pts.spdf@data[, fatefieldname])
    pts.spdf@data$WGT <- 0
    ## Add in the REPORTING.UNITS field with the value "Unspecified" if it's not there already.
    ## The only way it'd already be there is if the points were restricted coming in, which is currently impossible
    if (!("REPORTING.UNIT" %in% names(pts.spdf@data))) {
      pts.spdf@data$REPORTING.UNIT <- "Unspecified"
    }
    ## Add in the coordinates
    pts.spdf <- add.coords(spdf = pts.spdf,
                           xynames = c("LONGITUDE", "LATITUDE"))

    ## Creating the weight identity field.
    ## This will let us analyze the points whether or not there are new points from other designs being added in that have inherited new identities
    ## Basically, when combining designs, I need a field I can write their new identities into and that I can use no matter what to run the weight calculations later
    pts.spdf@data$WEIGHT.ID <- pts.spdf@data[, pointstratumfieldname]

    ## Get the stratum SPDF for this DD and call it frame.spdf
    frame.spdf <- dd.import$strata[[s]]
    ## If the frame.spdf was actually NULL, then grab the sample frame to use instead
    if (is.null(frame.spdf)) {
      frame.spdf <- dd.import$sf[[s]]
    }


    ## If there's a reporting.units.spdf provided, then we'll assign those identities to the SPDFs from dd.import and restrict by them reporting.units.spdf
    if (!is.null(reporting.units.spdf)) {
      ## Deal with the points
      pts.spdf <- attribute.shapefile(spdf1 = pts.spdf,
                                      spdf2 = reporting.units.spdf,
                                      newfield = "REPORTING.UNIT",
                                      attributefield = reportingunitfield)
      ## Overwrite whatever value was brought in from the reporting.units.spdf with TRUE because we only want to know if they were restricted or not
      # pts.spdf@data$REPORTING.UNIT.RESTRICTED <- TRUE
      ## Deal with frame.spdf. This involves an intersection and therefore is slow
      frame.spdf.intersect <- intersect(spdf1 = frame.spdf,
                                        ## This will use the appropriate field for strata or sample frame
                                        spdf1.attributefieldname = c("TERRA_SAMPLE_FRAME_ID", designstratumfield)[(c("TERRA_SAMPLE_FRAME_ID", designstratumfield) %in% names(frame.spdf@data))],
                                        spdf2 = reporting.units.spdf,
                                        spdf2.attributefieldname = reportingunitfield)
      ## Replace frame.spdf with this new thing, which is actually the weight categories!
      # frame.spdf <-  frame.spdf.intersect[, c(names(frame.spdf.intersect@data)[!(names(frame.spdf.intersect@data) %in% names(reporting.units.spdf@data))])]
      frame.spdf <-  frame.spdf.intersect
      ## Add REPORTING.UNIT.RESTRICTED
      frame.spdf@data$REPORTING.UNIT.RESTRICTED <- TRUE
    }

    ## Add the area to frame.spdf
    frame.spdf <- area.add(frame.spdf, byid = TRUE)

    ## Add the size of this DD's frame to my list of them so I can use it
    ## We use sum() here because each polygon's area was calculated individually because that streamlines some future use of the area information
    dd.order[s] <- sum(frame.spdf$AREA.HA)

    ## Put the manipulated SPDFs back into the dd.import for future use
    dd.import$pts[[s]] <- pts.spdf
    if (!is.null(dd.import$strata[[s]])) {
      dd.import$strata[[s]] <- frame.spdf
    } else {
      dd.import$sf[[s]] <- frame.spdf
    }
  }

  ## Time to reorder that list of DDs, if the user wants that, otherwise keep the order that they were fed to dd.reader() (and therefore appear in dd.import)
  if (reorder) {
    ## Turn the lsit into a vector and then sort it in ascending order
    dd.order <- unlist(dd.order)[dd.order %>% unlist() %>% sort.list(decreasing = FALSE)]
    ## Then take the names of the DDs assigned to those values because we want those, not the areas
    dd.order <- names(dd.order)
  } else {
    dd.order <- names(dd.order)
  }


  ## Initialize our vector of already-considered DDs which we'll use to work our way out sequentially through dd.order
  dd.completed <- c()

  ## Loop through each DD starting with the smallest-framed one and working up
  for (s in dd.order) {
    print(paste("Currently s is", s))
    ## Bring in this DD's frame, either strata or sample frame as appropriate
    frame.spdf <- dd.import$strata[[s]]
    if (is.null(frame.spdf)) {
      frame.spdf <- dd.import$sf[[s]]
    }
    if (is.null(frame.spdf)) {
      message("frame.spdf is NULL so no weight calculations will be done")
    } else {
      ## If this isn't the first pass through the loop, then the areas on the frame are incorrect because the SPDF has been subjected to erase()
      ## We also need to make sure that there's something left to even run this on, hence the nrow()
      if (!is.null(dd.completed) & !is.null(frame.spdf) > 0) {
        if (nrow(frame.spdf) > 0) {
          print("(Re)calculating areas for frame.spdf")
          frame.spdf <- area.add(frame.spdf)
        }
      }

      ## Bring in this DD's points
      pts.spdf <- dd.import$pts[[s]]

      ## Only do this if the user wants the DDs to be considered as one unit for analysis
      if (combine & length(dd.order) > 1) {
        ## For each DD that hasn't been considered yet:
        ## Retrieve the points, see if they land in this current frame, keep the ones that do as part of the current points, and write it back into dd.import without those
        for (r in dd.order[!(dd.order %in% c(dd.completed, s))]) {
          print(paste("Currently r is", r))
          ## First bring in the points
          pts.spdf.temp <- dd.import$pts[[r]]
          if (nrow(pts.spdf.temp@data) > 0) {
            ## Get a version of the points clipped to the current frame
            pts.spdf.temp.attribute <- attribute.shapefile(spdf1 = pts.spdf.temp,
                                                           spdf2 = frame.spdf,
                                                           ## This will pick the appropriate field for a sample frame or strata
                                                           attributefield = c("TERRA_SAMPLE_FRAME_ID", designstratumfield)[(c("TERRA_SAMPLE_FRAME_ID", designstratumfield) %in% names(frame.spdf@data))],
                                                           newfield = "WEIGHT.ID"
            )

            ## WEIGHT.ID might end up not containing strata after this, but that's fine because the part of the weighting code that looks in it for strata
            ## only runs in the event that WEIGHT.ID inherited strata identities instead of shapefile identities.

            ## If there was overlap then:
            print(paste("!is.null(pts.spdf.temp.attribute) evaluates to", !is.null(pts.spdf.temp.attribute)))
            if (!is.null(pts.spdf.temp.attribute)) {
              print("Inside the !is.null(pts.spdf.temp.attribute) loop, so the next word had better damn well be TRUE")
              print(!is.null(pts.spdf.temp.attribute))
              ## Bind these points to the current DD's
              if (pts.spdf@proj4string@projargs != pts.spdf.temp.attribute@proj4string@projargs) {
                pts.spdf.temp.attribute <- pts.spdf.temp.attribute %>% spTransform(pts.spdf@proj4string)
              }
              pts.spdf <- rbind(pts.spdf, pts.spdf.temp.attribute)
              ## Remove the points that fell in the current frame from the temporary points and write it back into dd.import
              ## This should find all the rows
              dd.import$pts[[r]] <- pts.spdf.temp[-as.numeric(rownames(plyr::match_df(pts.spdf.temp@data, pts.spdf.temp.attribute@data, on = c("TERRA_TERRADAT_ID", "PLOT_NM")))),]
            }
          }


          ## Then bring in the frame
          frame.spdf.temp <- dd.import$strata[[r]]
          if (is.null(frame.spdf.temp)) {
            print(paste("There aren't stratification polygons for", r))
            frame.spdf.temp <- dd.import$sf[[r]]
            if (is.null(frame.spdf.temp)) {
              print(paste("There aren't sample frame polygons for", r))
            }
          }

          ## So many checks to make sure that we aren't getting NULL SPDFs or SPDFS without values in @data to work with
          if (!is.null(nrow(frame.spdf.temp@data)) & !is.null(nrow(frame.spdf@data))) {
            print(paste("There were frame SPDFs for both", s, "and", r))
            if (nrow(frame.spdf.temp@data) > 0 & nrow(frame.spdf@data) > 0) {
              print(paste("Both frames had values in the @data slot"))
              ## Remove the current frame from the temporary frame. This will let us build concentric frame areas as we work up to larger designs through dd.order
              ## Note: I'm not sure what happens if these don't overlap or if x is completely encompassed by y
              print(paste("Attempting to remove the", s, "frame from the", r, "frame with flex.erase()"))

              frame.spdf.temp <- flex.erase(spdf = frame.spdf.temp,
                                            spdf.erase = frame.spdf,
                                            method = erase,
                                            temp.path = data.path)

              print("Erasure complete or at least attempted")

              if (!is.null(frame.spdf.temp)) {
                if (nrow(frame.spdf.temp@data) > 0) {
                  print(paste("!is.null(frame.spdf.temp) evaluates to", !is.null(frame.spdf.temp)))

                } else {
                  print(paste("There were no rows, so writing NULL in instead"))
                  spdf.frame.temp <- NULL
                }
              }

              ## Write that into dd.import
              if (!is.null(dd.import$strata[[r]])) {
                print(paste("Writing the temp frame into the stratification slot for", r))
                dd.import$strata[[r]] <- frame.spdf.temp
              } else {
                print(paste("Writing the temp frame into the sample frame slot for", r))
                dd.import$sf[[r]] <- frame.spdf.temp
              }
            }
          }
        }
      }

      print(paste("Calculating the weights for", s))

      #########################################
      ## RIPE FOR REPLACEMENT WITH A GENERALIZED WEIGHTING FUNCTION
      ## Sanitize
      pts.spdf@data$YEAR <- year.add(pts = pts.spdf@data, date.field = "DT_VST", source.field = "PANEL")[["YEAR"]]
      #### MAKE SURE TO FILTER OUT POINTS FROM THE FUTURE
      working.pts <- pts.spdf@data[!(pts.spdf@data$YEAR > as.numeric(format(Sys.Date(), "%Y"))), ]

      ## Filter out the forbidden date ranges!
      if (!is.null(daterange.max)) {
        working.pts <- working.pts[working.pts$YEAR <= lubridate::year(lubridate::as_date(daterange.max)),]
      }
      if (!is.null(daterange.min)) {
        pts.spdf <- pts.spdf[working.pts$YEAR >= lubridate::year(lubridate::as_date(daterange.min)),]
      }

      ## Now that the clipping and reassigning is all completed, we can start calculating weights
      ## The objects are used in the event that there are no strata in SPDFs that we can use and resort to using the sample frame
      target.count <- nrow(pts.spdf@data[pts.spdf@data[, fatefieldname] %in% target.values,])
      unknown.count <- nrow(pts.spdf@data[pts.spdf@data[, fatefieldname] %in% unknown.values,])
      nontarget.count <- nrow(pts.spdf@data[pts.spdf@data[, fatefieldname] %in% nontarget.values,])
      inaccessible.count <- nrow(pts.spdf@data[pts.spdf@data[, fatefieldname] %in% inaccessible.values,])
      unneeded.count <- nrow(pts.spdf@data[pts.spdf@data[, fatefieldname] %in% unneeded.values,])
      ## How many points had fate values that were found in fate vectors?
      sum <- sum(target.count, unknown.count, nontarget.count, inaccessible.count, unneeded.count)

      ## Let the user know what the 'bad fates' are that need to be added
      if (sum != nrow(pts.spdf)) {
        message("The following fate[s] need to be added to the appropriate fate argument[s] in your function call:")
        ## Take the vector of all the unique values in pts.spdf$final_desig (or another fate field) that aren't found in the fate vectors and collapse it into a single string, separated by ", "
        message(paste(unique(pts.spdf@data[, fatefieldname])[!(unique(pts.spdf@data[, fatefieldname]) %in% c(target.values, unknown.values, nontarget.values, inaccessible.values, unneeded.values))], collapse = ", "))
      }

      ## TODO: Needs to handle a polygon OR a raster df
      ## If the value for the current DD in the list strata is not NULL, then we have a strata SPDF
      if (!is.null(dd.import$strata[[s]])) {
        ## Because we have strata, use the design stratum attribute

        ## Create a data frame to store the area values in hectares for strata. The as.data.frame() is because it was a tibble for some reason
        area.df <- group_by_(frame.spdf@data, designstratumfield) %>% dplyr::summarize(AREA.HA.SUM = sum(AREA.HA)) %>% as.data.frame()

        ## Working points. This is a holdover, but it saves refactoring later code
        working.pts <- pts.spdf@data

        ## Creating a table of the point counts by point type within each stratum by year by project area ID
        working.pts$key[working.pts$FINAL_DESIG %in% target.values] <- "Observed.pts"
        working.pts$key[working.pts$FINAL_DESIG %in% nontarget.values] <- "Unsampled.pts.nontarget"
        working.pts$key[working.pts$FINAL_DESIG %in% inaccessible.values] <- "Unsampled.pts.inaccessible"
        working.pts$key[working.pts$FINAL_DESIG %in% unneeded.values] <- "Unsampled.pts.unneeded"
        working.pts$key[working.pts$FINAL_DESIG %in% unknown.values] <- "Unsampled.pts.unknown"

        ## Filter out points from THE FUTURE
        working.pts <- working.pts %>% filter(!(YEAR > as.numeric(stringr::str_extract(string = base::date(), pattern = "\\d{4}"))))

        ## N.B. I removed the references to the project area because that should be determined by now and not relevant, but you can add this if you need to
        # "TERRA_PRJCT_AREA_ID",
        pts.summary <- working.pts %>% ungroup() %>% group_by(key, WEIGHT.ID, YEAR) %>%
          dplyr::summarize(count = n())

        pts.summary$DD <- s

        ## Spreading that
        pts.summary.wide <- tidyr::spread(data = pts.summary,
                                          key = key,
                                          value = count,
                                          fill = 0)

        ## We need to know which of the types of points (target, non-target, etc.) are represented
        extant.counts <- names(pts.summary.wide)[grepl(x = names(pts.summary.wide), pattern = ".pts")]

        ## N.B. I removed the references to the project area because that should be determined by now and not relevant, but you can add this if you need to
        # "TERRA_PRJCT_AREA_ID",
        ## Only asking for summarize() to operate on those columns that exist because if, for example, there's no Unsampled.pts.unneeded column and we call it here, the function will crash and burn
        stratum.summary <- eval(parse(text = paste0("pts.summary.wide %>% group_by(DD,", "WEIGHT.ID", ", YEAR) %>% dplyr::summarize(sum(", paste0(extant.counts, collapse = "), sum("), "))")))
        ## Fix the naming becaue it's easier to do it after the fact than write paste() so that it builds names in in the line above
        names(stratum.summary) <- stringr::str_replace_all(string = names(stratum.summary), pattern = "^sum\\(", replacement = "")
        names(stratum.summary) <- stringr::str_replace_all(string = names(stratum.summary), pattern = "\\)$", replacement = "")

        ## Add in the missing columns if some point categories weren't represented
        for (name in c("Observed.pts", "Unsampled.pts.nontarget", "Unsampled.pts.inaccessible", "Unsampled.pts.unneeded", "Unsampled.pts.unknown")[!(c("Observed.pts", "Unsampled.pts.nontarget", "Unsampled.pts.inaccessible", "Unsampled.pts.unneeded", "Unsampled.pts.unknown") %in% names(stratum.summary))]) {
          stratum.summary[, name] <- 0
        }

        ## This is where this particular version of the data frame leaves the loop to be returned at the end of the function
        stats.df <- rbind(stats.df, stratum.summary)

        ## N.B. I removed the references to the project area because that should be determined by now and not relevant, but you can add this if you need to
        # "TERRA_PRJCT_AREA_ID",
        stratum.summary <- stratum.summary %>% group_by_("DD", "WEIGHT.ID") %>%
          dplyr::summarize(Observed.pts = sum(Observed.pts),
                           Unsampled.pts.nontarget = sum(Unsampled.pts.nontarget),
                           Unsampled.pts.inaccessible = sum(Unsampled.pts.inaccessible),
                           Unsampled.pts.unneeded = sum(Unsampled.pts.unneeded),
                           Unsampled.pts.unknown = sum(Unsampled.pts.unknown))

        ## Add in the areas of the strata
        stratum.summary <- merge(x = stratum.summary,
                                 y = area.df,
                                 by.x = "WEIGHT.ID",
                                 by.y = designstratumfield)

        ## Renaming is causing dplyr and tidyr to freak out, so we'll just copy the values into the fieldnames we want
        stratum.summary$Stratum <- stratum.summary$WEIGHT.ID
        stratum.summary$Area.HA <- stratum.summary$AREA.HA.SUM

        ## Calculate the rest of the values
        stratum.summary <- stratum.summary %>% group_by(Stratum) %>%
          ## The total points
          mutate(Total.pts = sum(Observed.pts, Unsampled.pts.nontarget, Unsampled.pts.inaccessible, Unsampled.pts.unneeded, Unsampled.pts.unknown)) %>%
          ## The proportion of the total points in the stratum that were "target"
          mutate(Prop.dsgn.pts.obsrvd = Observed.pts/Total.pts) %>%
          ## The effective "sampled area" based on the proportion of points that were surveyed
          mutate(Sampled.area.HA = unlist(Area.HA * Prop.dsgn.pts.obsrvd)) %>%
          ## The weight for each point in the stratum is the effective sampled area divided by the number of points surveyed in the stratum
          mutate(Weight = Sampled.area.HA/Observed.pts) %>% as.data.frame()

        ## I'm not sure who requested this feature, but it's here now
        if (!is.null(reporting.units.spdf)) {
          stratum.summary$Reporting.Unit.Restricted <- TRUE
        } else {
          stratum.summary$Reporting.Unit.Restricted <- FALSE
        }

        ## Getting just the columns we want in the order we want them
        stratum.summary <- stratum.summary[, c("DD",
                                               "Stratum",
                                               "Total.pts",
                                               "Observed.pts",
                                               "Unsampled.pts.nontarget",
                                               "Unsampled.pts.inaccessible",
                                               "Unsampled.pts.unneeded",
                                               "Unsampled.pts.unknown",
                                               "Area.HA",
                                               "Prop.dsgn.pts.obsrvd",
                                               "Sampled.area.HA",
                                               "Weight",
                                               "Reporting.Unit.Restricted")]

        ## When there are NaNs in the calculated fields, replace them with 0
        stratum.summary <- replace_na(stratum.summary, replace = list(Prop.dsgn.pts.obsrvd = 0, Sampled.area.HA = 0, Weight = 0))

        ## rbind() to the master.df so we can spring it out of the loop and return it from the function
        master.df <- rbind(master.df, stratum.summary)

        ## Add the weights to the points
        for (stratum in stratum.summary$Stratum) {
          working.pts$WGT[(working.pts$FINAL_DESIG %in% target.values) & working.pts[, "WEIGHT.ID"] == stratum] <- stratum.summary$Weight[stratum.summary$Stratum == stratum]
        }

        ## All the unassigned weights get converted to 0
        working.pts <- replace_na(working.pts, replace = list(WGT = 0))

        pointsweights.current <- working.pts[, c("TERRA_TERRADAT_ID", "PLOT_NM", "REPORTING.UNIT", "FINAL_DESIG", "WEIGHT.ID", "WGT", "LONGITUDE", "LATITUDE")]

        ## OKAY. So, when reporting units were used to restrict the design[s], we need to adjust the weights appropriately by weight categories.
        ## Luckily, the frame.spdf that we made waaaaay back when the reporting units were applied already represents those and frame.spdf will have the field UNIQUE.IDENTIFIER only if it went through that process
        ## If the field UNIQUE.IDENTIFIER is present, we know that we can use frame.spdf to adjust the weights on the points
        if ("UNIQUE.IDENTIFIER" %in% names(frame.spdf@data)) {
          pointsweights.current <- weight.adjust(points = pointsweights.current,
                                                 wgtcat.spdf = frame.spdf,
                                                 spdf.area.field = "AREA.HA.UNIT.SUM",
                                                 spdf.wgtcat.field = "UNIQUE.IDENTIFIER")
          pointsweights.current <- replace_na(pointsweights.current, replace = list(ADJWGT = 0))
        } else {
          ## We're going to put in the field regardless of weight adjustment so that we output a consistent data frame
          pointsweights.current$ADJWGT <- NA
        }
        ## Add the point SPDF now that it's gotten the extra fields to the list of point SPDFs so we can use it after the loop
        pointweights.df <- rbind(pointweights.df, pointsweights.current[, names(pointsweights.current)[names(pointsweights.current) != "PLOTKEY"]])
      } else if (!is.null(frame.spdf)) {
        ## If there aren't strata available to us in a useful format in the DD, we'll just weight by the sample frame
        ## since we lack stratification, use the sample frame to derive spatial extent in hectares
        area <- sum(frame.spdf@data$AREA.HA)

        ## derive weights
        Pprop <- 1 ## initialize - proportion of 1.0 means there were no nonresponses
        wgt <- 0 ## initialize wgt
        Sarea <- 0 ## initialize actual sampled area
        if (sum > 0) {
          Pprop <- target.count/sum ## realized proportion of the stratum that was sampled (observed/total no. of points)
        }
        if (target.count > 0) {
          wgt   <- (Pprop*area)/target.count  ## (The proportion of the total area that was sampled * total area [ha]) divided by the no. of observed points
          Sarea <-  Pprop*area	      ## Record the actual area(ha) sampled - (proportional reduction * stratum area)
        }

        ##Tabulate key information for this DD
        temp.df <- data.frame(DD = s,
                              Stratum = "Sample Frame",
                              Total.pts = sum,
                              Observed.pts = target.count,
                              Unsampled.pts.nontarget = nontarget.count,
                              Unsampled.pts.inaccessible = inaccessible.count,
                              Unsampled.pts.unneeded = unneeded.count,
                              Unsampled.pts.unknown = unknown.count,
                              Area.HA = area,
                              Prop.dsgn.pts.obsrvd = Pprop,
                              Sampled.area.HA = Sarea,
                              Weight = wgt,
                              Reporting.Unit.Restricted = FALSE,
                              stringsAsFactors = FALSE)
        if (!is.null(reporting.units.spdf)) {
          temp.df$Reporting.Unit.Restricted <- TRUE
        }

        ## Bind this stratum's information to the master.df initialized outside and before the loop started
        master.df <- rbind(master.df, temp.df)

        pointsweights.current <- pts.spdf@data

        ## If there are points to work with, do this
        if (nrow(pointsweights.current) > 0) {
          ## If a point had a target fate, assign the calculates weight
          pointsweights.current$WGT[pointsweights.current[, fatefieldname] %in% target.values] <- wgt
          ## If a point had a non-target or unknown designation, assign 0 as the weight
          pointsweights.current$WGT[pointsweights.current[, fatefieldname] %in% c(nontarget.values, unknown.values, inaccessible.values, unneeded.values)] <- 0

          if ("UNIQUE.IDENTIFIER" %in% names(frame.spdf@data)) {
            pointsweights.current <- weight.adjust(points = pointsweights.current,
                                                   wgtcat.spdf = frame.spdf,
                                                   spdf.area.field = "AREA.HA.UNIT.SUM",
                                                   spdf.wgtcat.field = "UNIQUE.IDENTIFIER")
            pointsweights.current <- replace_na(pointsweights.current, replace = list(ADJWGT = 0))
          } else {
            ## We're going to put in the field regardless of weight adjustment so that we output a consistent data frame
            pointsweights.current$ADJWGT <- NA
          }

          ## Add the point SPDF now that it's gotten the extra fields to the list of point SPDFs so we can use it after the loop
          pointweights.df <- rbind(pointweights.df, pointsweights.current[, c("TERRA_TERRADAT_ID", "PLOT_NM", "REPORTING.UNIT", "FINAL_DESIG", "WEIGHT.ID", "WGT", "LONGITUDE", "LATITUDE", "ADJWGT")])
        }
      }
      #########################################
    }
    ## Add this DD to the vector that we use to screen out points from consideration above
  }

  ## Diagnostics in case something goes pear-shaped
  # if (length(pointweights.df.merged$PLOTID[!(unique(pointweights.df.merged$PLOTID) %in% unique(pointweights.df.merged$PLOTID))]) > 0) {
  #   print("Somehow the following points were in the DD and weighted, but had no counterpart in the provided TerrADAT")
  #   print(paste(pointweights.df.merged$PLOTID[!(unique(pointweights.df.merged$PLOTID) %in% unique(pointweights.df.merged$PLOTID))], collapse = ", "))
  # }

  ## Rename the fields to what we want them to be in the output
  names(pointweights.df)[names(pointweights.df) == "TERRA_TERRADAT_ID"] <- "PRIMARYKEY"
  names(pointweights.df)[names(pointweights.df) == "PLOT_NM"] <- "PLOTID"

  ## Output is a named list with three data frames: information about the strata, strata weights, and information about the points
  return(list(strata.weights = master.df,
              point.weights = pointweights.df[, c("PRIMARYKEY", "PLOTID", "REPORTING.UNIT", "FINAL_DESIG", "WEIGHT.ID", "WGT", "ADJWGT", "LONGITUDE", "LATITUDE")],
              strata.stats = stats.df
  ))
}


#' Adjusting Weights Calculated from AIM Sample Designs
#'
#' This function takes the point weights data frame output from the function \code{weight()} and a SpatialPolygonsDataFrame defining the weight categories. Returns the data frame supplied as points with the new column \code{ADJWGT} containing the adjusted weights.
#' @param points Data frame output from \code{weight()}, equivalent to \code{weight()[["point.weights"]]} or \code{weight()[[2]]}.
#' @param wgtcat.spdf SpatialPolygonsDataFrame describing the weight categories for adjusting the weights. Use the output from \code{intersect()}.
#' @param spdf.area.field Character string defining the field name in \code{wgtcat@data} that contains the areas for the weight categories. Defaults to \code{"AREA.HA.UNIT.SUM"}.
#' @param spdf.wgtcat.field Character string defining the field name in \code{wgtcat@data} that contains the unique identification for the weight categories. Defaults to \code{"UNIQUE.IDENTIFIER"}.
#' @param projection \code{sp::CRS()} argument. Defaults to NAD83 with \code{sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")}. Is used to reproject all SPDFs in order to perform spatial manipulations.
#' @keywords weights
#' @examples
#' weight.adjuster()
#' @export

weight.adjust <- function(points,
                          wgtcat.spdf,
                          spdf.area.field = "AREA.HA.UNIT.SUM",
                          spdf.wgtcat.field = "UNIQUE.IDENTIFIER",
                          projection = sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
){
  ## Sanitization
  names(points) <- stringr::str_to_upper(names(points))
  names(wgtcat.spdf@data) <- stringr::str_to_upper(names(wgtcat.spdf@data))
  spdf.area.field <- stringr::str_to_upper(spdf.area.field)
  spdf.wgtcat.field <- stringr::str_to_upper(spdf.wgtcat.field)

  ## Convert points to an SPDF
  points.spdf <- SpatialPointsDataFrame(coords = points[, c("LONGITUDE", "LATITUDE")],
                                        data = points,
                                        proj4string = projection)

  ## Attribute the points.spdf with the wgtcat identities from wgtcat.spdf
  points.spdf <- attribute.shapefile(spdf1 = points.spdf,
                                     spdf2 = wgtcat.spdf,
                                     attributefield = spdf.wgtcat.field,
                                     newfield = spdf.wgtcat.field)

  ## Add the areas in using the unique identifier
  data.current <- merge(x = points.spdf@data,
                        y = distinct(wgtcat.spdf@data[, c(spdf.wgtcat.field, spdf.area.field)]))

  ## The weighted points attributed by the combination of reporting units and strata
  ## We first restrict to the points that inherited identities (this should've already happened in the previous step, but just to be safe)
  # data.current <- data.attributed[!is.na(data.attributed[, points.wgtcat.field]),]

  ## We want to include all the points. So we make a logical vector of just T with a length equal to the number of plots
  sites.current <- (rep(TRUE, nrow(data.current)))

  ## Grab the current weights from those points as its own vector
  wgt.current <- data.current$WGT

  ## NB: The identity inherited from the shapefile needs to match the field used for name in framesize
  wtcat.current <- data.current[, spdf.wgtcat.field]

  ## The framesize information about each of the unique wgtcat identities
  ## I currently have this as an area, but I think it needs to be the inverse of the proportion of the area of the reporting unit that each identity represents
  ## so the framesize value for a particular wgtcat = [area of the whole spdf]/[area of particular wgtcat]
  framesize.current <- wgtcat.spdf@data[, spdf.area.field]
  names(framesize.current) <- wgtcat.spdf@data[, spdf.wgtcat.field]

  ## Run the weight adjustment
  data.current$ADJWGT <- spsurvey::adjwgt(sites.current, wgt.current, wtcat.current, framesize.current)

  return(data.current)
}
