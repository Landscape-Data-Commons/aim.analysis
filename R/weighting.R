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
# @param ... Optional character strings. These must exactly match the names of the field in \code{pts} and will be used to group the points beyond the identity/identities they share with the frame. When calculating \code{frame.stats} these will be passed to \code{dplyr::group_by_()}. They will have no impact on \code{frame.summary} or \code{point.weights}. \code{"YEAR"} would be a common string to pass here.
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
                       unneeded.values = NULL){
  ## Sanitize
  if (class(pts) == "SpatialPointsDataFrame") {
    working.pts <- pts@data
  } else {
    working.pts <- pts
  }
  if (class(working.pts) != "data.frame") {
    stop("pts must be either a data frame or a spatial points data frame.")
  }
  working.pts[[pts.fatefield]] <- toupper(working.pts[[pts.fatefield]])

  if (!all(working.pts[[pts.fatefield]] %in% c(target.values, unknown.values, nontarget.values, inaccessible.values, unneeded.values))) {
    message("The following fate[s] need to be added to the appropriate fate argument[s] in your function call:")
    ## Take the vector of all the unique values in pts.spdf$final_desig (or another fate field) that aren't found in the fate vectors and collapse it into a single string, separated by ", "
    stop(paste(unique(working.pts[[pts.fatefield]][!(working.pts[[pts.fatefield]] %in% c(target.values, unknown.values, nontarget.values, inaccessible.values, unneeded.values))]), collapse = ", "))
  }

  # additional.point.groups <- list(...)
  # if (!any(additional.point.groups %in% names(working.pts))) {
  #   message("The following additional grouping fields were not found in pts:")
  #   stop(paste(additional.point.groups[!(additional.point.groups %in% names(working.pts))], collapse = ", "))
  # }
  # additional.point.groups <- rlang::quos(...)

  ## Add areas in hectares to the frame if they're not there already
  if (!("AREA.HA" %in% names(frame.spdf@data))) {
    frame.spdf <- add.area(frame.spdf)
  }

  ## Creating a table of the point counts by point type
  ## Start with adding the point types
  working.pts$key[working.pts[[pts.fatefield]] %in% target.values] <- "Observed.pts"
  working.pts$key[working.pts[[pts.fatefield]] %in% nontarget.values] <- "Unsampled.pts.nontarget"
  working.pts$key[working.pts[[pts.fatefield]] %in% inaccessible.values] <- "Unsampled.pts.inaccessible"
  working.pts$key[working.pts[[pts.fatefield]] %in% unneeded.values] <- "Unsampled.pts.unneeded"
  working.pts$key[working.pts[[pts.fatefield]] %in% unknown.values] <- "Unsampled.pts.unknown"


  ## Here's the summary by key and pts.groupfield and year.field as appropriate
  pts.summary.fields <- c(pts.groupfield[!is.null(pts.groupfield)])

  pts.summary <- eval(parse(text = paste0("dplyr::summarize(.data = group_by(.data = ungroup(working.pts), key, ", paste(pts.summary.fields, collapse = ", "),"), count = n())")))

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
  names(points) <- toupper(names(points))
  names(wgtcat.spdf@data) <- toupper(names(wgtcat.spdf@data))
  spdf.area.field <- toupper(spdf.area.field)
  spdf.wgtcat.field <- toupper(spdf.wgtcat.field)

  ## Convert points to an SPDF
  points.spdf <- SpatialPointsDataFrame(coords = points[, c("LONGITUDE", "LATITUDE")],
                                        data = points,
                                        proj4string = projection)

  ## Attribute the points.spdf with the wgtcat identities from wgtcat.spdf
  points.spdf <- attribute.shapefile(spdf1 = points.spdf,
                                     spdf2 = wgtcat.spdf,
                                     attributefield = spdf.wgtcat.field,
                                     newfield = spdf.wgtcat.field)

  if (is.null(points.spdf)) {
    message("There was no overlap between the points and the wgtcat polygons. Returning NULL.")
    return(NULL)
  } else {
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
}
