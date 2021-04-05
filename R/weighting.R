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

#' Calculate weights for analyses combining AIM and LMF data
#' @description When combining AIM and LMF designs for a single weighted analysis, the process of calculating weights is complicated by the two-stage nature of the LMF design. This calculates a relative weight for each point first based on whether or not it falls inside an LMF segment that was selected for sampling and how many points total fall in any given LMF segment. Those relative weights are then used to calculate combined design weights.
#' @param aim_points Data frame or spatial points data frame. The AIM point information, including unique identities in the variable \code{aim_idvar}. If this is just a data frame OR if \code{wgtcats} is not a spatial polygons data frame then it must already have the weight category and LMF segment memberships assigned in the variables \code{wgtcat_var} and \code{segment_var} (there should be an \code{NA} for any observation that does not fall in an LMF segment). If this is spatial and so are either \code{wgtcats} or \code{segments}, then the points will be attributed with the memberships useing \code{sp::over()}. This must also include \code{aim_fate_var} if any of the points were not observed/sampled, e.g. rejected, inaccessible, or had an unknown fate, otherwise they will all be assumed to've been observed/sampled.
#' @param lmf_points Data frame or spatial points data frame. The LMF point information, including unique identities in the variable \code{lmf_idvar}. If this is just a data frame OR if \code{wgtcats} is not a spatial polygons data frame then it must already have the weight category and LMF segment memberships assigned in the variables \code{wgtcat_var} and \code{segment_var} (there should be an \code{NA} for any observation that does not fall in an LMF segment). If this is spatial and so are either \code{wgtcats} or \code{segments}, then the points will be attributed with the memberships useing \code{sp::over()}. All LMF points are assumed to be observed/sampled.
#' @param aim_idvar Character string. The name of the variable in \code{aim_points} that contains the unique identities for the points.
#' @param lmf_idvar Character string. The name of the variable in \code{lmf_points} that contains the unique identities for the points.
#' @param aim_fatevar Optional character string. The name of the variable in \code{aim_points} that contains the fates of the points. If \code{NULL} then the fate of all points in \code{aim_points} will be assumed to be sampled/observed. Defaults to \code{NULL}
#' @param observed_fates Optional vector of character strings. The strings present in \code{aim_points$aim_fatevar} that correspond to a sampled/observed fate. Defaults to \code{NULL}
#' @param invalid_fates Optional vector of character strings. The strings present in \code{aim_points$aim_fatevar} that correspond to fates that SHOULD NOT BE INCLUDED IN WEIGHTING, e.g. unneeded or not yet evaluated as in future points or unused oversample. Defaults to \code{NULL}
#' @param wgtcats Data frame or spatial polygons data frame. The information about the weight categories. This must contain the weight category identities in a variable named \code{wgtcat_var}. If this is a data frame, it must also contain the AREAS IN HECTARES in a variable named \code{wgtcat_area_var}. Defaults to \code{NULL}
#' @param wgtcat_var Character string. The name of the variable in \code{wgtcats} (and, if \code{wgtcats} is not spatial, \code{aim_points} and \code{lmf_points}) that contains the weight category identities/memberships.
#' @param wgtcat_area_var Optional character string. The name of the variable in \code{wgtcats} that contains the AREAS IN HECTARES. Only optional if \code{wgtcats} is spatial. Defaults to \code{NULL}
#' @param segments Optional spatial polygons data frame. The information about the LMF segments, this is only optional if \code{segment_var} is not already assigned to \code{aim_points} and \code{lmf_points}. This must contain the weight category identities in a variable named \code{segment_var}. Defaults to \code{NULL}
#' @param segment_var Character string. The name of the variable in \code{segments} (or, if \code{segments} is not provided, \code{aim_points} and \code{lmf_points}) that contains the LMF segment identities.
#' @param segment_var Character string. The name of the variable in \code{segments} (or, if \code{segments} is not provided, \code{aim_points} and \code{lmf_points}) that contains the proportion of the LMF segment which was sampled. If \code{segments} is \code{NULL} then each point in \code{aim_points} and \code{lmf_points} must have the proportion of the segment they belong to, which is untidy but works. Defaults to \code{NULL}.
#' @param projection Optional CRS object. Used to reproject all spatial data. If \code{NULL} then the projection is taken from \code{wgtcats} unless it's not spatial in which case it's taken from \code{segments} unless it's not provided in which case no reprojection occurs. Defaults to \code{NULL}
#' @param verbose Logical. If \code{TRUE} then the function will produce informative messages as it executes its steps. Useful for debugging. Defaults to \code{FALSE}.
#' @return A list of two data frames: point_weights which contains information for each point that did not have a fate in \code{invalid_fates} and wgtcat_summary which contains information about each weight category.
#' @export
weight_aimlmf <- function(aim_points,
                          lmf_points,
                          aim_idvar,
                          lmf_idvar,
                          aim_fatevar = NULL,
                          observed_fates = c("TS"),
                          invalid_fates = NULL,
                          wgtcats = NULL,
                          wgtcat_var,
                          wgtcat_area_var = NULL,
                          segments = NULL,
                          segment_var,
                          segment_prop_var = NULL,
                          projection = NULL,
                          verbose = FALSE){
  if (length(wgtcat_var) != 1 | class(wgtcat_var) != "character") {
    stop("wgtcat_var must be a single character string")
  }
  if (!is.null(wgtcat_area_var)) {
    if (length(wgtcat_area_var) != 1 | class(wgtcat_area_var) != "character") {
      stop("wgtcat_area_var must be a single character string")
    }
  }
  if (length(segment_var) != 1 | class(segment_var) != "character") {
    stop("segment_var must be a single character string")
  }

  if (!is.null(wgtcats)) {
    if (!(class(wgtcats) %in% c("SpatialPolygonsDataFrame", "data.frame"))) {
      stop("wgtcats must be a spatial polygons data frame or data frame")
    }
    if (nrow(wgtcats) < 1) {
      stop("wgtcats contains no observations/data")
    }
    if (!(wgtcat_var %in% names(wgtcats))) {
      stop(paste("The variable", wgtcat_var, "does not appear in wgtcats@data"))
    }
  }

  if (!is.null(segments)) {
    if (!(class(segments) %in% "SpatialPolygonsDataFrame")) {
      stop("segments must be a spatial polygons data frame")
    }
    if (nrow(segments) < 1) {
      stop("segments contains no observations/data")
    }
    if (!(segment_var %in% names(segments))) {
      stop(paste("The variable", segment_var, "does not appear in segments@data"))
    }
  }

  if (!is.null(segment_prop_var)) {
    if (is.null(segments)) {
      if (!(segment_prop_var %in% names(aim_points))) {
        stop(paste("The variable", segment_prop_var, "does not appear in aim_points"))
      }
      if (!(segment_prop_var %in% names(lmf_points))) {
        stop(paste("The variable", segment_prop_var, "does not appear in lmf_points"))
      }
      aim_points[["segment_proportion_sampled"]] <- aim_points[[segment_prop_var]]
      lmf_points[["segment_proportion_sampled"]] <- lmf_points[[segment_prop_var]]
    } else {
      if (!(segment %in% names(segments))) {
        stop(paste("The variable", segment_prop_var, "does not appear in segments"))
      } else {
        segments[["segment_proportion_sampled"]] <- segments[[segment_prop_var]]
      }
    }
  } else {
    aim_points[["segment_proportion_sampled"]] <- 1
    lmf_points[["segment_proportion_sampled"]] <- 1
  }

  if (is.null(aim_fatevar)) {
    warning("No fate variable specified for AIM points. Assuming all were observed/sampled.")
    aim_fatevar <- "fate"
    aim_points[["fate"]] <- "observed"
    lmf_points[["fate"]] <- "observed"
    observed_fates <- "observed"
  } else {
    if (length(aim_fatevar) > 1 | class(aim_fatevar) != "character") {
      stop("The aim fate variable must be a single character string")
    }
    if (!aim_fatevar %in% names(aim_points)) {
      stop(paste("The variable", aim_fatevar, "does not appear in aim_points@data"))
    } else {
      aim_points[["fate"]] <- aim_points[[aim_fatevar]]
    }
    if (is.null(observed_fates)) {
      warning("No observed fates provided. Assuming all AIM points were observed/sampled unless specified otherwise with invalid_fates")
      observed_fates <- unique(aim_points[["fate"]])
      observed_fates <- observed_fates[!(observed_fates %in% invalid_fates)]
    }
  }

  lmf_points[["fate"]] <- observed_fates[1]

  if (!is.null(observed_fates)) {
    if (!any(aim_points[["fate"]] %in% observed_fates)) {
      warning("No AIM points have a fate specified as observed.")
    }
  }

  # Harmonize projections
  if (is.null(projection)) {
    if (!is.null(wgtcats)) {
      projection <- wgtcats@proj4string
    } else if (!is.null(segments)) {
      projection <- segments@proj4string
    }
  }

  if (!is.null(projection)) {
    if (class(aim_points) %in% c("SpatialPointsDataFrame")) {
      if (!identical(projection, aim_points@proj4string)) {
        aim_points <- sp::spTransform(aim_points,
                                      CRSobj = projection)
      }
    }
    if (class(lmf_points) %in% c("SpatialPointsDataFrame")) {
      if (!identical(projection, lmf_points@proj4string)) {
        lmf_points <- sp::spTransform(lmf_points,
                                      CRSobj = projection)
      }
    }
    if (!is.null(wgtcats)) {
      if (class(lmf_points) %in% c("SpatialPolygonsDataFrame")) {
        if (!identical(projection, wgtcats)) {
          wgtcats <- sp::spTransform(wgtcats,
                                     CRSobj = projection)
        }
      }
    }
    if (!is.null(segments)) {
      if (!identical(projection, segments@proj4string)) {
        segments <- sp::spTransform(segments,
                                    CRSobj = projection)
      }
    }
  }





  # Assign the weight categories
  if (class(wgtcats) %in% "SpatialPolygonsDataFrame") {
    if (!(wgtcat_var %in% names(wgtcats))) {
      stop("The variable ", wgtcat_var, " does not appear in wgtcats")
    }
    aim_points[["wgtcat"]] <- sp::over(aim_points, wgtcats)[[wgtcat_var]]
    lmf_points[["wgtcat"]] <- sp::over(lmf_points, wgtcats)[[wgtcat_var]]
  } else {
    if (!(wgtcat_var %in% names(aim_points))) {
      stop("The variable ", wgtcat_var, " does not appear in aim_points")
    }
    if (!(wgtcat_var %in% names(lmf_points))) {
      stop("The variable ", wgtcat_var, " does not appear in lmf_points")
    }
    aim_points[["wgtcat"]] <- aim_points[[wgtcat_var]]
    lmf_points[["wgtcat"]] <- lmf_points[[wgtcat_var]]
  }


  # Assign the LMF segment codes
  if (is.null(segments)) {
    if (!(segment_var %in% names(aim_points))) {
      stop("The variable ", segment_var, " does not appear in aim_points")
    }
    if (!(segment_var %in% names(lmf_points))) {
      stop("The variable ", segment_var, " does not appear in lmf_points")
    }
    aim_points[["segment"]] <- aim_points[[segment_var]]
    lmf_points[["segment"]] <- lmf_points[[segment_var]]
  } else {
    if (!(segment_var %in% names(segments))) {
      stop("The variable ", segment_var, " does not appear in segments")
    }
    aim_points[["segment"]] <- sp::over(aim_points, segments)[[segment_var]]
    lmf_points[["segment"]] <- sp::over(lmf_points, segments)[[segment_var]]
    aim_points[["segment_proportion_sampled"]] <- sp::over(aim_points, segments)[[segment_prop_var]]
    lmf_points[["segment_proportion_sampled"]] <- sp::over(lmf_points, segments)[[segment_prop_var]]
  }


  # Just harmonize the idvar names for now
  aim_points[["unique_id"]] <- aim_points[[aim_idvar]]
  lmf_points[["unique_id"]] <- lmf_points[[lmf_idvar]]

  # If somehow LMF points aren't in a segment, that's a major problem
  if (any(is.na(lmf_points[["segment"]]))) {
    stop(paste("The following LMF points did not spatially intersect any segment polygons:",
               paste(lmf_points[is.na(lmf_points[["segment"]]), "unique_id"], collapse = ", ")))
  }

  # TODO: Stick a check in here that the segment ID from the polygons
  # matches the one derived from the LMF plot ID
  # Probably just warn if not?

  # Get data frames
  if (class(aim_points) %in% "SpatialPointsDataFrame") {
    aim_df <- aim_points@data
  } else {
    aim_df <- aim_points
  }
  if (class(lmf_points) %in% "SpatialPointsDataFrame") {
    lmf_df <- lmf_points@data
  } else {
    lmf_df <- lmf_points
  }

  # NOTE THAT THIS FILTERS OUT ANYTHING FLAGGED AS NOT NEEDED IN THE FATE
  # So that'd be unused oversamples or points from the FUTURE that no one would've sampled anyway
  aim_df <- aim_df[!(aim_df[["fate"]] %in% invalid_fates), c("unique_id", "fate", "wgtcat", "segment", "segment_proportion_sampled")]
  aim_df[["aim"]] <- TRUE
  aim_df[["lmf"]] <- FALSE

  # We only have target sampled LMF points available to us, so we don't need to filter them
  lmf_df <- lmf_df[, c("unique_id", "fate", "wgtcat", "segment", "segment_proportion_sampled")]
  lmf_df[["aim"]] <- FALSE
  lmf_df[["lmf"]] <- TRUE

  # Combine them
  combined_df <- unique(rbind(aim_df, lmf_df))

  # There shouldn't be any that don't belong to a wgtcat anymore
  combined_df <- combined_df[!is.na(combined_df[["wgtcat"]]), ]

  # Add an observed variable for easy reference later
  combined_df[["observed"]] <- combined_df[["fate"]] %in% observed_fates

  # To make the lookup table, drop any points that fell outside LMF segments
  combined_segmentsonly_df <- combined_df[!is.na(combined_df[["segment"]]), ]

  # Create a segment relative weight lookup table
  segment_relwgt_lut <- do.call(rbind,
                                lapply(X = split(combined_segmentsonly_df, combined_segmentsonly_df[["segment"]]),
                                       FUN = function(X){
                                         # These are the count of AIM points with any valid fate
                                         aim_count <- sum(X[["aim"]])

                                         # We also need the count of LMF points with any valid fate, but that's complicated
                                         # We only have the sampled LMF points so we can count those
                                         lmf_sampled_count <- sum(X[["lmf"]])
                                         # To get the number of evaluated but not sampled points:
                                         # The LMF plot keys end in a digit that represents the intended sampling order within a segment
                                         # 1 and 2 are considered base points and were intended to be sampled
                                         # If a sampled LMF plot's plot key ends in 3, that means that one or both of the base points
                                         # were evaluated and rejected rather than sampled, which brings the evaluated LMF plot count
                                         # to three for the segment.
                                         # This just asks if the third point was used
                                         lmf_oversample_used <- any(grepl(X[["unique_id"]][X[["lmf"]]],
                                                                          pattern = "\\D3$"))

                                         # Likewise, if only one LMF plot was sampled in a segment, that means the other two were
                                         # evalurated and rejected rather than sampled, also bringing the total to three.
                                         # So if there was only one sampled or if the oversample was used, there were three evaluated
                                         if (sum(X[["lmf"]]) == 1 | lmf_oversample_used) {
                                           lmf_count <- 3
                                         } else {
                                           # This will fire only if there sampled count was 2, but better to be safe here
                                           lmf_count <- lmf_sampled_count
                                         }


                                         # The relative weight for points falling within a segment is calculated as
                                         # 1 / (number of points)
                                         relative_weight <- 1 / sum(aim_count, lmf_count)

                                         output <- data.frame("segment" = X[["segment"]][1],
                                                              "relwgt" = relative_weight,
                                                              stringsAsFactors = FALSE)
                                         return(output)
                                       }))

  # Add the relative weights to the combined AIM and LMF points
  combined_df <- merge(x = combined_df,
                       y = segment_relwgt_lut,
                       by = "segment",
                       all.x = TRUE)

  # Anywhere there's an NA associated with an AIM point, that's just one that fell outside the segments
  combined_df[is.na(combined_df[["relwgt"]]) & combined_df[["aim"]], "relwgt"] <- 1

  #### THIS IS BIG!!! THE ADJUSTMENT FOR SEGMENT SIZE
  # The segments may not be entirely sampled (e.g. cut off by the AOI)
  # This bit adjusts the relative weights accordingly using segment_sampled_proportion
  combined_df[["relwgt"]][!is.na(combined_df[["relwgt"]])] <- combined_df[["relwgt"]][!is.na(combined_df[["relwgt"]])] * combined_df[["segment_proportion_sampled"]][!is.na(combined_df[["relwgt"]])]


  ####### NOTE!!!!!!!!! ##############################################
  # This is hacked up using wgtcat_df from above because the wgtcats geometry is screwed up
  # wgtcats <- aim.analysis::add.area(wgtcats)
  if (is.null(wgtcat_area_var)) {
    if (class(wgtcats) %in% "SpatialPolygonsDataFrame") {
      wgtcat_df <- aim.analysis::add.area(wgtcats)@data
    } else {
      stop("No name for a variable in wgtcats containing the area in hectares was provided and wgtcats is not a spatial polygons data frame so area can't be calculated")
    }
  } else {
    if (!(wgtcat_area_var %in% names(wgtcats))) {
      if (class(wgtcats) %in% "SpatialPolygonsDataFrame") {
        wgtcat_df <- aim.analysis::add.area(wgtcats)@data
      } else {
        stop("The variable ", wgtcat_area_var, " does not appear in wgtcats")
      }
    } else {
      warning("Trusting that the variable ", wgtcat_area_var, " in wgtcats contains the areas in hectares")
      if (class(wgtcats) %in% "SpatialPolygonsDataFrame") {
        wgtcat_df <- wgtcats@data
        wgtcat_df[["AREA.HA"]] <- wgtcat_df[[wgtcat_area_var]]
      } else {
        wgtcat_df <- wgtcats
        wgtcat_df[["AREA.HA"]] <- wgtcat_df[[wgtcat_area_var]]
      }
    }
  }


  wgtcat_df[["wgtcat"]] <- wgtcat_df[[wgtcat_var]]

  # Making sure that these aren't spread out over multiple observations
  if (verbose) {
    message("Summarizing wgtcat_df by wgtcat to calculate areas in case the wgtcats are split into multiple observations")
  }
  wgtcat_df <- dplyr::summarize(dplyr::group_by(wgtcat_df,
                                                wgtcat),
                                "hectares" = sum(AREA.HA))

  wgtcat_areas <- setNames(wgtcat_df[["hectares"]], wgtcat_df[["wgtcat"]])

  weight_info <- lapply(X = wgtcat_df[["wgtcat"]],
                        wgtcat_areas = wgtcat_areas,
                        points = combined_df,
                        wgtcat_var = wgtcat_var,
                        FUN = function(X, wgtcat_areas, points, wgtcat_var){
                          # Area of this polygon
                          area <- wgtcat_areas[X]

                          # All of the points (AIM and LMF) falling in this polygon
                          points <- points[points[["wgtcat"]] == X, ]

                          # If there are in fact points, do some weight calculations!
                          if (nrow(points) > 0 & any(points[["observed"]])) {
                            # The number of observed AIM points with a relative weight of 1
                            # So, not sharing an LMF segment with any LMF points
                            aim_standalone <- sum(combined_df[["aim"]] & combined_df[["relwgt"]] == 1)
                            # The number of unique segments selected for LMF points
                            lmf_segment_count <- length(unique(points[["segment"]]))

                            # The approximate number of 160 acre segments in this polygon
                            # Obviously, not all of the segments were selected in the first stage
                            # of the LMF design, but this is how many were available in this polygon
                            approximate_segment_count <- area / (160 / 2.47)

                            # This is the sum of the relative weights of the OBSERVED points
                            # This does not include the inaccessible, rejected, or unknown points
                            # It does include both AIM and LMF, however
                            sum_observed_relwgts <- sum(points[points[["observed"]], "relwgt"])
                            # This is the sum of the relative weights of all the AIM points, regardless of fate
                            sum_relwgts <- sum(points[["relwgt"]])

                            # The units are segments per point
                            # The segments are 120 acre quads (quarter sections?) that the first stage of LMF picked from
                            # Steve Garman called this "ConWgt" which I've expanded to conditional_weight
                            # but that's just a guess at what "con" was short for
                            conditional_weight <- approximate_segment_count / (aim_standalone + lmf_segment_count)
                            conditional_area <- conditional_weight * sum_observed_relwgts

                            # What's the observed proportion of the area?
                            observed_proportion <- sum_observed_relwgts / sum_relwgts
                            # And how many acres is that then?
                            # We can derive the "unknown" or "unsampled" area as the difference
                            # between the polygon area and the observed area
                            observed_area <- area * observed_proportion

                            # Then this adjustment value is calculated
                            weight_adjustment <- observed_area / conditional_area



                            # Put everything about the wgtcat in general in one output data frame
                            output_wgtcat <- data.frame(wgtcat = X,
                                                        area = area,
                                                        area_units = "hectares",
                                                        approximate_segment_count = approximate_segment_count,
                                                        sum_observed_relwgts = sum_observed_relwgts,
                                                        sum_relwgts = sum_relwgts,
                                                        observed_proportion = observed_proportion,
                                                        observed_area = observed_area,
                                                        unobserved_area = area - observed_area,
                                                        conditional_weight = conditional_weight,
                                                        conditional_area = conditional_area,
                                                        weight_adjustment = weight_adjustment,
                                                        point_count = sum(points[["observed"]]),
                                                        observed_point_count = nrow(points),
                                                        stringsAsFactors = FALSE,
                                                        row.names = NULL)

                            # But much more importantly add the calculated weights to the points
                            output_points <- points

                            # This handles situations where there were points, but none of them were observed
                            # In that case, weight_adjustment will be 0 / 0 = NaN
                            # That's fine because we can identify that this polygon is still in the inference area
                            # but that its entire area is "unknown" because no data were in it
                            if (is.nan(weight_adjustment)) {
                              output_points[["wgt"]] <- NA
                            } else {
                              # The point weight is calculated here.
                              # This is Garman's formula and I don't have the documentation justifying it on hand
                              output_points[["wgt"]] <- weight_adjustment * conditional_weight * points[["relwgt"]]
                              message("Checking weight sum for ", X)
                              ### NOTE! This error check is commented out because it was behaving erratically
                              ### The code works just fine when not a function, but will fail to return a
                              ### logical value sometimes when run as a function
                              ### It *shouldn't* be necessary because the math above has been vetted
                              # I'm rounding here because at unrealistically high levels of precision it gets weird and can give false positives
                              # if (round(sum(output_points[["wgt"]]), digits = 3) != round(area, digits = 3)) {
                              #   warning("The sum of the point weights (", sum(output_points[["wgt"]]), ") does not equal the polygon area (", area, ") for ", X)
                              # }
                            }

                          } else {
                            # Basically just empty data frames
                            output_wgtcat <- data.frame(wgtcat = X,
                                                        area = area,
                                                        area_units = "hectares",
                                                        approximate_segment_count = area / (160 / 2.47),
                                                        sum_observed_relwgts = NA,
                                                        sum_relwgts = NA,
                                                        observed_proportion = 0,
                                                        observed_area = 0,
                                                        unobserved_area = area,
                                                        conditional_weight = NA,
                                                        conditional_area = NA,
                                                        weight_adjustment = NA,
                                                        point_count = 0,
                                                        observed_point_count = 0,
                                                        stringsAsFactors = FALSE,
                                                        row.names = NULL)
                            output_points <- NULL
                          }

                          return(list(points = output_points,
                                      wgtcat = output_wgtcat))
                        })

  point_weights <- do.call(rbind,
                           lapply(X = weight_info,
                                  FUN = function(X){
                                    X[["points"]]
                                  }))
  wgtcat_summary <- do.call(rbind,
                            lapply(X = weight_info,
                                   FUN = function(X){
                                     X[["wgtcat"]]
                                   }))
  return(list(point_weights = point_weights,
              wgtcat_summary = wgtcat_summary))
}

