# #' Calculate sample weights for points using design polygons
# #' @description Calculate the weight for points in a sample design based on the sample frame or strata used to draw them and the point fate. The outputs are a data frame of the points and their weights, a data frame summary of the fates of the points (optionally by year), and a data frame summary of the strata (optionally by year) if strata were used. No spatial checks are done, so make sure that \code{pts} and \code{frame.spdf} are from the same design, limited to the extent of the original sample frame, and have complete overlap.
# #' @param pts Data frame or spatial points data frame. This should be the points information. At minimum, it must contain a field matching the string \code{pts.fatefield} containing string values matching those in \code{target.values}, \code{unknown.values}, etc. If providing \code{pts.groupfield} it must also have a field matching that containing values matching values found in \code{frame.spdf$frame.groupfield}. If \code{date.field} or \code{year.source.field} is provided, then fields matching those must exist.
# #' @param pts.fatefield Character string. This must exactly match the name of the field in \code{pts} that contains string values matching those in \code{target.values}, \code{unknown.values}, etc.
# #' @param pts.groupfield Optional character string. This must exactly match the name of the field in \code{pts} that contains values matching those found in \code{frame.spdf$frame.groupfield}. This will most often be the field containing strata identities.
# #' @param frame.spdf Spatial polygons data frame. This must be the design's sample frame or strata restricted to the sample frame extent. If providing \code{frame.groupfield} it must also have a field matching that containing values matching values found in \code{pts$pts.groupfield}.
# #' @param frame.groupfield  Optional character string. This must exactly match the name of the field in \code{frame.spdf} that contains values matching those found in \code{pts$pts.groupfield}. This will most often be the field containing strata identities.
# #' @param target.values Character string or character vector. This defines what values in the point fate field count as target points. When using AIM design databases, this should be at minimum \code{c("Target Sampled", "TS")}. This is case insensitive.
# #' @param unknown.values Character string or character vector. This defines what values in the point fate field count as unknown points. When using AIM design databases, this should be at minimum \code{c("Unknown", "Unk")}. This is case insensitive.
# #' @param nontarget.values Character string or character vector. This defines what values in the point fate field count as non-target points. When using AIM design databases, this should be at minimum \code{c("Non-Target", "NT", NA)}. This is case insensitive.
# #' @param inaccessible.values Character string or character vector. This defines what values in the point fate field count as non-target points. When using AIM design databases, this should be at minimum \code{c("Inaccessible")}. This is case insensitive.
# #' @param unneeded.values Character string or character vector. This defines what values in the point fate field count as not needed or unneeded points. When using AIM design databases, this should be at minimum \code{c("Not needed")}. This is case insensitive.
# # @param ... Optional character strings. These must exactly match the names of the field in \code{pts} and will be used to group the points beyond the identity/identities they share with the frame. When calculating \code{frame.stats} these will be passed to \code{dplyr::group_by_()}. They will have no impact on \code{frame.summary} or \code{point.weights}. \code{"YEAR"} would be a common string to pass here.
# #' @return A list containing the named data frames \code{frame.stats}, \code{frame.summary} (if groupfield strings were provided), and \code{point.weights}.
# #' @export
# weight.gen <- function(points,
#                        points_fate_var = NULL,
#                        points_wgtcat_var = NULL,
#                        frame,
#                        frame_wgtcat_var,
#                        frame_area_var = "AREA_HA",
#                        target_fates) {
#   ## Sanitize
#   if (!any(c("sf", "data.frame") %in% class(frame))) {
#     stop("points must be either a data frame or an sf points object.")
#   }
#   if (!any(c("sf", "data.frame") %in% class(points))) {
#     stop("points must be either a data frame or an sf points object.")
#   }
#
#   if (!(frame_wgtcat_var %in% names(frame))) {
#     stop(paste("Unable to find", frame_wgtcat_var, "in the provided frame."))
#   }
#   if (!is.null(points_wgtcat_var)) {
#     if (!(points_wgtcat_var %in% names(points))) {
#       stop(paste("Unable to find", points_wgtcat_var, "in the provided points."))
#     }
#   } else if ("sf" %in% class(frame) & "sf" %in% class(points)) {
#     points_wgtcat_var <- "wgtcat"
#     points <- sf::st_intersection(x = points,
#                                   y = dplyr::select(.data = sf::st_transform(x = frame,
#                                                                              crs = sf::st_crs(points)),
#                                                     wgtcat = frame_wgtcat_var))
#   } else {
#     stop("Please either provide points_wgtcat_var or both points and frame as sf objects.")
#   }
#
#   ## Add areas in hectares to the frame if they're not there already
#   if (!is.null(frame_area_var)) {
#     if (!(frame_area_var %in% names(frame))) {
#       if ("sf" %in% class(frame)) {
#         warning(paste("Unable to find the variable", frame_area_var, "in the provided frame. Adding an area variable with that name. If there is already an area variable present, please change frame_area_var to point to it."))
#         frame <- add_area(frame)
#         frame_area_var <- "AREA_HA"
#       } else {
#         stop(paste("Unable to find the variable", frame_area_var, "in the provided frame. If there is already an area variable present, please change frame_area_var to point to it."))
#       }
#     }
#   } else if ("sf" %in% class(frame)) {
#     frame <- add_area(frame)
#     frame_area_var <- "AREA_HA"
#   } else {
#     stop("Either provide a value for frame_wgtcat_var corresponding to the variable in the frame containing the polygon area(s) OR provide theframe as an sf polygons object so that areas can be calculated.")
#   }
#
#
#
#
#   # Get a data frame of the points without geometry that has some fate things
#   # sorted for later use.
#   points <- dplyr::rename(.data = points,
#                           setNames(object = points_fate_var,
#                                    nm = "fate")) |>
#     dplyr::mutate(.data = _,
#                   fate = dplyr::case_when(fate %in% target_fates ~ "Observed",
#                                           .default = "Unsampled"))
#
#   points_summary <- dplyr::summarize(.data = points,
#                                      .by = tidyselect::all_of(c(points_wgtcat_var,
#                                                                 "fate")),
#                                      count = dplyr::n()) |>
#     tidyr::pivot_wider(data = _,
#                        names_from = fate,
#                        values_from = count,
#                        values_fill = 0)
#
#   # Add in missing fates
#   for (missing_fate in setdiff(x = c("Observed", "Unsampled"), y = unique(points[["fate"]]))) {
#     points_summary[[missing_fate]] <- 0
#   }
#
#
#
#   ## TODO: Needs to handle a polygon OR a raster for the frame
#   ## HERE WE FORK FOR IF THERE ARE STRATA OR NOT
#   if (!is.null(frame.groupfield)) {
#     ## Because we have strata, use the design stratum attribute
#     ## Create a data frame to store the area values in hectares for strata. The as.data.frame() is because it was a tibble for some reason
#     area.df <- group_by_(frame.spdf@data, frame.groupfield) %>% dplyr::summarize(AREA.HA.SUM = sum(AREA.HA)) %>% as.data.frame()
#
#
#     ## Get the sums of point types
#     frame.summary <- frame.stats %>% group_by_(pts.groupfield) %>%
#       dplyr::summarize(Observed.pts = sum(Observed.pts),
#                        Unsampled.pts.nontarget = sum(Unsampled.pts.nontarget),
#                        Unsampled.pts.inaccessible = sum(Unsampled.pts.inaccessible),
#                        Unsampled.pts.unneeded = sum(Unsampled.pts.unneeded),
#                        Unsampled.pts.unknown = sum(Unsampled.pts.unknown))
#
#     ## Add in the areas of the strata
#     frame.summary <- merge(x = frame.summary,
#                            y = area.df,
#                            by.x = pts.groupfield,
#                            by.y = frame.groupfield)
#
#     ## Renaming is causing dplyr and tidyr to freak out, so we'll just copy the values into the fieldnames we want
#     frame.summary$Stratum <- frame.summary[[pts.groupfield]]
#     frame.summary$Area.HA <- frame.summary$AREA.HA.SUM
#
#     ## Calculate the rest of the values
#     frame.summary <- frame.summary %>% group_by(Stratum) %>%
#       ## The total points, minus the unneeded so we don't penalize projects for them!
#       mutate(Total.pts = sum(Observed.pts, Unsampled.pts.nontarget, Unsampled.pts.inaccessible, Unsampled.pts.unknown)) %>%
#       ## The proportion of the total points in the stratum that were "target"
#       mutate(Prop.dsgn.pts.obsrvd = Observed.pts/Total.pts) %>%
#       ## The effective "sampled area" based on the proportion of points that were surveyed
#       mutate(Sampled.area.HA = unlist(Area.HA * Prop.dsgn.pts.obsrvd)) %>%
#       ## The weight for each point in the stratum is the effective sampled area divided by the number of points surveyed, unknown, and inaccessible in the stratum
#       mutate(Weight = Sampled.area.HA/sum(Observed.pts, Unsampled.pts.inaccessible, Unsampled.pts.unknown)) %>% as.data.frame()
#
#     ## When there are NaNs in the calculated fields, replace them with 0
#     frame.summary <- tidyr::replace_na(frame.summary, replace = list(Prop.dsgn.pts.obsrvd = 0, Sampled.area.HA = 0, Weight = 0))
#
#     ## Add the weights to the points, but only the observed ones
#     for (stratum in frame.summary$Stratum) {
#       working.pts$WGT[(working.pts[[pts.fatefield]] %in% target.values) & working.pts[[pts.groupfield]] == stratum] <- frame.summary$Weight[frame.summary$Stratum == stratum]
#     }
#
#     ## All the unassigned weights get converted to 0
#     working.pts <- tidyr::replace_na(working.pts, replace = list(WGT = 0))
#
#     point.weights <- working.pts
#
#   } else if (is.null(frame.groupfield)) {
#
#     ## Treat it as a single unit for lack of stratification
#     area <- sum(frame.spdf@data$AREA.HA)
#
#     ## derive weights
#     proportion.observed <- 1 ## initialize - proportion of 1.0 means there were no nonresponses
#     wgt <- 0 ## initialize wgt
#     sample.area <- 0 ## initialize actual sampled area
#     if (sum(frame.stats[, point.types]) > 0) {
#       proportion.observed <- sum(frame.stats$Observed.pts)/sum(frame.stats[, c("Observed.pts", "Unsampled.pts.nontarget", "Unsampled.pts.inaccessible", "Unsampled.pts.unknown")]) ## realized proportion of the stratum that was sampled (observed/total no. of points that weren't "unneeded")
#     }
#     if (sum(frame.stats$Observed.pts) > 0) {
#       ## Record the actual area(ha) sampled - (proportional reduction * stratum area)
#       sample.area <- proportion.observed*area
#       ## (The proportion of the total area that was sampled * total area [ha]) divided by the number of observed, inaccessible, and unknown points
#       wgt <- (sample.area)/sum(frame.stats$Observed.pts, frame.stats$Inaccessible.pts, frame.stats$Unknown.pts)
#
#     }
#
#     ##Tabulate key information for this DD
#     frame.summary <- data.frame(Stratum = "Frame",
#                                 Total.pts = sum(frame.stats[, point.types]),
#                                 Observed.pts = sum(frame.stats$Observed.pts),
#                                 Unsampled.pts.nontarget = sum(frame.stats$Unsampled.pts.nontarget),
#                                 Unsampled.pts.inaccessible = sum(frame.stats$Unsampled.pts.inaccessible),
#                                 Unsampled.pts.unneeded = sum(frame.stats$Unsampled.pts.unneeded),
#                                 Unsampled.pts.unknown = sum(frame.stats$Unsampled.pts.unknown),
#                                 Area.HA = area,
#                                 Prop.dsgn.pts.obsrvd = proportion.observed,
#                                 Sampled.area.HA = sample.area,
#                                 Weight = wgt,
#                                 stringsAsFactors = FALSE)
#
#     point.weights <- working.pts
#
#     ## If there are points to work with, do this
#     if (nrow(point.weights) > 0) {
#       ## If a point had a target fate, assign the calculates weight
#       point.weights$WGT[point.weights[[pts.fatefield]] %in% target.values] <- wgt
#       ## If a point had a non-target or unknown designation, assign 0 as the weight
#       point.weights$WGT[point.weights[[pts.fatefield]] %in% c(nontarget.values, unknown.values, inaccessible.values, unneeded.values)] <- 0
#     }
#   }
#
#   ## Make sure that this is in the order we want
#   frame.summary <- frame.summary[, c("Stratum",
#                                      "Total.pts",
#                                      "Observed.pts",
#                                      "Unsampled.pts.nontarget",
#                                      "Unsampled.pts.inaccessible",
#                                      "Unsampled.pts.unneeded",
#                                      "Unsampled.pts.unknown",
#                                      "Area.HA",
#                                      "Prop.dsgn.pts.obsrvd",
#                                      "Sampled.area.HA",
#                                      "Weight")]
#
#   names(point.weights)[names(point.weights) == "key"] <- "POINT.FATE"
#
#   output <- list("frame.stats" = frame.stats, "frame.summary" = frame.summary, "point.weights" = point.weights)
#   return(output[!is.null(output)])
# }


#' Calculate weights for analyses combining AIM and LMF data
#' @description When combining AIM and LMF designs for a single weighted analysis, the process of calculating weights is complicated by the two-stage nature of the LMF design. This calculates a relative weight for each point first based on whether or not it falls inside an LMF segment that was selected for sampling and how many points total fall in any given LMF segment. Those relative weights are then used to calculate combined design weights.
#' @param aim_points Data frame or sf points object. The AIM point information, including unique identities in the variable \code{aim_idvar}. If this is just a data frame OR if \code{wgtcats} is not a spatial polygons data frame then it must already have the weight category and LMF segment memberships assigned in the variables \code{wgtcat_var} and \code{segment_var} (there should be an \code{NA} for any observation that does not fall in an LMF segment). If this is spatial and so are either \code{wgtcats} or \code{segments}, then the points will be attributed with the memberships. This must also include \code{aim_fate_var} if any of the points were not observed/sampled, e.g. rejected, inaccessible, or had an unknown fate, otherwise they will all be assumed to've been observed/sampled.
#' @param lmf_points Data frame or sf points object. The LMF point information, including unique identities in the variable \code{lmf_idvar}. If this is just a data frame OR if \code{wgtcats} is not a spatial polygons data frame then it must already have the weight category and LMF segment memberships assigned in the variables \code{wgtcat_var} and \code{segment_var} (there should be an \code{NA} for any observation that does not fall in an LMF segment). If this is spatial and so are either \code{wgtcats} or \code{segments}, then the points will be attributed with the memberships. All LMF points are assumed to be observed/sampled.
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
#' @param segment_prop_var Character string. The name of the variable in \code{segments} (or, if \code{segments} is not provided, \code{aim_points} and \code{lmf_points}) that contains the proportion of the LMF segment which was sampled. If \code{segments} is \code{NULL} then each point in \code{aim_points} and \code{lmf_points} must have the proportion of the segment they belong to, which is untidy but works. Defaults to \code{NULL}.
#' @param projection Optional CRS object. Used to reproject all spatial data. If \code{NULL} then the projection is taken from \code{wgtcats} unless it's not spatial in which case it's taken from \code{segments} unless it's not provided in which case no reprojection occurs. Defaults to \code{NULL}
#' @param verbose Logical. If \code{TRUE} then the function will produce informative messages as it executes its steps. Useful for debugging. Defaults to \code{FALSE}.
#' @return A list of two data frames: point_weights which contains information for each point that did not have a fate in \code{invalid_fates} and wgtcat_summary which contains information about each weight category.
#' @export
weight_aimlmf <- function(aim_points,
                          lmf_points,
                          aim_idvar,
                          lmf_idvar,
                          aim_fatevar = NULL,
                          observed_fates = NULL,
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
    if (!any(c("sf", "data.frame") %in% class(wgtcats))) {
      stop("wgtcats must be an sf object or data frame")
    }
    if (nrow(wgtcats) < 1) {
      stop("wgtcats contains no observations/data")
    }
    if (!(wgtcat_var %in% names(wgtcats))) {
      stop(paste("The variable", wgtcat_var, "does not appear in wgtcats"))
    }
  }

  if (!is.null(segments)) {
    if (!("sf" %in% class(segments))) {
      stop("segments must be an sf object")
    }
    if (nrow(segments) < 1) {
      stop("segments contains no observations/data")
    }
    if (!(segment_var %in% names(segments))) {
      stop(paste("The variable", segment_var, "does not appear in segments"))
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
      stop(paste("The variable", aim_fatevar, "does not appear in aim_points"))
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
  # We only have to do this if there are polygons to worry about. Otherwise,
  # projection doesn't matter because we're just working with tabular data.
  if (is.null(projection)) {
    if (!is.null(wgtcats)) {
      if ("sf" %in% class(wgtcats)) {
        projection <- sf::st_crs(wgtcats)
      }
    } else if (!is.null(segments)) {
      if ("sf" %in% class(wgtcats)) {
        projection <- sf::st_crs(segments)
      }
    }
  } else if (class(projection) == "character") {
    projection <- sf::st_crs(projection)
  }

  if (!is.null(projection)) {
    if ("sf" %in% class(aim_points)) {
      if (!identical(projection, sf::st_crs(aim_points))) {
        aim_points <- sf::st_transform(x = aim_points,
                                       crs = projection)
      }
    }
    if ("sf" %in% class(lmf_points)) {
      if (!identical(projection, sf::st_crs(lmf_points))) {
        lmf_points <- sf::st_transform(x = lmf_points,
                                       crs = projection)
      }
    }
    if (!is.null(wgtcats)) {
      if ("sf" %in% class(wgtcats)) {
        if (!identical(projection, sf::st_crs(wgtcats))) {
          wgtcats <- sf::st_transform(x = wgtcats,
                                      crs = projection)
        }
      }
    }
    if (!is.null(segments)) {
      if ("sf" %in% class(segments)) {
        if (!identical(projection, sf::st_crs(segments))) {
          segments <- sf::st_transform(x = segments,
                                       crs = projection)
        }
      }
    }
  }


  # Assign the weight categories
  if ("sf" %in% class(wgtcats)) {
    if (!(wgtcat_var %in% names(wgtcats))) {
      stop("The variable ", wgtcat_var, " does not appear in wgtcats")
    }
    aim_points <- sf::st_intersection(x = aim_points,
                                      y = dplyr::select(.data = wgtcats,
                                                        tidyselect::all_of(wgtcat_var)))
    lmf_points <- sf::st_intersection(x = lmf_points,
                                      y = dplyr::select(.data = wgtcats,
                                                        tidyselect::all_of(wgtcat_var)))
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
    aim_points <- sf::st_intersection(x = aim_points,
                                      y = dplyr::select(.data = segments,
                                                        tidyselect::all_of(c(segment_var,
                                                                             segment_prop_var)))) |>
      dplyr::rename(.data = _,
                    setNames(object = c("segment",
                                        "segment_proportion_sampled"),
                             nm = c(segment_var,
                                    segment_prop_var)))
    lmf_points <- sf::st_intersection(x = lmf_points,
                                      y = dplyr::select(.data = segments,
                                                        tidyselect::all_of(c(segment_var,
                                                                             segment_prop_var)))) |>
      dplyr::rename(.data = _,
                    setNames(object = c("segment",
                                        "segment_proportion_sampled"),
                             nm = c(segment_var,
                                    segment_prop_var)))
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

  # Get data frames and only the data we need, which means (for now) removing
  # points which are flagged as "unneeded" or intended to be sampled at some
  # point in the future.
  minimum_point_vars <- c("unique_id",
                          "fate",
                          "wgtcat",
                          "segment",
                          "segment_proportion_sampled")
  aim_df <- sf::st_drop_geometry(aim_points) |>
    dplyr::select(.data = _,
                  # I'm using any_of() because I'm not sure that we'll have the
                  # segment variables. I don't think we're putting in
                  # placeholder values when there aren't segments involved.
                  tidyselect::any_of(minimum_point_vars)) |>
    dplyr::filter(.data = _,
                  !(fate %in% invalid_fates)) |>
    dplyr::mutate(.data = _,
                  aim = TRUE,
                  lmf = FALSE)
  lmf_df <- sf::st_drop_geometry(lmf_points) |>
    dplyr::select(.data = _,
                  # I'm using any_of() because I'm not sure that we'll have the
                  # segment variables. I don't think we're putting in
                  # placeholder values when there aren't segments involved.
                  tidyselect::any_of(minimum_point_vars)) |>
    # For LMF points, this shouldn't matter, but we'll do it anyway for
    # consistency!
    dplyr::filter(.data = _,
                  !(fate %in% invalid_fates)) |>
    dplyr::mutate(.data = _,
                  aim = FALSE,
                  lmf = TRUE)

  # Combine the two data sets and do some munging.
  combined_df <- dplyr::distinct(dplyr::bind_rows(aim_df,
                                                  lmf_df)) |>
    # Make sure we don't have any that *still* don't have a wgtcat assigned.
    dplyr::filter(.data = _,
                  !is.na(wgtcat)) |>
    # Add an observed variable for easy reference later.
    dplyr::mutate(.data = _,
                  observed = dplyr::case_when(fate %in% observed_fates ~ TRUE,
                                              .default = FALSE))

  # To make the lookup table, drop any points that fell outside LMF segments
  combined_segmentsonly_df <- dplyr::filter(.data = combined_df,
                                            !is.na(segment))

  # Create a segment relative weight lookup table
  segment_relwgt_lut <- lapply(X = split(x = combined_segmentsonly_df,
                                         f = combined_segmentsonly_df[["segment"]]),
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
                                                                  # \\D means "not a digit"
                                                                  pattern = "\\D3$"))

                                 # Likewise, if only one LMF plot was sampled in a segment, that means the other two were
                                 # evalurated and rejected rather than sampled, also bringing the total to three.
                                 # So if there was only one sampled or if the oversample was used, there were three evaluated
                                 if (lmf_sampled_count == 1 | lmf_oversample_used) {
                                   lmf_count <- 3
                                 } else {
                                   # This will fire only if there sampled count was 2, but better to be safe here
                                   lmf_count <- lmf_sampled_count
                                 }


                                 # The relative weight for points falling within a segment is calculated as
                                 # (relative size of the segment) / (number of points)
                                 # The relative size is 1 for the largest segment
                                 # in play.
                                 relative_weight <- X[["segment_proportion_sampled"]][1] / sum(aim_count, lmf_count)

                                 output <- data.frame("segment" = X[["segment"]][1],
                                                      "relwgt" = relative_weight,
                                                      stringsAsFactors = FALSE)
                                 return(output)
                               }) |>
    dplyr::bind_rows()

  # Add the relative weights to the combined AIM and LMF points
  combined_df <- dplyr::left_join(x = combined_df,
                                  y = segment_relwgt_lut,
                                  by = "segment",
                                  relationship = "many-to-one") |>
    # If there's no relative weight, that means it was outside a segment and
    # therefore the relative weight should be 1. This shouldn't happen with LMF
    # points because they were drawn using the segments in the first place.
    dplyr::mutate(.data = _,
                  relwgt = dplyr::case_when(is.na(relwgt) & aim ~ 1,
                                            .default = relwgt))

  ####### NOTE!!!!!!!!! ##############################################
  # This is hacked up using wgtcat_df from above because the wgtcats geometry
  # is screwed up
  # wgtcats <- aim.analysis::add.area(wgtcats)
  if (is.null(wgtcat_area_var)) {
    if ("sf" %in% class(wgtcats)) {
      wgtcat_df <- aim.analysis::add_area(wgtcats) |>
        sf::st_drop_geometry(obj = _)
    } else {
      stop("No name for a variable in wgtcats containing the area in hectares was provided and wgtcats is not a spatial polygons data frame so area can't be calculated")
    }
  } else {
    if (!(wgtcat_area_var %in% names(wgtcats))) {
      if ("sf" %in% class(wgtcats)) {
        wgtcat_df <- aim.analysis::add_area(wgtcats) |>
          sf::st_drop_geometry(obj = _)
      } else {
        stop("The variable ", wgtcat_area_var, " does not appear in wgtcats")
      }
    } else {
      warning("Trusting that the variable ", wgtcat_area_var, " in wgtcats contains the areas in hectares")
      wgtcat_df <- sf::st_drop_geometry(obj = wgtcat) |>
        dplyr::rename(.data = _,
                      setNames(object = wgtcat_area_var,
                               nm = "area_ha"))
    }
  }

  wgtcat_df[["wgtcat"]] <- wgtcat_df[[wgtcat_var]]

  # Making sure that these aren't spread out over multiple observations
  if (verbose) {
    message("Summarizing wgtcat_df by wgtcat to calculate areas in case the wgtcats are split into multiple observations")
  }
  wgtcat_df <- dplyr::summarize(.data = wgtcat_df,
                                .by = tidyselect::all_of("wgtcat"),
                                "hectares" = sum(area_ha))

  wgtcat_areas <- setNames(object = wgtcat_df[["hectares"]],
                           nm = wgtcat_df[["wgtcat"]])

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

