#' Identify and repair invalid geometry in spatial polygons data frames
#' @description Using functions from sf, check the geometry of a set of polygons. If the geometry invalid, it attempts to buffer the polygons with \code{sf::st_buffer(dist = 0)}. If the geometry is corrupt or fine, it does nothing.
#' @param polygons Spatial polygons (either sf or spatial polygons data frame). The polygons to be checked. Note that the function will halt if the geometry is corrupt and not return a value.
#' @param verbose Logical. If \code{TRUE} then the function will produce informative messages as it executes its steps. Useful for debugging. Defaults to \code{FALSE}.
#' @param force Logical. If \code{TRUE} then both valid and invalid polygons will be buffered by 0. This shouldn't be necessary, but is a feature for the paranoid.
#' @return The spatial polygons data frame \code{polygons1}. This will be unchanged if the geometry was valid or repaired if it was invalid.
#' @export
repair_geometry <- function(polygons,
                            verbose = FALSE,
                            force = FALSE) {
  if(class(polygons) == "SpatialPolygonsDataFrame") {
    polygons_sf <- sf::st_as_sf(polygons)
    spdf <- TRUE
  } else if ("sf" %in% class(polygons)) {
    polygons_sf <- polygons
    spdf <- FALSE
  } else {
    stop("polygons must either be a spatial polygons data frame or an sf object")
  }

  validity_check <- sf::st_is_valid(polygons_sf)

  if (any(is.na(validity_check))) {
    stop("The geometry of the polygons is corrupt. Unable to repair.")
  }

  if (!all(validity_check)) {
    if (verbose) {
      message("Invalid geometry found. Attempting to repair.")
    }
    output <- sf::st_buffer(x = polygons_sf,
                            dist = 0)
  } else if (force) {
    if (verbose) {
      message("No invalid geometry found. Attempting to repair anyway.")
    }
    output <- sf::st_buffer(x = polygons_sf,
                            dist = 0)
  } else {
    if (verbose) {
      message("No invalid geometry found.")
    }
    output <- polygons_sf
  }

  if (spdf) {
    output <- methods::as(output, "Spatial")
  }

  return(output)
}

#' Generate weight category polygons
#' @description Create weight category polygons from the intersection of multiple spatial polygons data frames. When combining overlapping designs, weight categories are the unique combinations of the sample frame (or stratification) identities from the designs. This takes a list of spatial polygons data frames which have a common variable containing their unique weighting identifier (e.g. three stratification schemes that all contain a variable named "wgt_cat" that holds the stratum identities) and intersects them to find the weight categories. This is extremely vulnerable to malformed/invalid geometry and so it is highly recommended that you use the argument \code{makevalid = TRUE} to prompt the function to check and attempt to repair geometry as needed after every geoprocessing step. This is slower, but the intersections will almost certainly fail without it. If you are feeling especially paranoid, you can also use \code{force = TRUE} to force repair actions even on valid geometry.
#' @param polygons List of spatial polygons data frames. These are the polygons to intersect and should be the same as the polygons used to draw the designs being combined. They must all have a the \code{idvar} variable containing the identities of the individual polygons (e.g. stratum). If you are combining designs that used the same polygons (e.g. two different designs using the exact same sample frame and stratification) ONLY INCLUDE ONE OF THEM. Including duplicates can result in very, very long processing times and failed intersections. They will be intersected sequentially in order, so there may be some slight speed gains if they're sorted smallest to largest, but no guarantees.
#' @param idvar Character string. The name of the variable found in every set of polygons that contains the polygon identities.
#' @param makevalid Logical. If \code{TRUE} then the function \code{repair_geometry()} will be applied to the product of every geoprocessing step. If \code{FALSE} then there will be no validity checks. It is STRONGLY recommended that you use \code{TRUE} Defaults to \code{TRUE}.
#' @param force Logical. This is passed to \code{repair_geometry()} if \code{makevalid = TRUE}. If \code{TRUE} this will force the function to attempt to repair geometry even if there are no errors. This is for the paranoid. Defaults to \code{FALSE}.
#' @param verbose Logical. If \code{TRUE} then the function will produce informative messages as it executes its steps. Useful for debugging. Defaults to \code{FALSE}.
#' @param scale Numeric. A value to be passed to \code{rgeos::setScale()} to use for the steps of this function. It will be returned to the previous value before the function returns its output. Defaults to \code{1e5} (precision to five decimal places).
#' @return A spatial polygons data frame with the single variable \code{"wgt_cat"} in the data slot containing the unique weight categories.
#' @export
wgtcat_gen <- function(polygons,
                       idvar,
                       makevalid = TRUE,
                       force = FALSE,
                       verbose = FALSE,
                       scale = 1e5) {
  if (class(polygons) != "list") {
    stop("The polygons must be provided as a list of spatial polygons data frames.")
  }
  if (!all(sapply(polygons, class) == "SpatialPolygonsDataFrame")) {
    stop("All objects in the list polygons must be spatial polygons data frames")
  }

  if (class(idvar) != "character") {
    stop("idvar must be a single character vector")
  }
  if (length(idvar) > 1) {
    stop("idvar must be a single character vector")
  }

  missing_idvar <- sapply(X = polygons,
                          idvar = idvar,
                          FUN = function(X, idvar){
                            !(idvar %in% names(X))
                          })
  if (any(missing_idvar)) {
    stop(paste("The variable", idvar, "is missing from the list of polygons at indices:", paste(which(missing_idvar), collapse = ", ")))
  }

  no_data <- sapply(X = polygons,
                    FUN = function(X, idvar){
                      nrow(X@data) < 1
                    })
  if (any(no_data)) {
    warning(paste("There are no data in one or more polygons. Dropping the empty polygons at indices:", paste(which(no_data), collapse = ", ")))
    polygons <- polygons[!no_data]
  }


  polygons_count <- length(polygons)
  if (polygons_count < 2) {
    stop("Must provide at least two polygons to intersect")
  }


  # Trim the polygons down to just the one variable
  # And also validate geometry if asked
  polygons <- lapply(polygons,
                     idvar = idvar,
                     makevalid = makevalid,
                     verbose = verbose,
                     force = force,
                     FUN = function(X, idvar, makevalid, verbose, force){
                       current_polygons <- X[, idvar]

                       if (makevalid) {
                         if (verbose) {
                           message("Evaluating integrity of polygons.")
                         }
                         current_polygons <- repair_geometry(polygons = current_polygons,
                                                             verbose = verbose,
                                                             force = force)
                       }
                       return(current_polygons)
                     })

  if (verbose) {
    message("Attempting to intersect the first two polygons in the list")
  }
  # Run the intersection
  results <- intersect_rgeos(polygons1 = polygons[[1]],
                             polygons2 = polygons[[2]],
                             keep_outer = TRUE,
                             verbose = verbose,
                             makevalid = makevalid,
                             force = force,
                             scale = scale)

  # Then create the weight category variable by jamming the other two together
  results@data[["wgtcat"]] <- mapply(var1 = results@data[[1]],
                                     var2 = results@data[[2]],
                                     FUN = function(var1, var2){
                                       paste(na.omit(c(var1, var2)), collapse = "_")
                                     })

  # Geometry errors are rampant, so always offer an opportunity to fix them
  if (makevalid) {
    if (verbose) {
      message("Evaluating integrity of current intersection results.")
    }
    results <- repair_geometry(polygons = results,
                               verbose = verbose,
                               force = force)
  }

  # So that we don't leave spaces in there that'll screw up potential future splits
  row.names(results) <- sapply(X = row.names(results),
                               FUN = function(X){
                                 gsub(X,
                                      pattern = " ",
                                      replacement = "_")
                               })
  rownames(results@data) <- row.names(results)
  # results <- dissolve(polygons = results,
  #                     dissolve_var = "wgtcat")
  if (makevalid) {
    if (verbose) {
      message("Evaluating integrity of dissolved intersection results.")
    }
    results <- repair_geometry(polygons = results,
                               verbose = verbose,
                               force = force)
  }


  # And now we enter the looping phase where each successive entry in polygons gets intersected with the results of the previous intersections
  # Same steps as above, just in sequence!
  if (polygons_count > 2) {
    for (index in 3:length(polygons)) {
      if (verbose) {
        message(paste("Attempting to intersect the current intersection results with the polygons at index", index))
      }
      results <- intersect_rgeos(polygons1 = results[, "wgtcat"],
                                 polygons2 = polygons[[index]],
                                 keep_outer = TRUE,
                                 verbose = verbose,
                                 makevalid = makevalid,
                                 force = force,
                                 scale = scale)

      if (!is.null(results)) {
        results@data[["wgtcat"]] <- mapply(var1 = results@data[[1]],
                                           var2 = results@data[[2]],
                                           FUN = function(var1, var2){
                                             paste(na.omit(c(var1, var2)), collapse = "_")
                                           })

        if (makevalid) {
          if (verbose) {
            message("Evaluating integrity of current intersection results.")
          }
          results <- repair_geometry(polygons = results,
                                     verbose = verbose,
                                     force = force)
        }
        if (verbose) {
          message("Dissolving intersection by wgtcat.")
        }

        # So that we don't leave spaces in there that'll screw up potential future splits
        row.names(results) <- sapply(X = row.names(results),
                                     FUN = function(X){
                                       gsub(X,
                                            pattern = " ",
                                            replacement = "_")
                                     })
        rownames(results@data) <- row.names(results)

        if (makevalid) {
          if (verbose) {
            message("Evaluating integrity of dissolved intersection results.")
          }
          results <- repair_geometry(polygons = results,
                                     verbose = verbose,
                                     force = force)
        }
      }
    }
  }

  results <- dissolve(polygons = results,
                      dissolve_var = "wgtcat")
  return(results[, "wgtcat"])
}



#' Creating the Intersection of Two Spatial Polygons Data Frames
#' @description Produce a spatial polygons data frame of the intersection of two spatial polygons data frames, inheriting all the variables from each. In the case of duplicate variables between the two, those in \code{polygons2@@data} will have a "2" appended to differentiate them. This uses \code{rgeos::ginstersect} and assumes that the row names in both \code{polygons1} and \code{polygons2} DO NOT HAVE SPACES.
#' @param polygons1 Spatial polygons data frame. One of the set of polygons to intersect. If \code{crs} is \code{NULL} the projection from \code{polygons1@@proj4string} will be used for the output.
#' @param polygons2 Spatial polygons data frame. One of the set of polygons to intersect.
#' @param keep_outer Logical. If \code{TRUE} then the non-intersecting portions of \code{polygons1} and \code{polygons2} will be kept as well.
#' @param projection Optional \code{sp::CRS()} call. Used to reproject both \code{polygons1} and \code{polygons2} using \code{sp::spTransform()} If \code{NULL} then the projection from \code{polygons1@@proj4string} will be used. Defaults to \code{NULL}.
#' @param verbose Logical. If \code{TRUE} then additional informative messages will be produced as the function runs. Defaults to \code{FALSE}.
#' @param scale Numeric. A value to be passed to \code{rgeos::setScale()} to use for the steps of this function. It will be returned to the previous value before the function returns its output. Defaults to \code{1e5} (precision to five decimal places).
#' @return A spatial polygons data frame consisting of the unique intersections of \code{polygons1} and \code{polygons2} with all the variables from both in the projection of either \code{projection} or \code{polygons1}. If there were variable names that occurred in both, those from \code{polygons2} will have a "2" appended.
#' @export
intersect_rgeos <- function(polygons1,
                            polygons2,
                            keep_outer = FALSE,
                            projection = NULL,
                            makevalid = TRUE,
                            force = FALSE,
                            verbose = FALSE,
                            scale = 1e5){

  current_scale <- rgeos::getScale()
  rgeos::setScale(scale)
  if (class(polygons1)[1] != "SpatialPolygonsDataFrame") {
    stop("polygons1 must be a spatial polygons data frame")
  }
  if (class(polygons2)[1] != "SpatialPolygonsDataFrame") {
    stop("polygons2 must be a spatial polygons data frame")
  }

  if (is.null(projection)) {
    projection <- polygons1@proj4string
  } else if (class(projection) != "CRS") {
    stop("The projection must be a CRS object, e.g. sp::CRS('+proj=aea')")
  }

  # Reproject if necessary
  if (!identical(projection, polygons1@proj4string)) {
    if (verbose) {
      message("Reprojecting polygons1")
    }
    polygons1 <- sp::spTransform(polygons1,
                                 projection)
  }
  if (!identical(projection, polygons2@proj4string)) {
    if (verbose) {
      message("Reprojecting polygons2")
    }
    polygons2 <- sp::spTransform(polygons2,
                                 projection)
  }

  # Make valid!
  if (makevalid) {
    polygons1 <- repair_geometry(polygons = polygons1,
                                 verbose = verbose,
                                 force = force)
    polygons2 <- repair_geometry(polygons = polygons2,
                                 verbose = verbose,
                                 force = force)
  }

  # In case there's naming collisions!
  common_varnames <- names(table(c(names(polygons1@data), names(polygons2@data))))[table(c(names(polygons1@data), names(polygons2@data))) == 2]
  if (length(common_varnames) > 0) {
    if (verbose) {
      message("Naming collisons detected. Adding the suffix '2' to the end of all duplicate variable names in polygons2@data")
    }
    for (name in common_varnames) {
      names(polygons2@data)[names(polygons2@data) == name] <- paste0(name, "2")
    }
  }
  variable_order <- unique(c(names(polygons1@data), names(polygons2@data)))

  # First off, just do the intersect with rgeos::gintersect()
  intersection_sp <- rgeos::gIntersection(polygons1,
                                          polygons2,
                                          byid = TRUE,
                                          drop_lower_td = TRUE)

  if (!is.null(intersection_sp)) {
    ## Now we need to build the data frame that goes back into this. It's a pain.
    # Split the rownames along " "
    intersection_rownames_split <- lapply(X = row.names(intersection_sp),
                                          FUN = function(X){
                                            unlist(stringr::str_split(X, pattern = " "))
                                          })
    # So that we don't leave spaces in there that'll screw up potential future splits
    row.names(intersection_sp) <- sapply(X = intersection_rownames_split,
                                         FUN = function(X){
                                           paste(X, collapse = "_")
                                         })
    # In each of those vectors, the first string is the row index from polygons1
    polygons1_indices <- sapply(X = intersection_rownames_split,
                                FUN = function(X){
                                  X[1]
                                  # as.numeric(X[1])
                                })
    # And the second is from polygons2
    polygons2_indices <- sapply(X = intersection_rownames_split,
                                FUN = function(X){
                                  X[2]
                                  # as.numeric(X[2])
                                })
    # So we mash the appropriate rows from the respective data frames together
    intersection_dataframe <- cbind(as.data.frame(polygons1@data[polygons1_indices, ],
                                                  stringsAsFactors = FALSE),
                                    as.data.frame(polygons2@data[polygons2_indices, ],
                                                  stringsAsFactors = FALSE))
    names(intersection_dataframe) <- c(names(polygons1@data), names(polygons2@data))

    # Set the rownames to be the same as the intersection SP
    # OTHERWISE THERE'S NO RELATIONSHIP WITH THE GEOMETRY
    rownames(intersection_dataframe) <- row.names(intersection_sp)

    # Make it an SPDF
    intersect_spdf <- sp::SpatialPolygonsDataFrame(Sr = intersection_sp,
                                                   data = intersection_dataframe)

    # Make valid!
    if (makevalid) {
      intersect_spdf <- repair_geometry(polygons = intersect_spdf,
                                        verbose = verbose,
                                        force = force)
    }
  } else {
    intersect_spdf <- NULL
  }



  # If we're keeping the non-intersecting parts as well
  if (keep_outer) {
    if (verbose) {
      message("Finding the non-intersecting geometry.")
    }
    # Same process as above, but with gDifference() followed by gIntersect()
    # The gIntersect() is necessary because we have to use byid = FALSE to get this to work
    remainders1_sp <- rgeos::gDifference(spgeom1 = polygons1,
                                         spgeom2 = polygons2,
                                         byid = FALSE,
                                         drop_lower_td = TRUE)
    if (!is.null(remainders1_sp)) {
      remainders1_rownames_split <- lapply(X = row.names(remainders1_sp),
                                           FUN = function(X){
                                             unlist(stringr::str_split(X, pattern = " "))
                                           })
      # So that we don't leave spaces in there that'll screw up potential future splits
      row.names(remainders1_sp) <- sapply(X = remainders1_rownames_split,
                                          FUN = function(X){
                                            paste(X, collapse = "_")
                                          })
      remainders1_indices <- sapply(X = remainders1_rownames_split,
                                    FUN = function(X){
                                      X[1]
                                    })
      remainders1_dataframe <- as.data.frame(polygons1@data[remainders1_indices, ],
                                             stringsAsFactors = FALSE)
      names(remainders1_dataframe) <- names(polygons1@data)
      rownames(remainders1_dataframe) <- row.names(remainders1_sp)
      remainders1_spdf <- sp::SpatialPolygonsDataFrame(Sr = remainders1_sp,
                                                       data = remainders1_dataframe)
      # Make valid!
      if (makevalid) {
        remainders1_spdf <- repair_geometry(polygons = remainders1_spdf,
                                            verbose = verbose,
                                            force = force)
      }

      # Here's the intersection to add detail back in lost to byid = FALSE
      intersection_remainders1_sp <- rgeos::gIntersection(polygons1,
                                                          remainders1_spdf,
                                                          byid = TRUE,
                                                          drop_lower_td = TRUE)

      ## Now we need to build the data frame that goes back into this. It's a pain.
      # Split the rownames along " "
      intersection_remainders1_rownames_split <- lapply(X = row.names(intersection_remainders1_sp),
                                                        FUN = function(X){
                                                          unlist(stringr::str_split(X, pattern = " "))
                                                        })
      # So that we don't leave spaces in there that'll screw up potential future splits
      row.names(intersection_remainders1_sp) <- sapply(X = intersection_remainders1_rownames_split,
                                                       FUN = function(X){
                                                         paste(X, collapse = "_")
                                                       })
      # In each of those vectors, the first string is the row index from polygons1
      polygons1_indices <- sapply(X = intersection_remainders1_rownames_split,
                                  FUN = function(X){
                                    X[1]
                                  })
      # So we mash the appropriate rows from the respective data frames together
      intersection_remainders1_dataframe <- as.data.frame(polygons1@data[polygons1_indices, ],
                                                          stringsAsFactors = FALSE)
      names(intersection_remainders1_dataframe) <- names(polygons1@data)

      # Set the rownames to be the same as the intersection SP
      # OTHERWISE THERE'S NO RELATIONSHIP WITH THE GEOMETRY
      rownames(intersection_remainders1_dataframe) <- row.names(intersection_remainders1_sp)

      # Make it an SPDF
      remainders1_spdf <- sp::SpatialPolygonsDataFrame(Sr = intersection_remainders1_sp,
                                                       data = intersection_remainders1_dataframe)

      # Make valid!
      if (makevalid) {
        remainders1_spdf <- repair_geometry(polygons = remainders1_spdf,
                                            verbose = verbose,
                                            force = force)
      }


      # Add in the missing variable names so we can mash these together
      missing_from_remainders1 <- variable_order[!(variable_order %in% names(remainders1_spdf@data))]
      for (name in missing_from_remainders1) {
        remainders1_spdf@data[[name]] <- NA
      }
      remainders1_spdf@data <- remainders1_spdf@data[, variable_order]
    } else {
      remainders1_spdf <- NULL
    }

    remainders2_sp <- rgeos::gDifference(spgeom1 = polygons2,
                                         spgeom2 = polygons1,
                                         byid = FALSE,
                                         drop_lower_td = TRUE)

    if (!is.null(remainders2_sp)) {
      remainders2_rownames_split <- lapply(X = row.names(remainders2_sp),
                                           FUN = function(X){
                                             unlist(stringr::str_split(X, pattern = " "))
                                           })
      # So that we don't leave spaces in there that'll screw up potential future splits
      row.names(remainders2_sp) <- sapply(X = remainders2_rownames_split,
                                          FUN = function(X){
                                            paste(X, collapse = "_")
                                          })
      remainders2_indices <- sapply(X = remainders2_rownames_split,
                                    FUN = function(X){
                                      X[1]
                                    })
      remainders2_dataframe <- as.data.frame(polygons2@data[remainders2_indices, ],
                                             stringsAsFactors = FALSE)
      names(remainders2_dataframe) <- names(polygons2@data)
      rownames(remainders2_dataframe) <- row.names(remainders2_sp)
      remainders2_spdf <- sp::SpatialPolygonsDataFrame(Sr = remainders2_sp,
                                                       data = remainders2_dataframe)
      if (makevalid) {
        remainders2_spdf <- repair_geometry(polygons = remainders2_spdf,
                                            verbose = verbose,
                                            force = force)
      }

      # Here's the intersection to add detail back in lost to byid = FALSE
      intersection_remainders2_sp <- rgeos::gIntersection(polygons2,
                                                          remainders2_spdf,
                                                          byid = TRUE,
                                                          drop_lower_td = TRUE)

      ## Now we need to build the data frame that goes back into this. It's a pain.
      # Split the rownames along " "
      intersection_remainders2_rownames_split <- lapply(X = row.names(intersection_remainders2_sp),
                                                        FUN = function(X){
                                                          unlist(stringr::str_split(X, pattern = " "))
                                                        })
      # So that we don't leave spaces in there that'll screw up potential future splits
      row.names(intersection_remainders2_sp) <- sapply(X = intersection_remainders2_rownames_split,
                                                       FUN = function(X){
                                                         paste(X, collapse = "_")
                                                       })
      # In each of those vectors, the first string is the row index from polygons1
      polygons2_indices <- sapply(X = intersection_remainders2_rownames_split,
                                  FUN = function(X){
                                    X[1]
                                  })
      # So we mash the appropriate rows from the respective data frames together
      intersection_remainders2_dataframe <- as.data.frame(polygons2@data[polygons2_indices, ],
                                                          stringsAsFactors = FALSE)
      names(intersection_remainders2_dataframe) <- names(polygons2@data)

      # Set the rownames to be the same as the intersection SP
      # OTHERWISE THERE'S NO RELATIONSHIP WITH THE GEOMETRY
      rownames(intersection_remainders2_dataframe) <- row.names(intersection_remainders2_sp)

      # Make it an SPDF
      remainders2_spdf <- sp::SpatialPolygonsDataFrame(Sr = intersection_remainders2_sp,
                                                       data = intersection_remainders2_dataframe)

      # Make valid!
      if (makevalid) {
        remainders2_spdf <- repair_geometry(polygons = remainders2_spdf,
                                            verbose = verbose,
                                            force = force)
      }


      missing_from_remainders2 <- variable_order[!(variable_order %in% names(remainders2_spdf@data))]
      for (name in missing_from_remainders2) {
        remainders2_spdf@data[[name]] <- NA
      }
      remainders2_spdf@data <- remainders2_spdf@data[, variable_order]
    } else {
      remainders2_spdf <- NULL
    }


    # Mash these together
    output_list <- list(intersect_spdf,
                        remainders1_spdf,
                        remainders2_spdf)
    non_null <- !sapply(output_list, is.null)
    if (any(non_null)) {
      output <- do.call(rbind,
                        output_list[non_null])
    } else {
      output <- NULL
    }
  } else {
    output <- intersect_spdf
  }

  rgeos::setScale(current_scale)

  return(output)
}

#' Dissolve polygons according to unique identities
#' @description For a spatial polygons data frame, return a spatial polygons data frame that contains only one observation for each unique identifier associated with the combined geometry of all polygons belonging to that identifier.
#' @param polygons Spatial Polygons Data Frame. Must have at least the variable matching the value of \code{dissolve_var}.
#' @param dissolve_var Character string. Must match the name of the variable in \code{polygons@@data} that contains the unique identifiers to dissolve along.
#' @return Spatial polygons data frame containing only the variable named in \code{dissolve_var} and only one observation per value in that variable.
#' @export
dissolve <- function(polygons,
                     dissolve_var){
  # TODO: Use multiple variables and split()?
  if (class(polygons) != "SpatialPolygonsDataFrame") {
    stop("polygons must be a spatial polygons data frame or a list of spatial polygons data frames")
  }
  if (!(dissolve_var %in% names(polygons@data))) {
    stop("The variable ", dissolve_var, " does not appear in the polygons")
  }

  dissolve_ids <- unique(polygons@data[[dissolve_var]])
  dissolved_list <- lapply(X = dissolve_ids,
                           polygons = polygons,
                           dissolve_var = dissolve_var,
                           FUN = function(X, polygons, dissolve_var){
                             polygons_current <- polygons[polygons@data[[dissolve_var]] == X, ]
                             polygons_current <- methods::as(sf::st_combine(sf::st_as_sf(polygons_current)), "Spatial")
                             df <- data.frame(id = X,
                                              stringsAsFactors = FALSE)
                             names(df) <- dissolve_var
                             rownames(df) <- polygons_current@polygons[[1]]@ID
                             polygons_current <- sp::SpatialPolygonsDataFrame(Sr = polygons_current,
                                                                              data = df)
                             return(polygons_current)
                           })

  if (length(dissolved_list) == 1) {
    output <- dissolved_list[[1]]
  } else {
    output <- do.call(rbind,
                      dissolved_list)
  }

  return(output)
}

#' Remove the geometry of one set of polygons from another
#' @description Using sf functions, find the difference in geometry between two spatial polygons data frames
#' @param polygons Spatial polygons data frame. The polygons to remove geometry from. Also the decider of the output projection.
#' @param removal_polygons Spatial polygons data frame. The geometry being removed from \code{polygons}.
#' @export
punchout <- function(polygons,
                     removal_polygons) {
  if (class(polygons) != "SpatialPolygonsDataFrame") {
    stop("polygons must be a spatial polygons data frame or a list of spatial polygons data frames")
  }
  if (class(removal_polygons) != "SpatialPolygonsDataFrame") {
    stop("polygons must be a spatial polygons data frame or a list of spatial polygons data frames")
  }

  difference <- sf::st_difference(x = sf::st_union(sf::st_as_sf(polygons)),
                                  y = sf::st_union(sf::st_as_sf(removal_polygons)))

  types <- vapply(X = sf::st_geometry(difference),
                  FUN = function(X){
                    class(X)[2]},
                  "")

  output <- methods::as(difference[grepl(types,
                                         pattern = "*POLYGON")], "Spatial")

  return(output)
}


#' Order polygons according to size
#' @description For a spatial polygons data frame or list of spatial polygons data frames, return character vector of all the unique identifiers in either ascending or descending order of area. If the unique identifiers have more than on observation associated with them, their polygons are combined into a single observation before the areas are calculated. If no variable for unique identifiers is provided, then all polygons in each spatial polygons data frame provided are considered to be a single observation; note that this is inappropriate unless providing a list of SPDFs. In the case of a list of SPDFs either the variable containing unique identifiers must be named identically for all (in which case there can be more than one polygon identity recognized in each) or no variable provided (in which case each SPDF will be dissolved to a single observation and assigned an identity based on the list's names or if the list is unnamed using the index of the SPDF in the list).
#' @param polygons Spatial Polygons Data Frame or list of Spatial Polygons Data Frames. If only a single SPDF, it must have at least the variable matching the value of \code{id_var}. If a list of SPDFs and using \code{id_var} then every SPDF must contain that variable, otherwise each SPDF will be dissolved and assigned the ID corresponding to the list index it occupies or, if the list is named, the name.
#' @param id_var Optional character string. Must match the name of the variable in every SPDF in \code{polygons@@data} that contains the unique identifiers to dissolve along. Must provide if \code{polygons} is a single SPDF. Defaults to \code{NULL}.
#' @param order Charcter string. Either "ascending" or "descending" which determines the order to sort polygons in according to their areas. Defaults to \code{"ascending"}.
#' @return Vector of unique identifiers.
#' @export
order_polygons <- function(polygons,
                           id_var = NULL,
                           order = "ascending") {
  if (order == "ascending") {
    decreasing <- FALSE
  } else if (order == "descending") {
    decreasing <- TRUE
  } else {
    stop("order must be either 'ascending' or 'descending'")
  }

  if (class(polygons) == "list") {
    if (!all(sapply(polygons, class) == "SpatialPolygonsDataFrame")) {
      stop("All objects in the list polygons must be spatial polygons data frames")
    }

    polygon_list <- lapply(X = 1:length(polygons),
                           polygons = polygons,
                           id_var = id_var,
                           FUN = function(X, polygons, id_var){
                             current_polygons <- polygons[[X]]

                             if (is.null(id_var)) {
                               if (is.null(names(polygons))) {
                                 current_polygons@data[["polygon_id"]] <- X
                               } else {
                                 current_polygons@data[["polygon_id"]] <- names(polygons)[X]
                               }
                             } else {
                               if (!(id_var %in% names(current_polygons@data))) {
                                 stop("The variable ", id_var, " does not appear in the polygons data frame.")
                               }
                               current_polygons@data[["polygon_id"]] <- current_polygons@data[[id_var]]
                             }

                             if (length(unique(current_polygons@data[["polygon_id"]])) > nrow(current_polygons@data)) {
                               output <- dissolve(current_polygons,
                                                  dissolve_var = "polygon_id")
                             } else {
                               output <- current_polygons
                             }

                             return(output)
                           })

    dissolved_polygons <- do.call(rbind,
                                  polygon_list)

  } else if (class(polygons) != "SpatialPolygonsDataFrame") {
    stop("polygons must be a spatial polygons data frame or a list of spatial polygons data frames")
  } else {
    if (is.null(id_var)) {
      warning("There is no ID variable, so all polygons will be combined. If this was intended, consider simply using the function dissolve() instead.")
      polygons@data[["polygon_id"]] <- "polygon"
    } else {
      if (!(id_var %in% names(polygons@data))) {
        stop("The variable ", id_var, " does not appear in the polygons data frame.")
      }
      polygons@data[["polygon_id"]] <- polygons@data[[id_var]]
    }
    if (length(unique(polygons@data[["polygon_id"]])) > nrow(polygons@data)) {
      dissolved_polygons <- dissolve(polygons,
                                     dissolve_var = "polygon_id")
    } else {
      dissolved_polygons <- polygons
    }

  }

  dissolved_polygons <- add.area(spdf = dissolved_polygons)

  polygons_df <- dissolved_polygons@data

  polygons_df <- polygons_df[order(polygons_df[["AREA.HA"]],
                                   decreasing = decreasing), ]

  return(polygons_df[["polygon_id"]])
}


#' Attributing a spatial data frame using another spatial data frame
#'
#' This function will take a SpatialPoints/PolygonsDataFrame and add one attribute fields from a second SpatialPoints/PolygonsDataFrame
#' @param spdf1 A SpatialPoints/PolygonsDataFrame containing the geometry to add an attribute to
#' @param spdf2 A SpatialPoints/PolygonsDataFrame containing the geometry to add an attribute from
#' @param attributefield The name of the field in \code{spdf2} as a string containing the values to add to \code{spdf1}
#' @param newfield The name of the field in \code{spdf1} as a string to add the values from \code{spdf2$attributefield} to. If NULL, the field will use \code{attributefield}. Defaults to NULL.
#' @param projection An \code{sp::CRS()} argument to apply in the event that \code{spdf1} and \code{spdf2} have different projections. Defaults to \code{CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")}
#' @return The original SPDF spdf1 with the new field containing the values inherited from spdf2.
#' @examples
#' attribute.shapefile()
#' @export

attribute.shapefile <- function(spdf1,
                                spdf2,
                                attributefield = NULL,
                                newfield = NULL,
                                projection = sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
){
  if (!grepl(class(spdf1), pattern = "^Spatial((Points)|(Polygons))DataFrame$")) {
    stop("spdf1 must be either a Spatial Points or Spatial Polygons Data Frame")
  }
  if (class(spdf2) != "SpatialPolygonsDataFrame") {
    stop("spdf2 must be a Spatial Polygons Data Frame.")
  }
  if (is.null(attributefield) | !(attributefield %in% names(spdf2@data))) {
    stop("attributefield must be a field name found in spdf2")
  }

  if (any(is.na(spdf2@data[[attributefield]]))) {
    message(paste0("Removing geometry from spdf2 with NA in the field ", attributefield))
    spdf2 <- spdf2[!is.na(spdf2@data[[attributefield]]),]
  }

  if (is.null(newfield)) {
    newfield <- attributefield
  }

  if (newfield %in% names(spdf1@data)) {
    message(paste0("The variable ", newfield, " is already in spdf1 and will be overwritten."))
  }

  remove.coords <- FALSE
  coord.names <- colnames(spdf1@coords)

  if (spdf1@proj4string@projargs != spdf2@proj4string@projargs) {
    ## Make sure that the points also adhere to the same projection
    spdf2 <- sp::spTransform(spdf2, CRSobj = spdf1@proj4string)
  } else {
    projection <- spdf1@proj4string
  }

  ## Initialize list for attributed SPDFs
  attributed.dfs <- list()

  ## We'll check each attribute field value independently
  for (n in unique(spdf2@data[, attributefield])) {
    ## Create a copy of the points to work with on this loop
    current.spdf <- spdf1
    ## Get the data frame from over()
    over.result <- sp::over(current.spdf,
                            spdf2[spdf2@data[[attributefield]] %in% n, ])
    ## Add the values to the newfield column
    current.spdf@data[, newfield] <- over.result[, attributefield]
    if (!(coord.names[1] %in% names(current.spdf@data)) & !(coord.names[2] %in% names(current.spdf@data))){
      current.spdf@data <- cbind(current.spdf@data, current.spdf@coords)
      remove.coords <- TRUE
    }
    ## Make sure that the polygons have unique IDs
    if (class(current.spdf) == "SpatialPolygonsDataFrame") {
      current.spdf <- sp::spChFIDs(current.spdf,
                                   paste(runif(n = 1, min = 0, max = 666666666),
                                         row.names(current.spdf),
                                         sep = "."))
    }
    current.df <- current.spdf@data

    ## Only if the number of coordinates is greater than 0!
    print(nrow(current.df[current.df[[newfield]] %in% n, ]))
    if (nrow(current.df[current.df[[newfield]] %in% n, ]) > 0) {
      attributed.dfs[[paste(n)]] <- current.df[current.df[[newfield]] %in% n, ]
    }
  }

  if (length(attributed.dfs) > 0) {
    if (length(attributed.dfs) == 1) {
      output <- sp::SpatialPointsDataFrame(data = attributed.dfs[[1]],
                                           coords = attributed.dfs[[1]][, coord.names],
                                           proj4string = projection)
    } else {
      output <- sp::SpatialPointsDataFrame(data = dplyr::bind_rows(attributed.dfs),
                                           coords = dplyr::bind_rows(attributed.dfs)[, coord.names],
                                           proj4string = projection)
    }
    if (remove.coords) {
      output <- output[, names(output)[!(names(output) %in% coord.names)]]
    }
  } else {
    output <- NULL
  }
  return(output)
}

#' Creating the Intersection of Two Spatial Polygons Data Frames using sf functions
#' @description This uses \code{sf::st_intersection} to find ONLY the intersecting geometry of two SPDFs and return a single attribute variable from each.
#' @param polygons1 Spatial polygons data frame. One of the two sets of geometry to intersect. \code{polygons1@@data} must contain a variable with the name given as \code{polygons1_attribute}.
#' @param polygons2 Spatial polygons data frame. One of the two sets of geometry to intersect. \code{polygons2@@data} must contain a variable with the name given as \code{polygons2_attribute}.
#' @param polygons1_attribute Character string. The name of the variable in \code{polygons1@@data} for the output to inherit values from.
#' @param polygons2_attribute Character string. The name of the variable in \code{polygons2@@data} for the output to inherit values from.
#' @param polygons1_attribute_output Optional character string. The name of the variable in the output for the values inherited from \code{polygons1}. If \code{NULL} then the name from \code{polygons1_attribute} will be used. Defaults to \code{NULL}
#' @param polygons2_attribute_output Optional character string. The name of the variable in the output for the values inherited from \code{polygons2}. If \code{NULL} then the name from \code{polygons2_attribute} will be used. Defaults to \code{NULL}
#' @param projection Optional CRS object. Used as the projection for both \code{polygons1} and \code{polygons2}. If \code{NULL} then the \code{polygons@@proj4string} will be used instead. Defaults to \code{NULL}.
#' @export
sf_intersect <- function(polygons1,
                         polygons2,
                         polygons1_attribute,
                         polygons2_attribute,
                         polygons1_attribute_output = NULL,
                         polygons2_attribute_output = NULL,
                         projection = NULL){
  # Make sure the inputs are good to go
  if (class(polygons1)[1] != "SpatialPolygonsDataFrame") {
    stop("spdf1 must be a spatial polygons data frame")
  }
  if (class(polygons2)[1] != "SpatialPolygonsDataFrame") {
    stop("spdf2 must be a spatial polygons data frame")
  }

  if (is.null(projection)) {
    projection <- polygons1@proj4string
  } else {
    if (class(projection) != "CRS") {
      stop("The projection argument must either be NULL or a CRS object, e.g. sp::CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0')")
    }
  }

  # Reproject if necessary
  if (!identical(projection, polygons1@proj4string)) {
    polygons1 <- sp::spTransform(polygons1,
                                 projection)
  }
  if (!identical(projection, polygons2@proj4string)) {
    polygons2 <- sp::spTransform(polygons2,
                                 projection)
  }

  if (is.null(polygons1_attribute_output)) {
    polygons1_attribute_output <- polygons1_attribute
  }

  if (is.null(polygons2_attribute_output)) {
    polygons2_attribute_output <- polygons2_attribute
  }

  # Warn the user if we're going to do violence to the variable names
  if (polygons1_attribute_output == polygons2_attribute_output) {
    polygons1_attribute_output <- paste0(polygons1_attribute_output, "_1")
    polygons2_attribute_output <- paste0(polygons2_attribute_output, "_2")
    warning("The variable names from the polygons are identical, so the variables in the output will be ", polygons1_attribute_output, " for the values from polygons1 and ", paste0(polygons1_attribute_output, ".1"), " for the values from polygons2")
  }

  polygons1@data[[polygons1_attribute_output]] <- polygons1@data[[polygons1_attribute]]
  polygons2@data[[polygons2_attribute_output]] <- polygons2@data[[polygons2_attribute]]

  polygons_intersected <- sf::st_intersection(x = sf::st_as_sf(polygons1[, polygons1_attribute_output]),
                                              y = sf::st_as_sf(polygons2[, polygons2_attribute_output]))

  types <- vapply(X = sf::st_geometry(polygons_intersected),
                  FUN = function(X){
                    class(X)[2]},
                  "")

  output_polygons <- methods::as(polygons_intersected[grepl(types,
                                                            pattern = "*POLYGON"), ], "Spatial")

  return(output_polygons)
}

#' Creating the Intersection of Two Spatial Polygons Data Frames
#' @description This uses \code{rgeos::ginstersect} and assumes that the row names in both \code{spdf1} and \code{spdf2} DO NOT HAVE SPACES.
#' @param spdf1 Spatial polygons data frame. One of the two set of polygons to intersect. If \code{crs} is \code{NULL} and \code{spdf1} and \code{spdf2} have different CRSes, then the CRS from \code{spdf1} will be use to reproject \code{spdf2}.
#' @param spdf2 Spatial polygons data frame.
#' @param crs Optional \code{sp::CRS()} call. Used to reproject both \code{spdf1} and \code{spdf2} using \code{sp::spTransform()}. Defaults to \code{NULL}.
#' @export
rgeos.intersect <- function(spdf1,
                            spdf2,
                            crs = NULL){

  if (class(spdf1)[1] != "SpatialPolygonsDataFrame") {
    stop("spdf1 must be a spatial polygons data frame")
  }
  if (class(spdf2)[1] != "SpatialPolygonsDataFrame") {
    stop("spdf2 must be a spatial polygons data frame")
  }


  if (!is.null(crs)) {
    spdf1 <- sp::spTransform(spdf1, CRSobj = crs)
    spdf2 <- sp::spTransform(spdf2, CRSobj = crs)
  }

  if (spdf1@proj4string@projargs != spdf2@proj4string@projargs) {
    spdf2 <- sp::spTransform(spdf2, CRSobj = spdf1@proj4string)
  }
  # First off, just do the intersect with rgeos::gintersect()
  intersect.sp <- rgeos::gIntersection(spdf1,
                                       spdf2,
                                       byid = TRUE,
                                       drop_lower_td = TRUE)

  ## Now we need to build the data frame that goes back into this. It's a pain
  ## Get the place in the rownames from the polygons where a " " occurs
  intersection.rownames.splitpoints <-  sapply(X = row.names(intersect.sp),
                                               FUN = function(X){
                                                 gregexpr(X, pattern = " ")[[1]][1]
                                               })

  # Usng thse split information, get the rows from the source data that match each and cbind() them into a data frame
  intersection.dataframe <- cbind(spdf1@data[mapply(X = row.names(intersect.sp),
                                                    Y = intersection.rownames.splitpoints,
                                                    rownames = rownames(spdf1@data),
                                                    FUN = function(X, Y, rownames){
                                                      rowname <- substr(X, start = 1, stop = Y-1)
                                                      row.index <- grep(rownames, pattern = paste0("^", rowname, "$"))
                                                      return(row.index)}),],
                                  spdf2@data[mapply(X = row.names(intersect.sp),
                                                    Y = intersection.rownames.splitpoints,
                                                    rownames = rownames(spdf2@data),
                                                    FUN = function(X, Y, rownames){
                                                      rowname <- substr(X, start = Y + 1, stop = nchar(X))
                                                      row.index <- grep(rownames, pattern = paste0("^", rowname, "$"))
                                                      return(row.index)}),])

  # Set the rownames to be the same as the intersection SP
  rownames(intersection.dataframe) <- row.names(intersect.sp)

  # Make it an SPDF
  intersect.spdf <- sp::SpatialPolygonsDataFrame(Sr = intersect.sp,
                                                 data = intersection.dataframe)

  return(intersect.spdf)
}


#' Add areas to a Spatial Polygons Data Frame
#'
#' This function takes a Spatial Polygons Data Frame and calculates and adds area fields to the data frame. Areas can be calculated either treating the whole SPDF as a unit or for each polygon individually.
#' @param spdf Spatial Polygons Data Frame to calculate areas for.
#' @param area.ha Logical. If \code{TRUE}, areas will be calculated and added in hectares. Default is \code{TRUE}.
#' @param area.sqkm Logical. If \code{TRUE}, areas will be calculated and added in square kilometers. Default is \code{TRUE}.
#' @param byid Logical. If \code{TRUE}, areas will be calculated and added for each polygon by ID. If \code{FALSE} the area of the whole SPDF will be calculated and added, so every value for that field will be the same, regardless of polygon ID. Default is \code{TRUE}.
#' @return The original Spatial Polygons Data Frame with an additional field for each area unit calculated.
#' @keywords area
#' @examples
#' add.area()
#' @export

add.area <- function(spdf,
                     area.ha = TRUE,
                     area.sqkm = TRUE,
                     byid = TRUE
){
  ## Create a version in Albers Equal Area
  spdf.albers <- sp::spTransform(x = spdf, CRSobj = sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

  ## Add the area in hectares, stripping the IDs from gArea() output
  spdf@data$AREA.HA <- unname(rgeos::gArea(spdf.albers, byid = byid) * 0.0001)
  ## Add the area in square kilometers, converting from hectares
  spdf@data$AREA.SQKM <- spdf@data$AREA.HA * 0.01

  if (!(area.ha)) {
    spdf@data$AREA.HA <- NULL
  }
  if (!(area.sqkm)) {
    spdf@data$AREA.SQKM <- NULL
  }
  return(spdf)
}



#' Restricting imported TerrADat data to the boundaries of the imported design databases
#' @param dd.raw The output from \code{read.dd()}. The function uses the spatial polygons data frames found in the \code{sf} list
#' @param tdat.spdf The output from \code{read.tdat()}.
#' @return The contents of \code{read.tdat} that overlap spatially with the sample frames in \code{dd.raw}
#' @export
restrict.tdat <- function(dd.raw,
                          tdat.spdf){
  ## NAD83 sp::CRS()
  nad83.prj <- sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
  ## Save this for later
  tdat.prj <- tdat.spdf@proj4string
  ## Create a list to dump restricted SPDFs into
  restrictions <- list()
  ## Restrict!
  for (design in names(dd.raw$sf)) {
    restrictions[[design]] <-sp::spTransform(attribute.shapefile(spdf1 = tdat.spdf,
                                                                 spdf2 = dd.raw$sf[[design]],
                                                                 attributefield = "TERRA_SAMPLE_FRAME_ID",
                                                                 newfield = "SampleFrame"),
                                             CRSobj = nad83.prj)
  }
  ## If there was more than one SPDF
  if (length(restrictions) > 1) {
    ## Prime this with an empty data frame made from an empty subset. Ridiculous but necessary
    tdat.spdf.restricted <- subset(sp::spTransform(tdat.spdf, nad83.prj), "SiteID" == "Nothing")
    ## Mash together all the SPDFs because beautiful dplyr::bind_rows() doesn't work on SPDFs
    for (n in seq_along(restrictions)) {
      tdat.spdf.restricted <- rbind(tdat.spdf.restricted, restrictions[[n]])
    }
    return(tdat.spdf.restricted)
  } else {
    tdat.spdf.restricted <- sp::spTransform(restrictions[[1]], nad83.prj)
  }

  ## Drop that SampleFrame variable
  tdat.spdf.restricted@data <- tdat.spdf.restricted@data[, names(tdat.spdf.restricted@data)[!(names(tdat.spdf.restricted@data) %in% "SampleFrame")]]
  ## Build a new SPDF? I must've done this for a reason?
  tdat.spdf.restricted <- sp::SpatialPointsDataFrame(data = dplyr::distinct(tdat.spdf.restricted@data),
                                                     coords = dplyr::distinct(tdat.spdf.restricted@data)[, c("coords.x1", "coords.x2")],
                                                     proj4string = tdat.prj)
  tdat.spdf.restricted <- sp::spTransform(tdat.spdf.restricted, CRSobj = nad83.prj)
  return(tdat.spdf.restricted)
}

#' Erase geometry from a Spatial Polygons/Points Data Frame using a Spatial Polygons Data Frame with rgeos
#' @param spdf A Spatial Polygons Data Frame to remove FROM.
#' @param spdf.erase  A Spatial Polygons Data Frame to remove geometry from \code{spdf} WITH.
#' @param sliverdrop Optional logical value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_dropSlivers()} to temporarily set the environment during the erasure attempt. Defaults to \code{TRUE}.
#' @param sliverwarn Optional logical value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_warnSlivers()} to temporarily set the environment during the erasure attempt. Defaults to \code{TRUE}.
#' @param sliverdrop Optional numeric value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_polyThreshold()} to temporarily set the environment during the erasure attempt. Defaults to \code{0.01}.
#' @return The remaining geometry and data in \code{spdf} after any parts overlapping \code{spdf.erase} has been removed from it.
#' @export
erase.rgeos <- function(spdf,
                        spdf.erase,
                        sliverdrop = TRUE,
                        sliverwarn = TRUE,
                        sliverthreshold = 0.01){
  if (spdf@proj4string@projargs != spdf.erase@proj4string@projargs) {
    spdf.erase <- sp::spTransform(spdf.erase, CRSobj = spdf@proj4string)
  }

  current.drop <- rgeos::get_RGEOS_dropSlivers()
  current.warn <- rgeos::get_RGEOS_warnSlivers()
  current.tol <- rgeos::get_RGEOS_polyThreshold()

  rgeos::set_RGEOS_dropSlivers(sliverdrop)
  rgeos::set_RGEOS_warnSlivers(sliverwarn)
  rgeos::set_RGEOS_polyThreshold(sliverthreshold)
  message(paste0("Attempting using rgeos::set_RGEOS_dropslivers(", sliverdrop, ") and rgeos::set_RGEOS_warnslivers(", sliverwarn, ") and set_REGOS_polyThreshold(", sliverthreshold, ")"))
  ## Making this Albers for right now for gBuffer()
  ## The gbuffer() is a common hack to deal with ring self-intersections, which it seems to do just fine here?
  sp.temp <- rgeos::gDifference(spgeom1 = rgeos::gBuffer(sp::spTransform(spdf, sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")),
                                                         byid = TRUE,
                                                         width = 0.1),
                                spgeom2 = rgeos::gBuffer(sp::spTransform(spdf.erase,
                                                                         sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")),
                                                         byid = TRUE,
                                                         width = 0.1),
                                drop_lower_td = TRUE)

  rgeos::set_RGEOS_dropSlivers(current.drop)
  rgeos::set_RGEOS_warnSlivers(current.warn)
  rgeos::set_RGEOS_polyThreshold(current.tol)

  if (!is.null(frame.sp.temp)) {
    output <- sp::spTransform(sp::SpatialPolygonsDataFrame(sp.temp,
                                                           data = spdf@data[1:length(sp.temp@polygons),]),
                              CRSobj = spdf@proj4string)
  } else {
    output <- NULL
  }

  return(output)
}



#' Combine multiple spatial points data frames
#' @description Combines a list of spatial points data frames into a single spatial points data frame with only the variables common to all points.
#' @param points List of spatial points data frames. There must be at least one variable common to every set of points in the list.
#' @param add_missing Logical. If \code{TRUE} then the missing variables will be added to the points, but with \code{NA} because there are no matching values. This prevents the loss of variables. Defaults to \code{FALSE}.
#' @param projection Optional CRS object. Used as the projection for the output. If \code{NULL} then the projection from the points at index 1 of \code{points} will be used instead. Defaults to \code{NULL}.
#' @return Spatial points data frame with only variables common to all input points.
#' @export
combine_points <- function(points,
                           add_missing = FALSE,
                           projection = NULL){
  if (class(points) != "list") {
    stop("The argument points must be a list of two or more spatial points data frames.")
  }
  if (length(points) < 2) {
    stop("The argument points must be a list of two or more spatial points data frames.")
  }
  is_spdf <- sapply(points, class) == "SpatialPointsDataFrame"
  if (!all(is_spdf)) {
    stop(paste("The list has non-spdf values/objects at indices:", paste(which(!is_spdf), collapse = ", ")))
  }

  if (is.null(projection)) {
    projection <- points[[1]]@proj4string
  } else if (class(projection) != "CRS") {
    stop("The projection must either be NULL or a CRS object, e.g. sp::CRS('+proj=aea')")
  }

  var_summary <- table(unlist(sapply(points, names)))
  all_vars <- unique(unlist(sapply(points, names)))
  if (add_missing) {
    points_harmonized <- lapply(X = points,
                                vars = all_vars,
                                projection = projection,
                                FUN = function(X, vars, projection){
                                  current_points <- X
                                  if (nrow(current_points@data) > 0) {
                                    missing_vars <- all_vars[!(all_vars %in% names(current_points@data))]
                                    for (missing_var in missing_vars) {
                                      current_points@data[[missing_var]] <- NA
                                    }
                                    if (!identical(current_points@proj4string, projection)) {
                                      current_points <- sp::spTransform(current_points,
                                                                        CRSobj = projection)
                                    }
                                    return(current_points)
                                  } else {
                                    return(NULL)
                                  }
                                })
  } else {
    if (length(var_summary) < 1 | all(var_summary < 2)) {
      stop("There are no variable names common to all points. Aborting.")
    }
    common_vars <- names(var_summary)[var_summary == length(points)]
    common_vars <- names(points[[1]])[names(points[[1]]) %in% common_vars]
    points_harmonized <- lapply(X = points,
                                vars = common_vars,
                                projection = projection,
                                FUN = function(X, vars, projection){
                                  current_points <- X[, common_vars]
                                  if (nrow(current_points) > 0) {
                                    if (!identical(current_points@proj4string, projection)) {
                                      current_points <- sp::spTransform(current_points,
                                                                        CRSobj = projection)
                                    }
                                    return(current_points)
                                  } else {
                                    return(NULL)
                                  }
                                })
  }



  output <- do.call(rbind,
                    points_harmonized)

  return(output)
}

#' Combine spatial points data frames from TerrADat and sample design databases
#' @description Combines a list of spatial points data frames into a single spatial points data frame with only the variables common to all points. This also drops all the design points which correspond to TerrADat points because we want to use the actual sampled locations to determine weighting.
#' @param tdat_points Spatial points data frame or list of spatial points data frames. These must be points with the variables from TerrADat.
#' @param design_points Spatial points data frame or list of spatial points data frames. These must be points with the variables from AIM terrestrial sample design databases.
#' @param by Character string. The method to compare the TerrADat points and the design points to identify the design points that correspond to the TerrADat points. If \code{"key"} then it will be by the TerADat primary keys. If those are not populated in the design points, then use \code{"spatial"} to assume that any points within 100 m of the TerrADat points was sampled. Defaults to \code{"key"}.
#' @param projection Optional CRS object. Used as the projection for the output. If \code{NULL} then the projection from the points at index 1 of \code{tdat_points} will be used instead. Defaults to \code{NULL}.
#' @return Spatial points data frame with only variables common to all input points.
#' @export
combine_tdat_design <- function(terradat_points,
                                design_points,
                                by = "key",
                                add_coords = TRUE,
                                projection = NULL) {

  if (class(design_points) == "list") {
    design_points <- combine_points(design_points)
  } else if (class(design_points) != "SpatialPointsDataFrame") {
    stop("design_points must either be a spatial points data frame or a list of spatial points data frames")
  }
  if (class(terradat_points) == "list") {
    terradat_points <- combine_points(terradat_points)
  } else if (class(terradat_points) != "SpatialPointsDataFrame") {
    stop("terradat_points must either be a spatial points data frame or a list of spatial points data frames")
  }

  if (!identical(terradat_points@proj4string, design_points@proj4string)) {
    design_points <- sp::spTransform(design_points,
                                     CRSobj = terradat_points@proj4string)
  }

  if (!(by %in% c("key", "spatial"))) {
    stop("by must either be 'key' or 'spatial'")
  }



  ### Harmonize names between the sample design database and TerrADat
  varnames <- c("PLOT_NM" = "PlotID",
                "PLOT_KEY" = "PlotKey",
                "DT_VST" = "DateVisited",
                "TERRA_TERRADAT_ID" = "PrimaryKey",
                "FINAL_DESIG" = "FinalDesignation")
  missing_varnames <- names(varnames)[!(names(varnames) %in% names(design_points@data))]
  if (length(missing_varnames) > 1) {
    stop(paste("The following necessary variable names are missing from design_points:", paste(missing_varnames, collapse = ", ")))
  }
  for (varname in names(varnames)) {
    names(design_points@data)[names(design_points@data) == varname] <- varnames[varname]
  }

  # If they don't have designations, assume they're MYSTERIES
  design_points@data[is.na(design_points@data[["FinalDesignation"]]), "FinalDesignation"] <- "Unknown"

  terradat_points@data[["FinalDesignation"]] <- "Target Sampled"

  # Remove the points that ended up in TerrADat from the design points
  switch(by,
         "key" = {
           design_points <- design_points[!(design_points@data[["PrimaryKey"]] %in% terradat_points@data[["PrimaryKey"]]), ]
         },
         "spatial" = {
           polygons <- raster::buffer(terradat_points,
                                      width = 100,
                                      dissolve = FALSE)
           polygon_data <- terradat_points@data
           rownames(polygon_data) <- row.names(polygons)
           polygons <- sp::SpatialPolygonsDataFrame(Sr = polygons,
                                                    data = polygon_data)
           over_vector <- sp::over(x = design_points,
                                   y = polygons)[["PrimaryKey"]]
           design_points <- design_points[is.na(over_vector), ]
         })

  # Mash 'em up
  output <- combine_points(c(terradat_points, design_points),
                           projection = projection,
                           add_missing = TRUE)

  if (add_coords) {
    output <- add_coords(output,
                         xynames = c("x", "y"))
  }

  output@data[["temp_id"]] <- 1:nrow(output@data)

  return(output)
}

#' Use intersect_rgeos() to clip spatial points or polygons data frames
#' @description Spatially restrict spatial points or polygons data frames without losing any information (other than the excluded geometry).
#' @param x Spatial points or polygons data frame. The geometry to restrict.
#' @param clip_polygons Spatial polygons data frame. The geometry to restrict \code{x} to.
#' @param scale Numeric. A value to be passed to \code{rgeos::setScale()} to use for the steps of this function. It will be returned to the previous value before the function returns its output. Defaults to \code{1e5} (precision to five decimal places).
#' @export

clip_rgeos <- function(x,
                       clip_polygons,
                       verbose = FALSE,
                       scale = 1e5){

  current_scale <- rgeos::getScale()
  rgeos::setScale(scale)

  x_class <- class(x)[1]
  if (!(x_class %in% c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame"))) {
    stop("Polygons must be spatial polygons or points data frame")
  } else if (nrow(x@data) < 1) {
    stop("There are no data in polygons")
  }
  if (class(clip_polygons) != "SpatialPolygonsDataFrame") {
    stop("Polygons must be spatial polygons data frame")
  } else if (nrow(clip_polygons@data) < 1) {
    stop("There are no data in polygons")
  }

  if (!identical(x@proj4string, clip_polygons@proj4string)) {
    clip_polygons <- sp::spTransform(clip_polygons,
                                     CRSobj = x@proj4string)
  }

  switch(x_class,
         "SpatialPolygonsDataFrame" = {
           output <- intersect_rgeos(polygons1 = x,
                                     polygons2 = clip_polygons,
                                     verbose = verbose)
         },
         "SpatialPointsDataFrame" = {
           clip_polygons@data[["id"]] <- 1:nrow(clip_polygons@data)
           over_results <- sp::over(x = x,
                                    y = clip_polygons)[["id"]]
           output <- x[!is.na(over_results), ]
         })


  output <- output[, names(x@data)]

  rgeos::setScale(current_scale)

  return(output)
}
