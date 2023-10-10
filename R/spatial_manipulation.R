#' Identify and repair invalid geometry in spatial polygons data frames
#' @description Using functions from sf, check the geometry of a set of polygons. If the geometry invalid, it attempts to buffer the polygons with \code{sf::st_buffer(dist = 0)}. If the geometry is corrupt or fine, it does nothing.
#' @param polygons Spatial polygons (either sf or spatial polygons data frame). The polygons to be checked. Note that the function will halt if the geometry is corrupt and not return a value.
#' @param verbose Logical. If \code{TRUE} then the function will produce informative messages as it executes its steps. Useful for debugging. Defaults to \code{FALSE}.
#' @param force Logical. If \code{TRUE} then both valid and invalid polygons will be buffered by 0. This shouldn't be necessary, but is a feature for the paranoid.
#' @param use_spherical_geometry Logical. USE AT YOUR OWN RISK. Controls if sf uses spherical geometry or not. For particularly wonky sf objects, this may be necessary to effect any kind of repair but can have unintended consequences for the resulting geometry. If \code{TRUE} then \code{sf::sf_use_s2()} will be set to \code{TRUE} which is the default for the package. Defaults to \code{TRUE}.
#' @return The spatial polygons data frame \code{polygons1}. This will be unchanged if the geometry was valid or repaired if it was invalid.
#' @export
repair_geometry <- function(polygons,
                            verbose = FALSE,
                            force = FALSE,
                            use_spherical_geometry = TRUE) {
  if("SpatialPolygonsDataFrame" %in% class(polygons)) {
    polygons_sf <- sf::st_as_sf(polygons)
    spdf <- TRUE
  } else if ("sf" %in% class(polygons)) {
    polygons_sf <- polygons
    spdf <- FALSE
  } else {
    stop("polygons must either be a spatial polygons data frame or an sf object")
  }

  # Get the current setting for using s2 before setting it to what the user
  # has requested for this
  current_s2_status <- sf::sf_use_s2()
  sf::sf_use_s2(use_spherical_geometry)

  validity_check <- sf::st_is_valid(polygons_sf)

  if (any(is.na(validity_check))) {
    stop("The geometry of the polygons is corrupt. Unable to repair.")
  }

  if (!all(validity_check)) {
    if (verbose) {
      message("Invalid geometry found. Attempting to make the geometry valid.")
    }
    output <- sf::st_make_valid(polygons_sf)
  } else if (force) {
    if (verbose) {
      message("No invalid geometry found. Attempting to make the geometry valid anyway.")
    }
    output <- sf::st_make_valid(polygons_sf)
  } else {
    if (verbose) {
      message("No invalid geometry found.")
    }
    output <- polygons_sf
  }

  validity_check <- sf::st_is_valid(output)

  if (any(is.na(validity_check))) {
    stop("Attempting to make the polygons valid resulted in corrupted geometry. Unable to repair.")
  }

  if (!all(validity_check)) {
    if (verbose) {
      message("Invalid geometry still found. Attempting to repair via buffering.")
    }
    output <- sf::st_buffer(x = output,
                            dist = 0)

    postbuffer_validity_check <- sf::st_is_valid(output)

    if (!all(postbuffer_validity_check)) {
      warning("Even after buffering, geometry appears to still be invalid.")
    }

  } else if (force) {
    if (verbose) {
      message("No invalid geometry found. Attempting to repair via buffering anyway.")
    }
    output <- sf::st_buffer(x = output,
                            dist = 0)
  } else {
    if (verbose) {
      message("No invalid geometry found after attempted repair.")
    }
  }

  if (spdf) {
    output <- methods::as(output, "Spatial")
  }

  # Make sure we don't leave this in an unexpected state
  sf::sf_use_s2(current_s2_status)

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
wgtcat.gen <- function(polygons,
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

#' Generate weight category polygons
#' @description Create weight category polygons from the intersection of multiple spatial polygons data frames. When combining overlapping designs, weight categories are the unique combinations of the sample frame (or stratification) identities from the designs. This takes a list of spatial polygons data frames which have a common variable containing their unique weighting identifier (e.g. three stratification schemes that all contain a variable named "wgt_cat" that holds the stratum identities) and intersects them to find the weight categories. This is extremely vulnerable to malformed/invalid geometry and so it is highly recommended that you use the argument \code{makevalid = TRUE} to prompt the function to check and attempt to repair geometry as needed after every geoprocessing step. This is slower, but the intersections will almost certainly fail without it. If you are feeling especially paranoid, you can also use \code{force = TRUE} to force repair actions even on valid geometry.
#' @param polygons List of spatial polygons data frames. These are the polygons to intersect and should be the same as the polygons used to draw the designs being combined. They must all have a the \code{idvar} variable containing the identities of the individual polygons (e.g. stratum). If you are combining designs that used the same polygons (e.g. two different designs using the exact same sample frame and stratification) ONLY INCLUDE ONE OF THEM. Including duplicates can result in very, very long processing times and failed intersections. They will be intersected sequentially in order, so there may be some slight speed gains if they're sorted smallest to largest, but no guarantees.
#' @param idvar Character string. The name of the variable found in every set of polygons that contains the polygon identities.
#' @param makevalid Logical. If \code{TRUE} then the function \code{repair_geometry()} will be applied to the product of every geoprocessing step. If \code{FALSE} then there will be no validity checks. It is STRONGLY recommended that you use \code{TRUE} Defaults to \code{TRUE}.
#' @param force Logical. This is passed to \code{repair_geometry()} if \code{makevalid = TRUE}. If \code{TRUE} this will force the function to attempt to repair geometry even if there are no errors. This is for the paranoid. Defaults to \code{FALSE}.
#' @param verbose Logical. If \code{TRUE} then the function will produce informative messages as it executes its steps. Useful for debugging. Defaults to \code{FALSE}.
#' @param use_spherical_geometry Logical. USE AT YOUR OWN RISK. Controls if sf uses spherical geometry or not. For particularly wonky sf objects, this may be necessary to effect any kind of repair but can have unintended consequences for the resulting geometry. If \code{TRUE} then \code{sf::sf_use_s2()} will be set to \code{TRUE} which is the default for the package. Defaults to \code{TRUE}.
#' @param scale Numeric. A value to be passed to \code{rgeos::setScale()} to use for the steps of this function. It will be returned to the previous value before the function returns its output. Defaults to \code{1e5} (precision to five decimal places).
#' @return A spatial polygons data frame with the single variable \code{"wgt_cat"} in the data slot containing the unique weight categories.
#' @export
wgtcat_gen <- function(polygons,
                       id_var,
                       makevalid = TRUE,
                       force = FALSE,
                       verbose = FALSE,
                       use_spherical_geometry = TRUE,
                       scale = 1e5) {
  if (!("sf" %in% class(polygons))) {
    stop("The polygons must be provided as a single polygon or multipolygon sf object.")
  }
  if (!any(c("POLYGON", "MULTIPOLYGON") %in% sf::st_geometry_type(polygons))) {
    stop("The polygons must be provided as a single polygon or multipolygon sf object.")
  }

  if (class(id_var) != "character") {
    stop("idvar must be a single character string")
  }
  if (length(id_var) > 1) {
    stop("idvar must be a single character string")
  }
  if (!(id_var %in% names(polygons))) {
    stop(paste("The variable", id_var, "is missing from the polygon attributes."))
  }

  if (nrow(polygons) < 1) {
    stop("The polygons contain no values.")
  }

  if (nrow(polygons) == 1) {
    warning("There is only one polygon (or multipolygon), making intersection impossible.")
    return(polygons)
  }

  # If the polygons are to be made valid, do that
  if (makevalid) {
    if (verbose) {
      message("Checking polygons for invalid geometry")
    }
    polygons <- repair_geometry(polygons = polygons,
                                verbose = verbose,
                                force = force,
                                use_spherical_geometry = use_spherical_geometry)
  }


# Find those intersections!!!
  if (verbose) {
    message("Attempting to find the intersection of all provided polygons")
  }
  # Run the intersection
  polygons_intersected <- sf::st_intersection(polygons)

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

#' Add areas to a polygon or multipolygon sf object
#'
#' This function takes a polygon or multipolygon sf object and calculates and adds area fields to the data frame. It will always add square meters, but may additionally add square kilometers and hectares as well.
#' @param polygons Polygon or multipolygon sf object. These are the polygons to add the area variables to. Areas will be added to each observation in the attributes, so make sure that polygons are "dissolved" by their identities before calculating areas.
#' @param area_ha Logical. If \code{TRUE}, areas will be calculated and added in hectares. Default is \code{TRUE}.
#' @param area_sqkm Logical. If \code{TRUE}, areas will be calculated and added in square kilometers. Default is \code{TRUE}.
#' @return The original sf object with an additional field for each area unit calculated, always with at least \code{"area_sqm"}.
#' @keywords area
#' @examples
#' add_area()
#' @export
add_area <- function(polygons,
                     area_ha = TRUE,
                     area_sqkm = TRUE,
                     byid = TRUE) {
  proj_aea <- sf::st_crs("+proj=aea +lat_1=29.5 +lat_2=42.5")

  if (!("sf" %in% class(polygons))) {
    stop("polygons must be a polygon or multipolygon sf object")
  }

  current_crs <- sf::st_crs(polygons)

  if (!identical(proj_aea, current_crs)) {
    polygons <- sf::st_transform(x = polygons,
                                 crs = proj_aea)
  }

  polygons$area_sqm <- as.vector(sf::st_area(x = polygons))

  if (area_ha) {
    polygons$area_ha <- polygons$area_sqm / 10000
  }

  if (area_sqkm) {
    polygons$area_sqkm <- polygons$area_sqm / 1000000
  }

  sf::st_transform(x = polygons,
                   crs = current_crs)
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
