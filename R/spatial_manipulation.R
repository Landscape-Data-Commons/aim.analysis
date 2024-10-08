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
  if (!("sf" %in% class(polygons))) {
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

  # Make sure we don't leave this in an unexpected state
  sf::sf_use_s2(current_s2_status)

  return(output)
}

# #' Generate weight category polygons
# #' @description Create weight category polygons from the intersection of multiple spatial polygons data frames. When combining overlapping designs, weight categories are the unique combinations of the sample frame (or stratification) identities from the designs. This takes a list of spatial polygons data frames which have a common variable containing their unique weighting identifier (e.g. three stratification schemes that all contain a variable named "wgt_cat" that holds the stratum identities) and intersects them to find the weight categories. This is extremely vulnerable to malformed/invalid geometry and so it is highly recommended that you use the argument \code{makevalid = TRUE} to prompt the function to check and attempt to repair geometry as needed after every geoprocessing step. This is slower, but the intersections will almost certainly fail without it. If you are feeling especially paranoid, you can also use \code{force = TRUE} to force repair actions even on valid geometry.
# #' @param polygons List of spatial polygons data frames. These are the polygons to intersect and should be the same as the polygons used to draw the designs being combined. They must all have a the \code{idvar} variable containing the identities of the individual polygons (e.g. stratum). If you are combining designs that used the same polygons (e.g. two different designs using the exact same sample frame and stratification) ONLY INCLUDE ONE OF THEM. Including duplicates can result in very, very long processing times and failed intersections. They will be intersected sequentially in order, so there may be some slight speed gains if they're sorted smallest to largest, but no guarantees.
# #' @param idvar Character string. The name of the variable found in every set of polygons that contains the polygon identities.
# #' @param makevalid Logical. If \code{TRUE} then the function \code{repair_geometry()} will be applied to the product of every geoprocessing step. If \code{FALSE} then there will be no validity checks. It is STRONGLY recommended that you use \code{TRUE} Defaults to \code{TRUE}.
# #' @param force Logical. This is passed to \code{repair_geometry()} if \code{makevalid = TRUE}. If \code{TRUE} this will force the function to attempt to repair geometry even if there are no errors. This is for the paranoid. Defaults to \code{FALSE}.
# #' @param verbose Logical. If \code{TRUE} then the function will produce informative messages as it executes its steps. Useful for debugging. Defaults to \code{FALSE}.
# #' @param use_spherical_geometry Logical. USE AT YOUR OWN RISK. Controls if sf uses spherical geometry or not. For particularly wonky sf objects, this may be necessary to effect any kind of repair but can have unintended consequences for the resulting geometry. If \code{TRUE} then \code{sf::sf_use_s2()} will be set to \code{TRUE} which is the default for the package. Defaults to \code{TRUE}.
# #' @param scale Numeric. A value to be passed to \code{rgeos::setScale()} to use for the steps of this function. It will be returned to the previous value before the function returns its output. Defaults to \code{1e5} (precision to five decimal places).
# #' @return A spatial polygons data frame with the single variable \code{"wgt_cat"} in the data slot containing the unique weight categories.
# #' @export
# wgtcat_gen <- function(polygons,
#                        id_var,
#                        makevalid = TRUE,
#                        force = FALSE,
#                        verbose = FALSE,
#                        use_spherical_geometry = TRUE,
#                        scale = 1e5) {
#   if (!("sf" %in% class(polygons))) {
#     stop("The polygons must be provided as a single polygon or multipolygon sf object.")
#   }
#   if (!any(c("POLYGON", "MULTIPOLYGON") %in% sf::st_geometry_type(polygons))) {
#     stop("The polygons must be provided as a single polygon or multipolygon sf object.")
#   }
#
#   if (class(id_var) != "character") {
#     stop("idvar must be a single character string")
#   }
#   if (length(id_var) > 1) {
#     stop("idvar must be a single character string")
#   }
#   if (!(id_var %in% names(polygons))) {
#     stop(paste("The variable", id_var, "is missing from the polygon attributes."))
#   }
#
#   if (nrow(polygons) < 1) {
#     stop("The polygons contain no values.")
#   }
#
#   if (nrow(polygons) == 1) {
#     warning("There is only one polygon (or multipolygon), making intersection impossible.")
#     return(polygons)
#   }
#
#   # If the polygons are to be made valid, do that
#   if (makevalid) {
#     if (verbose) {
#       message("Checking polygons for invalid geometry")
#     }
#     polygons <- repair_geometry(polygons = polygons,
#                                 verbose = verbose,
#                                 force = force,
#                                 use_spherical_geometry = use_spherical_geometry)
#   }
#
#
#   # Find those intersections!!!
#   if (verbose) {
#     message("Attempting to find the intersection of all provided polygons")
#   }
#   # Run the intersection
#   polygons_intersected <- sf::st_intersection(polygons)
#
#   # Then create the weight category variable by jamming the other two together
#   results@data[["wgtcat"]] <- mapply(var1 = results@data[[1]],
#                                      var2 = results@data[[2]],
#                                      FUN = function(var1, var2){
#                                        paste(na.omit(c(var1, var2)), collapse = "_")
#                                      })
#
#   # Geometry errors are rampant, so always offer an opportunity to fix them
#   if (makevalid) {
#     if (verbose) {
#       message("Evaluating integrity of current intersection results.")
#     }
#     results <- repair_geometry(polygons = results,
#                                verbose = verbose,
#                                force = force)
#   }
#
#   # So that we don't leave spaces in there that'll screw up potential future splits
#   row.names(results) <- sapply(X = row.names(results),
#                                FUN = function(X){
#                                  gsub(X,
#                                       pattern = " ",
#                                       replacement = "_")
#                                })
#   rownames(results@data) <- row.names(results)
#   # results <- dissolve(polygons = results,
#   #                     dissolve_var = "wgtcat")
#   if (makevalid) {
#     if (verbose) {
#       message("Evaluating integrity of dissolved intersection results.")
#     }
#     results <- repair_geometry(polygons = results,
#                                verbose = verbose,
#                                force = force)
#   }
#
#
#   # And now we enter the looping phase where each successive entry in polygons gets intersected with the results of the previous intersections
#   # Same steps as above, just in sequence!
#   if (polygons_count > 2) {
#     for (index in 3:length(polygons)) {
#       if (verbose) {
#         message(paste("Attempting to intersect the current intersection results with the polygons at index", index))
#       }
#       results <- intersect_rgeos(polygons1 = results[, "wgtcat"],
#                                  polygons2 = polygons[[index]],
#                                  keep_outer = TRUE,
#                                  verbose = verbose,
#                                  makevalid = makevalid,
#                                  force = force,
#                                  scale = scale)
#
#       if (!is.null(results)) {
#         results@data[["wgtcat"]] <- mapply(var1 = results@data[[1]],
#                                            var2 = results@data[[2]],
#                                            FUN = function(var1, var2){
#                                              paste(na.omit(c(var1, var2)), collapse = "_")
#                                            })
#
#         if (makevalid) {
#           if (verbose) {
#             message("Evaluating integrity of current intersection results.")
#           }
#           results <- repair_geometry(polygons = results,
#                                      verbose = verbose,
#                                      force = force)
#         }
#         if (verbose) {
#           message("Dissolving intersection by wgtcat.")
#         }
#
#         # So that we don't leave spaces in there that'll screw up potential future splits
#         row.names(results) <- sapply(X = row.names(results),
#                                      FUN = function(X){
#                                        gsub(X,
#                                             pattern = " ",
#                                             replacement = "_")
#                                      })
#         rownames(results@data) <- row.names(results)
#
#         if (makevalid) {
#           if (verbose) {
#             message("Evaluating integrity of dissolved intersection results.")
#           }
#           results <- repair_geometry(polygons = results,
#                                      verbose = verbose,
#                                      force = force)
#         }
#       }
#     }
#   }
#
#   results <- dissolve(polygons = results,
#                       dissolve_var = "wgtcat")
#   return(results[, "wgtcat"])
# }

# #' Dissolve polygons according to unique identities
# #' @description For a spatial polygons data frame, return a spatial polygons data frame that contains only one observation for each unique identifier associated with the combined geometry of all polygons belonging to that identifier.
# #' @param polygons Spatial Polygons Data Frame. Must have at least the variable matching the value of \code{dissolve_var}.
# #' @param dissolve_var Character string. Must match the name of the variable in \code{polygons@@data} that contains the unique identifiers to dissolve along.
# #' @return Spatial polygons data frame containing only the variable named in \code{dissolve_var} and only one observation per value in that variable.
# #' @export
# dissolve <- function(polygons,
#                      dissolve_var){
#   # TODO: Use multiple variables and split()?
#   if (!("sf" %in% class(polygons))) {
#     stop("polygons must be an sf polygons object.")
#   }
#   if (!(dissolve_var %in% names(polygons))) {
#     stop("The variable ", dissolve_var, " does not appear in the polygons.")
#   }
#
#   dissolved_list <- lapply(X = unique(polygons[[dissolve_var]]),
#                            polygons = polygons,
#                            dissolve_var = dissolve_var,
#                            FUN = function(X, polygons, dissolve_var){
#                              current_crs <- sf::st_crs(polygons)
#                              polygons_current <- dplyr::filter(.data = polygons,
#                                                                dissolve_var == X)
#                              polygons_current <- sf::st_combine(sf::st_as_sf(polygons_current))
#                              df <- data.frame(id = X,
#                                               stringsAsFactors = FALSE)
#                              names(df) <- dissolve_var
#                              rownames(df) <- polygons_current@polygons[[1]]@ID
#                              polygons_current <- sp::SpatialPolygonsDataFrame(Sr = polygons_current,
#                                                                               data = df)
#                              return(polygons_current)
#                            })
#
#   if (length(dissolved_list) == 1) {
#     output <- dissolved_list[[1]]
#   } else {
#     output <- dplyr::bind_rows(dissolved_list)
#   }
#
#   return(output)
# }


#' Add areas to a polygon or multipolygon sf object
#'
#' This function takes a polygon or multipolygon sf object and calculates and adds area fields to the data frame. It will always add square meters, but may additionally add square kilometers and hectares as well.
#' @param polygons Polygon or multipolygon sf object. These are the polygons to add the area variables to. Areas will be added to each observation in the attributes, so make sure that polygons are "dissolved" by their identities before calculating areas.
#' @param area_ha Logical. If \code{TRUE}, areas will be calculated and added in hectares. Default is \code{TRUE}.
#' @param area_sqkm Logical. If \code{TRUE}, areas will be calculated and added in square kilometers. Default is \code{TRUE}.
#' @return The original sf object with an additional field for each area unit calculated, always with at least \code{"area_sqm"}.
#' @keywords area
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

#' Adding coordinate variables to an sf points object
#'
#' @description Adds one or more of the following: the coordinates from the current coordinate reference system; coordinates in NAD83; and coordinates in Albers Equal Area. This does not change the projection of the points.
#' @param points A points sf object. The coordinates for the points will be added for at least the current projection.
#' @param coord_names Character vector. The names for the coordinate variables using the CRS of \code{points}. Format is \code{c("name for x coord", "name for y coord")}. Defaults to \code{c("x_coord", "y_coord")}.
#' @param nad83 Logical. If \code{TRUE} Then the variables \code{LONGITUDE_NAD83} and \code{LATITUDE_NAD83} will be added using NAD83 as the CRS. Defaults to \code{FALSE}.
#' @param albers Logical. If \code{TRUE} Then the variables \code{X_METERS_AL} and \code{Y_METERS_AL} will be added using Albers Equal Area as the CRS. Defaults to \code{FALSE}.
#' @return \code{points} with coordinate fields added to the data frame as requested.
#' @export
add_coords <- function(points,
                       coord_names = c("x_coord", "y_coord"),
                       nad83 = FALSE,
                       albers = FALSE){

  projections <- list(input = sf::st_crs(points),
                      nad83 = sf::st_crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"),
                      albers = sf::st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

  coord_names <- list(input = setNames(object = c("X", "Y"),
                                       nm = coord_names),
                      nad83 = c(LONGITUDE_NAD83 = "X",
                                LATITUDE_NAD83 = "Y"),
                      albers = c(X_METERS_AL = "X",
                                 Y_METERS_AL = "Y"))

  for (current_projection in c("input", c("nad83", "albers")[c(nad83, albers)])) {
    current_coords <- sf::st_transform(x = points,
                                       crs = projections[[current_projection]]) |>
      sf::st_coordinates(x = _)

    points <- dplyr::bind_cols(points,
                               current_coords) |>
      dplyr::rename(.data = _,
                    coord_names[[current_projection]]) |>
      dplyr::select(.data = _,
                    tidyselect::all_of(names(points)),
                    tidyselect::everything())
  }

  points
}
