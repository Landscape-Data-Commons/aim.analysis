#' Confirm polygons contain points
#' @export
check_polygons <- function(polygons,
                           points,
                           minimum = 1,
                           polygon_id_var = NULL,
                           verbose = FALSE) {
  if (is.null(polygon_id_var)) {
    # if (verbose) {
    #   message("Assuming each record in polygons is a separate polygont to check. If this is not true, please provide the variable containing the unique identifiers for the polygons.")
    # }
    polygons[["internal_use_uid"]] <- seq_len(length.out = nrow(polygons))
  } else {
    polygons[["internal_use_uid"]] <- polygons[[polygon_id_var]]
  }

  represented_polygons <- sf::st_intersection(x = sf::st_set_agr(x = polygons,
                                                                 value = "constant"),
                                              y = sf::st_set_agr(x = points,
                                                                 value = "constant")) |>
    sf::st_drop_geometry() |>
    dplyr::pull(.data = _,
                internal_use_uid) |>
    unique()

  if (!all(polygons[["internal_use_uid"]] %in% represented_polygons) & verbose) {
    message(paste0("The following polygons did not contain any of the provided points: ",
                   paste(setdiff(x = unique(polygons[["internal_use_uid"]]),
                                 y = represented_polygons),
                         collapse = ", ")))
  }
  all(polygons[["internal_use_uid"]] %in% represented_polygons)
}


#' Generate density partitions
#' @export
dpart_gen <- function(frame,
                      points,
                      n_dpart = 2,
                      min_points = 1,
                      iteration_limit = 1,
                      # This is the sampling distance in meters for calculating density.
                      # The smaller this number, the finer-grained the density map but that can cause
                      # serious memory allocation issues when you have larger areas.
                      density_sample_spacing = 100,
                      buffer_distance = 1.55 * density_sample_spacing,
                      max_cells_subset = 500000,
                      output_list = FALSE,
                      accept_failure = FALSE,
                      sub_frame = TRUE,
                      verbose = FALSE){

  # Define Alber's Equal Area CRS.
  # This is for adding area and just standardization.
  projection <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

  # Sanitization
  if (!("sf" %in% class(frame))) {
    stop("`frame` must be an sf polygon object")
  } else {
    frame <- frame[sf::st_geometry_type(frame) %in% c("POLYGON", "MULTIPOLYGON"), ]
  }
  if (nrow(frame) < 1) {
    stop("`frame` must be an sf polygon object")
  } else {
    output_crs <- sf::st_crs(frame)
    frame <- sf::st_transform(x = frame,
                              crs = projection) |>
      # Remove any Z dimension
      # It screws with the process and is irrelevant here.
      sf::st_zm(x = _,
                drop = TRUE) |>
      # Dissolve!
      sf::st_union(x = _) |>
      sf::st_as_sf(x = _) |>
      dplyr::mutate(.data = _,
                    uid = 1) |>
      # This is so that sf doesn't constantly warn us about attributes being
      # assumed constant.
      sf::st_set_agr(x = _,
                     value = "constant")
  }


  if (!("sf" %in% class(points))) {
    stop("`points` must be an sf points object")
  } else if (!all(sf::st_geometry_type(points) %in% c("POINT"))) {
    stop("`points` must be an sf points object")
  }

  points <- sf::st_transform(x = points,
                             crs = projection) |>
    # This is so that sf doesn't constantly warn us about attributes being
    # assumed constant.
    sf::st_set_agr(x = _,
                   value = "constant") |>
    # Keep only points within the frame!
    sf::st_intersection(x = _,
                        y = frame) |>
    # Can't trust them to not have a Z dimension which breaks steps downstream
    sf::st_zm(x = _,
              drop = TRUE)

  if (nrow(points) < (n_dpart * min_points)) {
    stop(paste0("There are only ", nrow(points), " points available in the frame, too few to put ",
                min_points,
                " points in each of ",
                n_dpart,
                " density partitions."))
  }
  start_time <- Sys.time()
  # Get the point coordinates to feed into spatstat.geom::ppp()
  point_coords <- unique(sf::st_coordinates(points))

  # Get the owin object for spatstat.geom::ppp()
  # We're going to get weird here. The polygons are often too complicated to
  # generate an owin object from, so we're going to remove holes and buffer a bit
  # to get simpler polygons that are slightly larger than the inference area so
  # we can calculate density then trim those results down to the inference area.
  if (verbose) {
    message("Finding density across the frame.")
  }
  density_frame <- sf::st_buffer(x = frame,
                                 dist = buffer_distance)

  frame_owin <- spatstat.geom::as.owin(density_frame)

  # if (verbose) {
  #   message("Creating point pattern object for density partitioning.")
  # }
  #
  # Make the ppp object (a point pattern)
  points_ppp <- spatstat.geom::ppp(x = point_coords[, 1],
                                   y = point_coords[, 2],
                                   window = frame_owin)

  # if (verbose) {
  #   message("Finding distribution density from point pattern. If this is very slow, consider increasing the value of density_sample_spacing.")
  # }
  # Get the density info from the point pattern
  points_density <- density(points_ppp,
                            # For the CRS we're using, the units on this will
                            # be meters but it's CRS-dependent
                            eps = density_sample_spacing)

  # Make a data frame of coordinates with the density at each coordinate
  density_df <- expand.grid(y = points_density$yrow,
                            x = points_density$xcol)
  density_df$density <- as.vector(points_density$v)


  # And now that we have the density data frame
  # if (verbose) {
  #   message("Finding partition breaks. If this is very slow, consider decreasing the value of max_cells_subset.")
  # }

  if (nrow(density_df) > max_cells_subset & verbose) {
    message(paste0("The number of cells in the density data is ", nrow(density_df),
                   " which is more than the current max_cells_subset value of ", max_cells_subset, ". ",
                   "To use all the density values and not just a subset, set max_cells_subset to Inf."))
  }

  current_partition_count <- n_dpart
  # Figure out where the breaks are for the partitions
  partition_breaks <- BAMMtools::getJenksBreaks(var = density_df[["density"]],
                                                subset = min(nrow(density_df),
                                                             max_cells_subset),
                                                # This'll a number of values
                                                # equal to k and the terminal
                                                # values will be the min and
                                                # max.
                                                # So in order to get partition
                                                # ranges, we need to add 1 to
                                                # k so we get enough breakpoints
                                                k = current_partition_count + 1)

  if (verbose) {
    message("Partitioning area by densities according to identified breaks.")
  }

  for (partition_id in length(partition_breaks):2) {
    # if (verbose) {
    #   message(paste0("Identifying partition ", partition_id - 1))
    # }

    # Get the upper and lower cutoff values
    upper <- partition_breaks[partition_id]
    lower <- partition_breaks[partition_id - 1]

    # Determine if a value is below the current upper
    # bound and above the current lower bound for
    # the quantile
    below_upper <- sapply(X = density_df$density,
                          upper = upper,
                          FUN = function(X, upper){
                            if (is.na(X)) {
                              FALSE
                            } else {
                              X <= upper
                            }
                          })
    above_lower <- sapply(X = density_df$density,
                          lower = lower,
                          FUN = function(X, lower){
                            if (is.na(X)) {
                              FALSE
                            } else {
                              X >= lower
                            }
                          })

    applicable_indices <- mapply(X = below_upper,
                                 Y = above_lower,
                                 FUN = function(X, Y){
                                   X & Y
                                 })

    # Write in the current quantile ID to the relevant indices
    density_df$uid[applicable_indices] <- partition_id - 1
  }

  # Get a stars object, which is basically a raster
  density_output <- stars::st_as_stars(.x = density_df,
                                       # This defaults to 1:2, but that transposes
                                       # the x and y axes, so we'll do 2:1
                                       coords = 2:1)

  # Convert the stars object to polygons
  # We have to use quantile_id instead of quantile because this'll only work with
  # numeric values. We can always get quantiles in there later with a join
  density_output <- sf::st_as_sf(density_output["uid"],
                                 as_points = FALSE,
                                 merge = TRUE)
  # Make sure that the CRS is assigned
  sf::st_crs(density_output) <- sf::st_crs(frame)

  polygons_valid <- check_polygons(polygons = density_output,
                                   points = points,
                                   minimum = min_points,
                                   polygon_id_var = "uid",
                                   verbose = verbose)
  current_iteration <- iteration_limit
  while (!polygons_valid & current_iteration > 0) {
    current_iteration <- current_iteration - 1
    current_partition_count <- current_partition_count + 1

    if (verbose) {
      message(paste0("Not all density partitions contained the minimum number of points. Attempting again with an additional break and combining the lowest-density partitions."))
    }

    partition_breaks <- BAMMtools::getJenksBreaks(var = density_df[["density"]],
                                                  # This is intentional and NOT
                                                  # accidentally repeating the
                                                  # incrementing of the count
                                                  k = current_partition_count + 1)

    # if (verbose) {
    #   message("Classifying area by densities according to breaks.")
    # }

    for (partition_id in length(partition_breaks):2) {
      # if (verbose) {
      #   message(paste0("Identifying partition ", partition_id - 1))
      # }

      upper <- partition_breaks[partition_id]
      lower <- partition_breaks[partition_id - 1]

      below_upper <- sapply(X = density_df$density,
                            upper = upper,
                            FUN = function(X, upper){
                              if (is.na(X)) {
                                FALSE
                              } else {
                                X <= upper
                              }
                            })
      above_lower <- sapply(X = density_df$density,
                            lower = lower,
                            FUN = function(X, lower){
                              if (is.na(X)) {
                                FALSE
                              } else {
                                X >= lower
                              }
                            })

      applicable_indices <- mapply(X = below_upper,
                                   Y = above_lower,
                                   FUN = function(X, Y){
                                     X & Y
                                   })

      density_df$uid[applicable_indices] <- partition_id - 1
    }

    density_output <- stars::st_as_stars(.x = density_df,
                                         coords = 2:1)

    density_output <- sf::st_as_sf(density_output["uid"],
                                   as_points = FALSE,
                                   merge = TRUE)

    sf::st_crs(density_output) <- sf::st_crs(frame)

    polygons_valid <- check_polygons(polygons = density_output,
                                     points = points,
                                     minimum = min_points,
                                     polygon_id_var = "uid",
                                     verbose = verbose)
  }

  if (!polygons_valid & current_iteration == 0) {
    if (accept_failure) {
      if (sub_frame) {
        warning("Unable to find density partitions that meet current parameters. Returning the frame.")
        return(frame)
      } else {
        warning("Unable to find density partitions that meet current parameters. Returning NULL.")
        return(NULL)
      }

    } else {
      stop("Unable to find density partitions that meet current parameters.")
    }
  }

  end_time <- Sys.time()
  output <- lapply(X = unique(density_output$uid),
                   density_output = density_output,
                   FUN = function(X, density_output){
                     dplyr::filter(.data = density_output,
                                   uid == X) |>
                       sf::st_union(x = _) |>
                       sf::st_as_sf(x = _) |>
                       dplyr::mutate(.data = _,
                                     uid = X,
                                     elapsed_time = end_time - start_time) |>
                       sf::st_intersection(x = _,
                                           y = dplyr::rename(.data = frame,
                                                             frame_id = uid)) |>
                       dplyr::select(.data = _,
                                     -frame_id)
                   })

  if (!output_list) {
    dplyr::bind_rows(output)
  } else {
    output
  }
}

#' Create Thiessen/Voronoi polygons from a set of points and bounding polygons
#' @description Generate Thiessen/Voronoi polygons for a set of points and clip the results using a set of polygons
#' @param frame An sf polygon or multipolygon object. This is the clipping boundary which will be applied to the otherise "infinite" Thiessen/Voronoi polygons.
#' @param n_polygons Numeric value. The number of Thiessen polygons to draw within the frame.
#' @param points Optional sf point object. If provided, then the Thiessen polygons will be redrawn with new random seeds until each contains at least \code{points_min} of these points. Defaults to \code{NULL}.
#' @param points_min Optional numeric value. If \code{points} is not \code{NULL} then this is the minimum number of points that each Thiessen polygon will contain. Defaults to \code{2}.
#' @param envelope An sfc polygon object. This will be the outer envelope for the Thiessen polygons before they're clipped to \code{frame}. This will only be applied if it's larger than the default envelope in \code{sf::st_voronoi()}. If \code{NULL} then the default envelope will be used. Defaults to \code{NULL}.
#' @param seed_number Optional numeric value. The seed number to use for generating the polygon centroids. A random seed will be used if this is \code{NULL}. Defaults to \code{NULL}.
#' @param seed_increment Optional numeric value. If attempting to produce polygons with \code{points_min} points from \code{points} in each polygon, this is the step to increment \code{seed_number} by on each attempt. Defaults to \code{100000}.
#' @param use_albers Logical. If \code{TRUE} then \code{centroids} and \code{frame} will be reprojected into Albers Equal Area (AEA) and the output will be in AEA. If \code{FALSE} then everything will be reprojected to match the coordinate reference system (CRS) of \code{frame} and the output will be in that CRS. CRSs using decimal degrees will throw errors or warnings. Defaults to \code{TRUE}.
#' @param verbose Logical. If \code{TRUE} then the function will return diagnostic messages as it runs. Defaults to \code{FALSE}.)
#' @return An sf object composed of polygon or multipolygon geometry
#' @export
tpoly_gen <- function(frame,
                      n_tpolys = 3,
                      n_tpolys_min = n_tpolys,
                      points = NULL,
                      polygon_id_var = NULL,
                      min_points = 1,
                      n_tpoly_solutions = 1,
                      accept_failure = FALSE,
                      sub_frame = TRUE,
                      envelope = NULL,
                      seed_number = NULL,
                      seed_increment = 100000,
                      iteration_limit = 500,
                      keep_crs = TRUE,
                      verbose = FALSE) {
  # Define Alber's Equal Area CRS.
  # This is for adding area and just standardization.
  projection <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

  # Sanitization
  if (!("sf" %in% class(frame))) {
    stop("`frame` must be an sf polygon object")
  } else if (!all(sf::st_geometry_type(frame) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("`frame` must be an sf polygon object")
  } else {
    output_crs <- sf::st_crs(frame)

    frame <- sf::st_transform(x = frame,
                              crs = projection) |>
      # Remove any Z dimension
      # It screws with the process and is irrelevant here.
      sf::st_zm(x = _,
                drop = TRUE) |>

      # This is so that sf doesn't constantly warn us about attributes being
      # assumed constant.
      sf::st_set_agr(x = _,
                     value = "constant")

    if (is.null(polygon_id_var)) {
      # Dissolve!
      frames <- sf::st_union(x = frame) |>
        sf::st_as_sf(x = _) |>
        dplyr::mutate(.data = _,
                      uid = 1) |>
        list(.x = _)
    } else {
      frames <- lapply(X = unique(frame[[polygon_id_var]]),
                       frame = frame,
                       FUN = function(X, frame){
                         dplyr::select(.data = frame,
                                       uid = tidyselect::all_of(x = polygon_id_var)) |>
                           dplyr::filter(.data = _,
                                         uid == X)
                       })
    }
  }


  if (!(class(n_tpolys) %in% c("numeric", "integer")) | length(n_tpolys) > 1) {
    stop("`n_tpolys` must be a single numeric value")
  }

  if (!is.null(points)) {
    if (!("sf" %in% class(points))) {
      stop("`points` must be an sf points object")
    } else if (!all(sf::st_geometry_type(points) %in% c("POINT"))) {
      stop("`points` must be an sf points object")
    }

    points <- sf::st_transform(x = points,
                               crs = projection) |>

      # This is so that sf doesn't constantly warn us about attributes being
      # assumed constant.
      sf::st_set_agr(x = _,
                     value = "constant") |>

      # Keep only points within the frame!
      sf::st_intersection(x = _,
                          y = frame) |>
      # Can't trust them to not have a Z dimension which breaks steps downstream
      sf::st_zm(x = _,
                drop = TRUE)

    while (nrow(points) < (n_tpolys * min_points) & n_tpolys > n_tpolys_min) {
      n_tpolys <- n_tpolys - 1
    }

    if (nrow(points) < (n_tpolys * min_points)) {
      stop(paste0("There are only ", nrow(points), " points available in the frame, too few to put ",
                  min_points,
                  " points in each of ",
                  n_tpolys,
                  " Thiessen polygons."))
    }
  }

  if (!is.null(seed_number)) {
    if (!(class(seed_number) %in% c("numeric", "integer")) | length(seed_number) > 1) {
      stop("`seed_number` must be a single numeric value when provided.")
    }
  }
  set.seed(seed = seed_number)

  # These are the seeds that we'll find tpolys for.
  # These are random, but based on the input seed number.
  seeds <- sample(x = 1:(n_tpoly_solutions * 10),
                  size = n_tpoly_solutions,
                  replace = FALSE)

  #### Finding solutions -------------------------------------------------------

  tpoly_list <- list()
  for (current_frame_index in seq_len(length.out = length(frames))) {
    tpoly_list[[paste0(current_frame_index)]] <- list()
  }
  skipped_seed_count <- 0
  # This is done per-seed
  for (current_seed_index in seq_len(length.out = n_tpoly_solutions)) {
    start_time <- Sys.time()
    current_seed <- seeds[current_seed_index]
    # This could potentially get incremented down to n_tpolys_min
    current_n_tpolys <- n_tpolys
    for (current_frame_index in seq_len(length.out = length(frames))) {
      if (verbose) {
        message("Working on Thiessen polygons for frame polygon ", current_frame_index, " of ", length(frames), ".")
      }

      frame <- frames[[current_frame_index]][, "uid"]

      # Draw centroids
      # if (verbose) {
      #   message("Drawing Thiessen polygons (solution ", current_seed_index, " of ", n_tpoly_solutions, ").")
      # }
      centroids <- points_gen(frame = frame[, "uid"],
                              sample_type = "simple",
                              n_points = current_n_tpolys,
                              seed_number = current_seed,
                              projection = projection)

      ## Draw Thiessen polygons
      # The points need to be a multipoint object, apparently, so we'll grab the
      # geometry and use sf::st_combine() to produce that before feeding it into
      # sf::st_voronoi() which produces a list that needs conversion.
      thiessen_polygons <- sf::st_geometry(centroids) |>
        sf::st_combine() |>
        sf::st_voronoi(x = _,
                       envelope = sf::st_union(frame)) |>
        # We want specifically the POLYGON components here.
        # Skipping this produces very weird geometry errors when trying to clip
        # at later steps.
        sf::st_collection_extract(x = _,
                                  type = "POLYGON") |>
        # And lastly we turn this into an sf object.
        sf::st_sf() |>

        # This is so that sf doesn't constantly warn us about attributes being
        # assumed constant.
        sf::st_set_agr(x = _,
                       value = "constant")

      # Having gotten a set of tpolys, now we'll clip to the frame and assign simple
      # unique IDs.
      thiessen_polygons <- sf::st_intersection(x = thiessen_polygons,
                                               y = frame) |>
        dplyr::mutate(.data = _,
                      uid = paste0(frame[["uid"]][1], "-",
                                   dplyr::row_number()))

      ##### Validity check -------------------------------------------------------
      if (!is.null(points)) {
        # if (verbose) {
        #   message("Checking that the current Thiessen polygons all contain enough of the provided points. Iterating if not.")
        # }
        polygons_valid <- check_polygons(polygons = thiessen_polygons,
                                         points = points,
                                         minimum = min_points,
                                         polygon_id_var = "uid",
                                         verbose = FALSE)
        current_iteration <- iteration_limit
        while (!polygons_valid & current_iteration > 0) {
          current_iteration <- current_iteration - 1
          current_seed <- current_seed + seed_increment

          # if (verbose) {
          #   message(paste0("The current set of Thiessen polygons does not meet the minimum number of points per polygon. Attempting iteration ", iteration_limit - current_iteration, " out of ", iteration_limit, "."))
          # }

          centroids <- points_gen(frame = frame[, "uid"],
                                  sample_type = "simple",
                                  n_points = current_n_tpolys,
                                  seed_number = current_seed,
                                  projection = projection)

          thiessen_polygons <- sf::st_geometry(centroids) |>
            sf::st_combine() |>
            sf::st_voronoi(x = _,
                           envelope = sf::st_union(frame)) |>
            sf::st_collection_extract(x = _,
                                      type = "POLYGON") |>
            sf::st_sf() |>
            # This is so that sf doesn't constantly warn us about attributes being
            # assumed constant.
            sf::st_set_agr(x = _,
                           value = "constant")

          thiessen_polygons <- sf::st_intersection(x = thiessen_polygons,
                                                   y = frame) |>
            dplyr::mutate(.data = _,
                          uid = paste0(frame[["uid"]][1], "-",
                                       dplyr::row_number()))

          polygons_valid <- check_polygons(polygons = thiessen_polygons,
                                           points = points,
                                           minimum = min_points,
                                           polygon_id_var = "uid",
                                           verbose = FALSE)

          if (!polygons_valid & current_iteration == 0) {
            if (current_n_tpolys > n_tpolys_min) {
              if (verbose) {
                message(paste0("Unable to find a valid Thiessen polygon solution for the current seed with ", current_n_tpolys, " tpolys. Trying again with ", current_n_tpolys - 1, " tpolys."))
              }
              current_n_tpolys <- current_n_tpolys - 1
              current_iteration <- iteration_limit
            } else if (accept_failure) {
              skipped_seed_count <- skipped_seed_count + 1
              if (sub_frame) {
                if (verbose) {
                  message("Unable to find a valid Thiessen polygon solution for the current seed. Moving on and returning the current frame for this 'solution'.")
                }
                frame$area_m2 <- sf::st_area(x = frame)  |>
                  as.vector() |>
                  as.numeric()
                tpoly_list[[paste0(current_frame_index)]][[paste0(current_seed_index)]] <- dplyr::select(.data = frame,
                                                                                                         uid,
                                                                                                         area_m2)
              } else {
                if (verbose) {
                  message("Unable to find a valid Thiessen polygon solution for the current seed. Moving on and returning NULL for this 'solution'.")
                }
                tpoly_list[[paste0(current_frame_index)]][[paste0(current_seed_index)]] <- NULL
              }
            } else {
              stop("Unable to find a valid Thiessen polygon solution for the current seed for this frame. To skip seeds that fail, set accept_failure to TRUE and sub_frame to either TRUE or FALSE.")
            }
          }

          if (polygons_valid) {
            thiessen_polygons$area_m2 <- sf::st_area(x = thiessen_polygons) |>
              as.vector() |>
              as.numeric()
          }
        }
      } else {
        # Add in the areas for the polygons
        thiessen_polygons$area_m2 <- sf::st_area(x = thiessen_polygons) |>
          as.vector() |>
          as.numeric()
      }
      # Silly, but this is still the most intuitive way I think of for adding
      # these to the list.
      # This also puts them in the intended output projection to match the input
      # frame.
      end_time <- Sys.time()
      tpoly_list[[paste0(current_frame_index)]][[paste0(current_seed_index)]] <- sf::st_transform(x = thiessen_polygons,
                                                                                                  crs = output_crs) |>
        dplyr::mutate(.data = _,
                      elapsed_time = end_time - start_time)
    }

    if (skipped_seed_count > 0) {
      warning(paste0("Only ", n_tpoly_solutions - skipped_seed_count,
                     " valid sets of Thiessen polygons out of the requested ", n_tpoly_solutions,
                     " were found for the frame ", unique(frames[["uid"]])[[current_frame_index]], "."))
    }
  }

  # Remove the NULLs from the tpoly lists.
  # This is important because we're going to just use the shortest length list
  # to constrain the output.
  tpoly_list <- lapply(X = tpoly_list,
                       FUN = function(X){
                         X[!sapply(X = X,
                                   FUN = is.null)]
                       })

  # It's important to not do any more munging of the contents of this list at
  # this point because some of these might be NULL and that's a pain to handle
  # as an exception.
  output_list <- lapply(X = sapply(X = tpoly_list,
                                   FUN = length) |>
                          min() |>
                          seq_len(length.out = _),
                        tpoly_list = tpoly_list,
                        frame_count = length(frames),
                        FUN = function(X, tpoly_list, frame_count){
                          lapply(X = seq_len(length.out = frame_count),
                                 tpoly_list = tpoly_list,
                                 tpoly_solution_index = X,
                                 FUN = function(X, tpoly_list, tpoly_solution_index){
                                   tpoly_list[[X]][[tpoly_solution_index]]
                                 }) |>
                            dplyr::bind_rows() |>
                            sf::st_set_geometry(x = _,
                                                value = "geometry")
                        })

  if (length(output_list) == 1) {
    output_list[[1]]
  } else {
    output_list
  }
}

#' Create Thiessen/Voronoi polygons from a set of points and bounding polygons
#' @description Generate Thiessen/Voronoi polygons for a set of points and clip the results using a set of polygons
#' @param centroids An sf points object. These points are used as centroids for the Thiessen/Voronoi polygons.
#' @param frame An sf polygon or multipolygon object. This is the clipping boundary which will be applied to the otherise "infinite" Thiessen/Voronoi polygons.
#' @param envelope An sfc polygon object. This will be the outer envelope for the Thiessen polygons before they're clipped to \code{frame}. This will only be applied if it's larger than the default envelope in \code{sf::st_voronoi()}. If \code{NULL} then the default envelope will be used. Defaults to \code{NULL}.
#' @param use_albers Logical. If \code{TRUE} then \code{centroids} and \code{frame} will be reprojected into Albers Equal Area (AEA) and the output will be in AEA. If \code{FALSE} then \code{frame} will be reprojected to match the coordinate reference ssytem (CRS) of \code{centroids} and the output will be in that CRS. CRSs using decimal degrees will throw errors or warnings. Defaults to \code{TRUE}.
#' @return An sf object composed of polygon or multipolygon geometry
#' @export
thiessen_polygons_gen_fixed <- function(centroids,
                                        frame,
                                        envelope = NULL,
                                        use_albers = TRUE) {
  # Define Alber's Equal Area CRS
  aea_proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

  # Sanitization
  if (!("sf" %in% class(centroids))) {
    stop("`centroids` must be an sf points object")
  } else if (!all(sf::st_geometry_type(centroids) %in% c("POINT"))) {
    stop("`centroids` must be an sf points object")
  }
  if (!("sf" %in% class(frame))) {
    stop("`frame` must be an sf polygon object")
  } else if (!all(sf::st_geometry_type(frame) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("`frame` must be an sf polygon object")
  }

  if (!is.null(envelope)) {
    if (!("sfc" %in% class(envelope))) {
      stop("`envelope` must be an sfc polygon object")
    } else if (!all(sf::st_geometry_type(envelope) %in% c("POLYGON", "MULTIPOLYGON"))) {
      stop("`envelope` must be an sfc polygon object")
    }
  }

  # Remove any Z dimension
  # It screws with the process and is irrelevant
  centroids <- sf::st_zm(centroids,
                         drop = TRUE)
  frame <- sf::st_zm(frame,
                     drop = TRUE)

  # Reproject as necessary
  if (use_albers) {
    centroids <- sf::st_transform(x = centroids,
                                  crs = aea_proj)
    frame <- sf::st_transform(x = frame,
                              crs = aea_proj)
    envelope <- sf::st_transform(x = envelope,
                                 crs = aea_proj)
  } else {
    # This just forces the polygons into the same projection as the centroids
    centroids_crs <- sf::st_crs(centroids)
    frame <- sf::st_transform(frame,
                              crs = centroids_crs)
    envelope <- sf::st_transform(envelope,
                                 crs = centroids_crs)
  }

  # Draw Thiessen polygons
  # Here's where it gets weird
  # The points need to be a multipoint object, apparently
  points_multipoint <- sf::st_combine(sf::st_geometry(centroids))

  # Generate the Thiessen polygons
  # If there's a provided envelope, we attempt to use it
  # This appears to be a list??? It's fine, I promise. We'll convert it in a bit
  if (is.null(envelope)) {
    thiessen_polygons_raw <- sf::st_voronoi(x = points_multipoint)
  } else {
    thiessen_polygons_raw <- sf::st_voronoi(x = points_multipoint,
                                            envelope = envelope)
  }

  # This is making sure that we only have polygon features
  # No idea why this is necessary, but without it all kinds of geometry errors pop up at clipping
  thiessen_polygons_raw <- sf::st_collection_extract(thiessen_polygons_raw,
                                                     type = "POLYGON")

  # Convert the polygons to an sf object
  # Finally, something comfortingly familiar
  thiessen_polygons <- sf::st_sf(thiessen_polygons_raw)

  # Clip to sample frame
  thiessen_polygons_clipped <- sf::st_intersection(x = thiessen_polygons,
                                                   y = frame)

  # Add in a unique ID for each polygon
  thiessen_polygons_clipped$polygon_unique_id <- seq_len(nrow(thiessen_polygons_clipped))

  # Add in the areas for the polygons
  thiessen_polygons_clipped$area_m2 <- as.vector(sf::st_area(x = thiessen_polygons_clipped))

  return(thiessen_polygons_clipped)
}

#' @param frame An sf polygon or multipolygon object. This is the clipping boundary which will be applied to the otherise "infinite" Thiessen/Voronoi polygons.
#' @param points An sf point object. These will be broken into clusters and one Thiessen polygon drawn for the centroid of each cluster.
#' @param n_polygons Numeric value. The number of Thiessen polygons to draw within the frame. This is also the number of clusters to break \code{points} into.
#' @param envelope An sfc polygon object. This will be the outer envelope for the Thiessen polygons before they're clipped to \code{frame}. This will only be applied if it's larger than the default envelope in \code{sf::st_voronoi()}. If \code{NULL} then the default envelope will be used. Defaults to \code{NULL}.
#' @param projection Optional character string or CRS object. The coordinate reference system for the output. May be a PROJ4 string or a CRS object. Defaults to the projection of \code{frame}.
#' @param verbose Logical. If \code{TRUE} then the function will return diagnostic messages as it runs. Defaults to \code{FALSE}.)
#' @export
thiessen_polygons_gen_clustered <- function(frame,
                                            points,
                                            n_polygons,
                                            envelope = NULL,
                                            projection = NULL,
                                            verbose = FALSE) {
  # Sanitization
  if (!("sf" %in% class(frame))) {
    stop("`frame` must be an sf polygon object")
  } else if (!all(sf::st_geometry_type(frame) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("`frame` must be an sf polygon object")
  }
  if (!("sf" %in% class(points))) {
    stop("`points`` must be an sf points object")
  } else if (!all(sf::st_geometry_type(points) %in% c("POINT"))) {
    stop("`points` must be an sf points object")
  }
  if (n_polygons > nrow(points)) {
    stop("`n_polygons` must be less than the number of observations in `points`")
  }
  if (!is.null(envelope)) {
    if (!("sfc" %in% class(envelope))) {
      stop("`envelope` must be an sfc polygon object")
    } else if (!all(sf::st_geometry_type(envelope) %in% c("POLYGON", "MULTIPOLYGON"))) {
      stop("`envelope` must be an sfc polygon object")
    }
  }

  if (is.null(projection)) {
    projection <- sf::st_crs(frame)
  }
  frame <- sf::st_transform(frame,
                            crs = projection)
  points <- sf::st_transform(points,
                             crs = projection)

  # Make an envelope from the frame
  if (is.null(envelope)) {
    envelope <- sf::st_as_sfc(sf::st_bbox(frame,
                                          crs = projection))
  }


  # Get the point coordinates. We'll need them to calculate distances
  points_coords <- as.data.frame(sf::st_coordinates(points))
  names(points_coords) <- c("x", "y")

  # Get a distance matrix
  points_distance_matrix <- geosphere::distm(x = points_coords)

  # Do some hierarchical clustering based on the distances
  hierarchical_clusters <- hclust(as.dist(m = points_distance_matrix),
                                  method = "complete")

  # Put them into a number of clusters matching the Thiessen polygon count
  cluster_membership <- cutree(tree = hierarchical_clusters,
                               k = n_polygons)

  # Write that info into the points object
  points$cluster <- cluster_membership
  points_coords$cluster <- cluster_membership

  # For each cluster, make an sf object for the centroid
  centroid_sf_list <- lapply(X = split(points_coords, points_coords$cluster),
                             projection = projection,
                             FUN = function(X,
                                            projection) {
                               coords <- X
                               current_cluster <- coords$cluster[1]
                               centroid_x <- mean(coords$x)
                               centroid_y <- mean(coords$y)

                               centroid_df <- data.frame(cluster = current_cluster,
                                                         x = centroid_x,
                                                         y = centroid_y)

                               coords_matrix <- as.matrix(centroid_df[, c("x", "y")])

                               centroid_sfc <- sf::st_point(x = coords_matrix)

                               # For some reason, I have to do this to feed into sf::st_sf()
                               # instead of just giving it the sfc object
                               centroid_sfc_geometry <- sf::st_geometry(centroid_sfc)

                               centroid_sf <- sf::st_sf(centroid_sfc_geometry,
                                                        crs = projection)

                               centroid_sf$cluster <- current_cluster

                               centroid_sf
                             })

  centroids_sf <- do.call(rbind,
                          centroid_sf_list)

  tpolys <- thiessen_polygons_gen_fixed(centroids = centroids_sf,
                                        frame = frame,
                                        envelope = envelope)

  tpolys
}

#' Create Thiessen/Voronoi polygons from a set of points and bounding polygons
#' @description Generate Thiessen/Voronoi polygons for a set of points and clip the results using a set of polygons
#' @param frame An sf polygon or multipolygon object. This is the clipping boundary which will be applied to the otherise "infinite" Thiessen/Voronoi polygons.
#' @param n_polygons Numeric value. The number of Thiessen polygons to draw within the frame.
#' @param points Optional sf point object. If provided, then the Thiessen polygons will be redrawn with new random seeds until each contains at least \code{points_min} of these points. Defaults to \code{NULL}.
#' @param points_min Optional numeric value. If \code{points} is not \code{NULL} then this is the minimum number of points that each Thiessen polygon will contain. Defaults to \code{2}.
#' @param envelope An sfc polygon object. This will be the outer envelope for the Thiessen polygons before they're clipped to \code{frame}. This will only be applied if it's larger than the default envelope in \code{sf::st_voronoi()}. If \code{NULL} then the default envelope will be used. Defaults to \code{NULL}.
#' @param seed_number Optional numeric value. The seed number to use for generating the polygon centroids. A random seed will be used if this is \code{NULL}. Defaults to \code{NULL}.
#' @param seed_increment Optional numeric value. If attempting to produce polygons with \code{points_min} points from \code{points} in each polygon, this is the step to increment \code{seed_number} by on each attempt. Defaults to \code{100000}.
#' @param use_albers Logical. If \code{TRUE} then \code{centroids} and \code{frame} will be reprojected into Albers Equal Area (AEA) and the output will be in AEA. If \code{FALSE} then everything will be reprojected to match the coordinate reference system (CRS) of \code{frame} and the output will be in that CRS. CRSs using decimal degrees will throw errors or warnings. Defaults to \code{TRUE}.
#' @param verbose Logical. If \code{TRUE} then the function will return diagnostic messages as it runs. Defaults to \code{FALSE}.)
#' @return An sf object composed of polygon or multipolygon geometry
#' @export
thiessen_polygons_gen_random <- function(frame,
                                         n_polygons,
                                         points = NULL,
                                         points_min = 2,
                                         envelope = NULL,
                                         seed_number = NULL,
                                         seed_increment = 100000,
                                         iteration_limit = 1000,
                                         use_albers = TRUE,
                                         verbose = FALSE) {
  # Define Alber's Equal Area CRS
  projection <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

  # Sanitization
  if (!("sf" %in% class(frame))) {
    stop("`frame` must be an sf polygon object")
  } else if (!all(sf::st_geometry_type(frame) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("`frame` must be an sf polygon object")
  }
  if (!(class(n_polygons) %in% c("numeric", "integer")) | length(n_polygons) > 1) {
    stop("`n_points` must be a single numeric value")
  }
  if (!is.null(points)) {
    if (!("sf" %in% class(points))) {
      stop("`points` must be an sf points object")
    } else if (!all(sf::st_geometry_type(points) %in% c("POINT"))) {
      stop("`points` must be an sf points object")
    }
    if (nrow(points) < (n_polygons * points_min)) {
      stop(paste0("There are too few points available to put ",
                  points_min,
                  " points in each of ",
                  n_polygons,
                  " Thiessen polygons"))
    }
  }
  if (!is.null(seed_number)) {
    if (!(class(seed_number) %in% c("numeric", "integer")) | length(seed_number) > 1) {
      stop("`seed_number` must be a single numeric value")
    }
  } else {
    seed_number <- sample(x = 1:9999999,
                          size = 1)
  }

  # Remove any Z dimension
  # It screws with the process and is irrelevant
  frame <- sf::st_zm(frame,
                     drop = TRUE)

  # Dissolve!
  frame <- sf::st_as_sf(sf::st_union(x = frame))
  frame$id <- 1

  if (!is.null(points)) {
    points <- sf::st_zm(points,
                        drop = TRUE)
  }

  if (verbose) {
    message("Making sure the projections are the same")
  }
  # Reproject as necessary
  if (use_albers) {
    frame <- sf::st_transform(x = frame,
                              crs = projection)
    if (!is.null(points)) {
      points <- sf::st_transform(x = points,
                                 crs = projection)
    }
  } else {
    # This just forces the points into the same projection as the polygons
    projection <- sf::st_crs(frame)
    if (!is.null(points)) {
      points <- sf::st_transform(x = points,
                                 crs = projection)
    }
  }

  # # Check to make sure we even have enough points for the request
  # if (!is.null(points)) {
  #   if (verbose) {
  #     message("Checking to see if there are enough points for all polygons to meet the minimum point count.")
  #   }
  #   points <- sf::st_intersection(x = points,
  #                                 y = dplyr::select(.data = frame,
  #                                                   id))
  #   if (nrow(points) < min_points * n_polygons) {
  #     stop(paste0("Insufficient points for the number of polygons requested. There are ",
  #                 nrow(points), " in the frame but ", n_polygons, " with at least ", min_points,
  #                 " points each would require at least ", min_points * n_polygons, " points"))
  #   }
  # }



  # Draw centroids
  if (verbose) {
    message("Drawing centroids")
  }
  centroids <- points_gen(frame = frame[, "id"],
                          sample_type = "simple",
                          n_points = n_polygons,
                          seed_number = seed_number,
                          projection = projection)

  # Draw Thiessen polygons
  if (verbose) {
    message("Drawing first set of Thiessen polygons")
  }
  thiessen_polygons <- thiessen_polygons_gen_fixed(centroids = centroids,
                                                   frame = frame,
                                                   envelope = envelope,
                                                   use_albers = FALSE)

  # Get the final variables in there
  thiessen_polygons$tpoly_seed <- seed_number
  thiessen_polygons$tpoly_id <- paste0("tpoly_",
                                       thiessen_polygons$tpoly_seed,
                                       "-",
                                       thiessen_polygons$polygon_unique_id)

  # if (verbose) {
  #   message("Clipping polygons using frame")
  # }
  # thiessen_polygons_clipped <- sf::st_intersection(x = thiessen_polygons,
  #                                                  y = frame)

  # We only care about hitting our minimum number of points per polygon if we have points in the first place
  if (!is.null(points)) {
    ## Check that polygons contain enough points
    if (verbose) {
      message("Attributing points with Thiessen polygon IDs via spatial join")
    }
    points_attributed <- sf::st_join(x = points,
                                     y = thiessen_polygons[, c("tpoly_id")])

    tpoly_summary <- data.frame(tpoly_id = names(table(points_attributed$tpoly_id)),
                                n_points = as.vector(table(points_attributed$tpoly_id)),
                                stringsAsFactors = FALSE)

    # But what if there were polygons with no points at all?????
    missing_tpoly_ids <- unique(thiessen_polygons$tpoly_id)[!(unique(thiessen_polygons$tpoly_id) %in% tpoly_summary$tpoly_id)]

    if (length(missing_tpoly_ids) > 0) {
      missing_tpoly_summary <- data.frame(tpoly_id = missing_tpoly_ids,
                                          n_points = 0,
                                          stringsAsFactors = FALSE)

      tpoly_summary <- rbind(tpoly_summary,
                             missing_tpoly_summary)
    }

    current_iteration <- 1
    # So, if the polygons didn't have enough points each, increment the seed number and try again
    # over and over until it actually pans out
    while (!all(tpoly_summary[["n_points"]] >= points_min)) {
      current_iteration <- current_iteration + 1
      if (current_iteration > iteration_limit) {
        warning("Iteration limit reached without a solution. Returning NULL.")
        return(NULL)
      }
      seed_number <- seed_number + seed_increment
      if (verbose) {
        message(paste("Not enough points in all thiessen polygons. Drawing new centroids with seed", seed_number))
      }

      # Draw centroids
      centroids <- points_gen(frame = frame,
                              sample_type = "simple",
                              n_points = n_polygons,
                              seed_number = seed_number,
                              projection = projection)

      if (verbose) {
        message("Generating Thiessen polygons from centroids")
      }
      thiessen_polygons <- thiessen_polygons_gen_fixed(centroids = centroids,
                                                       frame = frame,
                                                       envelope = envelope,
                                                       use_albers = FALSE)



      # Get the final variables in there
      thiessen_polygons$tpoly_seed <- seed_number
      thiessen_polygons$tpoly_id <- paste0("tpoly_",
                                           thiessen_polygons$tpoly_seed,
                                           "-",
                                           thiessen_polygons$polygon_unique_id)

      ## Check that polygons contain enough points
      if (verbose) {
        message("Attributing points with Thiessen polygon IDs via spatial join")
      }
      points_attributed <- sf::st_join(x = points,
                                       y = thiessen_polygons[, c("tpoly_id")])

      tpoly_summary <- data.frame(tpoly_id = names(table(points_attributed$tpoly_id)),
                                  n_points = as.vector(table(points_attributed$tpoly_id)),
                                  stringsAsFactors = FALSE)
      # But what if there were polygons with no points at all?????
      missing_tpoly_ids <- unique(thiessen_polygons$tpoly_id)[!(unique(thiessen_polygons$tpoly_id) %in% tpoly_summary$tpoly_id)]

      if (length(missing_tpoly_ids) > 0) {
        missing_tpoly_summary <- data.frame(tpoly_id = missing_tpoly_ids,
                                            n_points = 0,
                                            stringsAsFactors = FALSE)

        tpoly_summary <- rbind(tpoly_summary,
                               missing_tpoly_summary)
      }
    }

    # Add in the point counts while we're here
    output <- merge(thiessen_polygons,
                    tpoly_summary,
                    by = "tpoly_id")

    # And why not weights also
    output$weight <- output$area_m2 / output$n_points

    return(output)
  } else {
    # Return the polygons
    # return(thiessen_polygons_clipped[, c("tpoly_id", "tpoly_seed")])
    return(thiessen_polygons[, c("tpoly_id", "tpoly_seed")])
  }
}
