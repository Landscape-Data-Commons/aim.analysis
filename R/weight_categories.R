#' Write out a subset of polygon shapefiles from a sample design database
#' @description This will take a reporting unit polygon and produce a set of polygons ready for intersection to generate weight categories using an appropriately formatted sample design database and, optionally, Landscape Management Framework (LMF) stratification polygons. Please note that all input polygons MUST be dissolved by their unique identifiers before use.
#' @param reportingunit An sf polygons object, Spatial Polygons Data Frame, or character string. If a character string, it must be the name of a polygon feature class found in \code{reportingunit_path}. Regardless, the feature class must contain exactly one feature representing a single reporting unit.
#' @param reportingunit_path Optional character string. The filepath to the folder or geodatabase containing the feature class specified with the character string \code{reportingunit}. Not necessary if \code{reportingunit} is either an sf polygon object or Spatial Polygons Data Frame. Defaults to \code{NULL}.
#' @param sdd_path Character string. The full filepath to the sample design database (SDD), including .GDB file extension. The SDD must follow the standard schema, including the feature classes: TerrestrialPointEvaluation, TerrestrialSamplePoints, TerrestrialStrata, and TerrestrialSampleFrame.
#' @param lmf_strata Optional sf polygons object, Spatial Polygons Data Frame, or character string. If a character string, it must be the name of a polygon feature class found in \code{reportingunit_path}. Regardless, the feature class must contain exactly one feature representing a single reporting unit. Defaults to \code{NULL}.
#' @param lmf_path Optional character string. If the weight categories need to account for the Landscape Monitoring Framework (LMF) strata, this must be the full filepath, including file extension, to the geodatabase containing the strata feature class with the LMF strata.
#' @param pointvisits Optional sf points object, Spatial Points Data Frame, or character string. If a character string, it must be the name of a point feature class found in \code{pointvisits_path}. Regardless, the feature class must contain the points to be considered and in the same format as the feature class TerrestrailPointEvaluation. This would only be used if you have already restricted which point evaluations should be considered for this process; otherwise, leave \code{NULL} and use the full set of points in the sample design database defined by \code{sdd_path}. Defaults to \code{NULL}.
#' @param pointvisits_path Optional character string. If \code{pointvisits} is a character string naming a feature class then this must be the path to the folder or geodatabase containing that feature class. Defaults to \code{NULL}.
#' @param output_path Character string. The filepath to the folder into which the resulting feature classes will be written.
#' @param verbose Logical. If \code{TRUE} then diagnostic messages will be returned throughout processing. Defaults to \code{FALSE}.
#' @return Nothing. Writes out polygon shapefiles to a specified location.
#' @export

sdd_clip <- function(reportingunit,
                     reportingunit_path = NULL,
                     sdd_path,
                     lmf_strata= NULL,
                     lmf_path = NULL,
                     pointvisits = NULL,
                     pointvisits_path = NULL,
                     output_path,
                     verbose = FALSE){
  # Because everything will be reprojected into Albers Equal Area Conic
  aea_proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  #### Sanitization ####
  if (!file.exists(output_path)) {
    stop("The output_path must point to an existing folder")
  }

  if (!(class(sdd_path) %in% c("character"))) {
    stop("sdd_path must be a character string")
  } else if (!file.exists(sdd_path)) {
    stop("The file pointed to by sdd_path does not exist")
  }


  if (is.null(lmf_strata)) {
    lmf_strata_sf <- NULL
  } else if (any(class(lmf_strata) %in% c("sf"))) {
    lmf_strata_sf <- lmf_strata
    # Reproject!
    lmf_strata_sf <- sf::st_transform(lmf_strata_sf, aea_proj)
  } else if (class(lmf_strata) %in% c("SpatialPolygonsDataFrame")) {
    lmf_strata_sf <- methods::as(lmf_strata, "sf")
    # Reproject!
    lmf_strata_sf <- sf::st_transform(lmf_strata_sf, aea_proj)
  } else if (class(lmf_strata) %in% c("character")) {
    if (is.null(lmf_path)) {
      stop("If lmf_strata is a character string, you must provide lmf_path")
    } else {
      if (!(class(lmf_path) %in% c("character"))) {
        stop("path_to_lmf must be a character string")
      } else if (!file.exists(lmf_path)) {
        stop("The file pointed to by path_to_lmf does not exist")
      }
      lmf_strata_sf <- sf::st_read(dsn = lmf_path,
                                   layer = lmf_strata)
    }
  } else {
    stop("If lmf_strata is a character string, you must provide lmf_path pointing to the folder or geodatabase containing the named feature class.")
  }



  if (any(class(reportingunit) %in% c("sf"))) {
    reporting_unit_sf <- reportingunit
  } else if (class(reportingunit) %in% c("SpatialPolygonsDataFrame")) {
    reporting_unit_sf <- methods::as(reportingunit, "sf")
  } else if (class(reportingunit) %in% c("character")) {
    if (is.null(reportingunit_path)) {
      stop("You must specify a path to the reporting unit")
    } else if (class(reportingunit_path) != "character") {
      stop("reportingunit_path must be a character string")
    } else if (!file.exists(reportingunit_path)) {
      stop("reportingunit_path points to a file which does not exist")
    }
    if (verbose) {
      message("Reading in reporting units")
    }
    reporting_unit_sf <- sf::st_read(dsn = reportingunit_path,
                                     layer = reportingunit)
  } else {
    stop("reportingunit must be either a character string specifying the name of a polygon feature class to read in or an sf or SPDF object")
  }
  # Reproject!
  reporting_unit_sf <- sf::st_transform(reporting_unit_sf, aea_proj)

  if (is.null(pointvisits)) {
    if (verbose) {
      message("Reading in the TerrestrialPointEvaluation feature class")
    }
    pointvisits_sf <- sf::st_read(dsn = sdd_path,
                                  layer = "TerrestrialPointEvaluation")
  } else if (any(class(pointvisits) %in% c("sf"))) {
    pointvisits_sf <- pointvisits
  } else if (class(pointvisits) %in% c("SpatialPolygonsDataFrame")) {
    pointvisits_sf <- methods::as(pointvisits, "sf")
  } else if (class(pointvisits) %in% c("character")) {
    if (is.null(pointvisits_path)) {
      stop("You must specify a path to the reporting unit")
    } else if (class(pointvisits_path) != "character") {
      stop("pointvisits_path must be a character string")
    } else if (!file.exists(pointvisits_path)) {
      stop("pointvisits_path points to a file which does not exist")
    }
    if (verbose) {
      message("Reading in pointvisits")
    }
    if (verbose) {
      message("Reading in the pointvisits feature class")
    }
    pointvisits_sf <- sf::st_read(dsn = pointvisits_path,
                                  layer = pointvisits)
  } else {
    stop("pointvisits must be either a character string specifying the name of a polygon feature class to read in or an sf or SPDF object")
  }
  # Reproject!
  pointvisits_sf <- sf::st_transform(pointvisits_sf, aea_proj)


  #### Reporting Units and Points ####
  if (verbose) {
    message("Restricting to only points within the reporting unit")
  }
  # Limit TerrestrialPointEvaluation to those points within the reporting unit
  overlapping_indices <- as.vector(sf::st_covered_by(x = pointvisits_sf,
                                                     y = reporting_unit_sf,
                                                     # I don't "get" this argument, but
                                                     # sparse = TRUE returns an object I
                                                     # don't understand how to use
                                                     sparse = FALSE))
  pointvisits_relevant_sf <- pointvisits_sf[overlapping_indices, ]

  if (nrow(pointvisits_relevant_sf) < 1) {
    stop("There are no points in TerrestrialPointEvaluation that overlap with the reporting unit")
  }

  # Plot a visual confirmation that everything is going according to plan
  # ggplot() +
  #   geom_sf(data = reporting_unit_sf,
  #           color = "Red") +
  #   geom_sf(data = pointvisits_relevant_sf)

  # Get the unique SamplePointKeys that occur in the evaluated points that
  # fall within the reporting units
  relevant_samplepointkeys <- unique(pointvisits_relevant_sf$SamplePointKey)

  #### Reading in SamplePoints ####
  # Use a SQL query to read in TerrestrialSamplePoints with those keys
  # Paste them together, separated by commas (to jam into the SQL query string)
  samplepoints_ids_for_query <- paste0("('",
                                       paste0(relevant_samplepointkeys,
                                              collapse = "', '"),
                                       "')")
  # Assemble the SQL query string
  samplepoints_query <- paste("SELECT * FROM",
                              "TerrestrialSamplePoints",
                              "WHERE",
                              "SamplePointKey IN",
                              samplepoints_ids_for_query)
  if (verbose) {
    message("Reading in the TerrestrialSamplePoints feature class")
  }
  # Read in only the points that satisfy the SQL query
  samplepoints_sf <- sf::st_read(dsn = sdd_path,
                                 layer = "TerrestrialSamplePoints",
                                 query = samplepoints_query)

  # Reproject!
  samplepoints_sf <- sf::st_transform(samplepoints_sf, aea_proj)

  # Identify the unique TerrestrialSampleFrameID values in the TerrestrialSamplePoints that we just read in
  relevant_sampleframeids <- unique(samplepoints_sf$TerrestrialSampleFrameID)

  #### Clip LMF Strata ####
  if (is.null(lmf_strata)) {
    lmf_strata_for_combining_sf <- NULL
  } else {
    if (verbose) {
      message("Repairing LMF strata geometry")
    }
    # Buffer by 0 to correct geometry errors
    lmf_strata_buffered_sf <- sf::st_buffer(lmf_strata_sf,
                                            dist = 0)

    if (verbose) {
      message("Clipping LMF strata")
    }
    # Clip the strata to the reporting unit
    lmf_strata_clipped_sf <- sf::st_intersection(x = lmf_strata_buffered_sf,
                                                 y = reporting_unit_sf)

    lmf_strata_clipped_sf[["unique_id"]] <- paste0("lmf", 1:nrow(lmf_strata_clipped_sf))

    if (verbose) {
      message("Repairing clipped LMF strata geometry")
    }
    lmf_strata_clipped_sf <- sf::st_buffer(lmf_strata_clipped_sf,
                                           dist = 0)

    lmf_strata_for_combining_sf <- lmf_strata_clipped_sf[, "unique_id"]
  }
  #### Clip Strata ####
  # Use a SQL query to read in strata associated with the identified sample frames
  # N.B. This is currently impossible because there is no TerrestrialSampleFrame:TerrestrialStrata relationship
  sampleframe_ids_for_query <- paste0("('",
                                      paste0(relevant_sampleframeids,
                                             collapse = "', '"),
                                      "')")

  strata_query <- paste("SELECT * FROM",
                        "TerrestrialStrata",
                        "WHERE",
                        "TerrestrialSampleFrameID IN",
                        sampleframe_ids_for_query)

  if (verbose) {
    message("Reading in TerrestrialStrata feature class")
  }
  strata_sf <- sf::st_read(dsn = sdd_path,
                           layer = "TerrestrialStrata",
                           query = strata_query)

  # Reproject!
  strata_sf <- sf::st_transform(strata_sf, aea_proj)

  if (nrow(strata_sf) > 0) {
    if (verbose) {
      message("Repairing TerrestrialStrata geometry")
    }
    # Buffer by 0 to correct geometry errors
    strata_buffered_sf <- sf::st_buffer(strata_sf,
                                        dist = 0)

    if (verbose) {
      message("Clipping TerrestrialStrata")
    }
    # Clip the strata to the reporting unit
    strata_clipped_sf <- sf::st_intersection(x = strata_buffered_sf,
                                             y = reporting_unit_sf)

    strata_clipped_sf[["unique_id"]] <- paste0("s", 1:nrow(strata_clipped_sf))

    if (verbose) {
      message("Repairing clipped strata geometry")
    }
    # Buffer by 0 to correct geometry errors
    strata_clipped_sf <- sf::st_buffer(strata_clipped_sf,
                                       dist = 0)

    strata_for_combining_sf <- strata_clipped_sf[, "unique_id"]
  } else {
    strata_for_combining_sf <- NULL
  }




  # Use a SQL query to read in the sample frames with no strata
  # So, which IDs are associated with points but not with strata?
  sampleframeids_missing_strata <- relevant_sampleframeids[!(relevant_sampleframeids %in% strata_sf$TerrestrialSampleFrameID)]

  #### Read and Clip Sample Frames ####
  sampleframe_ids_for_query <- paste0("('",
                                      paste0(sampleframeids_missing_strata,
                                             collapse = "', '"),
                                      "')")

  sampleframe_query <- paste("SELECT * FROM",
                             "TerrestrialSampleFrame",
                             "WHERE",
                             "TerrestrialSampleFrameID IN",
                             sampleframe_ids_for_query)
  if (verbose) {
    message("Reading in TerrestrialSampleFrame feature class")
  }
  sampleframe_sf <- sf::st_read(dsn = sdd_path,
                                layer = "TerrestrialSampleFrame",
                                query = sampleframe_query)

  # Reproject!
  sampleframe_sf <- sf::st_transform(sampleframe_sf, aea_proj)

  if (nrow(sampleframe_sf) > 0) {
    # Buffer by 0 to repair geometry
    if (verbose) {
      message("Repairing TerrestrialSampleFrame geometry")
    }
    sampleframe_sf <- sf::st_buffer(sampleframe_sf,
                                    dist = 0)


    # Clip the sample frames to the reporting unit
    if (verbose) {
      message("Clipping TerrestrialSampleFrame")
    }
    sampleframe_clipped_sf <- sf::st_intersection(x = sampleframe_sf,
                                                  y = reporting_unit_sf)

    # Add a unique identifier
    sampleframe_clipped_sf[["unique_id"]] <- paste0("sf", 1:nrow(sampleframe_clipped_sf))

    if (verbose) {
      message("Repairing clipped sample frames geometry")
    }
    sampleframe_clipped_sf <- sf::st_buffer(sampleframe_clipped_sf,
                                            dist = 0)

    sampleframe_for_combining_sf <- sampleframe_clipped_sf[, "unique_id"]
  } else {
    sampleframe_for_combining_sf <- NULL
  }




  #### Write out polygons ####
  if (verbose) {
    message("Combining all clipped polygons")
  }
  # Combine the strata and the sample frames
  combined_frames_sf <- rbind(strata_for_combining_sf,
                              sampleframe_for_combining_sf,
                              lmf_strata_for_combining_sf)

  # Finally perform the intersection!
  if (verbose) {
    message("Writing out feature classes; this may take a while")
  }

  # Combine the lists of sample frames and strata into a single list
  output_list <- split(combined_frames_sf,
                       combined_frames_sf$unique_id)

  # Write out each sf or SPDF object in the list as a shapefile for intersection to create weight categories
  for (index in seq_along(output_list)) {
    sf::st_write(obj = output_list[[index]],
                 dsn = output_path,
                 layer = paste0("frame",
                                index,
                                ".shp"),
                 driver = "ESRI Shapefile")
  }
}


#' Generating weight category polygons from a sample design database
#' @description This will take a reporting unit polygon and produce weight category polygons for it using an appropriately formatted sample design database and, optionally, Landscape Management Framework (LMF) stratification polygons. Please note that all input polygons MUST be dissolved by their unique identifiers before use.
#' @param reportingunit An sf polygons object, Spatial Polygons Data Frame, or character string. If a character string, it must be the name of a polygon feature class found in \code{reportingunit_path}. Regardless, the feature class must contain exactly one feature representing a single reporting unit.
#' @param reportingunit_path Optional character string. The filepath to the folder or geodatabase containing the feature class specified with the character string \code{reportingunit}. Not necessary if \code{reportingunit} is either an sf polygon object or Spatial Polygons Data Frame. Defaults to \code{NULL}.
#' @param sdd_path Character string. The full filepath to the sample design database (SDD), including .GDB file extension. The SDD must follow the standard schema, including the feature classes: TerrestrialPointEvaluation, TerrestrialSamplePoints, TerrestrialStrata, and TerrestrialSampleFrame.
#' @param lmf_strata Optional sf polygons object, Spatial Polygons Data Frame, or character string. If a character string, it must be the name of a polygon feature class found in \code{reportingunit_path}. Regardless, the feature class must contain exactly one feature representing a single reporting unit. Defaults to \code{NULL}.
#' @param lmf_path Optional character string. If the weight categories need to account for the Landscape Monitoring Framework (LMF) strata, this must be the full filepath, including file extension, to the geodatabase containing the strata feature class with the LMF strata.
#' @param pointvisits Optional sf points object, Spatial Points Data Frame, or character string. If a character string, it must be the name of a point feature class found in \code{pointvisits_path}. Regardless, the feature class must contain the points to be considered and in the same format as the feature class TerrestrailPointEvaluation. This would only be used if you have already restricted which point evaluations should be considered for this process; otherwise, leave \code{NULL} and use the full set of points in the sample design database defined by \code{sdd_path}. Defaults to \code{NULL}.
#' @param pointvisits_path Optional character string. If \code{pointvisits} is a character string naming a feature class then this must be the path to the folder or geodatabase containing that feature class. Defaults to \code{NULL}.
#' @param output Character string. Determines the output format and must be either \code{"sf"} for an sf object or \code{"spdf"} for a Spatial Polygons Data Frame. Defaults to \code{"sf"}
#' @param verbose Logical. If \code{TRUE} then diagnostic messages will be returned throughout processing. Defaults to \code{FALSE}.
#' @return An sf polygon object with the attributed "unique_is" and "area_ha" (area in hectares) projected in Albers Equal Area Conic
#' @export

wgtcat_gen <- function(reportingunit,
                       reportingunit_path = NULL,
                       sdd_path,
                       lmf_strata= NULL,
                       lmf_path = NULL,
                       pointvisits = NULL,
                       pointvisits_path = NULL,
                       output = "sf",
                       verbose = FALSE){
  # Because everything will be reprojected into Albers Equal Area Conic
  aea_proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  #### Sanitization ####
  if (!(output %in% c("sf", "spdf"))) {
    stop("output must be either 'sf' or 'spdf'")
  }

  if (!(class(sdd_path) %in% c("character"))) {
    stop("sdd_path must be a character string")
  } else if (!file.exists(sdd_path)) {
    stop("The file pointed to by sdd_path does not exist")
  }


  if (is.null(lmf_strata)) {
    lmf_strata_sf <- NULL
  } else if (any(class(lmf_strata) %in% c("sf"))) {
    lmf_strata_sf <- lmf_strata
    # Reproject!
    lmf_strata_sf <- sf::st_transform(lmf_strata_sf, aea_proj)
  } else if (class(lmf_strata) %in% c("SpatialPolygonsDataFrame")) {
    lmf_strata_sf <- methods::as(lmf_strata, "sf")
    # Reproject!
    lmf_strata_sf <- sf::st_transform(lmf_strata_sf, aea_proj)
  } else if (class(lmf_strata) %in% c("character")) {
    if (is.null(lmf_path)) {
      stop("If lmf_strata is a character string, you must provide lmf_path")
    } else {
      if (!(class(lmf_path) %in% c("character"))) {
        stop("path_to_lmf must be a character string")
      } else if (!file.exists(lmf_path)) {
        stop("The file pointed to by path_to_lmf does not exist")
      }
      lmf_strata_sf <- sf::st_read(dsn = lmf_path,
                                   layer = lmf_strata)
    }
  } else {
    stop("If lmf_strata is a character string, you must provide lmf_path pointing to the folder or geodatabase containing the named feature class.")
  }



  if (any(class(reportingunit) %in% c("sf"))) {
    reporting_unit_sf <- reportingunit
  } else if (class(reportingunit) %in% c("SpatialPolygonsDataFrame")) {
    reporting_unit_sf <- methods::as(reportingunit, "sf")
  } else if (class(reportingunit) %in% c("character")) {
    if (is.null(reportingunit_path)) {
      stop("You must specify a path to the reporting unit")
    } else if (class(reportingunit_path) != "character") {
      stop("reportingunit_path must be a character string")
    } else if (!file.exists(reportingunit_path)) {
      stop("reportingunit_path points to a file which does not exist")
    }
    if (verbose) {
      message("Reading in reporting units")
    }
    reporting_unit_sf <- sf::st_read(dsn = reportingunit_path,
                                     layer = reportingunit)
  } else {
    stop("reportingunit must be either a character string specifying the name of a polygon feature class to read in or an sf or SPDF object")
  }
  # Reproject!
  reporting_unit_sf <- sf::st_transform(reporting_unit_sf, aea_proj)

  if (is.null(pointvisits)) {
    if (verbose) {
      message("Reading in the TerrestrialPointEvaluation feature class")
    }
    pointvisits_sf <- sf::st_read(dsn = sdd_path,
                                  layer = "TerrestrialPointEvaluation")
  } else if (any(class(pointvisits) %in% c("sf"))) {
    pointvisits_sf <- pointvisits
  } else if (class(pointvisits) %in% c("SpatialPolygonsDataFrame")) {
    pointvisits_sf <- methods::as(pointvisits, "sf")
  } else if (class(pointvisits) %in% c("character")) {
    if (is.null(pointvisits_path)) {
      stop("You must specify a path to the reporting unit")
    } else if (class(pointvisits_path) != "character") {
      stop("pointvisits_path must be a character string")
    } else if (!file.exists(pointvisits_path)) {
      stop("pointvisits_path points to a file which does not exist")
    }
    if (verbose) {
      message("Reading in pointvisits")
    }
    if (verbose) {
      message("Reading in the pointvisits feature class")
    }
    pointvisits_sf <- sf::st_read(dsn = pointvisits_path,
                                  layer = pointvisits)
  } else {
    stop("pointvisits must be either a character string specifying the name of a polygon feature class to read in or an sf or SPDF object")
  }
  # Reproject!
  pointvisits_sf <- sf::st_transform(pointvisits_sf, aea_proj)


  #### Reporting Units and Points ####
  if (verbose) {
    message("Restricting to only points within the reporting unit")
  }
  # Limit TerrestrialPointEvaluation to those points within the reporting unit
  overlapping_indices <- as.vector(sf::st_covered_by(x = pointvisits_sf,
                                                     y = reporting_unit_sf,
                                                     # I don't "get" this argument, but
                                                     # sparse = TRUE returns an object I
                                                     # don't understand how to use
                                                     sparse = FALSE))
  pointvisits_relevant_sf <- pointvisits_sf[overlapping_indices, ]

  if (nrow(pointvisits_relevant_sf) < 1) {
    stop("There are no points in TerrestrialPointEvaluation that overlap with the reporting unit")
  }

  # Plot a visual confirmation that everything is going according to plan
  # ggplot() +
  #   geom_sf(data = reporting_unit_sf,
  #           color = "Red") +
  #   geom_sf(data = pointvisits_relevant_sf)

  # Get the unique SamplePointKeys that occur in the evaluated points that
  # fall within the reporting units
  relevant_samplepointkeys <- unique(pointvisits_relevant_sf$SamplePointKey)

  #### Reading in SamplePoints ####
  # Use a SQL query to read in TerrestrialSamplePoints with those keys
  # Paste them together, separated by commas (to jam into the SQL query string)
  samplepoints_ids_for_query <- paste0("('",
                                       paste0(relevant_samplepointkeys,
                                              collapse = "', '"),
                                       "')")
  # Assemble the SQL query string
  samplepoints_query <- paste("SELECT * FROM",
                              "TerrestrialSamplePoints",
                              "WHERE",
                              "SamplePointKey IN",
                              samplepoints_ids_for_query)
  if (verbose) {
    message("Reading in the TerrestrialSamplePoints feature class")
  }
  # Read in only the points that satisfy the SQL query
  samplepoints_sf <- sf::st_read(dsn = sdd_path,
                                 layer = "TerrestrialSamplePoints",
                                 query = samplepoints_query)

  # Reproject!
  samplepoints_sf <- sf::st_transform(samplepoints_sf, aea_proj)

  # Identify the unique TerrestrialSampleFrameID values in the TerrestrialSamplePoints that we just read in
  relevant_sampleframeids <- unique(samplepoints_sf$TerrestrialSampleFrameID)

  #### Clip LMF Strata ####
  if (is.null(lmf_strata)) {
    lmf_strata_for_combining_sf <- NULL
  } else {
    if (verbose) {
      message("Repairing LMF strata geometry")
    }
    # Buffer by 0 to correct geometry errors
    lmf_strata_buffered_sf <- sf::st_buffer(lmf_strata_sf,
                                            dist = 0)

    if (verbose) {
      message("Clipping LMF strata")
    }
    # Clip the strata to the reporting unit
    lmf_strata_clipped_sf <- sf::st_intersection(x = lmf_strata_buffered_sf,
                                                 y = reporting_unit_sf)

    lmf_strata_clipped_sf[["unique_id"]] <- paste0("lmf", 1:nrow(lmf_strata_clipped_sf))

    if (verbose) {
      message("Repairing clipped LMF strata geometry")
    }
    lmf_strata_clipped_sf <- sf::st_buffer(lmf_strata_clipped_sf,
                                           dist = 0)

    lmf_strata_for_combining_sf <- lmf_strata_clipped_sf[, "unique_id"]
  }
  #### Clip Strata ####
  # Use a SQL query to read in strata associated with the identified sample frames
  # N.B. This is currently impossible because there is no TerrestrialSampleFrame:TerrestrialStrata relationship
  sampleframe_ids_for_query <- paste0("('",
                                      paste0(relevant_sampleframeids,
                                             collapse = "', '"),
                                      "')")

  strata_query <- paste("SELECT * FROM",
                        "TerrestrialStrata",
                        "WHERE",
                        "TerrestrialSampleFrameID IN",
                        sampleframe_ids_for_query)

  if (verbose) {
    message("Reading in TerrestrialStrata feature class")
  }
  strata_sf <- sf::st_read(dsn = sdd_path,
                           layer = "TerrestrialStrata",
                           query = strata_query)

  # Reproject!
  strata_sf <- sf::st_transform(strata_sf, aea_proj)

  if (nrow(strata_sf) > 0) {
    if (verbose) {
      message("Repairing TerrestrialStrata geometry")
    }
    # Buffer by 0 to correct geometry errors
    strata_buffered_sf <- sf::st_buffer(strata_sf,
                                        dist = 0)

    if (verbose) {
      message("Clipping TerrestrialStrata")
    }
    # Clip the strata to the reporting unit
    strata_clipped_sf <- sf::st_intersection(x = strata_buffered_sf,
                                             y = reporting_unit_sf)

    strata_clipped_sf[["unique_id"]] <- paste0("s", 1:nrow(strata_clipped_sf))

    if (verbose) {
      message("Repairing clipped strata geometry")
    }
    # Buffer by 0 to correct geometry errors
    strata_clipped_sf <- sf::st_buffer(strata_clipped_sf,
                                       dist = 0)

    strata_for_combining_sf <- strata_clipped_sf[, "unique_id"]
  } else {
    strata_for_combining_sf <- NULL
  }




  # Use a SQL query to read in the sample frames with no strata
  # So, which IDs are associated with points but not with strata?
  sampleframeids_missing_strata <- relevant_sampleframeids[!(relevant_sampleframeids %in% strata_sf$TerrestrialSampleFrameID)]

  #### Read and Clip Sample Frames ####
  sampleframe_ids_for_query <- paste0("('",
                                      paste0(sampleframeids_missing_strata,
                                             collapse = "', '"),
                                      "')")

  sampleframe_query <- paste("SELECT * FROM",
                             "TerrestrialSampleFrame",
                             "WHERE",
                             "TerrestrialSampleFrameID IN",
                             sampleframe_ids_for_query)
  if (verbose) {
    message("Reading in TerrestrialSampleFrame feature class")
  }
  sampleframe_sf <- sf::st_read(dsn = sdd_path,
                                layer = "TerrestrialSampleFrame",
                                query = sampleframe_query)

  # Reproject!
  sampleframe_sf <- sf::st_transform(sampleframe_sf, aea_proj)

  if (nrow(sampleframe_sf) > 0) {
    # Buffer by 0 to repair geometry
    if (verbose) {
      message("Repairing TerrestrialSampleFrame geometry")
    }
    sampleframe_sf <- sf::st_buffer(sampleframe_sf,
                                    dist = 0)


    # Clip the sample frames to the reporting unit
    if (verbose) {
      message("Clipping TerrestrialSampleFrame")
    }
    sampleframe_clipped_sf <- sf::st_intersection(x = sampleframe_sf,
                                                  y = reporting_unit_sf)

    # Add a unique identifier
    sampleframe_clipped_sf[["unique_id"]] <- paste0("sf", 1:nrow(sampleframe_clipped_sf))

    if (verbose) {
      message("Repairing clipped sample frames geometry")
    }
    sampleframe_clipped_sf <- sf::st_buffer(sampleframe_clipped_sf,
                                            dist = 0)

    sampleframe_for_combining_sf <- sampleframe_clipped_sf[, "unique_id"]
  } else {
    sampleframe_for_combining_sf <- NULL
  }




  #### Create Wgtcats ####
  if (verbose) {
    message("Combining all clipped polygons")
  }
  # Combine the strata and the sample frames
  combined_frames_sf <- rbind(strata_for_combining_sf,
                              sampleframe_for_combining_sf,
                              lmf_strata_for_combining_sf)

  # Finally perform the intersection!
  if (verbose) {
    message("Intersecting polygons, this may take a while")
  }
  wgtcats_sf <- sf::st_intersection(combined_frames_sf)

  # Create a unique ID for each of the unique combination of polygons
  wgtcats_sf[["unique_id"]] <- sapply(X = wgtcats_sf$origins,
                                      FUN = function(X){
                                        paste(X, collapse = "-")
                                      })
  # Add in the area in hectares
  area_m2 <- sf::st_area(wgtcats_sf)
  area_ha <- area_m2
  units(area_ha) <- units::make_units(ha)
  wgtcats_sf[["area_ha"]] <- as.vector(area_ha)

  # Remove imaginary wgtcats
  wgtcats_sf <- wgtcats_sf[wgtcats_sf$area_ha > 0, ]

  # Plot for QA reasons!
  # ggplot() +
  #   geom_sf(data = wgtcats_sf,
  #           aes(fill = unique_id)) +
  #   geom_sf(data = pointvisits_relevant_sf)
  output <- wgtcats_sf[, c("unique_id", "area_ha")]

  if (output == "spdf") {
    output <- methods::as(output, "Spatial")
  }

  return(output)
}
