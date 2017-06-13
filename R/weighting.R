#' Calculating Weights from AIM Sample Designs
#'
#' This function takes the output from the function read.dd() and optionally a SpatialPolygonsDataFrame defining the extent of reporting units. Returns a list of two data frames: strata.weights with the weighting information by stratum and point.weights with the weighting information by point.
#' @param dd.import Output from read.dd(). This can be the result of running read.dd() on one or many sample design databases
#' @param combine Logical. If provided multiple DDs, should those be combined as part of the weighting calculations? Otherwise the weights will be calculated on a per-DD basis. Defaults to TRUE.
#' @param reorder Logical. If provided multiple DDs, should those be reordered to reflect ascending area? If FALSE, the DDs are considered in the order they appear in dd.import. If combine is FALSE, then this only potentially affects the order of the output information. Defaults to T.
#' @param reporting.units.spdf SpatialPolygonsDataFrame. Optional reporting units polygons to restrict the sample designs to. If provided, weights will be calculated appropriately based on the intersection of this SPDF and the design[s] Defaults to NULL
#' @param reportingunitfield Character string. If passing a reporting unit SPDF, what field in it defines the reporting unit[s]?
#' @param target.values Character string or character vector. This defines what values in the point fate field count as target points. The function always looks for "Target Sampled" and "TS", so this argument is only necessary if there are additional values in the sample design databases. This is case insensitive
#' @param unknown.values Character string or character vector. This defines what values in the point fate field count as unknown points. The function always looks for "Unknown" and "Unk", so this argument is only necessary if there are additional values in the sample design databases. This is case insensitive
#' @param nontarget.values Character string or character vector. This defines what values in the point fate field count as non-target points. The function always looks for "Non-Target", "NT", and NA, so this argument is only necessary if there are additional values in the sample design databases. This is case insensitive
#' @param inaccessible.values Character string or character vector. This defines what values in the point fate field count as non-target points. The function always looks for "Inaccessible", so this argument is only necessary if there are additional values in the sample design databases. This is case insensitive
#' @param unneeded.values Character string or character vector. This defines what values in the point fate field count as not needed or unneeded points. The function always looks for "Not needed", so this argument is only necessary if there are additional values in the sample design databases. This is case insensitive
#' @param fatefieldname Character string defining the field name in the points SPDF[s] in dd.import that contains the point fate. Defaults to "final_desig"
#' @param pointstratumfieldname Character string defining the field name in the points SPDF[s] in dd.import that contains the design stratum. Defaults to "dsgn_strtm_nm
#' @param designstratumfield Character string defining the field name in the strata SPDF[s] in dd.import that contains the design stratum. Defaults to "dmnt_strtm"
#' @param projection \code{sp::CRS()} argument. Defaults to NAD83 with \code{sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")}. Is used to reproject all SPDFs in order to perform spatial manipulations.
#' @keywords weights
#' @examples
#' weighter()
#' @export

## TODO: Figure out what to do about DDs that have future points

## This function produces point weights by design stratum (when the DD contains them) or by sample frame (when it doesn't)
weight <- function(dd.import, ## The output from read.dd()
                   combine = T, ## If provided multiple DDs, should those be combined into a single analysis? Otherwise the weights will be calculated on a per-DD basis
                   reorder = T, ## Should the DDs be reordered by size or ar they provided in the order that they should be considered? Depends on how they overlap and user discretion
                   reporting.units.spdf = NULL, ## An optional reporting unit SPDF that will be used to clip the DD import before calculating weights
                   reportingunitfield = "REPORTING.UNIT", ## If passing a reporting unit SPDF, what field in it defines the reporting unit[s]?
                   ## Keywords for point fate—the values in the vectors unknown and nontarget are considered nonresponses.
                   target.values = c("Target Sampled",
                                     "TS"),
                   unknown.values = c("Unknown",
                                      "UNK"),
                   nontarget.values = c("Non-Target",
                                        "NT",
                                        NA),
                   inaccessible.values = c("Inaccessible",
                                           "IA"),
                   unneeded.values = c("Not Needed"),
                   ## These shouldn't need to be changed from these defaults, but better to add that functionality now than regret not having it later
                   fatefieldname = "final_desig", ## The field name in the points SPDF to pull the point fate from
                   pointstratumfieldname = "dsgn_strtm_nm", ## The field name in the points SPDF to pull the design stratum
                   designstratumfield = "dmnnt_strtm", ## The field name in the strata SPDF to pull the stratum identity from
                   projection = sp::CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"), ## Standard NAD83
                   sliverdrop = T,
                   sliverwarn = T,
                   sliverthreshold = 0.01
){
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

  ## The fate values that we know about are hardcoded here.
  ## Whatever values are provided in the function arguments get concatenated and then we keep only the unique values from that result
  target.values <- c(target.values,
                     "Target Sampled",
                     "TS") %>% unique() %>% stringr::str_to_upper()
  unknown.values <- c(unknown.values,
                      "Unknown",
                      "UNK") %>% unique() %>% stringr::str_to_upper()
  nontarget.values <- c(nontarget.values,
                        "Non-Target",
                        "NT",
                        NA) %>% unique() %>% stringr::str_to_upper()
  inaccessible.values <- c(inaccessible.values,
                           "Inaccessible",
                           "IA") %>% unique() %>% stringr::str_to_upper()
  unneeded.values <- c(unneeded.values,
                       "Not Needed") %>% unique() %>% stringr::str_to_upper()

  ## In case the DDs are from different generations, we need to restrict them to only the shared fields
  if (length(dd.import$pts) > 1) {
    fieldnames.common <- lapply(dd.raw$pts, names) %>% unlist() %>%
      data.frame(fields = ., stringsAsFactors = F) %>%
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
      ## Overwrite whatever value was brought in from the reporting.units.spdf with T because we only want to know if they were restricted or not
      # pts.spdf@data$REPORTING.UNIT.RESTRICTED <- T
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
      frame.spdf@data$REPORTING.UNIT.RESTRICTED <- T
    }

    ## Add the area to frame.spdf
    frame.spdf <- area.add(frame.spdf, byid = T)

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
    dd.order <- unlist(dd.order)[dd.order %>% unlist() %>% sort.list(decreasing = F)]
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
              print(paste("Attempting to remove the", s, "frame from the", r, "frame with rgeos::gDifference()"))

              ## This lets rgeos deal with tiny fragments of polygons without crashing
              ## This and the following tryCatch() may be unnecessary since the argument drop_lower_td = T was added, but it works so I'm leaving it
              current.drop <- rgeos::get_RGEOS_dropSlivers()
              current.warn <- rgeos::get_RGEOS_warnSlivers()
              current.tol <- rgeos::get_RGEOS_polyThreshold()

              frame.spdf.temp <- tryCatch(
                expr = {
                  rgeos::set_RGEOS_dropSlivers(sliverdrop)
                  rgeos::set_RGEOS_warnSlivers(sliverwarn)
                  rgeos::set_RGEOS_polyThreshold(sliverthreshold)
                  print(paste0("Attempting using rgeos::set_RGEOS_dropslivers(", sliverdrop, ") and rgeos::set_RGEOS_warnslivers(", sliverwarn, ") and set_REGOS_polyThreshold(", sliverthreshold, ")"))
                  rgeos::gDifference(spgeom1 = frame.spdf.temp %>%
                                       ## Making this Albers for right now for gBuffer()
                                       sp::spTransform(CRS("+proj=aea")) %>%
                                       ## The gbuffer() is a common hack to deal with ring self-intersections, which it seems to do just fine here?
                                       rgeos::gBuffer(byid = TRUE, width = 0),
                                     spgeom2 = frame.spdf %>%
                                       sp::spTransform(CRS("+proj=aea")) %>%
                                       rgeos::gBuffer(byid = TRUE, width = 0),
                                     drop_lower_td = T) %>%
                    sp::SpatialPolygonsDataFrame(data = frame.spdf.temp@data) %>%
                    sp::spTransform(CRSobj = frame.spdf.temp@proj4string)
                },
                error = function(e) {
                  print("Received the error:")
                  print(paste(e))
                  if (grepl(x = e, pattern = "cannot get a slot")) {
                    print("This is extremely problematic (unless it means your whole frame was correctly erased), but there's no automated error handling for it.")
                  } else if (grepl(x = e, pattern = "few points in geometry")) {
                    print(paste0("Attempting again with rgeos::set_RGEOS_dropslivers(T), rgeos::set_RGEOS_warnslivers(T), and rgeos::set_RGEOS_polyThresholds(", sliverthreshold, ")"))
                    rgeos::set_RGEOS_dropSlivers(T)
                    rgeos::set_RGEOS_warnSlivers(T)
                    rgeos::set_RGEOS_polyThreshold(sliverthreshold)
                    rgeos::gDifference(spgeom1 = frame.spdf.temp %>%
                                         sp::spTransform(CRS("+proj=aea")) %>%
                                         rgeos::gBuffer(byid = TRUE, width = 0),
                                       spgeom2 = frame.spdf %>%
                                         sp::spTransform(CRS("+proj=aea")) %>%
                                         rgeos::gBuffer(byid = TRUE, width = 0),
                                       drop_lower_td = T) %>%
                      sp::SpatialPolygonsDataFrame(data = frame.spdf.temp@data) %>%
                      sp::spTransform(CRSobj = frame.spdf.temp@proj4string)
                  } else if (grepl(x = e, pattern = "SET_VECTOR_ELT")) {
                    print(paste0("Attempting again with rgeos::set_RGEOS_dropslivers(F) and rgeos::set_RGEOS_warnslivers(F)"))
                    rgeos::set_RGEOS_dropSlivers(F)
                    rgeos::set_RGEOS_warnSlivers(F)
                    rgeos::set_RGEOS_polyThreshold(0)
                    rgeos::gDifference(spgeom1 = frame.spdf.temp %>%
                                         sp::spTransform(CRS("+proj=aea")) %>%
                                         rgeos::gBuffer(byid = TRUE, width = 0),
                                       spgeom2 = frame.spdf %>%
                                         sp::spTransform(CRS("+proj=aea")) %>%
                                         rgeos::gBuffer(byid = TRUE, width = 0),
                                       drop_lower_td = T) %>%
                      sp::SpatialPolygonsDataFrame(data = frame.spdf.temp@data) %>%
                      sp::spTransform(CRSobj = frame.spdf.temp@proj4string)
                  }
                }
              )

              rgeos::set_RGEOS_dropSlivers(current.drop)
              rgeos::set_RGEOS_warnSlivers(current.warn)
              rgeos::set_RGEOS_polyThreshold(current.tol)

              print("Erasure complete or at least attempted")

              if (!is.null(frame.spdf.temp)) {
                if (nrow(frame.spdf.temp@data) > 0) {
                  print(paste("!is.null(frame.spdf.temp) evaluates to", !is.null(frame.spdf.temp)))

                } else {
                  print(paste("There were no rows, so writing NULL in instead"))
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

      ## Sanitize
      pts.spdf@data[, fatefieldname] <- stringr::str_to_upper(pts.spdf@data[, fatefieldname])

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

        ## Check to see if the panel names contain the intended year (either at the beginning or end of the panel name) and use those to populate the YEAR
        working.pts$YEAR[grepl(x = working.pts$PANEL, pattern = "\\d{4}$")] <- working.pts$PANEL %>%
          stringr::str_extract(string = ., pattern = "\\d{4}$") %>% na.omit() %>% as.numeric()
        working.pts$YEAR[grepl(x = working.pts$PANEL, pattern = "^\\d{4}")] <- working.pts$PANEL %>%
          stringr::str_extract(string = ., pattern = "^\\d{4}") %>% na.omit() %>% as.numeric()

        ## Use the sampling date if we can. This obviously only works for points that were sampled. It overwrites an existing YEAR value from the panel name if it exists
        working.pts$YEAR[!is.na(working.pts$DT_VST)] <- working.pts$DT_VST[!is.na(working.pts$DT_VST)] %>% stringr::str_extract(string = ., pattern = "^\\d{4}") %>% as.numeric()

        ## For some extremely mysterious reasons, sometimes there are duplicate fields here. This will remove them
        working.pts <- working.pts[, (1:length(names(working.pts)))] %>% dplyr::select(-ends_with(match = ".1"))


        ## To create a lookup table in the case that we're working solely from sampling dates. Let's get the most common sampling year for each panel
        panel.years <- working.pts %>% dplyr::group_by(PANEL) %>%
          dplyr::summarize(YEAR = names(sort(summary(as.factor(YEAR)), decreasing = T)[1]))

        ## If we still have points without dates at this juncture, we can use that lookup table to make a good guess at what year they belong to
        for (p in panel.years$PANEL) {
          working.pts$YEAR[is.na(working.pts$YEAR) & working.pts$PANEL == p] <- panel.years$YEAR[panel.years$PANEL == p]
        }

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
          stratum.summary$Reporting.Unit.Restricted <- T
        } else {
          stratum.summary$Reporting.Unit.Restricted <- F
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
                              Reporting.Unit.Restricted = F,
                              stringsAsFactors = F)
        if (!is.null(reporting.units.spdf)) {
          temp.df$Reporting.Unit.Restricted <- T
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

          names(pointsweights.current)[] <- c("LONGITUDE", "LATITUDE")
          ## Add the point SPDF now that it's gotten the extra fields to the list of point SPDFs so we can use it after the loop
          pointweights.df <- rbind(pointweights.df, pointsweights.current[, c("TERRA_TERRADAT_ID", "PLOT_NM", "REPORTING.UNIT", "FINAL_DESIG", "WEIGHT.ID", "WGT", "LONGITUDE", "LATITUDE", "ADJWGT")])
        }
      }
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
#' @param wgtcat.spdf SpatialPolygonsDataFrame describing the weight categories for adjusting the weights. Use the output from \code{intersect()}
#' @param spdf.area.field Character string defining the field name in \code{wgtcat@data} that contains the areas for the weight categories. Defaults to \code{"AREA.HA.UNIT.SUM"}.
#' @param spdf.wgtcat.field Character string defining the field name in \code{wgtcat@data} that contains the unique identification for the weight categories. Defaults to \code{"UNIQUE.IDENTIFIER"}.
#' @param projection \code{sp::CRS()} argument. Defaults to NAD83 with \code{sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")}. Is used to reproject all SPDFs in order to perform spatial manipulations
#' @keywords weights
#' @examples
#' weight.adjuster()
#' @export

weight.adjust <- function(points, ## The weighted output from weighter(), so weighter()["point.weights"] | weighter()[2] IF YOU RESTRICTED THE SDD INPUT BY THE REPORTING UNIT POLYGON
                          wgtcat.spdf, ## The SPDF that's represents all the weird possible combinations of the reporting unit and strata
                          spdf.area.field = "AREA.HA.UNIT.SUM", ## The name of the field in the SPDF that contains the areas of the weight categories
                          spdf.wgtcat.field = "UNIQUE.IDENTIFIER", ## The name of the field in the SPDF that contains the identifiers for weight categories
                          projection = sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs") ## NAD83, standard issue as always
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
  sites.current <- (rep(T, nrow(data.current)))

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
