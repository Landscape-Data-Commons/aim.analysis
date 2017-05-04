#' Categorical Analysis of AIM Data
#'
#' This function takes the outputs from the functions \code{weight()} optionally passed through \code{weight.adjust()}, \code{benchmarker()}, and a TerrADat SpatialPointsDataFrame with a field for reporting units. It applies \code{spsurvey::cat.analysis()} and returns the results
#' @param evaluated.points Data frame output from \code{benchmark()}.
#' @param point.weights Data frame output from \code{weight()}, equivalent to \code{weight()[["point.weights"]]} or \code{weight()[[2]]}.
#' @param default.reportingunit A string to populate the reporting unit field with in the case that designs were not restricted by reporting unit. Defaults to \code{"No reporting unit"}
#' @param reportingunit.type A string to populate the reporting unit type field with. Depends on the reporting units used, e.g. it may be \code{"Watershed"} or \code{"Sage-grouse Habitat"}. Defaults to \code{NA}.
#' @param adjustedweights Logical. If \code{T}, use the values in the field ADJWGT instead of the field WGT. Allows for quick comparison between the results of analysis with and without the weight adjustment. Defaults to \code{T}.
#' @return A data frame with the output from \code{spsurvey::cat.analysis()}
#' @keywords analysis
#' @examples
#' analyze()
#' @export

analyze <- function(evaluated.points,
                     point.weights,
                     default.reportingunit = "No reporting unit",
                     reportingunit.type = NA,
                     adjustedweights = T
                     ){

  ## Sanitization
  names(evaluated.points) <- stringr::str_to_upper(names(evaluated.points))
  names(point.weights) <- stringr::str_to_upper(names(point.weights))
  ## Limiting to points that have valid PrimaryKey values
  point.weights <- point.weights %>% filter(grepl(x = PRIMARYKEY, pattern = "^[0-9]{15,24}-[0-9]{1,3}-[0-9]{1,3}$"))
  evaluated.points <- evaluated.points %>% filter(grepl(x = PRIMARYKEY, pattern = "^[0-9]{15,24}-[0-9]{1,3}-[0-9]{1,3}$"))
  if (is.null(reportingunit.type)){
    reportingunit.type <- NA
  }

  ## Then we combine them!
  data <- merge(x = evaluated.points,
                y = point.weights,
                by.x = "PRIMARYKEY",
                by.y = "PRIMARYKEY") %>% distinct()

  ## If there are NA values, just add in the default
  data$REPORTING.UNIT[is.na(data$REPORTING.UNIT)] <- default.reportingunit

  ## We're going to add ".ind" to the end of each indicator name so we can find them easily later with a select() after we've spread() this data frame
  data$INDICATOR <- paste0(data$INDICATOR, ".ind") %>% as.factor()

  ## Sometimes you'll get duplicate PLOTID fields? We're going to remove the one that came from point.weights, i.e. PLOTID.y
  if ("PLOTID.x" %in% names(data)) {
    names(data)[names(data) == "PLOTID.x"] <- "PLOTID"
    data <- data[, names(data)[names(data) != "PLOTID.y"]]
  }

  ## Initialize the output data frame
  output <- data.frame()
  warnings <- data.frame()

  ## The actual analysis will be done on a per-objective level, so we're just going to loop through those because apply() is kind of a pain and this is computationally cheap enough (I think)
  for (o in unique(data$MANAGEMENT.QUESTION)) {
    data.current <- data[data$MANAGEMENT.QUESTION == o,]

    ## Make the data set wide because that's the format that makes our lives easier for cat.analysis()
    ## Need to remove the columns Value and so that each plot ends up existing on just one row per evaluation stratum it has membership in
    data.wide.current <- spread(data = data.current %>% dplyr::select(-VALUE, -EVALUATION.STRATUM), ## Data frame to make wide
                                key = INDICATOR, ## Column that contains the column names
                                value = EVALUATION.CATEGORY, ## Column that contains the values
                                fill = NA ## Where there's an NA, fill it with 0
    )

    ## Because it's easier to do this now while the data frame is still just one object and not four or five
    names(data.wide.current)[names(data.wide.current) == "PLOTID"] <- "siteID"
    ## If there are adjusted weights and we want to use them, then this is where to do it.
    if (adjustedweights & ("ADJWGT" %in% names(data.wide.current))) {
      names(data.wide.current)[names(data.wide.current) == "ADJWGT"] <- "wgt"
      if ((NA %in% data.wide.current$wgt) & length(unique(data.wide.current$wgt)) == 1) {
        stop("The adjusted weight values are all NA. If they should be something else, check your weighting step.")
      }

    } else {
      names(data.wide.current)[names(data.wide.current) == "WGT"] <- "wgt"
    }
    names(data.wide.current)[names(data.wide.current) == "REPORTING.UNIT"] <- "Reporting.Unit"
    names(data.wide.current)[names(data.wide.current) == "LONGITUDE"] <- "xcoord"
    names(data.wide.current)[names(data.wide.current) == "LATITUDE"] <- "ycoord"
    ## All the sites are active? Sure! Why not?
    data.wide.current$Active <- T

    ## First, the sites. This is a data frame with the siteIDs and whether they're active or not
    aim.sites <- data.wide.current[, c("siteID", "Active")] %>% distinct()

    ## The subpopulations. This is a data frame of the siteIDs and reporting units. I think each siteID can only appear once, so we need to programmatically create this from the tall data frame
    aim.subpop <- data.wide.current[, c("siteID", "Reporting.Unit")] %>% distinct()

    ## The design information
    aim.design <- data.wide.current[, c("siteID", "wgt", "xcoord", "ycoord")] %>% distinct()

    ## The data. A data frame with siteID and columns for each indicator (with the evaluation category strings as factors)
    aim.datacat <- data.wide.current %>% dplyr::select(siteID, matches("\\.ind$")) %>% distinct()
    ## Fix the names of the indicators so that the output doesn't include the ".ind" suffix which interferes with the automated report knitting
    names(aim.datacat) <- names(aim.datacat) %>% str_replace_all("\\.ind$", "")

    ## TODO: Think abot how to get sum of wgt by stratum and set up a stratified aim.popsize list
    ## The areas should be the sum of the weights, right?
    areas.df <- data.wide.current %>% group_by(Reporting.Unit) %>% dplyr::summarize(area = sum(wgt))

    ## So we're converting them to a list
    area.list <- areas.df$area %>% as.list()
    ## And naming them with the reporting unit they belong to
    names(area.list) <- areas.df$Reporting.Unit

    ## This example is for unstratified sampling (also for simplicity) for stratified, need to add the stratum field to the design data frame
    ## and add the stratum areas to the popsize list
    aim.popsize <- list("Reporting.Unit" = area.list)

    ## In case we got warnings last time, nullify them
    warn.df <- NULL

    ### Now run cat.analysis
    aim.analysis <- cat.analysis(sites = aim.sites, subpop = aim.subpop, design = aim.design, data.cat = aim.datacat, popsize = aim.popsize)

    ## Add in the reporting unit type
    aim.analysis$Type <- as.character(aim.analysis$Type)
    aim.analysis$Type <- reportingunit.type

    ## Add the objective/management question info to those results
    aim.analysis$MANAGEMENT.QUESTION <- o

    ## rbind() these results to the output data frame and the warnings if there are any
    output <- rbind(output, aim.analysis)
    if (!is.null(warn.df)) {
      warnings <- rbind(warnings, warn.df)
    }
  }

  return(list(analyses = output, warnings = warnings))
}
