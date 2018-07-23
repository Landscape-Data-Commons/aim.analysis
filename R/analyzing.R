#' Categorical Analysis of AIM Data
#'
#' This function takes the outputs from the functions \code{weight()} optionally passed through \code{weight.adjust()}, \code{benchmarker()}, and a TerrADat SpatialPointsDataFrame with a field for reporting units. It applies \code{spsurvey::cat.analysis()} and returns the results
#' @param evaluated.points Data frame output from \code{benchmark()}.
#' @param point.weights Data frame output from \code{weight()}, equivalent to \code{weight()[["point.weights"]]} or \code{weight()[[2]]}.
#' @param default.reportingunit A string to populate the reporting unit field with in the case that designs were not restricted by reporting unit. Defaults to \code{"No reporting unit"}
#' @param reportingunit.type A string to populate the reporting unit type field with. Depends on the reporting units used, e.g. it may be \code{"Watershed"} or \code{"Sage-grouse Habitat"}. Defaults to \code{NA}.
#' @param adjustedweights Logical. If \code{TRUE}, use the values in the field ADJWGT instead of the field WGT. Allows for quick comparison between the results of analysis with and without the weight adjustment. Defaults to \code{FALSE}.
#' @param conf Numeric. The confidence level. Defaults to \code{80}.
#' @return A data frame with the output from \code{spsurvey::cat.analysis()}
#' @keywords analysis
#' @examples
#' analyze()
#' @export

analyze <- function(evaluated.points,
                    point.weights,
                    points.joinfield,
                    points.valuefield,
                    points.keyfield,
                    points.idfield,
                    points.splitfield = NULL,
                    points.xcoordfield,
                    points.ycoordfield,
                    weights.joinfield,
                    weights.weightfield,
                    weights.reportingfield,
                    default.reportingunit = "No reporting unit",
                    reportingunit.type = NA,
                    conf = 80
){
  if (is.null(reportingunit.type)){
    reportingunit.type <- NA
  }


  ## Then we combine them!
  data <- dplyr::distinct(merge(x = evaluated.points[, c(points.joinfield,
                                                         points.idfield,
                                                         points.valuefield,
                                                         points.keyfield,
                                                         points.splitfield,
                                                         points.xcoordfield,
                                                         points.ycoordfield)],
                                y = point.weights[, c(weights.joinfield,
                                                      weights.weightfield,
                                                      weights.reportingfield)],
                                by.x = points.joinfield,
                                by.y = weights.joinfield))

  ## If there are NA values, just add in the default
  missing.reporting <- is.na(data[[weights.reportingfield]])
  if (any(missing.reporting)) {
    data[missing.reporting, weights.reportingfield] <- default.reportingunit
  }

  # Split the data frame into a list to work with so we can just lapply() the same actions across each
  # Or just put it in a list if we're not splitting it. That lets us still use the lapply()
  if (is.null(points.splitfield)) {
    df.list <- list(data)
  } else {
    df.list <- split(data, data[[points.splitfield]])
  }

  # This is the step that creates the objects for cat.analysis() and runs it.
  # The lapply() means that we can efficiently apply this to each of the
  analyses.list <- lapply(X = df.list,
                          splitfield = points.splitfield,
                          idfield = points.idfield,
                          valuefield = points.valuefield,
                          keyfield = points.keyfield,
                          weightfield = weights.weightfield,
                          reportingfield = weights.reportingfield,
                          xcoordfield = points.xcoordfield,
                          ycoordfield = points.ycoordfield,
                          splitype = points.splitfield.type,
                          FUN = function(X,
                                         splitfield,
                                         idfield,
                                         valuefield,
                                         keyfield,
                                         weightfield,
                                         reportingfield,
                                         xcoordfield,
                                         ycoordfield,
                                         splitype){
                            # Just for ease of reference, so X isn't going on all over the place
                            data.current <- X

                            # Get all the indicators now. We'll use these to identify newly-made columns post-spread()
                            indicators <- unique(data.current[[keyfield]])

                            # Get the split value (Managment Objective for AIM)
                            splitvalue <- data.current[[splitfield]][1]

                            ## Make the data set wide because that's the format that makes our lives easier for cat.analysis()
                            data.wide.current <- tidyr::spread(data = data.current,
                                                               ## Column that contains the column names
                                                               # The !!rlang::quo() lets us pass the string stored as keyfield to spread() which wants bare variables
                                                               key = !!rlang::quo(keyfield),
                                                               ## Column that contains the values
                                                               value = !!rlang::quo(valuefield),
                                                               ## Where there's an NA, fill it with 0
                                                               fill = NA)

                            # Set the variable names to the ones expected by cat.analysis() to make it easy to create the objects it needs
                            names(data.wide.current)[names(data.wide.current) == idfield] <- "siteID"
                            names(data.wide.current)[names(data.wide.current) == weightfield] <- "wgt"
                            names(data.wide.current)[names(data.wide.current) == reportingfield] <- "Reporting.Unit"
                            names(data.wide.current)[names(data.wide.current) == xcoordfield] <- "xcoord"
                            names(data.wide.current)[names(data.wide.current) == ycoordfield] <- "ycoord"
                            # All of the data will be considered active when running cat.analysis()
                            data.wide.current$Active <- TRUE


                            ## All weights need to be positive values, so drop any 0s that have found their way in
                            data.wide.current <- data.wide.current[data.wide.current$wgt > 0,]

                            ## First, the sites. This is a data frame with the siteIDs and whether they're active or not
                            sites <- dplyr::distinct(data.wide.current[, c("siteID", "Active")])

                            ## The subpopulations. This is a data frame of the siteIDs and reporting units
                            subpop <- dplyr::distinct(data.wide.current[, c("siteID", "Reporting.Unit")])

                            ## The design information
                            design <- dplyr::distinct(data.wide.current[, c("siteID", "wgt", "xcoord", "ycoord")])

                            ## The data. A data frame with siteID and columns for each indicator
                            datacat <- dplyr::distinct(data.wide.current[, c("siteID", indicators)])

                            ## TODO: Think about how to get sum of wgt by stratum and set up a stratified aim.popsize list
                            ## The areas should be the sum of the weights
                            ## This example is for unstratified sampling (also for simplicity) for stratified, need to add the stratum field to the design data frame
                            ## and add the stratum areas to the popsize list
                            popsize <- list("Reporting.Unit" = lapply(split(data.wide.current, data.wide.current$Reporting.Unit),
                                                                      FUN = function(X){
                                                                        ru <- X$Reporting.Unit[1]
                                                                        area <- setNames(sum(X$wgt), ru)
                                                                        return(area)
                                                                      })
                            )

                            ### Now run cat.analysis
                            analysis <- spsurvey::cat.analysis(sites = sites,
                                                               subpop = subpop,
                                                               design = design,
                                                               data.cat = datacat,
                                                               popsize = popsize,
                                                               conf = conf)

                            ## Add in the reporting unit type
                            analysis$Type <- as.character(analysis$Type)
                            analysis$Type <- reportingunit.type

                            ## Add the split value info to those results
                            analysis[[splitfield]] <- splitvalue

                            return(analysis)
                          })

  # Turn those data frames into a single output data frame
  output <- dplyr::bind_rows(analyses.list)

  # This calculates binomial Wilson confidence intervals!
  conf.ints <- mapply(n = output$NResp,
                      proportion = output$Estimate.P/100,
                      conf.level = rep.int(conf/100, times = nrow(output)),
                      methods = rep.int("wilson", times = nrow(output)),
                      FUN = function(n, proportion, conf.level, methods){
                        x <- (proportion * n)

                        alpha <- 1 - conf.level
                        z <- qnorm(1 - alpha/2)

                        ## See this paper for equations:
                        # Interval Estimation for a Binomial Proportion
                        # Author(s): Lawrence D. Brown, T. Tony Cai and Anirban DasGupta
                        # Source: Statistical Science, Vol. 16, No. 2 (May, 2001), pp. 101-117
                        # Published by: Institute of Mathematical Statistics
                        # Stable URL: http://www.jstor.org/stable/2676784
                        ci.l.w <- ((x + (z^2)/2)/(n + (z^2))) - ((z*sqrt(n))/(n + (z^2)))*sqrt(proportion*(1 - proportion) + ((z^2)/(4*n))) # lower limit of the confidence interval
                        ci.u.w <- ((x + (z^2)/2)/(n + (z^2))) + ((z*sqrt(n))/(n + (z^2)))*sqrt(proportion*(1 - proportion) + ((z^2)/(4*n))) # upper limit of the confidence interval
                        return(c(ci.l.w = ci.l.w, ci.u.w = ci.u.w))})

  # This converts that matrix into a data frame, ready to be joined
  conf.ints.df <- dplyr::bind_rows(lapply(1:ncol(conf.ints),
                                          conf = conf,
                                          conf.ints,
                                          FUN = function(X, conf, conf.ints){
                                            output <- data.frame(conf.ints[1,X]*100,
                                                                 conf.ints[2,X]*100,
                                                                 stringsAsFactors = FALSE)
                                            names(output) <- paste0(c("LCB", "UCB"), conf, "Pct.W")
                                            rownames(output) <- NULL
                                            return(output)
                                          }))

  # Add in those Wilson's binomial confidence intervals for the sake of the wildlife biologists
  output <- cbind(output, conf.ints.df)

  return(output)
}
