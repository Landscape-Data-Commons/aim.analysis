#' Categorical Analysis of AIM Data
#'
#' This function takes the outputs from the functions \code{weight()} optionally passed through \code{weight.adjust()}, \code{benchmarker()}, and a TerrADat SpatialPointsDataFrame with a field for reporting units. It applies \code{spsurvey::cat.analysis()} and returns the results
#' @param benchmarked_points Data frame. This should be the output from \code{apply_benchmarks()} or an equivalent and must contain the variables specified in the arguments \code{id_var}, \code{value_var}, \code{indicator_var}, \code{x_var}, and \code{x_var}. If \code{split_var} is not \code{NULL} then that variable must also be in the data frame.
#' @param point_weights Data frame. This should be the \code{"point_weights"} data frame output from \code{weight()} or an equivalent. It must contain the variables specified in the arguments \code{weights.joinfield}, \code{weights.weightfield}, and \code{weights.reportingfield}.
#' @param id_var Character string. The name of the variable in \code{benchmarked_points} and \code{point_weights} that contains the values used to join them. The relationship of \code{point_weights}:\code{benchmarked_points} must be 1:1 or 1:Many.
#' @param value_var Character string. The name of the variable in \code{benchmarked_points} that contains the categorical value assignments for the indicators appearing in \code{benchmarked_points$indicator_var}.
#' @param indicator_var Character string. The name of the variable in \code{benchmarked_points} that contains the indicators for which there are categorical assignments in \code{benchmarked_points$value_var}.
#' @param x_var Character string. The name of the variable in \code{benchmarked_points} that contains the numeric values corresponding to the x coordinates (longitude) of the plots/observations.
#' @param y_var Character string. The name of the variable in \code{benchmarked_points} that contains the numeric values corresponding to the y coordinates (latitude) of the plots/observations.
#' @param wgt_var Character string. The name of the variable in \code{point_weights} that contains the numeric weight values.
#' @param reporting_var Character string. The name of the variable in \code{point_weights} that contains the values corresponding to the reporting unit (subpopulation) that the observations belong to.
#' @param split_var Optional character string or character vector. The name(s) of variable(s) that analyses should be split along. This must be done if a point had more than one benchmark for an indicator, e.g. splitting along the management objective variable if two different objects had distinct benchmarks for the same indicator that covered the same plot(s).
#' @param conf Numeric. The confidence level in percent. Defaults to \code{80}.
#' @return A data frame with the output from \code{spsurvey::cat.analysis()}
#' @keywords analysis
#' @examples
#' analyze()
#' @export

analyze <- function(benchmarked_points,
                    point_weights,
                    id_var,
                    indicator_var = "indicator",
                    value_var,
                    weight_var = "wgt",
                    x_var = "x",
                    y_var = "y",
                    reporting_var,
                    split_var = NULL,
                    conf = 80){
  if (class(benchmarked_points) != "data.frame") {
    stop("benchmarked_points must be a data frame")
  }
  expected_in_benchmarked <- c(id_var, split_var, value_var, indicator_var, x_var, y_var)
  if (class(expected_in_benchmarked) != "character") {
    stop("All *_var arguments must be character strings or character vectors.")
  }
  missing_from_benchmarked <- expected_in_benchmarked[!(expected_in_benchmarked %in% names(benchmarked_points))]
  if (length(missing_from_benchmarked) > 0) {
    stop(paste0("The following variables are missing from benchmarked_points: ", paste(missing_from_benchmarked, collapse = ", ")))
  }


  if (class(point_weights) != "data.frame") {
    stop("point_weights must be a data frame")
  }
  expected_in_weights <- c(id_var, weight_var, reporting_var)
  if (class(expected_in_weights) != "character") {
    stop("All *_var arguments must be character strings or character vectors.")
  }
  missing_from_weights <- expected_in_weights[!(expected_in_weights %in% names(point_weights))]
  if (length(missing_from_weights) > 0) {
    stop(paste0("The following variables are missing from point_weights: ", paste(missing_from_weights, collapse = ", ")))
  }


  points <- dplyr::distinct(merge(x = benchmarked_points[, expected_in_benchmarked],
                                  y = point_weights[, expected_in_weights],
                                  by = id_var))

  # Split the data frame into a list to work with so we can just lapply() the same actions across each
  # Or just put it in a list if we're not splitting it. That lets us still use the lapply()
  if (is.null(split_var)) {
    df_list <- list(points)
  } else {
    df_list <- split(points, points[[split_var]])
  }

  # This is the step that creates the objects for cat.analysis() and runs it.
  # The lapply() means that we can efficiently apply this to each of the
  analyses_list <- lapply(X = df_list,
                          split_var = split_var,
                          id_var = id_var,
                          value_var = value_var,
                          indicator_var = indicator_var,
                          weight_var = weight_var,
                          reporting_var = reporting_var,
                          x_var = x_var,
                          y_var = y_var,
                          FUN = function(X,
                                         split_var,
                                         id_var,
                                         value_var,
                                         indicator_var,
                                         weight_var,
                                         reporting_var,
                                         x_var,
                                         y_var){
                            # Just for ease of reference, so X isn't going on all over the place
                            data <- X

                            # I'm not positive this is okay, but we're doing it. Check later when I've slept enough
                            data <- data[!is.na(data[[indicator_var]]), ]

                            # Get all the indicators now. We'll use these to identify newly-made columns post-pivot_wider()
                            indicators <- unique(data[[indicator_var]])
                            indicators <- indicators[!is.na(indicators)]

                            # Get the split value (Managment Objective for AIM)
                            if (!is.null(split_var)) {
                              split_value <- data[[split_var]][1]
                            }

                            ## Make the data set wide because that's the format that makes our lives easier for cat.analysis()
                            data_wide <- as.data.frame(tidyr::pivot_wider(data = data,
                                                                          names_from = tidyselect::one_of(indicator_var),
                                                                          values_from = tidyselect::one_of(value_var),
                                                                          values_fill = setNames(list("Uknown/Irrelevant"), value_var),
                                                                          # values_fill = setNames(list(NA), value_var)
                            ),
                            stringsAsFactors = FALSE)


                            # Set the variable names to the ones expected by cat.analysis() to make it easy to create the objects it needs
                            names(data_wide)[names(data_wide) == id_var] <- "siteID"
                            names(data_wide)[names(data_wide) == weight_var] <- "wgt"
                            names(data_wide)[names(data_wide) == reporting_var] <- "Reporting.Unit"
                            names(data_wide)[names(data_wide) == x_var] <- "xcoord"
                            names(data_wide)[names(data_wide) == y_var] <- "ycoord"
                            # All of the data will be considered active when running cat.analysis()
                            data_wide[["Active"]] <- TRUE


                            ## All weights need to be positive values, so drop any 0s that have found their way in
                            data_wide <- data_wide[data_wide[["wgt"]] > 0, ]

                            ## First, the sites. This is a data frame with the siteIDs and whether they're active or not
                            sites <- dplyr::distinct(data_wide[, c("siteID", "Active")])

                            ## The subpopulations. This is a data frame of the siteIDs and reporting units
                            subpop <- dplyr::distinct(data_wide[, c("siteID", "Reporting.Unit")])

                            ## The design information
                            design <- dplyr::distinct(data_wide[, c("siteID", "wgt", "xcoord", "ycoord")])

                            ## The data. A data frame with siteID and columns for each indicator
                            datacat <- dplyr::distinct(data_wide[, c("siteID", indicators)])

                            ## TODO: Think about how to get sum of wgt by stratum and set up a stratified aim.popsize list
                            ## The areas should be the sum of the weights
                            ## This example is for unstratified sampling (also for simplicity) for stratified, need to add the stratum field to the design data frame
                            ## and add the stratum areas to the popsize list
                            popsize <- list("Reporting.Unit" = lapply(X = split(data_wide, data_wide[["Reporting.Unit"]]),
                                                                      FUN = function(X){
                                                                        ru <- X[["Reporting.Unit"]][1]
                                                                        area <- setNames(sum(X[["wgt"]]), ru)
                                                                        return(area)
                                                                      }))

                            ### Now run cat.analysis
                            analysis <- spsurvey::cat.analysis(sites = sites,
                                                               subpop = subpop,
                                                               design = design,
                                                               data.cat = datacat,
                                                               popsize = popsize,
                                                               conf = conf)

                            # ## Add in the reporting unit type
                            # analysis$Type <- as.character(analysis$Type)
                            # analysis$Type <- reportingunit.type

                            if (!is.null(split_var)) {
                              ## Add the split value info to those results
                              analysis[[split_var]] <- split_value
                            }

                            return(analysis)
                          })

  # Turn those data frames into a single output data frame
  output <- do.call(rbind,
                    analyses_list)

  # This calculates binomial Wilson confidence intervals!
  conf_intervals <- mapply(n = output[["NResp"]],
                           proportion = output[["Estimate.P"]]/100,
                           conf_level = rep.int(conf/100, times = nrow(output)),
                           methods = rep.int("wilson", times = nrow(output)),
                           FUN = function(n, proportion, conf_level, methods){
                             x <- (proportion * n)

                             alpha <- 1 - conf_level
                             z <- qnorm(1 - alpha / 2)

                             ## See this paper for equations:
                             # Interval Estimation for a Binomial Proportion
                             # Author(s): Lawrence D. Brown, T. Tony Cai and Anirban DasGupta
                             # Source: Statistical Science, Vol. 16, No. 2 (May, 2001), pp. 101-117
                             # Published by: Institute of Mathematical Statistics
                             # Stable URL: http://www.jstor.org/stable/2676784
                             # Lower limit of the confidence interval
                             ci.l.w <- ((x + (z^2) / 2)/(n + (z^2))) - ((z * sqrt(n)) / (n + (z^2))) * sqrt(proportion * (1 - proportion) + ((z^2) / (4*n)))
                             # Upper limit of the confidence interval
                             ci.u.w <- ((x + (z^2) / 2)/(n + (z^2))) + ((z * sqrt(n)) / (n + (z^2))) * sqrt(proportion * (1 - proportion) + ((z^2) / (4*n)))

                             return(c(ci.l.w = ci.l.w, ci.u.w = ci.u.w))})

  # This converts that matrix into a data frame, ready to be joined
  conf_intervals_df <- do.call(rbind,
                               lapply(X = 1:ncol(conf_intervals),
                                      conf = conf,
                                      conf_intervals,
                                      FUN = function(X, conf, conf_intervals){
                                        output <- data.frame(conf_intervals[1, X] * 100,
                                                             conf_intervals[2, X] * 100,
                                                             stringsAsFactors = FALSE)
                                        names(output) <- paste0(c("LCB", "UCB"), conf, "Pct.W")
                                        rownames(output) <- NULL
                                        return(output)
                                      }))

  # Add in those Wilson's binomial confidence intervals for the sake of the wildlife biologists
  output <- cbind(output, conf_intervals_df)

  return(output)
}

#' Calculate Goodman's multinomial confidence intervals
#' @description Calculate confidence intervals for multinomial proportions using the method described by Leo Goodman in "On Simultaneous Confidence Intervals for Multinomial Proportions" in Technometrics in 1965.
#' @param counts Numeric vector, optionally named. The counts for each of the categories being considered. If there are unequal weights, be sure to adjust these counts by proportional weight with the formula: adjusted count for a category = total observations * sum of weights of observations in the category / sum of all weights. If these values are named, those will be included in the output data frame.
#' @param alpha Numeric value. Must be between 0 and 1. The alpha for the confidence calculation, e.g. for 80% confidence, the alpha is 0.2. Defaults to \code{0.2}.
#' @param chisq Character string. This decides which chi squared quantile calculation to use. The accepted values are \code{"A"}, \code{"B"}, or \code{"best"} (use the one which minimizes the confidence intervals). Goodman describes A as his default, calculated as the upper alpha times 100th percentage point of the chi-square distribution with k - 1 degrees of freedom. He also notes the alternative B, calculated as the upper alpha / k times 100th percentage point of the chi-square distribution with one degree of freedom, which will produce tighter intervals when k > 2 and alpha is 0.1, 0.5, or 0.01. Defaults to \code{"best"}
#' @param verbose Logical. If \code{TRUE} then the function will generate additional messages as it executes. Defaults to \code{FALSE}.
goodman_cis <- function(counts,
                        alpha = 0.2,
                        chisq = "best",
                        verbose = FALSE){
  if (!is.numeric(counts) | length(counts < 2)) {
    stop("counts must be a numeric vector with at least two values")
  }

  if (!(chisq %in% c("A", "B", "best"))) {
    stop("The only valid values for chisq are 'A', 'B', and 'best'.")
  }

  # Goodman describes the upper and lower bounds with the equations:
  # Lower estimated pi_i = {A + 2n_i - {A[A + 4n_i(N - n_i) / N]}^0.5} / [2(N + A)]
  # Upper estimated pi_i = {A + 2n_i + {A[A + 4n_i(N - n_i) / N]}^0.5} / [2(N + A)]

  # n_i is the "observed cell frequencies in population of size N" (aka count of observations) from a category
  # so that's the incoming argument counts. We'll rename for consistency with the original math (and statistics as a discipline)
  n <- counts

  # N is the population those counts make up, or, in lay terms, the total observation count
  N <- sum(counts)

  # k is the number of categories the population has been sorted into
  # Useful for degrees of freedom
  k <- length(counts)

  # "A is the upper alpha * 100-th percentage point of the chi-square distribution with k - 1 degrees of freedom"
  # and B is an alternative which uses alpha / k and one degree of freedom
  # Goodman states that B should be less than A for situations
  # where k > 2 AND alpha is 0.1, 0.05, or 0.01.
  chisq_quantiles <- c("A" = stats::qchisq(p = 1 - alpha,
                                           df = k - 1),
                       "B" = stats::qchisq(p = 1 - (alpha / k),
                                           df = 1))


  # According to Goodman, A and B are both valid options for the chi-square quantile
  # So the user can specify which they want or just ask for the one that minimizes the confidence intervals
  chisq_quantile <- switch(chisq,
                           "A" = {chisq_quantiles["A"]},
                           "B" = {chisq_quantiles["B"]},
                           "best" = {
                             pick <- which.min(chisq_quantiles)
                             if (verbose){
                               switch(names(chisq_quantiles)[pick],
                                      "A" = message("The chi-square quantile calculation that will provide the tighter confidence intervals is A, the upper alpha X 100-th percentage point of the chi-square distribution with k - 1 degrees of freedom"),
                                      "B" = message("The chi-square quantile calculation that will provide the tighter confidence intervals is B, the upper alpha / k X 100-th percentage point of the chi-square distribution with 1 degree of freedom"))
                             }
                             chisq_quantiles[pick]
                           })

  # Calculate the bounds!
  # Note that these ARE symmetrical, just not around the proportions.
  # They're symmetrical around A + 2 * n / (2 * (N + A))
  # The variable A has been replaced with chisq_quantile because it may be A or B, depending
  # Since the only multi-value vector involved here is n, these will be vectors of length k,
  # having one value for each of the values in n and in the same order as n
  lower_bounds <- (chisq_quantile + 2 * n - sqrt(chisq_quantile * (chisq_quantile + 4 * n * (N - n) / N))) / (2 * (N + chisq_quantile))
  upper_bounds <- (chisq_quantile + 2 * n + sqrt(chisq_quantile * (chisq_quantile + 4 * n * (N - n) / N))) / (2 * (N + chisq_quantile))

  # A proportion can never be greater than 1 or less than 0 (duh)
  # So we'll add bounds any CIs in case that happens
  # That's definitely a thing that can happen if the magnitude of sqrt(A * (A + 4 * n * (N - n) / N))
  # is large enough
  lower_bounds[lower_bounds < 0] <- 0
  upper_bounds[upper_bounds > 1] <- 1

  # Build the output
  output <- data.frame(count = n,
                       proportion = n / N,
                       lower_bound = lower_bounds,
                       upper_bound = upper_bounds)

  # What are the categories called? If anything, that is
  k_names <- names(n)

  if (!is.null(k_names)) {
    output[["category"]] <- k_names
    output <- output[, c("category", "count", "proportion", "lower_bound", "upper_bound")]
  }

  return(output)
}
