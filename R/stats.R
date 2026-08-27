#### STATISTICS ################################################################
##### Mean ---------------------------------------------------------------------

#' Calculate a weighted mean
#' @export
weighted_mean <- function(values,
                          weights){
  if (!is.numeric(values) | !is.vector(values)) {
    stop("values must be a numeric vector.")
  }

  if (!is.numeric(weights) | !is.vector(weights)) {
    stop("weights must be a numeric vector.")
  }

  if (length(weights) != length(values)) {
    stop("values and weights must be the same length")
  }

  # The weighted mean is the summation of the values multiplied by their weights
  # divided by the sum of the weights.
  sum(values * weights) / sum(weights)
}

##### Standard error -----------------------------------------------------------

#' Calculated weighted standard error
#' @export
weighted_se <- function(values,
                        weights,
                        value_type = "continuous"){
  output <- switch(EXPR = value_type,
         "continuous" = {
           weighted_continuous_se(values,
                                  weights)
         },
         "categorical" = {
           weighted_categorical_se(values,
                                   weights)
         })

  output
}

# Literally just from https://en.wikipedia.org/wiki/Weighted_arithmetic_mean
weighted_continuous_se <- function(values,
                                   weights){
  normalized_weights <- weights / sum(weights)
  variance <- var(x = values)
  sqrt(variance) * sqrt(sum(normalized_weights^2))
}

weighted_categorical_se <- function(values,
                                    weights){
  if (!all(unique(values) %in% c(1, 0))) {
    stop(paste0("Categorical values must all be either 1 for values which correspond to the category being evaluated and 0 for values corresponding to any other categories. The following invalid values are present: ",
                paste(setdiff(x = unique(values),
                              y = c(1, 0)),
                      collapse = ", ")))
  }

  sum_of_weights <- sum(weights)

  sum_of_squared_weights <- sum(weights^2)

  # Because all the values are 1 or 0, the mean is the proportion of values
  weighted_proportion <- weighted_mean(values,
                                       weights)

  effective_sample_size <- sum_of_weights^2 / sum_of_squared_weights

  variance <- weighted_proportion * (1 - weighted_proportion)

  standard_error <- sqrt(variance / effective_sample_size)

  standard_error
}

##### Standard deviation -------------------------------------------------------

#' Calculate a weighted standard deviation
#' @export
weighted_sd <- function(values,
                        weights){
  if (!is.numeric(values) | !is.vector(values)) {
    stop("values must be a numeric vector.")
  }

  if (!is.numeric(weights) | !is.vector(weights)) {
    stop("weights must be a numeric vector.")
  }

  if (!length(weights) %in% c(length(values), 1)) {
    stop("values and weights must be the same length or weights must be a single value.")
  }

  n_nonzero_weights <- sum(!weights %in% c(0))

  # Just to lay it out here narratively:
  # The standard deviation is the square root of a fraction where
  #   The numerator of the fraction is the summation of:
  #     The weights multiplied by the squares of the difference between the
  #     values and the mean of the values
  #   The denominator is the summation of the weights multiplied by the number
  #     of nonzero weights minus 1 and divided by the number of nonzero weights
  sqrt(sum((values - weighted_mean(values = values,
                                   weights = weights))^2) /
         (sum(weights) * (n_nonzero_weights - 1) / n_nonzero_weights))

  # This was previously used but is inappropriate?
  # sqrt(sum(weights * values * values) / sum(weights) - weighted_mean(values = values,
  #                                                                    weights = weights)^2)
}

##### Coefficient of variance --------------------------------------------------

#' Calculate a weighted coefficient of variance
#' @export
weighted_cv <- function(values,
                        weights){
  if (!is.numeric(values) | !is.vector(values)) {
    stop("values must be a numeric vector.")
  }

  if (!is.numeric(weights) | !is.vector(weights)) {
    stop("weights must be a numeric vector.")
  }

  if (!length(weights) %in% c(length(values), 1)) {
    stop("values and weights must be the same length or weights must be a single value.")
  }

  weighted_sd(values = values,
              weights = weights) / weighted_mean(values = values,
                                                 weights = weights)
}

##### Variance -----------------------------------------------------------------

#' Calculate a weighted variance
#' @param values Numeric vector. The values to calculate the weighted variance for.
#' @param weights Numeric vector. The weights for the vector \code{values}. They must be in the same order as \code{values}.
#' @param na_remove Logical. If \code{TRUE} then any data with either a value or weight of \code{NA} will be removed before calculating. Defaults to \code{FALSE}.
#' @export
weighted_variance <- function(values,
                              weights,
                              na_remove = FALSE) {
  # Remove the NAs if asked
  if (na_remove) {
    valid_indices <- !is.na(values) & !is.na(weights)
    values <- values[valid_indices]
    weights <- weights[valid_indices]
  }
  # Get the sum of the weights
  sum_of_weights <- sum(weights)
  # Get the sum of the squares of the weights
  sum_of_squared_weights <- sum(weights^2)

  # Calculate variance!
  # Narratively, this is:
  #   The sum of the weights divided by the difference between the square of
  #     the sum of weights and the sum of the squared weights
  #   then multiplied by the summation of:
  #     the weights multiplied by the squared difference between the values and
  #     the weighted mean of the values
  variance <- (sum_of_weights / (sum_of_weights^2 - sum_of_squared_weights)) * sum(weights * (values - weighted_mean(values,
                                                                                                                     weights))^2,
                                                                                   na.rm = na_remove)
  variance
}

#### CONFIDENCE INTERVALS ######################################################

#' Calculate Goodman's multinomial confidence intervals
#' @description Calculate confidence intervals for multinomial proportions using the method described by Leo Goodman in "On Simultaneous Confidence Intervals for Multinomial Proportions" in Technometrics in 1965. This function can only handle one group of categorical counts at a time, so if you want to calculate confidence intervals for multiple groups, you need to do each separately.
#' @param counts Numeric vector, optionally named. The counts for each of the categories being considered. If there are unequal weights, be sure to adjust these counts by proportional weight with the formula: adjusted count for a category = total observations * sum of weights of observations in the category / sum of all weights. If these values are named, those will be included in the output data frame.
#' @param alpha Numeric value. Must be between 0 and 1. The alpha for the confidence calculation, e.g. for 80 percent confidence, the alpha is 0.2. Defaults to \code{0.2}.
#' @param chisq Character string. This decides which chi squared quantile calculation to use. The accepted values are \code{"A"}, \code{"B"}, or \code{"best"} (use the one which minimizes the confidence intervals). Goodman describes A as his default, calculated as the upper alpha times 100th percentage point of the chi-square distribution with k - 1 degrees of freedom. He also notes the alternative B, calculated as the upper alpha / k times 100th percentage point of the chi-square distribution with one degree of freedom, which will produce tighter intervals when k > 2 and alpha is 0.1, 0.5, or 0.01. Defaults to \code{"best"}
#' @param verbose Logical. If \code{TRUE} then the function will generate additional messages as it executes. Defaults to \code{FALSE}.
#' @export
goodman_cis <- function(counts,
                        alpha = 0.2,
                        chisq = "best",
                        verbose = FALSE){
  if (!is.numeric(counts) | length(counts) < 2) {
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
                       upper_bound = upper_bounds,
                       stringsAsFactors = FALSE,
                       row.names = NULL)

  # What are the categories called? If anything, that is
  k_names <- names(n)

  if (!is.null(k_names)) {
    output[["category"]] <- k_names
    output <- output[, c("category", "count", "proportion", "lower_bound", "upper_bound")]
  }

  return(output)
}

#' Calculate the upper and lower bounds for a mean given and alpha value
#' @param mean Numeric value. The mean to compute bounds for.
#' @param sd Numeric value. The standard deviation of \code{mean}.
#' @param n Numeric value. The number of observations that were used to calculate \code{mean}.
#' @param alpha Numeric value. The alpha value to use to compute the upper and lower confidence bounds for \code{mean}. Defaults to \code{0.05}.
#' @returns A named list containing the upper and lower bounds for the mean for the given confidence.
#' @export
ci_mean <- function(mean,
                    sd,
                    n,
                    alpha) {
  if (!is.numeric(mean)) {
    stop("`mean` must be a numeric value")
  }
  if (!is.numeric(sd)) {
    stop("`sd` must be a numeric value")
  }
  if (!is.numeric(n)) {
    stop("`n` must be a numeric value")
  } else if (n <= 1) {
    stop("`n` must be greater than 1")
  }
  if (!is.numeric(alpha)) {
    stop("`alpha` must be a numeric value")
  } else if (alpha <= 0 | alpha >= 1) {
    stop("`alpha` must be a value between 0 and 1")
  }

  standard_error <- sd / sqrt(n)
  degrees_freedom <- n - 1
  t_score <- qt(p = alpha / 2,
                df = degrees_freedom)
  margin_error <- abs(standard_error * t_score)
  mean_bound_lower <- mean - margin_error
  mean_bound_upper <- mean + margin_error

  list(lower_bound = mean_bound_lower,
       upper_bound = mean_bound_upper)
}

# calculates transformed SD/SE and CIs on the logit scale to avoid CIs going
# outside boundaries
#
# Inputs: mean on real scale, sd on real scale, alpha as confidence level
#' Calcualte (un)transformed CIs
#' @export
ci_delta <- function(mean,
                     stddev = NULL,
                     variance = NULL,
                     transform = "none",
                     alpha = 0.05) {

  valid_transforms <- c("logit", "log", "none")
  if (!transform %in% valid_transforms) {
    stop(paste0("Invalid value provided for transform. Valid values are: '",
                paste(valid_transforms,
                      collapse = "', '"), "'"))
  }

  # Let the user provide SD or variance.
  if (is.null(stddev)) {
    if (is.null(variance)) {
      stop("Provide a value for either stddev or variance")
    }
    stddev <- sqrt(variance)
  }

  # Transform mean (or don't, but we're still calling it "transformed")
  mean_transformed <- switch(EXPR = transform,
                             "logit" = log(mean / (1 - mean)),
                             "log" = log(mean),
                             "none" = mean)

  # Partial derivative of with respect to the untransformed mean
  partial_derivative <- switch(EXPR = transform,
                               "logit" = -1 / (mean * (1 - mean)),
                               "log" = 1 / mean,
                               "none" = 1)

  # Use delta method to calculate SD
  sd_delta <- sqrt(partial_derivative^2 * stddev^2)

  # Calculate CIs
  ci <- mean_transformed + qnorm(c(alpha/2, 1 - alpha/2)) * sd_delta

  # Transform CIs back to real scale (or don't)
  output <- switch(EXPR = transform,
                   "logit" = plogis(ci),
                   "log" = exp(ci),
                   "none" = ci)

  output |>
    unlist(x = _) |>
    setNames(object = _,
             nm = c("lower_bound",
                    "upper_bound"))
}

