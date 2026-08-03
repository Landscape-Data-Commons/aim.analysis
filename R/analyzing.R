# This is literally only here for the dang boostrapping
# special_mean <- function(data, indices) {
#   mean(data[indices],
#        trim = 0)
# }
# analyze_con_bootstrap <- function(data,
#                         weights,
#                         id_var,
#                         value_var,
#                         wgt_var,
#                         conf = 80,
#                         bootstrap_count = 100,
#                         verbose = FALSE){
#   # Make sure everything is the right class/length
#   if (!("data.frame" %in% class(data))) {
#     stop("data must be a data frame")
#   }
#   if (nrow(data) < 1) {
#     stop("There are no values in data")
#   }
#   if (!("data.frame" %in% class(weights))) {
#     stop("weights must be a data frame")
#   }
#   if (nrow(weights) < 1) {
#     stop("There are no values in weights")
#   }
#
#   if (class(id_var) != "character" | length(id_var) != 1) {
#     stop("id_var must be a single character string")
#   }
#   if (class(cat_var) != "character" | length(value_var) != 1) {
#     stop("value_var must be a single character string")
#   }
#   if (class(wgt_var) != "character" | length(wgt_var) != 1) {
#     stop("wgt_var must be a single character string")
#   }
#   if (conf <= 0 | conf >= 100) {
#     stop("conf must be a value between 0 and 100")
#   }
#
#   # Make sure all the variables are in place
#   required_data_vars <- c(id_var,
#                           value_var)
#   missing_data_vars <- required_data_vars[!(required_data_vars %in% names(data))]
#   if (length(missing_data_vars) > 0) {
#     stop("The following variables are missing from data: ",
#          paste(missing_data_vars,
#                collapse = , ", "))
#   }
#
#   # Just want the bare minimum here.
#   data <- dplyr::select(.data = data,
#                         tidyselect::all_of(required_data_vars))
#
#   # Check to make sure the unique identifiers are, in fact, unique
#   non_unique_ids <- any(table(data[[id_var]]) > 1)
#   if (non_unique_ids) {
#     stop("There are non-unique values in ", id_var, " in data.")
#   }
#
#   required_weights_vars <- c(id_var,
#                              wgt_var)
#   missing_weights_vars <- required_weights_vars[!(required_weights_vars %in% names(weights))]
#   if (length(missing_weights_vars) > 0) {
#     stop("The following variables are missing from weights: ",
#          paste(missing_weights_vars,
#                collapse = , ", "))
#   }
#
#   non_unique_ids <- any(table(weights[[id_var]]) > 1)
#   if (non_unique_ids) {
#     stop("There are non-unique values in ", id_var, " in weights.")
#   }
#
#   # Paring this down too.
#   weights <- dplyr::select(.data = weights,
#                            tidyselect::all_of(required_weights_vars))
#
#   if (!all(data[[id_var]] %in% weights[[id_var]])) {
#     warning("Not all data have corresponding weights. They will be dropped from the calculations.")
#   }
#   if (!all(weights[[id_var]] %in% data[[id_var]])) {
#     warning("Not all weights have corresponding data. Depending on your situation, this may be expected or may be indicative of an issue with the unique IDs.")
#   }
#
#   data <- dplyr::inner_join(x = data,
#                             y = weights,
#                             by = id_var,
#                             relationship = "one-to-one")
#   data <- dplyr::mutate(.data = data,
#                         weighted_value = value_var * wgt_var / sum(data[[wgt_var]]))
#
# estimate <- mean(data$weighted_value)
#
# bootstrap_results <- boot::boot(data = data$weighted_value,
#                                 statistic = special_mean,
#                                 R = bootstrap_count)
#
# if (length(unique(bootstrap_results$t)) == 1) {
#   data.frame(mean = estimate,
#              alpha = 1 - conf / 100,
#              n = nrow(data),
#              booststrap_replicates = bootstrap_count,
#              mean_bootstrap = bootstrap_results$t[1],
#              lower_bound = bootstrap_results$t[1],
#              upper_bound = bootstrap_results$t[1])
# } else {
#   bootstrap_cis <- boot::boot.ci(boot.out = bootstrap_results,
#                                  conf = conf / 100,
#                                  type = "basic")
#   data.frame(mean = estimate,
#              alpha = 1 - conf / 100,
#              n = nrow(data),
#              booststrap_replicates = bootstrap_count,
#              mean_bootstrap = bootstrap_results$t[1],
#              lower_bound = bootstrap_cis$basic[1, 4],
#              upper_bound = bootstrap_cis$basic[1, 5])
# }
# }

#' Estimation of weighted means of continuous data
#' @description Given continuous data and the weights for the individual observations, calculate estimated mean and confidence intervals.
#' @param data Data frame. Continuous data (e.g., numeric) with the unique identifiers for each observation/row in the variable \code{id_var} and the value for each observation/row in \code{value_var}. Note that the unique identifiers are the link between \code{data} and \code{weights}
#' @param weights Data frame. This must contain the weighting information using the variables \code{id_var} with a unique identifier for each observation/row and \code{wgt_var} with the relative numeric weight of each observation/row.
#' @param id_var Character string. The name of the variable in \code{data} and \code{weights} that contains the unique identifiers for the observations. All values in \code{data$id_var} must appear in \code{weights$id_var}.
#' @param value_var Character string. The name of the variable in \code{data} that contains the values as character strings.
#' @param wgt_var Character string. The name of the variable in \code{weights} that contains the numeric weight values.
#' @param conf Numeric. The confidence level in percent. Defaults to \code{80}.
#' @param verbose Logical. If \code{TRUE} then the function will generate additional messages as it executes. Defaults to \code{FALSE}.
#' @return A data frame containing the count of observations, weighted mean, and confidence intervals.
#' @export
analyze_con <- function(data,
                        weights,
                        id_var,
                        value_var,
                        wgt_var,
                        conf = 80,
                        verbose = FALSE){
  # Make sure everything is the right class/length
  if (!("data.frame" %in% class(data))) {
    stop("data must be a data frame")
  }
  if (nrow(data) < 1) {
    stop("There are no values in data")
  }
  if (!("data.frame" %in% class(weights))) {
    stop("weights must be a data frame")
  }
  if (nrow(weights) < 1) {
    stop("There are no values in weights")
  }

  if (class(id_var) != "character" | length(id_var) != 1) {
    stop("id_var must be a single character string")
  }
  if (class(value_var) != "character" | length(value_var) != 1) {
    stop("value_var must be a single character string")
  }
  if (class(wgt_var) != "character" | length(wgt_var) != 1) {
    stop("wgt_var must be a single character string")
  }
  if (conf <= 0 | conf >= 100) {
    stop("conf must be a value between 0 and 100")
  } else {
    alpha <- 1 - conf / 100
  }

  # Make sure all the variables are in place
  required_data_vars <- c(id_var,
                          value_var)
  missing_data_vars <- required_data_vars[!(required_data_vars %in% names(data))]
  if (length(missing_data_vars) > 0) {
    stop("The following variables are missing from data: ",
         paste(missing_data_vars,
               collapse = , ", "))
  }

  # Just want the bare minimum here.
  data <- dplyr::select(.data = data,
                        tidyselect::all_of(required_data_vars)) |>
    dplyr::rename(.data = _,
                  setNames(object = required_data_vars,
                           nm = c("id", "value")))

  # Check to make sure the unique identifiers are, in fact, unique
  non_unique_ids <- any(table(data[[id_var]]) > 1)
  if (non_unique_ids) {
    stop("There are non-unique values in ", id_var, " in data.")
  }

  required_weights_vars <- c(id_var,
                             wgt_var)
  missing_weights_vars <- required_weights_vars[!(required_weights_vars %in% names(weights))]
  if (length(missing_weights_vars) > 0) {
    stop("The following variables are missing from weights: ",
         paste(missing_weights_vars,
               collapse = , ", "))
  }

  non_unique_ids <- any(table(weights[[id_var]]) > 1)
  if (non_unique_ids) {
    stop("There are non-unique values in ", id_var, " in weights.")
  }

  # Paring this down too.
  weights <- dplyr::select(.data = weights,
                           tidyselect::all_of(required_weights_vars)) |>
    dplyr::rename(.data = _,
                  setNames(object = required_weights_vars,
                           nm = c("id", "weight"))) |>
    tidyr::drop_na()

  if (!all(data[[id_var]] %in% weights[[id_var]])) {
    warning("Not all data have corresponding weights. They will be dropped from the calculations.")
  }
  if (!all(weights[[id_var]] %in% data[[id_var]])) {
    warning("Not all weights have corresponding data. Depending on your situation, this may be expected or may be indicative of an issue with the unique IDs.")
  }

  data <- dplyr::inner_join(x = data,
                            y = weights,
                            by = "id",
                            relationship = "one-to-one") |>
    dplyr::mutate(.data = _,
                  weighted_value = value * weight / sum(weights$weight))

  n <- nrow(data)
  # Weighted mean is the sum of the weight-adjusted values divided by the sum of all weights
  mean_weighted <- sum(data$weighted_value)
  # Standard deviation is calculated differently for weighted values than unweighted
  sd_weighted <- sqrt(sum(data$weight * (data$value - mean(data$value))^2) / ((n - 1) / n * sum(data$weight)))
  # So is variance
  variance_weighted <- weighted_variance(values = data$value,
                                         weights = data$weight,
                                         na_remove = FALSE)
  bounds_weighted <- ci_mean(mean = mean_weighted,
                             sd = sd_weighted,
                             n = n,
                             alpha = alpha)

  data.frame(n = n,
             alpha = alpha,
             mean = mean_weighted,
             sd = sd_weighted,
             std_error = sd_weighted / sqrt(n),
             cv = sd_weighted / mean_weighted,
             variance = variance_weighted) |>
    dplyr::bind_cols(.x = _,
                     bounds_weighted)
}

#' Estimation of weighted proportions of categorical data
#' @description Given categorical data and the weights for the individual observations, calculate estimated proportions by category and Goodman's multinomial confidence intervals.
#' @param data Data frame. Categorical data with the unique identifiers for each observation/row in the variable \code{id_var} and the assigned category for each observation/row in \code{cat_var}. Note that the unique identifiers are the link between \code{data} and \code{weights}
#' @param weights Data frame. This must contain the weighting information using the variables \code{id_var} with a unique identifier for each observation/row and \code{wgt_var} with the relative numeric weight of each observation/row.
#' @param id_var Character string. The name of the variable in \code{data} and \code{weights} that contains the unique identifiers for the observations. All values in \code{data$id_var} must appear in \code{weights$id_var}.
#' @param cat_var Character string. The name of the variable in \code{data} that contains the category values as character strings.
#' @param wgt_var Character string. The name of the variable in \code{weights} that contains the numeric weight values.
#' @param definitions Conditionally optional character vector. The possible categories that the observation could've been classed into. This is NOT optional if there are categories that do not appear in \code{data} because no observations met their criteria because those categories must be included in the calculations. Must contain at least the values in \code{code$cat_var} but should include ALL possible categories.
#' @param conf Numeric. The confidence level in percent. Defaults to \code{80}.
#' @param verbose Logical. If \code{TRUE} then the function will generate additional messages as it executes. Defaults to \code{FALSE}.
#' @return A data frame containing the categories, counts of observations, weighted estimated proportions, and confidence intervals.
#' @export
analyze_cat <- function(data,
                        weights,
                        id_var,
                        cat_var,
                        wgt_var,
                        definitions = NULL,
                        conf = 80,
                        verbose = FALSE){
  # Make sure everything is the right class/length
  if (!("data.frame" %in% class(data))) {
    stop("data must be a data frame")
  }
  if (nrow(data) < 1) {
    stop("There are no values in data")
  }
  if (!("data.frame" %in% class(weights))) {
    stop("weights must be a data frame")
  }
  if (nrow(weights) < 1) {
    stop("There are no values in weights")
  }

  if (class(id_var) != "character" | length(id_var) != 1) {
    stop("id_var must be a single character string")
  }
  if (class(cat_var) != "character" | length(cat_var) != 1) {
    stop("cat_var must be a single character string")
  }
  if (class(wgt_var) != "character" | length(wgt_var) != 1) {
    stop("wgt_var must be a single character string")
  }
  if (conf <= 0 | conf >= 100) {
    stop("conf must be a value between 0 and 100")
  }

  # Make sure all the variables are in place
  required_data_vars <- c(id_var,
                          cat_var)
  missing_data_vars <- required_data_vars[!(required_data_vars %in% names(data))]
  if (length(missing_data_vars) > 0) {
    stop("The following variables are missing from data: ", paste(missing_data_vars, collapse = , ", "))
  }
  data <- data[, required_data_vars]
  category_class <- class(data[[cat_var]])
  # What categories were observed?
  present_categories <- setNames(object = unique(data[[cat_var]]),
                                 nm = unique(data[[cat_var]]))

  if (!is.null(definitions)) {
    if (!(category_class %in% class(definitions))) {
      stop("definitions must be the same class as the category values in data")
    }
    if (length(definitions) < 1) {
      stop("There are no values in definitions")
    }
  }

  # # Check to make sure the unique identifiers are, in fact, unique
  # non_unique_ids <- any(table(data[[id_var]]) > 1)
  # if (any(non_unique_ids)) {
  #   stop("There are non-unique values in ", id_var, " in data.")
  # }


  required_weights_vars <- c(id_var, wgt_var)
  missing_weights_vars <- required_weights_vars[!(required_weights_vars %in% names(weights))]
  if (length(missing_weights_vars) > 0) {
    stop("The following variables are missing from weights: ", paste(missing_weights_vars, collapse = , ", "))
  }
  non_unique_ids <- any(table(weights[[id_var]]) > 1)
  if (non_unique_ids) {
    stop("There are non-unique values in ", id_var, " in weights.")
  }
  weights <- weights[, required_weights_vars] |>
    tidyr::drop_na()

  # And what if the user provided definitions?
  # This is important for if there are categories that have no data that qualified!
  if (!is.null(definitions)) {
    missing_categories <- !(present_categories %in% definitions)
    if (any(missing_categories)) {
      stop("The following categories appear in data but not in categories: ",
           paste(present_categories[missing_categories], collapse = ", "))
    }
  }

  # Make sure the IDs line up
  data_ids_in_weights_indices <- data[[id_var]] %in% weights[[id_var]]
  if (!all(data_ids_in_weights_indices)) {
    stop("Not all unique IDs in data appear in weights")
  }
  weight_ids_in_weights_indices <- weights[[id_var]] %in% data[[id_var]]
  if (verbose & !all(weight_ids_in_weights_indices)) {
    message("Not all unique IDs in weights appear in data, just so you know.")
  }
  weights <- weights[weight_ids_in_weights_indices, ]


  # Get each observation with just its category and weight
  weighted_categories <- merge(x = data[, c(id_var, cat_var)],
                               y = weights,
                               by = id_var,
                               all.y = FALSE)

  # Calculate the sum of the weights for each of the observed categories
  category_weight_sums <- sapply(X = present_categories,
                                 data = weighted_categories,
                                 cat_var = cat_var,
                                 wgt_var = wgt_var,
                                 USE.NAMES = TRUE,
                                 FUN = function(X,
                                                data,
                                                cat_var,
                                                wgt_var){
                                   relevant_indices <- data[[cat_var]] == X
                                   current_weights <- data[relevant_indices, wgt_var]
                                   weight_sum <- sum(as.numeric(current_weights))
                                   return(weight_sum)
                                 })
  # Calculate the weighted proportions for each category
  category_weighted_proportions <- category_weight_sums / sum(category_weight_sums)
  # Get the pure counts of the categories
  category_counts <- table(weighted_categories[[cat_var]])
  # And the total number of observations. This should be the same as nrow(weighted_categories)
  total_observations <- sum(category_counts)
  # Using the total number of observations and the weighted proportions to calculate "adjusted counts"
  adjusted_counts <- (category_weighted_proportions * total_observations) |>
    setNames(object = _,
             nm = present_categories)

  # Here's a tricky bit! Calculating weighted standard error, which should be done
  # for each category as well
  category_weighted_se <- sapply(X = present_categories,
                                 data = weighted_categories,
                                 cat_var = cat_var,
                                 wgt_var = wgt_var,
                                 USE.NAMES = TRUE,
                                 FUN = function(X,
                                                data,
                                                cat_var,
                                                wgt_var){
                                   # For each category, we're going to treat that
                                   # category's records as 1 and the others as 0
                                   weighted_se(values = as.numeric(data[[cat_var]] %in% X),
                                                    weights = data[[wgt_var]])
                                 })
  category_weighted_cv <- sapply(X = present_categories,
                                 data = weighted_categories,
                                 cat_var = cat_var,
                                 wgt_var = wgt_var,
                                 USE.NAMES = TRUE,
                                 FUN = function(X,
                                                data,
                                                cat_var,
                                                wgt_var){
                                   # For each category, we're going to treat that
                                   # category's records as 1 and the others as 0
                                   weighted_cv(values = as.numeric(data[[cat_var]] %in% X),
                                                            weights = data[[wgt_var]])
                                 })

  category_weighted_variance <- sapply(X = present_categories,
                                       data = weighted_categories,
                                       cat_var = cat_var,
                                       wgt_var = wgt_var,
                                       USE.NAMES = TRUE,
                                       FUN = function(X,
                                                      data,
                                                      cat_var,
                                                      wgt_var){
                                         weighted_variance(values = as.numeric(data[[cat_var]] %in% X),
                                                           weights = data[[wgt_var]],
                                                           na_remove = FALSE)
                                       })

  # Okay, so if we have definitions to catch categories with zero observations, add those
  # Because it should matter for calculating confidence intervals
  if (!is.null(definitions)) {
    defined_categories <- definitions
    missing_categories <- defined_categories[!(defined_categories %in% present_categories)]
    # Looping because it's easy, not because it's the best solution
    # But we want to populate the 0s for all of these!
    for (category in missing_categories) {
      category_weighted_proportions[[category]] <- 0
      category_weight_sums[[category]] <- 0
      category_counts[[category]] <- 0
      adjusted_counts[[category]] <- 0
    }
  }

  # Finally ready to calculate confidence intervals!
  # But first we need the alpha value for our confidence level
  alpha <- 1 - (conf / 100)

  confidence_intervals <- goodman_cis(counts = adjusted_counts,
                                      alpha = alpha,
                                      chisq = "best",
                                      verbose = verbose)
  confidence_interval_vars <- c("category", "weighted_observation_count", "weighted_observation_proportion",
                                paste0(c("weighted_observation_proportion_lower_bound", "weighted_observation_proportion_upper_bound"),
                                       "_", conf, "pct"))
  names(confidence_intervals) <- confidence_interval_vars

  # And now it's a matter of combining and formatting
  # Yeah, yeah, yeah. It's not """best practice""" to calculate within the data frame construction
  # but I don't care. I'll do math and slicing wherever I want to. Deal with it.
  results <- data.frame(category = names(category_counts),
                        observation_count = as.vector(category_counts[names(category_counts)]),
                        observation_proportion = as.vector(category_counts[names(category_counts)] / total_observations),
                        total_observation_weight = category_weight_sums[names(category_counts)],
                        weighted_observation_proportion = category_weighted_proportions[names(category_counts)],
                        weighted_standard_error = category_weighted_se[names(category_counts)],
                        weighted_coefficient_of_variance = category_weighted_cv[names(category_counts)],
                        weighted_variance = category_weighted_variance[names(category_counts)],
                        row.names = NULL,
                        stringsAsFactors = FALSE)

  confidence_interval_keep_vars <- c("category",
                                     paste0(c("weighted_observation_proportion_lower_bound", "weighted_observation_proportion_upper_bound"),
                                            "_", conf, "pct"))

  # Combine the results and confidence intervals
  output <- merge(x = results,
                  y = confidence_intervals[, confidence_interval_keep_vars],
                  by = c("category"))

  # Get the variables restricted to what we care about and ordered properly
  output_vars <- c("category", "observation_count", "observation_proportion", "total_observation_weight", "weighted_observation_proportion", "weighted_standard_error", "weighted_coefficient_of_variance", "weighted_variance",
                   paste0(c("weighted_observation_proportion_lower_bound", "weighted_observation_proportion_upper_bound"),
                          "_", conf, "pct"))

  output <- output[, output_vars]

  return(output)
}


#' Estimation of weighted proportions of multiple subsets of categorical data
#' @description Given categorical data, subsetting information, and the weights for the individual observations, calculate estimated proportions by category and Goodman's multinomial confidence intervals for each subset. This can be done with data without subsetting by not providing values for \code{split_vars}. An example of using \code{split_vars} would be if the data ratings of indicators where the indicators each need to be estimated separately and the indicator information  is stored in \code{data$indicator} in which case you would use \code{split_var = "indicator"}. If indicators appear more than once with different ratings because there were different criteria for different objectives and the objective was stored in \code{data$objective} then you would use \code{split_vars = c("indicator", "objective")}.
#' @param data Data frame. Categorical data with the unique identifiers for each observation/row in the variable \code{id_var} and the assigned category for each observation/row in \code{cat_var}. If the data are being subset by unique combinations of values in one or more additional variables, those variables must be specified in \code{split_vars}. Note that the unique identifiers do not have to be unique for the whole of \code{data} so long as they are unique within each subset of \code{data}.
#' @param weights Data frame. This must contain the weighting information using the variables \code{id_var} with a unique identifier for each observation/row and \code{wgt_var} with the relative numeric weight of each observation/row.
#' @param id_var Character string. The name of the variable in \code{data} and \code{weights} that contains the unique identifiers for the observations. The values in this variable must be unique within subsets by \code{split_vars} or simply unique if \code{split_vars = NULL}.
#' @param cat_var Character string. The name of the variable in \code{data} and (if being used) \code{definitions} that contains the category values.
#' @param wgt_var Character string. The name of the variable in \code{weights} that contains the numeric weight values.
#' @param split_vars Optional character vector. One or more character strings corresponding to variable names in \code{data} and (if being used) \code{definitions}. The data will be subset for the calculations by unique combinations of values in these variables. Each subset must have only unique values in the variable \code{id_var}. If \code{NULL} then no subsetting will take place. Defaults to \code{NULL}.
#' @param definitions Optional data frame. The possible categories for the observations to be classed into, which may include categories that do not appear in \code{data} because no observations met their criteria. Must contain at least the variable \code{cat_var} with ALL possible categories. If \code{split_vars != NULL} then it must also contain all variables in \code{split_vars} and will be subset in the same way as \code{data}, in which case each subset must contain ALL possible categories for that subset.
#' @param conf Numeric. The confidence level in percent. Defaults to \code{80}.
#' @param verbose Logical. If \code{TRUE} then the function will generate additional messages as it executes. Defaults to \code{FALSE}.
#' @return A data frame containing the categories, counts of observations, weighted estimated proportions, and confidence intervals. If subset using \code{split_vars} then all those variables will be included and the estimates will be per unique combination of values within those variables.
#' @export
analyze_cat_multi <- function(data,
                              weights,
                              id_var,
                              cat_var,
                              wgt_var,
                              split_vars = NULL,
                              definitions = NULL,
                              conf = 80,
                              verbose = FALSE){
  # Make sure everything is the right class/length
  if (!("data.frame" %in% class(data))) {
    stop("data must be a data frame")
  }
  if (nrow(data) < 1) {
    stop("There are no values in data")
  }
  if (!("data.frame" %in% class(weights))) {
    stop("weights must be a data frame")
  }
  if (nrow(weights) < 1) {
    stop("There are no values in weights")
  }
  if (!is.null(definitions)) {
    if (!("data.frame" %in% class(definitions))) {
      stop("definitions must be a data frame")
    }
    if (nrow(definitions) < 1) {
      stop("There are no values in definitions")
    }
  }

  if (class(id_var) != "character" | length(id_var) != 1) {
    stop("id_var must be a single character string")
  }
  if (class(cat_var) != "character" | length(cat_var) != 1) {
    stop("cat_var must be a single character string")
  }
  if (class(wgt_var) != "character" | length(wgt_var) != 1) {
    stop("wgt_var must be a single character string")
  }
  if (conf <= 0 | conf >= 100) {
    stop("conf must be a value between 0 and 100")
  }

  if (!is.null(split_vars)) {
    if (class(split_vars) != "character" | length(split_vars) < 1) {
      stop("split_vars must be a vector of one or more character strings")
    }
  }

  # Make sure all the variables are in place
  required_data_vars <- c(id_var, cat_var, split_vars)
  missing_data_vars <- required_data_vars[!(required_data_vars %in% names(data))]
  if (length(missing_data_vars) > 0) {
    stop("The following variables are missing from data: ", paste(missing_data_vars, collapse = , ", "))
  }
  data <- data[, required_data_vars]
  # Split if necessary!
  if (is.null(split_vars)) {
    data_list <- list("only" = data)
  } else {
    data_list <- split(x = data,
                       f = data[, split_vars],
                       drop = TRUE)
  }
  # This will either be "only" if there are no split variables
  # or all the unique combinations that occur
  list_names <- names(data_list)
  # Check to make sure the unique identifiers are, in fact, unique
  non_unique_ids <- sapply(X = data_list,
                           id_var = id_var,
                           FUN = function(X, id_var){
                             counts <- table(X[[id_var]])
                             any(counts > 1)
                           })
  non_unique_ids_subsets <- list_names[non_unique_ids]
  if (any(non_unique_ids)) {
    if (is.null(split_vars)) {
      stop("There are non-unique values in ", id_var, " in data. Did you intend to subset your data with split_vars?")
    } else {
      stop("There are non-unique values in ", id_var, " in data the following unique combinations of values in ",
           paste(split_vars, collapse = ", "), ": ",
           paste(non_unique_ids_subsets, collapse = ", "))
    }
  }

  required_weights_vars <- c(id_var, wgt_var)
  missing_weights_vars <- required_weights_vars[!(required_weights_vars %in% names(weights))]
  if (length(missing_weights_vars) > 0) {
    stop("The following variables are missing from weights: ", paste(missing_weights_vars, collapse = , ", "))
  }
  non_unique_ids <- any(table(weights[[id_var]]) > 1)
  if (non_unique_ids) {
    stop("There are non-unique values in ", id_var, " in weights.")
  }
  weights <- weights[, required_weights_vars]

  # And what if the user provided definitions?
  # This is important for if there are categories that have no data that qualified!
  if (is.null(definitions)) {
    definitions_list <- list("only" = NULL)
  } else {
    required_definitions_vars <- c(cat_var, split_vars)
    missing_definitions_vars <- required_definitions_vars[!(required_definitions_vars %in% names(definitions))]
    if (length(missing_definitions_vars) > 0) {
      stop("The following variables are missing from definitions: ", paste(missing_definitions_vars, collapse = , ", "))
    }
    definitions <- definitions[, required_definitions_vars]

    if (is.null(split_vars)) {
      # Check for missing categories
      missing_categories <- data[[cat_var]][!(data[[cat_var]] %in% definitions[[cat_var]])]
      if (length(missing_categories) > 0) {
        stop("The following categories appear in data but not definitions: ",
             paste(missing_categories, collapse = ", "))
      }
      # We won't be splitting, but we will be putting it in a list for ease, I guess
      definitions_list <- list("only" = definitions)
    } else {
      # Make sure that the split values line up!
      # It's important that all the values from data appear in definitions
      # but not necessarily the other way around
      data_splitvars_in_def <- sapply(X = split_vars,
                                      data = data,
                                      definitions = definitions,
                                      FUN = function(X, data, definitions){
                                        all(data[[X]] %in% definitions[[X]])
                                      },
                                      USE.NAMES = TRUE)
      if (!all(data_splitvars_in_def)) {
        splitvars_missing_values <- names(data_splitvars_in_def)[!data_splitvars_in_def]
        stop("data has values in the following variables which do not occur in the same variables in definitions: ",
             paste(splitvars_missing_values, collapse = ", "))
      }
      def_splitvars_in_data <- sapply(X = split_vars,
                                      data = data,
                                      definitions = definitions,
                                      FUN = function(X, data, definitions){
                                        all(definitions[[X]] %in% data[[X]])
                                      },
                                      USE.NAMES = TRUE)
      if (verbose & !all(def_splitvars_in_data)) {
        splitvars_missing_values <- names(def_splitvars_in_data)[!def_splitvars_in_data]
        message("Just so you know, definitions has values in the following variables which do not occur in the same variables in data: ",
                paste(splitvars_missing_values, collapse = ", "))
      }
      # At this point, we know that it's safe to split definitions
      definitions_list <- split(definitions, definitions[, split_vars],
                                drop = TRUE)
      # And to restrict them (which also orders things for us later)
      definitions_list <- definitions_list[list_names]
    }


    # Okay, so do all of the categories from data show up in definitions?
    # This has to be yes!
    # But it's fine (and in fact the whole point) if the other way around isn't true
    # so I'm not even bothering to test that
    missing_categories <- sapply(X = list_names,
                                 data_list = data_list,
                                 definitions_list = definitions_list,
                                 cat_var = cat_var,
                                 FUN = function(X, data_list, definitions_list, cat_var){
                                   current_data <- data_list[[X]]
                                   data_cats <- current_data[[cat_var]]
                                   current_definitions <- definitions_list[[X]]
                                   def_cats <- current_definitions[[cat_var]]
                                   !all(data_cats %in% def_cats)
                                 },
                                 USE.NAMES = TRUE)
    if (any(missing_categories)) {
      stop("For the following unique combinations of values in ",
           paste(split_vars, collapse = ", "),
           "there are categories which occur in data but not in definitions: ",
           paste(names(missing_categories)[missing_categories], collapse = ", "))
    }
  }

  # Make sure the IDs line up
  data_ids_in_weights_indices <- data[[id_var]] %in% weights[[id_var]]
  if (!all(data_ids_in_weights_indices)) {
    stop("Not all unique IDs in data appear in weights")
  }
  weight_ids_in_weights_indices <- weights[[id_var]] %in% data[[id_var]]
  if (verbose & !all(weight_ids_in_weights_indices)) {
    message("Not all unique IDs in weights appear in data, just so you know.")
  }
  weights <- weights[weight_ids_in_weights_indices, ]


  # And now, finally, we can do the calculations!
  results_list <- lapply(X = list_names,
                         data_list = data_list,
                         definitions_list = definitions_list,
                         weights = weights,
                         id_var = id_var,
                         cat_var = cat_var,
                         wgt_var = wgt_var,
                         split_vars = split_vars,
                         conf = conf,
                         verbose = verbose,
                         FUN = function(X,
                                        data_list,
                                        definitions_list,
                                        weights,
                                        id_var,
                                        cat_var,
                                        wgt_var,
                                        split_vars,
                                        conf,
                                        verbose){
                           # Get the data frame for this subset
                           data <- data_list[[X]]
                           definitions <- definitions_list[[X]][[cat_var]]

                           results <- analyze_cat(data = data,
                                                  weights = weights,
                                                  id_var = id_var,
                                                  cat_var = cat_var,
                                                  wgt_var = wgt_var,
                                                  definitions = definitions,
                                                  conf = conf,
                                                  verbose = verbose)

                           # Add in the splitting vars if there are any
                           # I refuse to be ashamed of looping here
                           for (var in split_vars) {
                             var_value <- data[[var]][1]
                             results[[var]] <- var_value
                           }

                           return(results)
                         })

  # OKAY. So all those are analyzed and stuff. Time to combine everything into a
  # single output and return it.
  dplyr::bind_rows(results_list)
}

#' Weighted analyses
#' @export
analyze_weighted <- function(data,
                             weights = NULL,
                             id_vars,
                             indicator_type,
                             indicator_var = "indicator",
                             value_var = "value",
                             possible_categorical_values = NULL,
                             # continuous_transformation = c("logit",
                             #                               "log"),
                             # cat_estimate_type = "proportion",
                             conf = 80,
                             combine = c("none",
                                         "delta",
                                         "bootstrap",
                                         "mean",
                                         "mean_weights"),
                             bootstrap_replicates = 10000,
                             bootstrap_type = "bca",
                             verbose = FALSE){
  valid_combine_values <- c("none",
                            "bootstrap",
                            "delta",
                            "mean",
                            "mean_weights")
  combine <- intersect(unique(combine),
                       valid_combine_values)
  if (length(combine) < 1) {
    stop(paste0("combine must be one or more of the following: '", paste(valid_combine_values,
                                                                         collapse = "', '"), "'"))
  }

  # valid_cat_estimate_types <- c("proportion",
  #                               "percent")
  # cat_estimate_type <- intersect(unique(cat_estimate_type),
  #                                valid_cat_estimate_types)
  # if (length(cat_estimate_type) < 1) {
  #   stop(paste0("cat_estimate_type must be one of the following: '", paste(valid_cat_estimate_types,
  #                                                                          collapse = "', '"), "'"))
  # }

  if (any(is.na(data[[value_var]]))) {
    warning(paste0("There are ", sum(is.na(data[[value_var]])), " records NA in the variable '", value_var, "'. These will be dropped."))
    data <- data[!is.na(data[[value_var]]), ]
  }

  if (is.character(weights) & length(weights) == 1) {
    weights <- tidyr::unite(data = data,
                            col = "internal_uid_var",
                            tidyselect::all_of(id_vars)) |>
      dplyr::select(.data = _,
                    tidyselect::all_of(x = c("internal_uid_var")),
                    weight = tidyselect::all_of(x = weights)) |>
      dplyr::distinct() |>
      list(.x = _)
  } else if (is.data.frame(weights)) {
    weights <- tidyr::unite(data = weights,
                            col = "internal_uid_var",
                            tidyselect::all_of(id_vars)) |>
      dplyr::select(.data = _,
                    tidyselect::all_of(x = c("internal_uid_var",
                                             "weight"))) |>
      dplyr::distinct() |>
      list(.x = _)
  } else if ("list" %in% class(weights)) {
    weights <- lapply(X = weights,
                      FUN = function(X){
                        sf::st_drop_geometry(X) |>
                          tidyr::unite(data = _,
                                       col = "internal_uid_var",
                                       tidyselect::all_of(id_vars)) |>
                          dplyr::select(.data = _,
                                        tidyselect::all_of(x = c("internal_uid_var",
                                                                 "weight"))) |>
                          dplyr::distinct()
                      })
  }

  # If weights didn't qualify for any of the conversions above, it must be an
  # illegal format, so we'll tell the user.
  if (!("list" %in% class(weights))) {
    stop("weights must be a data frame with the id_vars variables and a 'weight' variable; the name of the variable in data containing the weights; or a list of data frames each with the id_vars variables and a 'weight' variable.")
  }

  weights <- lapply(X = weights,
                    FUN = tidyr::drop_na)

  if (length(weights) > 1) {
    all_weights_identical <- purrr::reduce(.x = weights,
                                           .f = identical)
  }

  data <- tidyr::unite(data = data,
                       col = "internal_uid_var",
                       tidyselect::all_of(id_vars),
                       remove = FALSE) |>
    dplyr::select(.data = _,
                  tidyselect::all_of(x = c("internal_uid_var",
                                           id_vars,
                                           indicator = indicator_var,
                                           value = value_var))) |>
    dplyr::filter(.data = _,
                  !is.na(value)) |>
    dplyr::distinct()



  if (length(indicator_type) == 1) {
    if ("character" %in% class(indicator_type)) {
      indicator_type <- intersect(x = indicator_type,
                                  y = c("continuous",
                                        "categorical"))
      if (length(indicator_type) != 1) {
        stop("When providing a single indicator_type value, it must be either 'categorical' or 'continuous'")
      }

      indicator_type <- rep(x = indicator_type,
                            times = length(unique(data[["indicator"]]))) |>
        setNames(object = _,
                 nm = unique(data[["indicator"]]))
    } else {
      stop("When providing a single indicator_type value, it must be either 'categorical' or 'continuous'")
    }
  } else {
    if ("list" %in% class(indicator_type)) {
      if (!all(names(indicator_type) %in% c("continuous",
                                            "categorical"))) {
        stop("When indicator_type is a list, it must contain vectors of values corresponding to values in the indicator_var variable of data, each of the vectors named 'continuous' or 'categorical' according to the kind of indicators in the vector.")
      }
      indicator_type <- lapply(X = seq_len(length.out = length(indicator_type)),
                               indicator_type = indicator_type,
                               FUN = function(X, indicator_type){
                                 rep(x = names(indicator_type[X]),
                                     times = length(indicator_type[[X]])) |>
                                   setNames(object = _,
                                            nm = unique(indicator_type[[X]]))
                               }) |>
        unlist()
    } else if (is.character(indicator_type)) {
      if (!all(indicator_type %in% c("continuous",
                                     "categorical"))) {
        stop("When indicator_type is a vector, it must be a named vector containing only 'categorical' or 'continuous' with the names corresponding to values in the indicator_var in data.")
      }
      indicator_type <- rep(x = indicator_type,
                            times = length(unique(data[["indicator"]]))) |>
        setNames(object = _,
                 nm = unique(data[["indicator"]]))
    }
  }

  # SUPPORT FOR MULTIPLE possible_categorical_values VALUES
  # Just in case someone is trying to jam a bunch through that have different
  # categories like "Meeting" and "Not Meeting" for some but "Suitable",
  # "Marginal", and "Unsuitable" for others. What a pain.
  # This makes sure that possible_categorical_values ends up as a named list
  # where the names are the categorical indicators and the vectors stored at the
  # indices are the possible categories for those indicators.

  # This any() looks like overkill, but at this point in the process
  # indicator_type should be a vector with a value for every indicator
  # represented in the data regardless of the format the user provided it in,
  # so it's actually rational.
  if (any(indicator_type %in% "categorical")) {
    if ("list" %in% class(possible_categorical_values)) {
      if (!all(names(possible_categorical_values) %in% names(indicator_type)[indicator_type %in% c("categorical")])) {
        stop("When providing a list as possible_categorical_values, all categorical indicators must be represented as a vector in the list. Not all categorical indicators appear in names(possible_categorical_values).")
      }
    } else if (is.vector(possible_categorical_values)) {
      if (verbose) {
        message("possible_categorical_values appears to be a vector of categories. Applying these to all categorical analyses.")
      }
      possible_categorical_values <- lapply(X = setNames(object = names(indicator_type)[indicator_type %in% c("categorical")],
                                                         nm = names(indicator_type)[indicator_type %in% c("categorical")]),
                                            possible_categorical_values = possible_categorical_values,
                                            FUN = function(X, possible_categorical_values){
                                              possible_categorical_values
                                            })
    } else if (is.null(possible_categorical_values)) {
      if (verbose) {
        message("Because possible_categorical_values is NULL, the possible values will be pulled from data. In the case that the data don't represent all possible values, this will produce incorrect results.")
      }
      possible_categorical_values <- lapply(X = setNames(object = names(indicator_type)[indicator_type %in% c("categorical")],
                                                         nm = names(indicator_type)[indicator_type %in% c("categorical")]),
                                            data = data,
                                            FUN = function(X, data){
                                              output <- data[["value"]][data[["indicator"]] %in% X] |>
                                                unique()
                                              if (length(output) < 2) {
                                                stop(paste0("There are fewer than two categories represented in data for the indicator ", X, ". Please use possible_categorical_values to define all possible categories."))
                                              }
                                              output
                                            })
    } else {
      stop("possible_categorical_values must either be a vector which contains the categories to use for all categorical analyses or a named list of vectors containing the categories for each categorical indicator.")
    }

    too_few_values <- names(possible_categorical_values)[sapply(X = possible_categorical_values,
                                                                FUN = length) < 2]

    if (length(too_few_values) > 0) {
      stop(paste0("The following categorical indicator have fewer than two possible values in possible_categorical_values: ",
                  paste(too_few_values,
                        collapse = ", ")))
    }
  }

  #### Analyses ----------------------------------------------------------------
  analyses <- lapply(X = setNames(object = names(indicator_type),
                                  nm = names(indicator_type))[!is.na(names(indicator_type))],
                     weights = weights |>
                       lapply(X = _,
                              FUN = function(X){
                                dplyr::mutate(.data = X,
                                              dplyr::across(.cols = tidyselect::any_of(x = "weight_set_id"),
                                                            .fns = as.character))
                              }),
                     data = data,
                     indicator_type = indicator_type,
                     value_var = value_var,
                     possible_categorical_values = possible_categorical_values,
                     conf = conf,
                     # continuous_transformation = continuous_transformation,
                     FUN = function(X, data, weights, indicator_type, value_var, possible_categorical_values, conf){
                       current_indicator_type <- indicator_type[X]
                       current_indicator <- X
                       current_data <- data[data[["indicator"]] %in% X, ]
                       if (current_indicator_type == "categorical") {
                         current_possible_categorical_values <- possible_categorical_values[[X]]
                       } else {
                         current_possible_categorical_values <- NULL
                       }

                       output_list <- list()
                       # Per indicator!
                       output_list[["none"]] <- lapply(weights = weights,
                                                       current_data = current_data,
                                                       current_indicator_type = current_indicator_type,
                                                       current_indicator = X,
                                                       value_var = "value",
                                                       current_possible_categorical_values = current_possible_categorical_values,
                                                       conf = conf,
                                                       # continuous_transformation = continuous_transformation,
                                                       # This X argument is down here so it
                                                       # doesn't mess with the other
                                                       # arguments that are using the
                                                       # previous layer's X value.
                                                       X = seq_len(length.out = length(weights)),
                                                       FUN = function(X, weights, current_data, current_indicator_type, current_indicator, value_var, current_possible_categorical_values, conf){


                                                         if (nrow(current_data) == 1) {
                                                           # When there's only one data point, can't do much about that.
                                                           analysis <- data.frame(n = 1,
                                                                                  alpha = 1 - conf / 100,
                                                                                  mean = current_data[[value_var]],
                                                                                  sd = 0,
                                                                                  variance = 0,
                                                                                  lower_bound = NA,
                                                                                  upper_bound = NA)
                                                         } else {
                                                           missing_weights <- setdiff(x = current_data$internal_uid_var,
                                                                                      y = weights[[X]]$internal_uid_var) |>
                                                             length()
                                                           if (missing_weights > 0) {
                                                             warning(paste0("Not all the provided records in data have a corresponding weight. Dropping unweighted records (", missing_weights, " of ", nrow(current_data), ")"))
                                                           }
                                                           current_data <- dplyr::inner_join(x = current_data,
                                                                                             y = weights[[X]],
                                                                                             by = "internal_uid_var",
                                                                                             relationship = "one-to-one")

                                                           if (nrow(current_data) < 1) {
                                                             warning("No data had corresponding weights. Returning NULL.")
                                                             return(NULL)
                                                           }

                                                           # Make sure we calculate using the correct method
                                                           if (current_indicator_type == "continuous") {
                                                             analysis <- dplyr::mutate(.data = current_data,
                                                                                       # Enforcing that these must be numeric!
                                                                                       value = as.numeric(value)) |>
                                                               analyze_con(data = _,
                                                                           weights = weights[[X]],
                                                                           id_var = "internal_uid_var",
                                                                           value_var = value_var,
                                                                           wgt_var = "weight",
                                                                           conf = conf,
                                                                           verbose = verbose) |>
                                                               dplyr::mutate(.data = _,
                                                                             standard_error = sd / sqrt(n),
                                                                             variance = sd^2) |>
                                                               dplyr::rename(.data = _,
                                                                             estimate = mean,
                                                                             standard_deviation = sd,
                                                                             coefficient_of_variance = cv)
                                                           } else {
                                                             analysis <- analyze_cat(data = current_data,
                                                                                     weights = weights[[X]],
                                                                                     id_var = "internal_uid_var",
                                                                                     cat_var = value_var,
                                                                                     wgt_var = "weight",
                                                                                     definitions = current_possible_categorical_values,
                                                                                     conf = conf,
                                                                                     # estimate_type = "percent",
                                                                                     verbose = verbose) |>
                                                               dplyr::select(.data = _,
                                                                             -tidyselect::any_of(x = c("observation_proportion"))) |>
                                                               dplyr::rename_with(.data = _,
                                                                                  .fn = ~ stringr::str_remove(string = .x,
                                                                                                              pattern = "_\\d+pct") |>
                                                                                    stringr::str_remove(string = _,
                                                                                                        pattern = "weighted_(observation_)?(proportion_)?")) |>
                                                               dplyr::rename(.data = _,
                                                                             n = observation_count,
                                                                             estimate = proportion) |>
                                                               dplyr::mutate(.data = _,
                                                                             standard_deviation = sqrt(variance),
                                                                             # standard_deviation = standard_error * n * n,
                                                                             #variance = standard_deviation^2
                                                               )
                                                           }
                                                         }
                                                         dplyr::mutate(.data = analysis,
                                                                       indicator = current_indicator,
                                                                       weight_set_id = X,
                                                                       alpha = 1 - conf / 100) |>
                                                           dplyr::select(.data = _,
                                                                         indicator,
                                                                         weight_set_id,
                                                                         tidyselect::everything())
                                                       }) |>
                         purrr::discard(.x = _,
                                        .p = is.null)

                       if (length(output_list[["none"]]) < 1) {
                         warning("No analysis possible. Returning NULL.")
                         return(NULL)
                       } else {
                         output_list[["none"]] <- dplyr::bind_rows(output_list[["none"]]) |>
                           dplyr::mutate(.data = _,
                                         combine = "none")
                       }


                       # if (current_indicator_type == "categorical") {
                       #   current_analysis_list <- split(x = output_list[["none"]],
                       #                                  f = ~ category)
                       # } else {
                       #   current_analysis_list <- list(output_list[["none"]])
                       # }

                       ##### Combining -------------------------------------
                       if ("bootstrap" %in% combine) {
                         # if (verbose) {
                         #   message("BOOTSTRAPPING")
                         # }
                         # Handling categorical indicators which
                         # will have multiple records per-run
                         # unlike continuous indicators which have
                         # a single value per-run.
                         # THIS ASSUMES category IS A VARIABLE
                         # PRESENT IN CATEGORICAL DATA.
                         # It will also treat unclassified indicators
                         # as continuous but with a warning.
                         if (current_indicator_type == "categorical") {
                           current_analysis_list <- split(x = output_list[["none"]],
                                                          f = ~ category)
                         } else {
                           current_analysis_list <- list(output_list[["none"]])
                         }

                         # Now do the bootstrapping on each data
                         # frame in the list. For continuous
                         # indicators this should be just one data
                         # frame and for categorical it'll be one
                         # per category.
                         output_list[["bootstrap"]] <- lapply(X = current_analysis_list,
                                                              current_indicator_type = current_indicator_type,
                                                              FUN = function(X, current_indicator_type){
                                                                bootstrap_results <- boot::boot(data = X$estimate,
                                                                                                # The function special_mean()
                                                                                                # is just mean() but with an
                                                                                                # added index argument so that
                                                                                                # the data are subset appropriately
                                                                                                # for each bootstrap replicate.
                                                                                                statistic = special_mean,
                                                                                                R = bootstrap_replicates)

                                                                output <- data.frame(indicator = X$indicator[1],
                                                                                     category = if ("category" %in% names(X)) {
                                                                                       X$category[1]
                                                                                     } else {
                                                                                       NA
                                                                                     },
                                                                                     estimate = bootstrap_results$t[1],
                                                                                     alpha = 1 - conf / 100,
                                                                                     n_input_estimates = nrow(X),
                                                                                     booststrap_replicates = bootstrap_replicates,
                                                                                     lower_bound = bootstrap_results$t[1],
                                                                                     upper_bound = bootstrap_results$t[1],
                                                                                     ci_bootstrap_type = "none")

                                                                # Remove the category variable if all the values are NA
                                                                # which we'd expect in the case of a continuous indicator.
                                                                if (all(is.na(output$category))) {
                                                                  output <- dplyr::select(.data = output,
                                                                                          -category)
                                                                }

                                                                # If we can, calculate confidence intervals.
                                                                # This isn't possible when there's just one
                                                                # result.
                                                                if (length(unique(bootstrap_results$t)) != 1) {
                                                                  bootstrap_cis <- boot::boot.ci(boot.out = bootstrap_results,
                                                                                                 conf = conf / 100,
                                                                                                 type = bootstrap_type)
                                                                  output <- lapply(X = setdiff(x = names(bootstrap_cis),
                                                                                               y = c("R", "t0", "call")),
                                                                                   bootstrap_cis = bootstrap_cis,
                                                                                   output = output,
                                                                                   FUN = function(X, bootstrap_cis, output){
                                                                                     bounds <- bootstrap_cis[[X]][(length(bootstrap_cis[[X]]) - 1):length(bootstrap_cis[[X]])]
                                                                                     dplyr::mutate(.data = output,
                                                                                                   lower_bound = bounds[1],
                                                                                                   upper_bound = bounds[2],
                                                                                                   ci_bootstrap_type = X)
                                                                                   }) |>
                                                                    dplyr::bind_rows()
                                                                }
                                                                output
                                                              }) |>
                           dplyr::bind_rows() |>
                           dplyr::mutate(.data = _,
                                         combine = "bootstrap")
                       }

                       if ("delta" %in% combine) {
                         if (current_indicator_type == "categorical") {
                           current_analysis_list <- split(x = output_list[["none"]],
                                                          f = ~ category)
                         } else {
                           current_analysis_list <- list(output_list[["none"]])
                         }

                         output_list[["delta"]] <- lapply(X = current_analysis_list,
                                                          FUN = function(X){
                                                            # variance_covariance_matrix <- diag(X[["standard_error"]])
                                                            # matrix_variance <-  matrix(1 / nrow(X),
                                                            #                            nrow = 1,
                                                            #                            ncol = nrow(X)) %*%
                                                            #   variance_covariance_matrix %*%
                                                            #   matrix(1 / nrow(X),
                                                            #          ncol = 1,
                                                            #          nrow = nrow(X)) |>
                                                            #   as.vector()


                                                            output <- data.frame(indicator = X$indicator[1],
                                                                                 category = if ("category" %in% names(X)) {
                                                                                   X$category[1]
                                                                                 } else {
                                                                                   NA
                                                                                 },
                                                                                 estimate = mean(X[["estimate"]]),
                                                                                 alpha = 1 - conf / 100,
                                                                                 n_input_estimates = nrow(X),
                                                                                 # variance = matrix_variance,
                                                                                 variance = var(X[["estimate"]]) + mean(X[["variance"]])
                                                            )

                                                            output <- lapply(X = c("none",
                                                                                   "logit"#,
                                                                                   # "log"
                                                            ),
                                                            mean = output$estimate,
                                                            variance = output$variance,
                                                            alpha = output$alpha,
                                                            FUN = function(X, mean, variance, alpha){
                                                              ci_delta(mean = mean,
                                                                       variance = variance,
                                                                       transform = X,
                                                                       alpha = alpha) |>
                                                                matrix(data =_,
                                                                       ncol = 2) |>
                                                                as.data.frame(x = _) |>
                                                                setNames(object = _,
                                                                         nm = paste0(c("lower_bound_",
                                                                                       "upper_bound_"), X))
                                                            }) |>
                                                              # dplyr::bind_cols() |>
                                                              dplyr::bind_cols(output,
                                                                               .x = _)


                                                            # Remove the category variable if all the values are NA
                                                            # which we'd expect in the case of a continuous indicator.
                                                            if (all(is.na(output$category))) {
                                                              output <- dplyr::select(.data = output,
                                                                                      -category)
                                                            }

                                                            output
                                                          }) |>
                           dplyr::bind_rows() |>
                           dplyr::mutate(.data = _,
                                         combine = "delta")
                       }

                       if ("mean" %in% combine) {
                         if (current_indicator_type == "categorical") {
                           current_analysis_list <- split(x = output_list[["none"]],
                                                          f = ~ category)
                         } else {
                           current_analysis_list <- list(output_list[["none"]])
                         }
                         analysis <- lapply(X = current_analysis_list,
                                            FUN = function(X){
                                              X <- dplyr::mutate(.data = X,
                                                                 variance = standard_deviation^2)
                                              # This is the variance used for the
                                              # calculation of confidence intervals that
                                              # account for the underlying variance in
                                              # the various samples and not just the
                                              # estimates from those samples
                                              total_variance <- var(x = X$estimate) + mean(X$variance)

                                              output <- data.frame(indicator = X$indicator[1],
                                                                   category = if ("category" %in% names(X)) {
                                                                     X$category[1]
                                                                   } else {
                                                                     NA
                                                                   },
                                                                   estimate = mean(X[["estimate"]]),
                                                                   alpha = 1 - conf / 100,
                                                                   n_input_estimates = nrow(X),
                                                                   variance = total_variance) |>
                                                dplyr::mutate(.data = _,
                                                              lower_bound = estimate - abs(sqrt(variance) * qt(p = alpha / 2,
                                                                                                               df = n_input_estimates - 1)),
                                                              upper_bound = estimate + abs(sqrt(variance) * qt(p = alpha / 2,
                                                                                                               df = n_input_estimates - 1)))

                                              # Remove the category variable if all the values are NA
                                              # which we'd expect in the case of a continuous indicator.
                                              if (all(is.na(output$category))) {
                                                output <- dplyr::select(.data = output,
                                                                        -category)
                                              }

                                              output
                                            }) |>
                           dplyr::bind_rows()

                         output_list[["mean"]] <- dplyr::mutate(.data = analysis,
                                                                indicator = current_indicator,
                                                                weight_set_id = X,
                                                                alpha = 1 - conf / 100) |>
                           dplyr::select(.data = _,
                                         indicator,
                                         weight_set_id,
                                         tidyselect::everything()) |>
                           dplyr::mutate(.data = _,
                                         combine = "mean")
                       }

                       if ("mean_weights" %in% combine) {
                         mean_weights <- lapply(X = seq_len(length(weights)),
                                                weights = weights,
                                                FUN = function(X, weights){
                                                  dplyr::mutate(.data = weights[[X]],
                                                                weightset_id = as.character(X))
                                                }) |>
                           dplyr::bind_rows(weights) |>
                           dplyr::summarize(.data = _,
                                            .by = tidyselect::all_of(x = c("internal_uid_var")),
                                            weight = mean(weight))

                         if (nrow(current_data) == 1) {
                           # When there's only one data point, can't do much about that.
                           analysis <- data.frame(n = 1,
                                                  alpha = 1 - conf / 100,
                                                  mean = data[[value_var]],
                                                  sd = 0,
                                                  variance = 0,
                                                  lower_bound = NA,
                                                  upper_bound = NA)
                         } else {
                           missing_weights <- setdiff(x = current_data$internal_uid_var,
                                                      y = mean_weights$internal_uid_var) |>
                             length()
                           if (missing_weights > 0) {
                             warning(paste0("Not all the provided records in data have a corresponding weight. Dropping unweighted records (", missing_weights, " of ", nrow(current_data), ")"))
                           }
                           current_data <- dplyr::inner_join(x = current_data,
                                                             y = mean_weights,
                                                             by = "internal_uid_var",
                                                             relationship = "one-to-one")

                           # Make sure we calculate using the correct method
                           if (current_indicator_type == "continuous") {
                             analysis <- dplyr::mutate(.data = current_data,
                                                       # Enforcing that these must be numeric!
                                                       value = as.numeric(value)) |>
                               analyze_con(data = _,
                                           weights = mean_weights,
                                           id_var = "internal_uid_var",
                                           value_var = value_var,
                                           wgt_var = "weight",
                                           conf = conf,
                                           verbose = verbose) |>
                               dplyr::mutate(.data = _,
                                             standard_error = sd / sqrt(n)) |>
                               dplyr::rename(.data = _,
                                             estimate = mean,
                                             standard_deviation = sd,
                                             coefficient_of_variance = cv)
                           } else {
                             analysis <- analyze_cat(data = current_data,
                                                     weights = mean_weights,
                                                     id_var = "internal_uid_var",
                                                     cat_var = value_var,
                                                     wgt_var = "weight",
                                                     definitions = current_possible_categorical_values,
                                                     conf = conf,
                                                     # estimate_type = "percent",
                                                     verbose = verbose) |>
                               dplyr::select(.data = _,
                                             -tidyselect::any_of(x = c("observation_proportion"))) |>
                               dplyr::rename_with(.data = _,
                                                  .fn = ~ stringr::str_remove(string = .x,
                                                                              pattern = "_\\d+pct") |>
                                                    stringr::str_remove(string = _,
                                                                        pattern = "weighted_(observation_)?(proportion_)?")) |>
                               dplyr::rename(.data = _,
                                             n = observation_count,
                                             estimate = proportion) |>
                               dplyr::mutate(.data = _,
                                             standard_deviation = standard_error * n * n)
                           }
                         }
                         output_list[["mean_weights"]] <- dplyr::mutate(.data = analysis,
                                                                        indicator = current_indicator,
                                                                        weight_set_id = X,
                                                                        alpha = 1 - conf / 100) |>
                           dplyr::select(.data = _,
                                         indicator,
                                         weight_set_id,
                                         tidyselect::everything()) |>
                           dplyr::mutate(.data = _,
                                         combine = "mean_weights")
                       }

                       # We needed the un-combined results for any combining,
                       # but they get dropped here if they weren't requested
                       # explicitly as outputs.
                       output <- lapply(X = output_list[combine],
                                        FUN = function(X){
                                          dplyr::mutate(.data = X,
                                                        dplyr::across(.cols = tidyselect::any_of(x = "weight_set_id"),
                                                                      .fns = as.character))
                                        }) |>
                         dplyr::bind_rows()

                       output
                     })
  analyses
}

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

# Literally just from https://en.wikipedia.org/wiki/Weighted_arithmetic_mean
#' Calcualted weighted standard error
#' @export
weighted_se <- function(values,
                             weights){
  normalized_weights <- weights / sum(weights)
  variance <- var(x = values)
  sqrt(variance) * sqrt(sum(normalized_weights^2))
}

#' Calculate a weighted mean
#' @export
weighted_mean <- function(values,
                          weights){
  if (!is.numeric(values) | !is.vector(values)) {
    stop("values must be a numeric vector.")
  }

  if (is.numeric(weights) | !is.vector(weights)) {
    stop("weights must be a numeric vector.")
  }

  if (length(weights) != length(values)) {
    stop("values and weights must be the same length")
  }

  sum(values * weights) / sum(weights)
}

#' Calculate a weighted standard deviation
#' @export
weighted_sd <- function(values,
                        weights){
  if (!is.numeric(values) | !is.vector(values)) {
    stop("values must be a numeric vector.")
  }

  if (is.numeric(weights) | !is.vector(weights)) {
    stop("weights must be a numeric vector.")
  }

  if (!length(weights) %in% c(length(values), 1)) {
    stop("values and weights must be the same length or weights must be a single value.")
  }

  sqrt(sum(weights * values * values) / sum(weights) - weighted_mean(values = values,
                                                                     weights = weights)^2)
}

#' Calculate a weighted coefficient of variance
#' @export
weighted_cv <- function(values,
                        weights){
  if (!is.numeric(values) | !is.vector(values)) {
    stop("values must be a numeric vector.")
  }

  if (is.numeric(weights) | !is.vector(weights)) {
    stop("weights must be a numeric vector.")
  }

  if (!length(weights) %in% c(length(values), 1)) {
    stop("values and weights must be the same length or weights must be a single value.")
  }

  weighted_sd(values = values,
              weights = weights) / weighted_mean(values = values,
                                                 weights = weights)
}

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
  sum_of_weights_squared <- sum(weights^2)
  # Get the weighted mean
  weighted_mean <- sum(values * weights) / sum_of_weights
  # Calculate variance!
  variance <- (sum_of_weights / (sum_of_weights^2 - sum_of_weights_squared)) * sum(weights * (values - weighted_mean)^2,
                                                                                   na.rm = na_remove)
  return(variance)
}


# This is literally only here for the dang bootstrapping
special_mean <- function(data, indices) {
  mean(data[indices],
       trim = 0)
}
