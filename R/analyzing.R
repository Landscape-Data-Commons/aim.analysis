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
                          cat_var,
                          split_vars)
  missing_data_vars <- required_data_vars[!(required_data_vars %in% names(data))]
  if (length(missing_data_vars) > 0) {
    stop("The following variables are missing from data: ",
         paste(missing_data_vars,
               collapse = , ", "))
  }

  # Just want the bare minimum here.
  data <- dplyr::select(.data = data,
                        tidyselect::all_of(required_data_vars))

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
                           tidyselect::all_of(required_weights_vars))

  # What categories were observed?
  present_categories <- unique(data[[cat_var]])

  # And what if the user provided definitions?
  # This is important for if there are categories that have no data that qualified!
  if (!is.null(definitions)) {
    if (!(class(data[[cat_var]]) %in% class(definitions))) {
      stop("definitions must be the same class as the category values in data")
    }
    if (length(definitions) < 1) {
      stop("There are no values in definitions")
    }
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
  weight_ids_in_data_indices <- weights[[id_var]] %in% data[[id_var]]
  if (verbose & !all(weight_ids_in_data_indices)) {
    message("Not all unique IDs in weights appear in data, just so you know.")
  }
  weights <- weights[weight_ids_in_data_indices, ]


  # Get each observation with just its category and weight
  weighted_categories <- dplyr::left_join(x = dplyr::select(.data = data,
                                                            tidyselect::all_of(c(id_var,
                                                                                 cat_var))),
                                          y = weights,
                                          by = id_var,
                                          relationship = "one-to-one")

  # Calculate the sum of the weights for each of the observed categories
  category_weight_summary <- dplyr::summarize(.data = weighted_categories,
                                           .by = tidyselect::matches(match = cat_var),
                                           observation_count = dplyr::n(),
                                           total_observation_weight = sum(wgt_var)) |>
    dplyr::rename(.data = _,
                  setNames(object = cat_var,
                           nm = "category")) |>
    dplyr::mutate(.data = _,
                  observation_weighted_proportion = total_observation_weight / sum(weighted_categories[[wgt_var]]),
                  observation_proportion = count / nrow(weighted_categories),
                  adjusted_count = observation_count * observation_weighted_proportion)

  # Okay, so if we have definitions to catch categories with zero observations, add those
  # Because it should matter for calculating confidence intervals
  if (!is.null(definitions)) {
    missing_categories <- definitions[!(definitions %in% present_categories)]

    if (length(missing_categories) > 0) {
      empty_category_weight_summary <- lapply(X = missing_categories,
                                              FUN = function(X){
                                                data.frame(category = X,
                                                           count = 0,
                                                           weight_sum = 0,
                                                           weighted_proportion = 0,
                                                           adjusted_count = 0)
                                              }) |>
        dplyr::bind_rows()

      category_weight_summary <- dplyr::bind_rows(category_weight_summary,
                                                  empty_category_weight_summary)
    }
  }

  # # Calculate the weighted proportions for each category
  # category_weighted_proportions <- category_weight_sums / sum(category_weight_sums)
  # # Get the pure counts of the categories
  # category_counts <- table(weighted_categories[[cat_var]])
  # # And the total number of observations. This should be the same as nrow(weighted_categories)
  # total_observations <- sum(category_counts)
  # # Using the total number of observations and the weighted proportions to calculate "adjusted counts"
  # adjusted_counts <- category_weighted_proportions * total_observations
  #
  # # Okay, so if we have definitions to catch categories with zero observations, add those
  # # Because it should matter for calculating confidence intervals
  # if (!is.null(definitions)) {
  #   defined_categories <- definitions[[cat_var]]
  #   missing_categories <- defined_categories[!(defined_categories %in% present_categories)]
  #   # Looping because it's easy, not because it's the best solution
  #   # But we want to populate the 0s for all of these!
  #   for (category in missing_categories) {
  #     category_weighted_proportions[[category]] <- 0
  #     category_weight_sums[[category]] <- 0
  #     category_counts[[category]] <- 0
  #     adjusted_counts[[category]] <- 0
  #   }
  # }

  # Finally ready to calculate confidence intervals!
  # But first we need the alpha value for our confidence level
  alpha <- 1 - (conf / 100)

  confidence_intervals <- goodman_cis(counts = setNames(object = category_weight_summary[["adjusted_count"]],
                                                        nm = category_weight_summary[["category"]]),
                                      alpha = alpha,
                                      chisq = "best",
                                      verbose = verbose)
  confidence_interval_vars <- c("category",
                                "weighted_observation_count",
                                "weighted_observation_proportion",
                                "weighted_observation_proportion_lower_bound",
                                "weighted_observation_proportion_upper_bound")
  names(confidence_intervals) <- confidence_interval_vars

  # Combine the results and confidence intervals
  output <- dplyr::left_join(x = category_weight_summary,
                  y = dplyr::select(.data = confidence_intervals,
                                    category,
                                    weighted_observation_proportion_lower_bound,
                                    weighted_observation_proportion_upper_bound),
                  by = "category")

  # Get the variables restricted to what we care about and ordered properly
  dplyr::select(.data = output,
                category,
                observation_count,
                observation_proportion,
                total_observation_weight,
                weighted_observation_proportion,
                weighted_observation_proportion_lower_bound,
                weighted_observation_proportion_upper_bound)
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
    if (non_unique_ids_subsets == "only") {
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

# wilson_cis

