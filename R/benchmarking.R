#' Apply benchmarks to data
#' @description Using data that has unique identifier and indicator value variables, apply benchmarks. The benchmarks are applied to the data based on the benchmark group(s) that the data belong to, which may already be assigned in the data or may be added with a benchmark group lookup table.
#' @param data Data frame. The data to be benchmarked. Must contain the variable(s) in \code{data_id_vars} as unique identifiers. May be "wide" or "long" data. If wide, then each indicator name should be a variable name. If long, then the indicator names and values should be in variables named \code{"indicator_var"} and \code{"value"}.
#' @param benchmarks Data frame. The benchmarks in the format produced by \code{read_benchmarks()}.
#' @param data_id_vars Character string or vecor of character strings. The names of the variable(s) in \code{data} which serve as unique identifiers.
#' @param data_group_var Optional character string. The name of the variable that holds the benchmark group memberships. If \code{NULL} then it defaults to the variable in \code{benchmark_group_var}. Defaults to \code{NULL}.
#' @param benchmark_group_var Character string. The name of the variable that holds the benchmark group memberships. Defaults to \code{"Benchmark_Group"}.
#' @param benchmark_indicator_var Character string. The name of the variable that holds the indicator names in the benchmarks. Defaults to \code{"Indicator"}.
# #' @param verbose Logical. If \code{TRUE} then the function will generate additional messages as it executes.
#' @param use_tdat_indicator_lookup Logical. Set to \code{TRUE} if the the values in variable \code{benchmark_indicator_var} are the standard human-readable names for the indicators and the corresponding variables in \code{data} use the variable names from the Terrestrial AIM Database. Defaults to \code{TRUE}.
# #' @param indicator_lookup_path Optional character string. The path to the folder where the lookup tables to be read in with
#' @return A long data frame of each point/benchmarked indicator combination with the assigned condition category.
#' @export
apply_benchmarks <- function(data,
                             benchmarks,
                             data_id_vars,
                             data_group_var = NULL,
                             # benchmark_idvars = NULL,
                             benchmark_group_var = "Benchmark_Group",
                             benchmark_indicator_var = "Indicator",
                             use_tdat_indicator_lookup = TRUE) {
  #### Benchmark sanitization --------------------------------------------------
  if (!("data.frame" %in% class(benchmarks))) {
    stop("Benchmarks must be a data frame.")
  }

  required_benchmark_vars <- c(benchmark_indicator_var)
  missing_benchmark_vars <- setdiff(x = required_benchmark_vars,
                                    y = names(benchmarks))
  if (length(missing_benchmark_vars) > 0) {
    stop(paste0("The following variables are missing from benchmarks: ",
                paste(missing_benchmark_vars,
                      collapse = ", ")))
  }
  benchmarks <- dplyr::rename(.data = benchmarks,
                              indicator_name = tidyselect::matches(match = benchmark_indicator_var))

  # This needs to be reconsidered for data moving forward.
  if (use_tdat_indicator_lookup) {
    indicator_lut <- indicator_lookup(tdat_version = 2)
    names(indicator_lut) <- c("indicator_var", "indicator_name")
    benchmarks <- dplyr::left_join(x = benchmarks,
                                   y = indicator_lut,
                                   by = "indicator_name",
                                   relationship = "many-to-one") |>
      dplyr::mutate(.data = _,
                    indicator_var = dplyr::case_when(is.na(indicator_var) ~ indicator_name,
                                                     .default = indicator_var))
  } else {
    benchmarks <- dplyr::rename(.data = benchmarks,
                                indicator_var = indicator_name)
  }

  if (is.null(benchmark_group_var)) {
    benchmarks <- dplyr::mutate(.data = benchmarks,
                                benchmark_group = "All")
  } else {
    if (!(benchmark_group_var %in% names(benchmarks))) {
      stop(paste("The benchmark_group_var variable", benchmark_group_var, "does not appear in the benchmarks."))
    }
    benchmarks <- dplyr::rename(.data = benchmarks,
                                benchmark_group = tidyselect::matches(match = benchmark_group_var))
  }

  #### Data sanitization -------------------------------------------------------
  if (!("data.frame" %in% class(data))) {
    stop("data must either be a data frame or a spatial data frame.")
  }
  data <- sf::st_drop_geometry(x = data)

  required_data_vars <- c(data_id_vars, data_group_var)
  missing_data_vars <- setdiff(x = required_data_vars,
                               y = names(data))
  if (length(missing_data_vars) > 0) {
    stop(paste0("The following variables are missing from data: ",
                paste(missing_data_vars,
                      collapse = ", ")))
  }
  if (is.null(data_group_var)) {
    data <- dplyr::mutate(.data = data,
                          benchmark_group = "All")
  } else {
    data <- dplyr::rename(.data = data,
                          benchmark_group = tidyselect::matches(match = data_group_var))
  }

  #### Data checks -------------------------------------------------------------
  if (!any(data$benchmark_group %in% benchmarks$benchmark_group) | !any(benchmarks$benchmark_group %in% data$benchmark_group)) {
    stop("None of these data qualify for any of these benchmarks according to the benchmark groups. Double check your data_group_var and benchmark_group_var values.")
  }
  unrepresented_data_groups <- setdiff(x = unique(data$benchmark_group),
                                       y = unique(benchmarks$benchmark_group))
  if (length(unrepresented_data_groups) > 0) {
    warning("Some data have no applicable benchmarks and will not be evaluted.")
  }
  unrepresented_benchmark_groups <- setdiff(x = unique(benchmarks$benchmark_group),
                                            y = unique(data$benchmark_group))
  if (length(unrepresented_benchmark_groups) > 0) {
    warning("Some benchmarks have no applicable data and will not be evaluted.")
  }

  benchmarks <- dplyr::filter(.data = benchmarks,
                              benchmark_group %in% data$benchmark_group)

  # if (is.null(benchmark_idvars)) {
  #   benchmarks_list <- list(benchmarks)
  # } else {
  # benchmarks_list <- split(x = benchmarks,
  #                          f = benchmarks$Management_Question)
  # }

  data <- dplyr::filter(.data = data,
                        benchmark_group %in% benchmarks$benchmark_group)

  indicator_variables_present <- intersect(x = unique(benchmarks$indicator_var),
                                           y = names(data))
  if (length(indicator_variables_present) == 0) {
    stop("None of the indicators in the benchmarks appear as variables in the data. Double check use_tdat_indicator_lut, the indicator names in the benchmarks, and the variable names in the data.")
  } else if (length(indicator_variables_present) != length(unique(benchmarks$indicator_var))) {
    warning("Not all indicators in the benchmarks appear in the data.")
  }

  benchmarks <- dplyr::filter(.data = benchmarks,
                              indicator_var %in% indicator_variables_present)

  #### Actual benchmarking -----------------------------------------------------
  # Because sometimes the benchmarks are for different classes of values (e.g.,
  # some character and some numeric), we'll do these one at a time.
  results <- list()
  for (current_management_question in unique(benchmarks$Management_Question)) {
    current_benchmarks <- dplyr::filter(.data = benchmarks,
                                        Management_Question == current_management_question)
    for (current_indicator in unique(current_benchmarks$indicator_var)) {
      data_tall <- dplyr::select(.data = data,
                                 tidyselect::all_of(data_id_vars),
                                 benchmark_group,
                                 tidyselect::all_of(current_indicator)) |>
        tidyr::pivot_longer(data = _,
                            cols = tidyselect::all_of(current_indicator),
                            names_to = "indicator_var",
                            values_to = "value") |>
        dplyr::inner_join(x = _,
                          y = current_benchmarks,
                          relationship = "many-to-many",
                          by = c("benchmark_group",
                                 "indicator_var"))

      eval_vars <- names(current_benchmarks)[grepl(names(current_benchmarks), pattern = "^evalstring\\d+$")]

      benchmark_vector <- sapply(X = 1:nrow(data_tall),
                                 data_tall = data_tall,
                                 eval_vars = eval_vars,
                                 FUN = function(X, data_tall, eval_vars){
                                   all(sapply(X = eval_vars,
                                              current_data = data_tall[X, ],
                                              FUN = function(X, current_data){
                                                current_value <- current_data[["value"]]
                                                # This nonsense is so that strings
                                                # have '' around them so that this
                                                # evaluates correctly
                                                if (class(current_value) == "character") {
                                                  current_value <- paste0("'", current_value, "'")
                                                  evalstring_vector <- stringr::str_split_1(string = current_data[[X]][1],
                                                                                            pattern = "[ ]")
                                                  target_value_index <- setdiff(x = c(1, 3),
                                                                                y = which(evalstring_vector == "x"))
                                                  evalstring_vector[target_value_index] <- paste0("'", evalstring_vector[target_value_index], "'")
                                                  evalstring <- paste(evalstring_vector,
                                                                      collapse = " ")
                                                } else {
                                                  evalstring <- current_data[[X]][1]
                                                }
                                                evalstring <- gsub(evalstring,
                                                                   pattern = "(x){1}",
                                                                   replacement = current_value)
                                                eval(parse(text = evalstring))
                                              }))
                                 })

      results[[paste0(current_management_question,
                      "_",
                      current_indicator)]] <- dplyr::select(.data = data_tall,
                                                            tidyselect::all_of(data_id_vars),
                                                            indicator = indicator_var,
                                                            Condition_Category)[benchmark_vector, ] |>
        dplyr::mutate(.data = _,
                      Management_Question = current_management_question)
    }
  }
  dplyr::bind_rows(results) |>
    # dplyr::filter(.data = _,
    #               # Figure out why these are necessary!!!!
    #               !is.na(indicator),
    #               !is.na(Condition_Category))
    dplyr::distinct()
}
