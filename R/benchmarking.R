#' Apply benchmarks to data
#' @description Using data that has unique identifier and indicator value variables, apply benchmarks. The benchmarks are applied to the data based on the benchmark group(s) that the data belong to, which may already be assigned in the data or may be added with a benchmark group lookup table.
#' @param data Data frame or spatial * data frame. The data to be benchmarked. Must contain the variable(s) in \code{idvars} as unique identifiers. May be "wide" or "long" data. If wide, then each indicator name should be a variable name. If long, then the indicator names and values should be in variables named \code{"indicator_var"} and \code{"value"}.
#' @param idvars Character string or vecor of character strings. The names of the variable(s) in \code{data} which serve as unique identifiers.
#' @param benchmark_group_var Optional character string. The name of the variable that holds the benchmark group memberships. If \code{NULL} then it defaults to the variable in \code{benchmark_group_lut} that is not in \code{idvars}. Defaults to \code{NULL}.
#' @param benchmark_group_lut Optional data frame. The lookup table used to assign benchmark group membership to \code{data}. Must contain all of the variables in \code{idvar} and one additional variable with the benchmark group memberships.
#' @param benchmarks Data frame. The benchmarks in the format produced by \code{read.benchmarks()}. If the benchmark group membership variable in other data frames is not \code{"Benchmark.Group"} then it must be renamed here.
#' @param verbose Logical. If \code{TRUE} then the function will generate additional messages as it executes.
#' @return A long data frame of each point/benchmarked indicator combination with the assigned condition category.
#' @export
apply_benchmarks <- function(data,
                             idvars,
                             benchmark_group_var = NULL,
                             benchmark_group_lut = NULL,
                             benchmarks,
                             verbose = FALSE) {
  if (grepl(class(data)[1], pattern = "^Spatial.*DataFrame$")) {
    data <- data@data
  } else if (class(data) != "data.frame") {
    stop("data must either be a data frame or a spatial data frame.")
  }

  idvars <- c(idvars, benchmark_group_var)
  missing_idvars <- idvars[!(idvars %in% names(data))]
  if (length(missing_idvars) > 0) {
    stop(paste0("The following variables are missing from data: ", paste(missing_idvars, collapse = ", ")))
  }

  if (class(benchmarks) != "data.frame") {
    stop("Benchmarks must be a data frame.")
  }

  if (!is.null(benchmark_group_var)) {
    if (!(benchmark_group_var %in% names(benchmarks)) & !("Benchmark.Group" %in% names(benchmarks))) {
      stop(paste0("The variable ", benchmark_group_var, "does not appear in the data frame benchmarks"))
    } else {
      if (verbose) {
        message(paste0("Adding the contents of benchmarks[['Benchmark.Group']] to benchmarks[['", benchmark_group_var, "']]"))
      }
      benchmarks[[benchmark_group_var]] <- benchmarks[["Benchmark.Group"]]
    }
  }

  if (is.null(benchmark_group_var) & is.null(benchmark_group_lut)) {
    stop("Either benchmark_group_var or benchmark_group_lut must not be NULL")
  } else if (!is.null(benchmark_group_var) & !is.null(benchmark_group_lut)) {
    if (verbose) {
      message("Both a benchmark variable and a benchmark group lookup table have been provided. Assuming that the benchmark_group_var is in the lookup table and will be applied to the data from there.")
    }
    if (!(benchmark_group_var) %in% names(benchmark_group_lut)) {
      stop(paste0("The variable ", benchmark_group_var, " does not appear in the names of benchmark_group_lut"))
    }
  } else if (!is.null(benchmark_group_var) & is.null(benchmark_group_lut)) {
    if (!(benchmark_group_var) %in% names(data)) {
      stop(paste0("The variable ", benchmark_group_var, " does not appear in the names of data. Did you mean to supply a lookup table as well?"))
    }
  }



  if (!is.null(benchmark_group_lut)) {
    common_vars_benchmarks <- names(table(c(names(benchmarks), names(benchmark_group_lut))))[table(c(names(benchmarks), names(benchmark_group_lut))) > 1]
    if (length(common_vars_benchmarks) < 1) {
      stop("There are no variables in common between benchmark_group_lut and benchmarks")
    }

    if (!is.null(benchmark_group_var)) {
      if (verbose) {
        message("Removing existing benchmark group variable from data to replace with values from lookup table.")
      }
      data <- data[, names(data)[!(names(data %in% benchmark_group_var))]]
    }
    common_vars <- names(table(c(names(data), names(benchmark_group_lut))))[table(c(names(data), names(benchmark_group_lut))) > 1]
    if (verbose) {
      message(paste0("Joining data and benchmark group lut by the variable(s): ", paste(common_vars, collapse = ", ")))
    }
    data_grouped <- merge(x = data,
                          y = benchmark_group_lut,
                          by = common_vars)
  } else {
    data_grouped <- data
  }

  if (verbose) {
    message("Applying benchmarks")
  }

  # Get the benchmarks merged to the points
  indicator_lut <- indicator.lookup()
  names(indicator_lut) <- c("indicator_var", "indicator_name")
  benchmarks <- merge(x = benchmarks,
                      y = indicator_lut,
                      by.x = "Indicator",
                      by.y = "indicator_name")

  eval_vars <- names(benchmarks)[grepl(names(benchmarks), pattern = "^evalstring\\d+$")]

  idvars <- c(idvars, "Benchmark.Group")

  # Make the data tall
  if (!all(c("indicator", "value") %in% names(data_grouped))) {
    indicator_vars_present <- names(data_grouped)[names(data_grouped) %in% indicator_lut[["indicator_var"]]]
    data_tall <- tidyr::pivot_longer(data = data_grouped[, c(idvars, indicator_vars_present)],
                                     cols = tidyselect::one_of(indicator_vars_present),
                                     names_to = "indicator",
                                     values_to = "value")
  } else {
    data_tall <- data
  }

  data_benchmarked <- merge(x = data_tall,
                            y = benchmarks,
                            by.x = c("indicator", "Benchmark.Group", "Reporting.Unit"),
                            by.y = c("indicator_var", "Benchmark.Group", "Reporting.Unit"))

  benchmark_vector <- sapply(X = 1:nrow(data_benchmarked),
                             data = data_benchmarked,
                             eval_vars = eval_vars,
                             FUN = function(X, data, eval_vars){
                               all(sapply(X = eval_vars,
                                          data = data[X, ],
                                          FUN = function(X, data){
                                            evalstring <- gsub(data[[X]][1],
                                                               pattern = "(x){1}",
                                                               replacement = data[["value"]])
                                            eval(parse(text = evalstring))
                                          }))
                             })

  output <- data_benchmarked[benchmark_vector, c(idvars, "indicator", "Condition.Category")]

  return(output)
}

#' Apply benchmarks to data
#' @description Using data that has unique identifier and indicator value variables, apply benchmarks. The benchmarks are applied to the data based on the benchmark group(s) that the data belong to, which may already be assigned in the data or may be added with a benchmark group lookup table.
#' @param data Data frame or spatial * data frame. The data to be benchmarked. Must contain the variable(s) in \code{idvars} as unique identifiers. May be "wide" or "long" data. If wide, then each indicator name should be a variable name. If long, then the indicator names and values should be in variables named \code{"indicator_var"} and \code{"value"}.
#' @param idvars Character string or vecor of character strings. The names of the variable(s) in \code{data} which serve as unique identifiers.
#' @param benchmark_group_var Optional character string. The name of the variable that holds the benchmark group memberships. If \code{NULL} then it defaults to the variable in \code{benchmark_group_lut} that is not in \code{idvars}. Defaults to \code{NULL}.
#' @param benchmark_group_lut Optional data frame. The lookup table used to assign benchmark group membership to \code{data}. Must contain all of the variables in \code{idvar} and one additional variable with the benchmark group memberships.
#' @param benchmarks Data frame. The benchmarks in the format produced by \code{read.benchmarks()}. If the benchmark group membership variable in other data frames is not \code{"Benchmark.Group"} then it must be renamed here.
#' @param verbose Logical. If \code{TRUE} then the function will generate additional messages as it executes.
#' @param tdat_version Version of TerrADat used in the analysis for use in \code{indicator lookup()}. Defaults to \code{"2"}.
#' @return A long data frame of each point/benchmarked indicator combination with the assigned condition category.
#' @export
apply_benchmarks <- function(data,
                             idvars,
                             benchmark_group_var = NULL,
                             benchmark_group_lut = NULL,
                             benchmarks,
                             verbose = FALSE,
                             tdat_version = "2") {
  if (grepl(class(data)[1], pattern = "^Spatial.*DataFrame$")) {
    data <- data@data
  } else if (class(data) != "data.frame") {
    stop("data must either be a data frame or a spatial data frame.")
  }

  idvars <- c(idvars, benchmark_group_var)
  missing_idvars <- idvars[!(idvars %in% names(data))]
  if (length(missing_idvars) > 0) {
    stop(paste0("The following variables are missing from data: ", paste(missing_idvars, collapse = ", ")))
  }

  if (class(benchmarks) != "data.frame") {
    stop("Benchmarks must be a data frame.")
  }

  if (!is.null(benchmark_group_var)) {
    if (!(benchmark_group_var %in% names(benchmarks)) & !("Benchmark.Group" %in% names(benchmarks))) {
      stop(paste0("The variable ", benchmark_group_var, "does not appear in the data frame benchmarks"))
    } else {
      if (verbose) {
        message(paste0("Adding the contents of benchmarks[['Benchmark.Group']] to benchmarks[['", benchmark_group_var, "']]"))
      }
      benchmarks[[benchmark_group_var]] <- benchmarks[["Benchmark.Group"]]
    }
  }

  if (is.null(benchmark_group_var) & is.null(benchmark_group_lut)) {
    stop("Either benchmark_group_var or benchmark_group_lut must not be NULL")
  } else if (!is.null(benchmark_group_var) & !is.null(benchmark_group_lut)) {
    if (verbose) {
      message("Both a banchmark variable and a benchmark group lookup table have been provided. Assuming that the benchmark_group_var is in the lookup table and will be applied to the data from there.")
    }
    if (!(benchmark_group_var) %in% names(benchmark_group_lut)) {
      stop(paste0("The variable ", benchmark_group_var, " does not appear in the names of benchmark_group_lut"))
    }
  } else if (!is.null(benchmark_group_var) & is.null(benchmark_group_lut)) {
    if (!(benchmark_group_var) %in% names(data)) {
      stop(paste0("The variable ", benchmark_group_var, " does not appear in the names of data. Did you mean to supply a lookup table as well?"))
    }
  }



  if (!is.null(benchmark_group_lut)) {
    common_vars_benchmarks <- names(table(c(names(benchmarks), names(benchmark_group_lut))))[table(c(names(benchmarks), names(benchmark_group_lut))) > 1]
    if (length(common_vars_benchmarks) < 1) {
      stop("There are no variables in common between benchmark_group_lut and benchmarks")
    }

    if (!is.null(benchmark_group_var)) {
      if (verbose) {
        message("Removing existing benchmark group variable from data to replace with values from lookup table.")
      }
      data <- data[, names(data)[!(names(data %in% benchmark_group_var))]]
    }
    common_vars <- names(table(c(names(data), names(benchmark_group_lut))))[table(c(names(data), names(benchmark_group_lut))) > 1]
    if (verbose) {
      message(paste0("Joining data and benchmark group lut by the variable(s): ", paste(common_vars, collapse = ", ")))
    }
    data_grouped <- merge(x = data,
                          y = benchmark_group_lut,
                          by = common_vars)
  } else {
    data_grouped <- data
  }

  if (verbose) {
    message("Applying benchmarks")
  }

  # Get the benchmarks merged to the points
  indicator_lut <- indicator.lookup(tdat_version)
  names(indicator_lut) <- c("indicator_var", "indicator_name")
  benchmarks <- merge(x = benchmarks,
                      y = indicator_lut,
                      by.x = "Indicator",
                      by.y = "indicator_name")

  eval_vars <- names(benchmarks)[grepl(names(benchmarks), pattern = "^evalstring\\d+$")]

  idvars <- c(idvars, "Benchmark.Group")

  # Make the data tall
  if (!all(c("indicator", "value") %in% names(data_grouped))) {
    indicator_vars_present <- names(data_grouped)[names(data_grouped) %in% indicator_lut[["indicator_var"]]]
    data_tall <- tidyr::pivot_longer(data = data_grouped[, c(idvars, indicator_vars_present)],
                                     cols = tidyselect::one_of(indicator_vars_present),
                                     names_to = "indicator",
                                     values_to = "value")
  } else {
    data_tall <- data
  }

  data_benchmarked <- merge(x = data_tall,
                            y = benchmarks,
                            by.x = c("indicator", "Benchmark.Group", "Reporting.Unit"),
                            by.y = c("indicator_var", "Benchmark.Group", "Reporting.Unit"))

  benchmark_vector <- sapply(X = 1:nrow(data_benchmarked),
                             data = data_benchmarked,
                             eval_vars = eval_vars,
                             FUN = function(X, data, eval_vars){
                               all(sapply(X = eval_vars,
                                          data = data[X, ],
                                          FUN = function(X, data){
                                            evalstring <- gsub(data[[X]][1],
                                                               pattern = "(x){1}",
                                                               replacement = data[["value"]])
                                            eval(parse(text = evalstring))
                                          }))
                             })

  output <- data_benchmarked[benchmark_vector, c(idvars, "indicator", "Condition.Category")]

  return(output)
}
