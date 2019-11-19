#' Applying indicator benchmarks to a data frame of TerrADat data.
#'
#' This evaluates the indicator values in TerrADat against the supplied benchmarks. If a plot should be evaluated in multiple evaluation strata/groups, there should be one copy of the plot per stratum/group.
#' @param benchmarks A data frame containing the benchmark information, probably imported from .XLSX with \code{read.benchmarks()}.
#' @param data Data frame. The data to be benchmarked. At a minimum, it must contain a variable  imported from TerrADat's terrestrial or remote sensing indicators feature classes AND a field defining the evaluation stratum/group each plot should be considered part of, probably added using \code{attribute.shapefile()}.
#' @param data.tall Logical. If \code{TRUE}, then the data frame provided as \code{data} is treated as tidy, where each row/observation is of a single indicator and its value with the indicator name in a variable matching the string provided as \code{data.indicatorfield}. Otherwise is the name that will be used when the function converts the data from wide to tall. Defaults to \code{FALSE}.
#' @param data.indicatorfield Optional character string. If \code{data.tall} is \code{TRUE} then this must match the name of the variable in \code{data} that contains the indicator names. Ignored if \code{data.tall} is \code{FALSE}. Defaults to \code{"indicator"}.
#' @param data.valuefield Character string. If \code{data.tall} is \code{TRUE} the name of the variable in \code{data} that contains the numeric values for each indicator. Ignored if \code{data.tall} is \code{FALSE}. Defaults to \code{"value"}.
#' @param data.groupfield Character string. The name of the variable in \code{data} that contains the benchmark group identities in the \code{benchmark.groupfield} variable of \code{benchmarks}. Defaults to \code{"Benchmark.Group"}.
#' @param use.indicator.lut Logical. If \code{TRUE} then use the data frame provided as \code{indicator.lut} or the default lookup table to match the indicators in the \code{benchmarks} data frame to the indicators in the \code{data} data frame. Defaults to \code{TRUE}.
#' @param indicator.lut Optional data frame. If \code{use.indicator.lut} is \code{TRUE} then it will be used to join \code{data} and \code{benchmarks}. Defaults to the output from \code{indicator.lookup()}.
#' @param lut.data.indicatorfield Optional character string. If using a lookup table, the name of the variable in \code{indicator.lut} that contains the indicator identities matching those in the variable \code{data.indicatorfield} in \code{data}. Defaults to \code{indicator}, appropriate for use with the default indicator lookup table.
#' @param lut.benchmark.indicatorfield Optional character string. If using a lookup table, the name of the variable in \code{indicator.lut} that contains the indicator identities matching those in the variable \code{benchmark.indicatorfield} in \code{benchmark}. Defaults to \code{indicator.name}, appropriate for use with the default indicator lookup table.
#' @param benchmark.indicatorfield Character string. This must match the name of the variable in \code{benchmarks} that contains the indicator names. Defaults to \code{"Indicator"}.
#' @param benchmark.groupfield Character string. The name of the variable in \code{benchmarks} that contains the benchmark group identities in the \code{data.groupfield} variable of \code{data}. Defaults to \code{"Benchmark.Group"}.
#' @param benchmark.categoryfield Character string. The nameof the variable in \code{benchmarks} that contains the categories that each benchmark corresponds to. Defaults to \code{"Condition.Category"}.
#' @param benchmark.evalstringfield Vector of character strings. The variable names for all relevant inequality strings to be evaluated for each benchmark. The string must express an inequality to be evaluated with the character \code{"x"} where the indicator value from the data set will be substituted, e.g. \code{"0 <= x"}. Defaults to \code{c("eval.string.lower", "eval.string.upper")}
#' @param benchmark.metagroupfield Vector of
#' @examples
#' benchmark()
#' @export

benchmark <- function(benchmarks,
                      data,
                      data.tall = FALSE,
                      data.indicatorfield = "indicator",
                      data.valuefield = "value",
                      data.groupfield = "benchmark.group",
                      use.indicator.lut = TRUE,
                      indicator.lut = NULL,
                      lut.data.indicatorfield = "indicator",
                      lut.benchmark.indicatorfield = "indicator.name",
                      benchmark.indicatorfield = "Indicator",
                      benchmark.groupfield = "Benchmark.Group",
                      benchmark.categoryfield = "Condition.Category",
                      benchmark.evalstringfield = c("eval.string.lower", "eval.string.upper"),
                      benchmark.metagroupfield = c("Management.Question")
){
  ## Only take a data frames
  if (class(data)[1] != "data.frame") {
    stop("The data must be a data frame")
  }
  if (class(benchmarks)[1] != "data.frame") {
    stop("The benchmarks must be a data frame")
  }

  # Make the benchmark data frame minimal

  # Make the data tall
  if (!data.tall) {
    # TODO
    # ## This shouldn't be needed except in weird scenarios, but occasionally you end up with the string <Null> where you shouldn't.
    # ## This is likely the result of exporting an attribute table from a geodatabase to a spreadsheet, converting that to a .csv, then reading it in and converting it to an SPDF
    # data.tall$Value[data.tall$Value == "<Null>"] <- NA
    #
  }

  ## Check that the variable exist in the inputs
  benchmark.missing.variables <- c(benchmark.indicatorfield, benchmark.evalstringfield, benchmark.groupfield, benchmark.categoryfield, benchmark.metagroupfield)[!(c(benchmark.indicatorfield, benchmark.evalstringfield, benchmark.groupfield, benchmark.categoryfield, benchmark.metagroupfield) %in% names(benchmarks))]
  if (length(benchmark.missing.variables) >  0) {
    stop(paste("The following required variables are missing from the benchmark data frame:", paste(benchmark.missing.variables, collapse = ", ")))
  }
  data.missing.variables <- c(data.indicatorfield, data.groupfield)[!(c(data.indicatorfield, data.groupfield) %in% names(data))]
  if (length(data.missing.variables) >  0) {
    stop(paste("The following required variables are missing from the data data frame:", paste(data.missing.variables, collapse = ", ")))
  }

  # Pare down the benchmarks
  benchmarks <- distinct(benchmarks[,c(benchmark.indicatorfield, benchmark.evalstringfield, benchmark.groupfield, benchmark.categoryfield, benchmark.metagroupfield)])

  if (use.indicator.lut) {
    if (is.null(indicator.lut)) {
      indicator.lut <- indicator.lookup()
    }

    # Make sure that the the variables are there
    lut.missing.variables <- c(lut.data.indicatorfield, lut.benchmark.indicatorfield)[!(c(lut.data.indicatorfield, lut.benchmark.indicatorfield) %in% names(indicator.lut))]
    if (length(lut.missing.variables) >  0) {
      stop(paste("The following expected variables are missing from the indicator lookup table:", paste(lut.missing.variables, collapse = ", ")))
    }

    # # Warn about indicators that show up in data and benchmarks but not the lookup table
    # missing.dataindicators <- data[data[[data.indicatorfield]] %in% indicator.lut[[lut.data.indicatorfield]], data.indicatorfield]
    # missing.benchmarkindicators <- benchmarks[benchmarks[[benchmark.indicatorfield]] %in% indicator.lut[[lut.benchmark.indicatorfield]], benchmark.indicatorfield]
    #
    # if (length(missing.dataindicators) > 0) {
    #   message("The following indicators in the data do not appear in the lookup table:")
    #   message(paste(unique(missing.dataindicators), collapse = ", "))
    # }
    # if (length(missing.benchmarkindicators) > 0) {
    #   message("The following indicators in the benchmarks do not appear in the lookup table:")
    #   message(paste(unique(missing.benchmarkindicators), collapse = ", "))
    # }

    data.benchmarked <- merge(x = merge(x = data,
                                        y = indicator.lut,
                                        by.x = data.indicatorfield,
                                        by.y = lut.data.indicatorfield),
                              y = benchmarks,
                              by.x = c(lut.benchmark.indicatorfield, data.groupfield),
                              by.y = c(benchmark.indicatorfield, benchmark.groupfield))

  } else {
    data.benchmarked <- merge(x =data,
                              y = benchmarks,
                              by.x = c(data.indicatorfield, data.groupfield),
                              by.y = c(benchmark.indicatorfield, benchmark.groupfield))
  }

  # Evaluate the inequalities into a data frame
  inequalities <- data.frame(lapply(benchmark.evalstringfield,
                                    data.benchmarked = data.benchmarked,
                                    data.valuefield = data.valuefield,
                                    FUN = function(X,
                                                   data.benchmarked,
                                                   data.valuefield){
                                      # For each inequality variable, sub the indicator values in for the character x in the relevant inequality string and evaluate it
                                      mapply(string = data.benchmarked[[X]],
                                             value = data.benchmarked[[data.valuefield]],
                                             FUN = function(string, value){
                                               evalstring <- gsub(string, pattern = "x", replacement = value, ignore.case = TRUE)
                                               return(eval(parse(text = evalstring)))
                                             })
                                    }))



  # Slice the benchmarked data to just the rows where all the inequalities evaluated to TRUE
  output <- data.benchmarked[sapply(X = 1:nrow(inequalities),
                                              inequalities = inequalities,
                                              FUN = function(X, inequalities){
                                                all(unlist(inequalities[X,]))
                                              }),]


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

