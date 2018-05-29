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
  benchmark.missing.variables <- c(benchmark.indicatorfield, benchmark.evalstringfield, benchmark.groupfield, benchmark.categoryfield, benchmark.metagroupfield)[c(benchmark.indicatorfield, benchmark.evalstringfield, benchmark.groupfield, benchmark.categoryfield, benchmark.metagroupfield) %in% names(benchmarks)]
  if (length(benchmark.missing.variables) >  0) {
    stop(paste("The following required variables are missing from the benchmark data frame:", paste(benchmark.missing.variables, collapse = ", ")))
  }
  data.missing.variables <- c(data.indicatorfield, data.groupfield)[c(data.indicatorfield, data.groupfield) %in% names(data)]
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
    lut.missing.variables <- c(lut.data.indicatorfield, lut.benchmark.indicatorfield)[c(lut.data.indicatorfield, lut.benchmark.indicatorfield) %in% names(indicator.lut)]
    if (length(lut.missing.variables) >  0) {
      stop(paste("The following expected variables are missing from the indicator lookup table:", paste(lut.missing.variables, collapse = ", ")))
    }

    # Warn about indicators that show up in data and benchmarks but not the lookup table
    missing.dataindicators <- data[data[[data.indicatorfield]] %in% indicator.lut[[lut.data.indicatorfield]], data.indicatorfield]
    missing.benchmarkindicators <- benchmarks[benchmarks[[benchmark.indicatorfield]] %in% indicator.lut[[lut.benchmark.indicatorfield]], benchmark.indicatorfield]

    if (length(missing.dataindicators) > 0) {
      message("The following indicators in the data do not appear in the lookup table:")
      message(paste(missing.dataindicators, collapse = ", "))
    }
    if (length(missing.benchmarkindicators) > 0) {
      message("The following indicators in the benchmarks do not appear in the lookup table:")
      message(paste(missing.benchmarkindicators, collapse = ", "))
    }

    data.benchmarked <- merge(x = merge(x = data,
                                        y = indicator.lut,
                                        by.x = data.indicatorfield,
                                        by.y = lut.data.indicatorfield),
                              y = benchmarks,
                              by.x = lut.benchmark.indicatorfield,
                              by.y = benchmark.indicatorfield)

  } else {
    data.benchmarked <- merge(x =data,
                              y = benchmarks,
                              by.x = data.indicatorfield,
                              by.y = benchmark.indicatorfield)
  }

  # Evaluate the inequalities into a data frame
  inequalities <- data.frame(lapply(benchmark.evalstringfield, data.benchmarked = data.benchmarked,
         FUN = function(X, data.benchmarked){
           # For each inequality variable, sub the indicator values in for the character x in the relevant inequality string and evaluate it
           mapply(data.benchmarked[[data.valuefield]], data.benchmarked[[X]],
                  FUN = function(string, value){
                    evalstring <- gsub(string, pattern = "x", replacement = value, ignore.case = TRUE)
                    return(eval(parse(text = evalstring)))
                  })
         }))



  # Slice the benchmarked data to just the rows where all the inequalities evaluated to TRUE
  data.benchmarked <- data.benchmarked[, sapply(1:nrow(inequalities), inequalities = inequalities,
                                                FUN = function(row, inequalities){
                                                  all(inequalities[row,])
                                                })]


  return(data.benchmarked)
}
