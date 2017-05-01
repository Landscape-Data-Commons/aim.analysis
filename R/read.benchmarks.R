#' Reading in the benchmarks from the Data Explorer
#'
#' @param data.path A string specifying the path to the folder containing the .XLSX with the benchmarks
#' @param benchmarks.filename A string specifying the filename of the .XLSX containing the benchmarks
#' @param indicator.lut A data frame with the column \code{"indicator.name"} matching the values in the Data Explorer "Indicator" field and one called \code{"indicator.tdat"} with corresponding value for the indicators' names in TerrADat.
#' @param indicator.lut.benchmarkfield The name of the column in \code{indicator.lut} that matches the "Indicator column of Data Explorer. Defaults to \code{"indicator.name"}
#' @return A data frame of the benchmarks from the Data Explorer with a field containing an evaluation string to use in testing indicator values against the benchmarks.
#' @example
#' read.benchmarks()

## TODO: Add capitalization sanitization stuff
read.benchmarks <- function(data.path = "",
                            benchmarks.filename = "",
                            indicator.lut,
                            indicator.lut.benchmarkfield = "indicator.name"
){
  ## Sanitizing inputs because users can't be trusted
  if (!grepl(x = data.path, pattern = "/$")) {
    data.path <- paste0(data.path, "/")
  }
  if (!grepl(x = benchmarks.filename, pattern = "\\.(XLSX)|(xlsx)$")) {
    benchmarks.filename <- paste0(benchmarks.filename, ".xlsx")
  }
  ## Import the spreadsheet from the workbook. Should work regardless of presence/absence of other spreadsheets as long as the name is the same
  benchmarks.raw <- read.xlsx(file = paste0(data.path, benchmarks.filename),
                              sheetName = "Monitoring Objectives",
                              header = T,
                              stringsAsFactors = F)

  ## In case there's a "Classification" column where we'd prefer a "Category" column. This lets us maintain backwards compatibility with older iterations of the spreadsheet
  names(benchmarks.raw)[names(benchmarks.raw) %in% c("Classification")] <- "Evaluation.Category"

  ## Strip out the extraneous columns and rows, which includes if they left the example in there
  benchmarks <- benchmarks.raw[!grepl(x = benchmarks.raw$Management.Question, pattern = "^[Ee].g.") & !is.na(benchmarks.raw$Indicator), 1:12]

  ## Create the evaluations for the upper and lower limits of each benchmark.
  ## The way the spreadsheet is configured, there should be no rows without both defined
  benchmarks$eval.string.lower <- paste(benchmarks$Lower.Limit, benchmarks$LL.Relation)
  benchmarks$eval.string.upper <- paste(benchmarks$UL.Relation, benchmarks$Upper.Limit)

  ## Assume that the upper limit is infinity for places where there's a lower limit but not an upper
  benchmarks$eval.string.upper[!is.na(benchmarks$Lower.Limit) & !is.na(benchmarks$LL.Relation) & is.na(benchmarks$UL.Relation) & is.na(benchmarks$Upper.Limit)] <- "< Inf"
  ## Assume that the lower limit is negative infinity for places where there's an upper limit but not a lower
  benchmarks$eval.string.lower[is.na(benchmarks$Lower.Limit) & is.na(benchmarks$LL.Relation) & !is.na(benchmarks$UL.Relation) & !is.na(benchmarks$Upper.Limit)] <- "-Inf <"

  ## Create an evaluation string for future use with the required proportion and its relationship
  benchmarks$eval.string.proportion[!is.na(benchmarks$Required.Proportion)] <- paste(benchmarks$Proportion.Relation[!is.na(benchmarks$Required.Proportion)], benchmarks$Required.Proportion[!is.na(benchmarks$Required.Proportion)])

  ## For each benchmark add in the name of the field in TerrADat that corresponds
  benchmarks <- merge(x = benchmarks, y = indicator.lut, by.x = "Indicator", by.y = indicator.lut.benchmarkfield)

  return(benchmarks)
}
