#' Reading in the benchmarks from the Data Explorer
#'
#' @param filename Character string. The filename, including filetype extension, of the .XLSX, .CSV, .XLSM, or .XLS containing the benchmarks. Expects to find columns with headers matching "Management Question", "Benchmark Source", "Benchmark Group", "Reporting Unit", "Lower Limit", "LL Relation", "Indicator", "UL Relation", "Upper Limit", "Unit", "Condition Category", "Proportion Relation", and "Required Proportion".
#' @param filepath Optional character string. The filepath to the location where the file matching \code{filename} is stored. Used to locate the file by combining with the filename. Will be ignored if \code{NULL}. Defaults to \code{NULL}.
#' @param sheet_name Optional character string. The sheet name of the spreadsheet in the Excel workbook specified by \code{filename}. Only used if \code{filename} is an Excel workbook. Defaults to \code{"Monitoring Objectives"}.
#' @param eval_strings Optional list of character vectors. If \code{NULL}, nothing will be done. Otherwise, each character vector should contain one or more variable/column names from the benchmarks. The only other string allowed is \code{"x"} which can be used as a placeholder for indicator values. As an example, \code{list(c("lower.relationship", "lower.limit", "x"))} would add a column to the output called \code{"evalstring1"} that contains the results of \code{paste(benchmarks$lower.limit, benchmarks$lower.relationship, "x")}. Defaults to \code{list(c("Lower.Limit", "LL.Relation", "x"), c("x", "UL.Relation", "Upper.Limit"), c("x", "Required.Relation", "Required.Proportion"))}, appropriate for use with the AIM "Benchmark Tool".
#' @return A data frame of the benchmarks from the specified file with fields containing evaluation strings to use in testing indicator values against the benchmarks.
#' @examples
#' read_benchmarks()
#' @export

## TODO: Add capitalization sanitization stuff
read_benchmarks <- function(filename = "",
                            filepath = NULL,
                            sheet_name = "Monitoring Objectives",
                            eval_strings = list(c("Lower_Limit", "LL_Relation", "x"),
                                                c("x", "UL_Relation", "Upper_Limit"),
                                                c("x", "Proportion_Relation", "Required_Proportion"))){
  ## Check for the file extension
  file_extension <- toupper(tools::file_ext(filename))
  if (!(file_extension %in% c("CSV", "XLS", "XLSX"))) {
    stop("The benchmark filename needs to have a valid file extension (XLSX, CSV, XLS). The most likely extension is XLSX.")
  }

  # Make the full filepath
  if (is.null(filepath)) {
    filepath <- filename
  } else {
    filepath <- file.path(filepath,
                          filename)
  }

  # Check to see if it exists
  if (file.exists(filepath)) {
    if (file_extension == "CSV") {
      benchmarks_raw <- read.csv(filepath,
                                 stringsAsFactors = FALSE)
    } else {
      benchmarks_raw <- readxl::read_excel(path = filepath,
                                           sheet = sheet_name)
      # Check to make sure that there's not a weird row up top we need to skip (very possible with the way the workbooks get formatted)
      if (!all(c("INDICATOR", "UNIT") %in% toupper(names(benchmarks_raw)))){
        benchmarks_raw <- readxl::read_excel(path = filepath,
                                             sheet = sheet_name,
                                             skip = 1)
      }
    }
    ## Change all the header names to use "_" instead of " " for consistency
    names(benchmarks_raw) <- sttingr::str_replace_all(string = names(benchmarks_raw),
                                                      pattern = " ",
                                                      replacement = "_")

    if (!all(c("INDICATOR", "UNIT") %in% toupper(names(benchmarks_raw)))){
      stop("Can't find the expected column headers in the provided benchmark file. Check to make sure that there are no non-header rows before the headers.")
    }
  } else {
    stop(paste("Can't find the benchmark file at", filepath))
  }

  ## Strip out the extraneous columns and rows, which includes if they left the example in there. The pattern to look for is "e.g"
  benchmarks <- dplyr::select(.data = benchmarks,
                              -tidyselect::matches(match = "__\\d+$")) |>
    dplyr::filter(.data = _,
                  !stringr::str_detect(string = Management_Question,
                                       pattern = "^[Ee].g."),
                  !is.na(Indicator))

  ## Create the evaluations strings if asked to!
  if (!is.null(eval_strings)) {
    # Figure out if any are missing (the character "x" is fine though because it's the indicator stand-in)
    varnames <- unlist(eval_strings)[unlist(eval_strings) != "x"]
    missing_varnames <- varnames[!(varnames %in% names(benchmarks))]
    if (length(missing.varnames) > 0) {
      stop(paste("The following expected variables for constructing eval strings are missing:", paste0(missing.varnames, collapse = ", ")))
    }
    # If none were missing, rename each of the vectors with "evalstring" and a suffix number. This will be their variable names in the data frame
    names(eval_strings) <- paste0("evalstring", 1:length(eval_strings))
    # Construct the strings. For each vector in the list
    strings <- lapply(X = eval_strings,
                      benchmarks = benchmarks,
                      FUN = function(X, benchmarks) {
                        # For each character string in the vector, grab the values in the matching variable in the benchmarks (or just return the "x")
                        vectors <- lapply(X = X,
                                          benchmarks = benchmarks,
                                          FUN = function(X, benchmarks) {
                                            if (X %in% names(benchmarks)) {
                                              return(benchmarks[[X]])
                                            } else {
                                              return(X)
                                            }
                                          })
                        # Paste them together. This gnarly eval(parse()) business is to do it regardless of how many components there are
                        output <- eval(parse(text = paste0("paste(", paste0("vectors[[", 1:length(vectors), "]]", collapse = ", "), ")")))
                        return(output)
                      })
    # Add the strings as variables to the benchmarks data frame
    benchmarks <- dplyr::bind_cols(benchmarks,
                                   data.frame(strings,
                                              stringsAsFactors = FALSE))

    names(benchmarks)[ncol(benchmarks)] <- "evalstring_threshhold"
  }

  return(benchmarks)
}
