#' Generating standard-format filenames for output from AIM analyses
#'
#' @description Creates an appropriate filename using the name of the project, the current date in the format DDMMYY, the type of data, and the file extension, separated by underscores.
#' @param name A string of the project name, e.g. "Idaho_SageGrouse"
#' @param type A string of the data type, e.g. "analysis" or "reportingunits"
#' @param extension A string of the file extension, e.g. "csv" or "shp". If this argument is not provided, no extension will be appended.
#' @export
filename.aim <- function(name,
                         type,
                         extension = NULL){
  if (is.null(extension)){
    extension <- ""
  } else {
    extension <- paste0(".",
                        stringr::str_to_lower(stringr::str_extract(extension,
                                                                   pattern = "[A-z]{3,4}$")))
  }
  filename <- paste0(name, "_", format(Sys.Date(), "%d%m%y"), "_", type, extension)
  return(filename)
}

#' Writing out analysis outputs as a .csv
#'
#' @description Creates a \code{.csv} file composed of the output from one or more executions of \code{analyze()}. If combining multiple analyze() outputs, do so as a list with \code{c(analyze(dataset1), analyze(dataset2), ...)}.
#' @param analysis.output Either a single output from \code{analyze()} or a list of multiple outputs, e.g. \code{list(analyze(dataset1), analyze(dataset2))}.
#' @param name A string of the project name, e.g. "Idaho_SageGrouse"
#' @param out.path A string specifying the output folder path
#' @export
write.analysis <- function(analysis.output,
                           name,
                           out.path){
  if (class(analysis.output) == "data.frame") {
    output <- analysis.output
  } else if (class(analysis.output) == "list") {
    if (length(analysis.output > 1)) {
      output <- dplyr::bind_rows(analysis.output[grep(names(analysis.output), pattern = "^analyses$")])
    } else {
      output <- analysis.output[[1]]
    }
  }

  write.csv(output, paste(out.path, filename.aim(name, "analysis", "csv"), sep = "/"))
}
