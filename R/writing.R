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
    extension <- extension %>% stringr::str_extract(pattern = "[A-z]{3,4}$") %>% stringr::str_to_lower() %>% paste0(".", .)
  }
  filename <- paste0(name, "_", Sys.Date() %>% format("%d%m%y"), "_", type, extension)
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

#' Writing out SPDFs as shapefiles
#'
#' @description Write our an ESRI shapefile from one or more SPDFs. If multiple SPDFs are being written out, they will be combined, optionally with \code{raster::union()} in the case of SpatialPolygonsDataFrame.
#' @param spdf Either a single SPDF, a list of SPDFs, or the output from \code{read.dd()}. If multiple SPDFs are provided, they will be output as a single combined shapefile. All SPDFs must be of the same class. If not being combined with a \code{raster::union()} or if they are SpatialPointsDataFrames the \code{data} slots must have identical variables.
#' @param dd Logical. If the argument \code{spdf} is the output from \code{read.dd()}, provide \code{TRUE}. Defaults to \code{FALSE}.
#' @param dd.list A string specifying which of the three lists in dd to pull SPDFs from. Only used if \code{dd = TRUE}. Valid values are \code{"sf"}, \code{"strata"}, and \code{"pts"}. Defaults to \code{"sf"}.
#' @param union Logical. If \code{TRUE} and multiple SatialPolygonDataFrames are provided as \code{spdf} then \code{raster::union()} will be applied.
#' @param name A string of the project name for use in the filename, e.g. "Idaho_SageGrouse".
#' @param type A string of the type of output for use in the filename, e.g. "reportingunits" or "sampleframes".
#' @param out.path A string specifying the output folder path.
#' @export
write.shapefile <- function(spdf,
                            dd = FALSE,
                            dd.list = "sf",
                            union = TRUE,
                            name,
                            type,
                            out.path){

  if (dd) {
    spdf.list <- spdf[[dd.list]][[1]]
    if (length(names(spdf[[dd.list]])) > 1) {
      for (design in names(spdf[[dd.list]])[-1]) {
        spdf.list <- c(spdf.list, spdf[[dd.list]][[design]])
      }
    }
  } else {
    spdf.list <- spdf
  }


  if (class(spdf.list) == "list" & length(spdf.list) > 1) {
    if ((lapply(spdf.list, FUN = class) %>% unlist() %>% unname() %>% unique() %>% length()) != 1) {
      stop("When providing multiple SPDFs, they must either all be SpatialPolygonsDataFrames or all SpatialPointsDataFrames.")
    }
    if (union & class(spdf.list[[1]])[1] == "SpatialPolygonsDataFrame") {
      spdf.union <- spdf.list[[1]]
      for (n in 2:length(spdf.list)) {
        spdf.union <- raster::union(spdf.union, spdf.list[[n]])
      }
      output <- spdf.union
    } else {
      spdf.bind <- spdf.list[[1]]
      for (n in 2:length(spdf.list)) {
        spdf.bind <- rbind(spdf.bind, spdf.list[[n]])
      }
      output <- spdf.bind
    }
  } else {
    output <- spdf.list
  }


  rgdal::writeOGR(obj = output,
           dsn = out.path,
           layer = filename.aim(name, type),
           driver = "ESRI Shapefile",
           overwrite_layer = TRUE)
}

#' Writing out shapefiles of benchmarked points
#' @description Writes out a point shapefile for each management question of points which were classified using the benchmarks for that question. Each
#' @param points.benchmarked Data frame output from \code{benchmark()}.
#' @param tdat A Spatial Points Data Frame containing the relevant geometry and fields called \code{PLOTID} and \code{PRIMARYKEY}.
#' @param out.path A string specifying the output folder path.
#' @param name A string of the project name for use in the filename, e.g. "Idaho_SageGrouse".
#' @export
write.benchmarkshp <- function(points.benchmarked, tdat, out.path, name){
  points.benchmarked %>% dplyr::select(-VALUE) %>% split(.$MANAGEMENT.QUESTION) %>%
    purrr::map(~ tidyr::spread(data = .x, key = INDICATOR, value = EVALUATION.CATEGORY) %>%
                 merge(x = tdat.spdf %>%
                         dplyr::select(starts_with(match = "plotid", ignore.case = TRUE),
                                       starts_with(match = "primarykey", ignore.case = TRUE)) %>%
                         setNames(object = .,
                                  stringr::str_to_upper(names(.))),
                       y = .,
                       all.x = FALSE,
                       all.y = TRUE
                 ) %>%
                 dplyr::select(-starts_with(match = "coords", ignore.case = TRUE)) %>%
                 rgdal::writeOGR(obj = .,
                                 dsn = paste0(out.path),
                                 layer = filename.aim(name, type = unique(.@data$MANAGEMENT.QUESTION)),
                                 driver = "ESRI Shapefile",
                                 overwrite_layer = TRUE)
    )
  benchmark.indicators.lut <- data.frame("INDICATOR" = points.benchmarked[, "INDICATOR"], stringsAsFactors = FALSE) %>%
    dplyr::distinct() %>% dplyr::mutate(INDICATOR.ABBREVIATION = abbreviate(INDICATOR, minlength = 7))
  write.csv(benchmark.indicators.lut,
            paste0(out.path, "/",
                   filename.aim(name,
                                type = "benchmark_shapefile_lookup",
                                extension = "csv")
            )
  )
}
