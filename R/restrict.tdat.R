#' Restricting imported TerrADat data to the boundaries of the imported design databases
#' @export
restrict.tdat <- function(dd.raw, tdat.spdf){
  ## NAD83 sp::CRS()
  nad83.prj <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
  tdat.prj <- tdat.spdf@proj4string
  for (design in names(dd.raw$sf)) {
    assign(value = attribute.shapefile(spdf1 = tdat.spdf,
                                       spdf2 = dd.raw$sf[[design]],
                                       attributefield = "TERRA_SAMPLE_FRAME_ID",
                                       newfield = "SampleFrame")  %>% spTransform(nad83.prj),
           x = paste0("temp", ".", grep(x = names(dd.raw$sf), pattern = paste0("^", design, "$")))
    )
  }
  if (length(ls()[grepl(x = ls(), pattern = "^temp.\\d{1,3}$")]) > 1) {
    tdat.spdf.restricted <- subset(tdat.spdf %>% spTransform(nad83.prj), "SiteID" == "Nothing")
    for (spdf in ls()[grepl(x = ls(), pattern = "^temp.\\d{1,3}$")]) {
      tdat.spdf.restricted <- rbind(tdat.spdf.restricted, get(spdf))
    }
  } else {
    tdat.spdf.restricted <- get(ls()[grepl(x = ls(), pattern = "^temp.\\d{1,3}$")]) %>% spTransform(nad83.prj)
  }
  tdat.spdf.restricted@data <- tdat.spdf.restricted@data[, names(tdat.spdf.restricted@data)[!(names(tdat.spdf.restricted@data) %in% "SampleFrame")]]
  tdat.spdf.restricted <- tdat.spdf.restricted@data %>% dplyr::distinct() %>%
    SpatialPointsDataFrame(data = .,
                           coords = .[, c("coords.x1", "coords.x2")],
                           proj4string = tdat.prj)
  tdat.spdf.restricted <- tdat.spdf.restricted %>% spTransform(nad83.prj)
  return(tdat.spdf.restricted)
}
