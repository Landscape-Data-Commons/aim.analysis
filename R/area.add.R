## Adds areas in hectares and/or square kilometers, by polygon ID
area.add <- function(spdf, ## SpatialPolygonsDataFrame to add area values to
                     area.ha = T, ## Add area in hectares?
                     area.sqkm = T, ## Add area in square kilometers?
                     byid = T ## Do it for the whole SPDF or on a per-polygon basis? Generally don't want to toggle this
){
  original.proj <- spdf@proj4string
  ## Make sure the SPDF is in Albers equal area projection
  spdf <- spTransform(x = spdf, CRSobj = CRS("+proj=aea"))

  ## Add the area in hectares, stripping the IDs from gArea() output
  spdf@data$AREA.HA <- gArea(spdf, byid = byid) * 0.0001 %>% unname()
  ## Add the area in square kilometers, converting from hectares
  spdf@data$AREA.SQKM <- spdf@data$AREA.HA * 0.01

  if (!(area.ha)) {
    spdf@data$AREA.HA <- NULL
  }
  if (!(area.sqkm)) {
    spdf@data$AREA.SQKM <- NULL
  }
  return(spTransform(spdf, original.proj))
}
