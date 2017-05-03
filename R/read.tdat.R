#' Reading in an exported TerrADat geodatabase
#' @export
read.tdat <- function(tdat.path, tdat.name){
  tdat.name <- sanitize(tdat.name, "gdb")
  tdat.terrestrial.spdf <- readOGR(dsn = paste0(tdat.path, "/", tdat.name), layer = "SV_IND_TERRESTRIALAIM", stringsAsFactors = F)
  tdat.remote.spdf <- readOGR(dsn = paste0(tdat.path, "/", tdat.name), layer = "SV_IND_REMOTESENSING", stringsAsFactors = F)
  tdat.spdf <- sp::merge(tdat.terrestrial.spdf, tdat.remote.spdf)
  return(tdat.spdf)
}
