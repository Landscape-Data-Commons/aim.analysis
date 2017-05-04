#' Reading in the benchmarks from the Data Explorer
#'
#' @param data.path A string specifying the path to the folder containing the .XLSX with the benchmarks
#' @param benchmarks.filename A string specifying the filename of the .XLSX containing the benchmarks
#' @param indicator.lut A data frame with the column \code{"indicator.name"} matching the values in the Data Explorer "Indicator" field and one called \code{"indicator.tdat"} with corresponding value for the indicators' names in TerrADat.
#' @param indicator.lut.benchmarkfield The name of the column in \code{indicator.lut} that matches the "Indicator column of Data Explorer. Defaults to \code{"indicator.name"}
#' @return A data frame of the benchmarks from the Data Explorer with a field containing an evaluation string to use in testing indicator values against the benchmarks.
#' @examples
#' read.benchmarks()
#' @export

## TODO: Add capitalization sanitization stuff
read.benchmarks <- function(data.path = "",
                            benchmarks.filename = "",
                            sheet.name = "Monitoring Objectives",
                            indicator.lut = NULL,
                            indicator.lut.benchmarkfield = "indicator.name",
                            convert.l2r = T
){
  ## Sanitizing inputs because users can't be trusted
  benchmarks.filename <- sanitize(benchmarks.filename, "xlsx")

  if (is.null(indicator.lut)) {
    indicator.lut <- data.frame(
      indicator.tdat = c('BareSoilCover_FH', 'TotalFoliarCover_FH', 'GapPct_25_50', 'GapPct_51_100', 'GapPct_101_200', 'GapPct_200_plus', 'GapPct_25_plus', 'NonInvPerenForbCover_AH', 'NonInvAnnForbCover_AH', 'NonInvPerenGrassCover_AH', 'NonInvAnnGrassCover_AH', 'NonInvAnnForbGrassCover_AH', 'NonInvPerenForbGrassCover_AH', 'NonInvSucculentCover_AH', 'NonInvShrubCover_AH', 'NonInvSubShrubCover_AH', 'NonInvTreeCover_AH', 'InvPerenForbCover_AH', 'InvAnnForbCover_AH', 'InvPerenGrassCover_AH', 'InvAnnGrassCover_AH', 'InvAnnForbGrassCover_AH', 'InvPerenForbGrassCover_AH', 'InvSucculentCover_AH', 'InvShrubCover_AH', 'InvSubShrubCover_AH', 'InvTreeCover_AH', 'SagebrushCover_AH', 'WoodyHgt_Avg', 'HerbaceousHgt_Avg', 'SagebrushHgt_Avg', 'OtherShrubHgt_Avg', 'NonInvPerenGrassHgt_Avg', 'InvPerenGrassHgt_Avg', 'InvPlantCover_AH', 'InvPlant_NumSp', 'SoilStability_All', 'SoilStability_Protected', 'SoilStability_Unprotected', 'HerbLitterCover_FH', 'WoodyLitterCover_FH', 'TotalLitterCover_FH', 'RockCover_FH', 'BiologicalCrustCover_FH', 'VagrLichenCover_FH', 'LichenMossCover_FH', 'DepSoilCover_FH', 'WaterCover_FH', 'NonInvPerenForbCover_FH', 'NonInvAnnForbCover_FH', 'NonInvPerenGrassCover_FH', 'NonInvAnnGrassCover_FH', 'NonInvSucculentCover_FH', 'NonInvShrubCover_FH', 'NonInvSubShrubCover_FH', 'NonInvTreeCover_FH', 'InvPerenForbCover_FH', 'InvAnnForbCover_FH', 'InvPerenGrassCover_FH', 'InvAnnGrassCover_FH', 'InvSucculentCover_FH', 'InvShrubCover_FH', 'InvSubShrubCover_FH', 'InvTreeCover_FH', 'SageBrushCover_FH'),
      indicator.name = c('Bare Soil (%)', 'Foliar Cover (%)', 'Percent in Gaps 25-50 cm', 'Percent in Gaps 51-100 cm', 'Percent in Gaps 101-200 cm', 'Percent in Gaps > 200 cm', 'Percent in Gaps > 25 cm', 'Non-invasive Perennial Forb Cover (%, any hit)', 'Non-invasive Annual Forb Cover (%, any hit)', 'Non-invasive Perennial Grass Cover (%, any hit)', 'Non-invasive Annual Grass Cover (%, any hit)', 'Non-invasive Annual Forb/Grass Cover (%, any hit)', 'Non-invasive Perennial Forb/Grass Cover (%, any hit)', 'Non-invasive Succulent Cover (%, any hit)', 'Non-invasive Shrub Cover (%, any hit)', 'Non-invasive Sub-shrub Cover (%, any hit)', 'Non-invasive Tree Cover (%, any hit)', 'Invasive Perennial Forb Cover (%, any hit)', 'Invasive Annual Forb Cover (%, any hit)', 'Invasive Perennial Grass Cover (%, any hit)', 'Invasive Annual Grass Cover (%, any hit)', 'Invasive Annual Forb/Grass Cover (%, any hit)', 'Invasive Perennial Forb/Grass Cover (%, any hit)', 'Invasive Succulent Cover (%, any hit)', 'Invasive Shrub Cover (%, any hit)', 'Invasive Sub-shrub Cover (%, any hit)', 'Invasive Tree Cover (%, any hit)', 'Sagebrush Cover (%, any hit)', 'Average Woody Height (cm)', 'Average Herbaceous Height (cm)', 'Average Sagebrush Height (cm)', 'Averge Non-Sagebrush Shrub Height (cm)', 'Average Non-invasive Perennial Grass Height (cm)', 'Average Invasive Perennial Grass Height (cm)', 'Invasive Plant Cover (%, any hit)', 'Number of Invasive Plant Species', 'Overall Soil Stability Rating', 'Protected Surface Soil Stability Rating', 'Unprotected Surface Soil Stability Rating', 'Between-Canopy Herbaceous Litter Cover (%)', 'Between-Canopy Woody Litter (%)', 'Between-Canopy Herbaceous and Woody Litter (%)', 'Between-Canopy Rock Cover (%)', 'Between-Canopy Biological Crust Cover (%)', 'Between-Canopy Vagrant Lichen Cover (%)', 'Between-Canopy Lichen and Moss Cover (%)', 'Between-Canopy Deposited Soil Cover (%)', 'Between-Canopy Water Cover (%)', 'Non-invasive Perennial Forb Cover (%, first hit)', 'Non-invasive Annual Forb Cover (%, first hit)', 'Non-invasive Perennial Grass Cover (%, first hit)', 'Non-invasive Annual Grass Cover (%, first hit)', 'Non-invasive Succulent Cover (%, first hit)', 'Non-invasive Shrub Cover (%, first hit)', 'Non-invasive Sub-shrub Cover (%, first hit)', 'Non-invasive Tree Cover (%, first hit)', 'Invasive Perennial Forb Cover (%, first hit)', 'Invasive Annual Forb Cover (%, first hit)', 'Invasive Perennial Grass Cover (%, first hit)', 'Invasive Annual Grass Cover (%, first hit)', 'Invasive Succulent Cover (%, first hit)', 'Invasive Shrub Cover (%, first hit)', 'Invasive Sub-shrub Cover (%, first hit)', 'Invasive Tree Cover (%, first hit)', 'Sagebrush Cover (%, first hit)')
    )
  }

  ## Import the spreadsheet from the workbook. Should work regardless of presence/absence of other spreadsheets as long as the name is the same
  benchmarks.raw <- readxl::read_excel(path = paste0(data.path, "/", benchmarks.filename),
                                       sheet = sheet.name)

  names(benchmarks.raw) <- names(benchmarks.raw) %>% stringr::str_replace_all(pattern = " ", replacement = "\\.")

  ## In case there's a "Classification" column where we'd prefer a "Category" column. This lets us maintain backwards compatibility with older iterations of the spreadsheet
  names(benchmarks.raw)[names(benchmarks.raw) %in% c("Classification")] <- "Evaluation.Category"

  ## Strip out the extraneous columns and rows, which includes if they left the example in there
  benchmarks <- benchmarks.raw[!grepl(x = benchmarks.raw$Management.Question, pattern = "^[Ee].g.") & !is.na(benchmarks.raw$Indicator), 1:12]

  if (convert.l2r) {
    benchmarks$LL.Relation[benchmarks$LL.Relation == ">="] <- "<"
    benchmarks$LL.Relation[benchmarks$LL.Relation == ">"] <- "<="
  }

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

#' Importing Sample Design Databases for AIM Sample Designs
#'
#' This function imports one or more Sample Design Database[s] and returns a list of the named lists sf, pts, and strata. The named lists contain SpatialPoints/PolygonsDataFrames of the sample frame, points, and strata from each of the geodatabases. The SPDFs are named using the filename of the geodatabase source, so that each list has one SPDF named for each geodatabase imported and those names are identical between lists. If a Sample Design Database is missing any one of those features, a NULL value replaces the SPDF.
#' @param src Character string defining the filepath containing the sample design database[s].
#' @param dd.src Character string or character vector containing the filenames of the geodatabases to import. Each filename should include the extension ".gdb"
#' @param func Character string. Defines whether to use the \code{rgdal::} or \code{arcgisbinding::} package to read in the geodatabases. Defaults to \code{"arcgisbinding"}. Valid values are \code{"arcgisbinding"} and \code{"readogr"}. This is not case sensitive
#' @param projection \code{sp::CRS()} argument. Defaults to NAD83 with \code{sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")}. Is used to reproject all SPDFs in order to perform spatial manipulations
#' @keywords weights
#' @examples
#' weighter()
#' @export

## TODO: Should try to handle raster location/import either within dd.reader() or as an independent function
## Reads in DDs. Returns a named list of lists of SPDFs: sf, pts, strata.
## The index order is maintained as well, so output[1][1] and output[2][1] correspond to the same DD source
## If a feature class couldn't be found, there will be a NULL instead of SPDF for that DD in the list
read.dd <- function(src = "", ## A filepath as a string
                    dd.src, ## A character string or vector of character strings with the filename[s] for the relevant .gdb in the filepath src
                    func = "arcgisbinding", ## This can be "readOGR" or "arcgisbinding" depending on which you prefer to or can use
                    # validate.keys = T, ## Should the process also produce a data frame in the output of points in the design dtabases that have issues with final designations or TerrAdat primary keys?
                    target.values = c("Target Sampled",
                                      "TS"),
                    omitNAdesignations = F, ## Strip out plots with a final designation value of NA
                    projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83
){

  ## readOGR() wrapped in safely() so that it will return NULL instead of an error
  safe.readOGR <- safely(readOGR, otherwise = NULL)

  ## Sanitization
  func <- str_to_upper(func)
  target.values <- c(target.values,
                     "Target Sampled",
                     "TS") %>% unique() %>% str_to_upper()

  ## Checking that func is a valid value
  if (!(func %in% c("ARCGISBINDING", "READOGR"))) {
    print("The argument func needs to be 'arcgisbinding' or 'readOGR'")
  }

  ## Only keeping the DD filenames that actually exist in the src filepath provided
  dd.src.exist <- dd.src[dd.src %in% list.files(path = src)]

  ## Reporting the filenames it couldn't find in the folder
  if (length(dd.src) != length(dd.src.exist)) {
    print(paste0("Couldn't find the following .gdb[s]: ", paste(dd.src[!(dd.src %in% list.files(path = src))], collapse = ", ")))
  }

  switch(func,
         READOGR = {
           ## Looped so that it can execute across all the DDs in the vector (if there are more than one)
           for (s in dd.src.exist) {
             ## Read in the sample frame feature class inside the current DD.
             sf <- safe.readOGR(dsn = paste(src, s, sep = "/"),
                                layer = "Terra_Sample_Frame",
                                stringsAsFactors = F)[[1]] ## The [[]] is to get the SPDF (or NULL) out of the list returned by the safely()
             # The spTransform() is just to be safe, but probably isn't necessary
             if (!is.null(sf)) {
               sf <- spTransform(sf, projection)
             }
             ## Sanitize the column names
             names(sf@data) <- str_to_upper(names(sf@data))
             ## Stores the current sf SPDF with the name sf.[DD name]
             assign(x = paste("sf", s, sep = "."), value = sf)

             #Read in the Strata
             strata <- safe.readOGR(dsn = paste(src, s, sep = "/"),
                                    layer = "Terra_Strtfctn",
                                    stringsAsFactors = F)[[1]]
             if (!is.null(strata)) {
               strata <- spTransform(strata, projection)
               names(strata@data) <- str_to_upper(names(strata@data))
             }
             assign(x = paste("strata", s, sep = "."), value = strata)

             #Read in the Points
             points <- safe.readOGR(dsn = paste(src, s, sep = "/"),
                                    layer = "Terra_Sample_Points",
                                    stringsAsFactors = F)[[1]]
             if (!is.null(points)) {
               points <- spTransform(points, projection)
             }
             names(points@data) <- str_to_upper(names(points@data))
             ## Strip out points with an NA value in the FINAL_DESIG field if asked
             if (omitNAdesignations) {
               points <- points[!is.na(points@data$FINAL_DESIG)]
             }
             assign(x = paste("pts", s, sep = "."), value = points)
           }
         },
         ARCGISBINDING = {
           for (s in dd.src.exist) {
             ## Identify/create the filepath to the sample frame feature class inside the current DD
             sf <- paste(src, s, "Terra_Sample_Frame", sep = "/")
             ## Creates an SPDF with the name sf.[DD name] using the filepath to that feature class
             assign(x = paste("sf", s, sep = "."),
                    value = sf %>% arc.open() %>% arc.select %>%
                      SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .) %>% spTransform(projection)
             )
             eval(parse(text = paste0("names(", paste("sf", s, sep = "."), ") <- str_to_upper(names(", paste("sf", s, sep = "."), "))")))

             #Read in the Strata
             #first check for strata
             ## Identify/create the filepath to the design stratification feature class inside the current DD
             strata <- paste(src, s, "Terra_Strtfctn", sep = "/")
             #this loads enough of the feature class to tell if there are strata
             strata <- strata %>% arc.open() %>% arc.select
             #check for strata, if there are, then we will finish loading the file.
             if (nrow(strata) > 0) {
               ## Identify/create the filepath to the design stratification feature class inside the current DD
               strata <- paste(src, s, "Terra_Strtfctn", sep = "/")
               ## Creates an SPDF with the name strat.[DD name] using the filepath to that feature class
               assign(x = paste("strata", s, sep = "."),
                      value = strata %>% arc.open() %>% arc.select %>%
                        SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()},
                                                 data = .) %>% spTransform(projection))
               eval(parse(text = paste0("names(", paste("strata", s, sep = "."), ") <- str_to_upper(names(", paste("strata", s, sep = "."), "))")))

             } else {
               ## If the stratification feature class is empty, we'll just save ourselves some pain and store NULL
               assign(x = paste("strata", s, sep = "."),
                      value = NULL)
             }

             #Read in the Points
             ## Identify/create the filepath to the design points feature class inside the current DD
             pts <- paste(src, s, "Terra_Sample_Points", sep = "/")
             ## Creates an SPDF with the name pts.[DD name] using the filepath to that feature class
             assign(x = paste("pts", s, sep = "."),
                    value = pts %>% arc.open() %>% arc.select %>%
                      #read in the feature class, notice the difference between Polygons and points (different function with different arguments needs)
                      SpatialPointsDataFrame(coords = {arc.shape(.) %>% arc.shape2sp()}) %>% spTransform(projection))
             eval(parse(text = paste0("names(", paste("pts", s, sep = "."), ") <- str_to_upper(names(", paste("pts", s, sep = "."), "))")))
             ## Strip out points with an NA value in the FINAL_DESIG field if asked
             if (omitNAdesignation) {
               eval(parse(text = paste0(paste("pts", s, sep = "."), " <- ", paste("pts", s, sep = "."), "[!is.na(", paste("pts", s, sep = "."), "@data$FINAL_DESIG)]")))
             }
           }
         }
  )

  ## Create a list of the sample frame SPDFs.
  ## This programmatically create a string of the existing object names that start with "sf." separated by commas
  ## then wraps that in "list()" and runs the whole string through parse() and eval() to execute it, creating a list from those SPDFs
  sf.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^sf\\.") & !grepl(x = ls(), pattern = "^sf.list$")], collapse = "`, `"), "`)")))
  ## Rename them with the correct DD name because they'll be in the same order that ls() returned them earlier. Also, we need to remove sf.list itself
  names(sf.list) <- ls()[grepl(x = ls(), pattern = "^sf\\.") & !grepl(x = ls(), pattern = "^sf.list$")] %>% str_replace(pattern = "^sf\\.", replacement = "")

  ## Creating the named list of all the pts SPDFs created by the loop
  pts.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")], collapse = "`, `"), "`)")))
  names(pts.list) <- ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")] %>% str_replace(pattern = "^pts\\.", replacement = "")

  ## Creating the named list of all the strata SPDFs created by the loop
  strata.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^strata\\.") & !grepl(x = ls(), pattern = "^strata.list$")], collapse = "`, `"), "`)")))
  names(strata.list) <- ls()[grepl(x = ls(), pattern = "^strata\\.") & !grepl(x = ls(), pattern = "^strata.list$")] %>% str_replace(pattern = "^strata\\.", replacement = "")

  output <- list(sf = sf.list, pts = pts.list, strata = strata.list)

  # if (validate.keys) {
  #   key.errors.df <- validate.keys(output)
  #
  #   ## Append this to the output list
  #   output <- list(output, "errors" = key.errors.df)
  # }

  return(output)
}

#' Reading in an exported TerrADat geodatabase
#'
#' @description Reads in the terrestrial and remote sensing indicator feature classes from an ESRI geodatabase exported from the BLM NOC TerrADat. Returns a single SpatialPointsDataFrame containing the data from both.
#' @param tdat.path A string of the folder path that contains the \code{.gdb}.
#' @param tdat.name A string of the filename of the geodatabase to import from
#' @export
read.tdat <- function(tdat.path, tdat.name){
  tdat.name <- sanitize(tdat.name, "gdb")
  tdat.terrestrial.spdf <- readOGR(dsn = paste0(tdat.path, "/", tdat.name), layer = "SV_IND_TERRESTRIALAIM", stringsAsFactors = F)
  tdat.remote.spdf <- readOGR(dsn = paste0(tdat.path, "/", tdat.name), layer = "SV_IND_REMOTESENSING", stringsAsFactors = F)
  tdat.spdf <- sp::merge(tdat.terrestrial.spdf, tdat.remote.spdf)
  return(tdat.spdf)
}
