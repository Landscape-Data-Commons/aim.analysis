#' Reading in the benchmarks from the Data Explorer
#'
#' @param data.path A string specifying the path to the folder containing the .XLSX with the benchmarks
#' @param benchmarks.filename A string specifying the filename of the .XLSX containing the benchmarks
#' @param indicator.lut A data frame with the column \code{"indicator.name"} matching the values in the Data Explorer "Indicator" field and one called \code{"indicator.tdat"} with corresponding value for the indicators' names in TerrADat.
#' @param indicator.lut.benchmarkfield The name of the column in \code{indicator.lut} that matches the "Indicator column of Data Explorer. Defaults to \code{"indicator.name"}
#' @return A data frame of the benchmarks from the Data Explorer with a field containing an evaluation string to use in testing indicator values against the benchmarks.
#' @examples
#' read.benchmarks()

## TODO: Add capitalization sanitization stuff
read.benchmarks <- function(data.path = "",
                            benchmarks.filename = "",
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
                                       sheet = "Monitoring Objectives")

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
