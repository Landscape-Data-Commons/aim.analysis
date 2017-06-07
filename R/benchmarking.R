#' Applying indicator benchmarks to a data frame of TerrADat data.
#'
#' This evaluates the indicator values in TerrADat against the supplied benchmarks. If a plot should be evaluated in multiple evaluation strata/groups, there should be one copy of the plot per stratum/group.
#' @param benchmarks A data frame containing the benchmark information, imported from .XLSX with \code{read.benchmarks()}.
#' @param tdat A data frame imported from TerrADat's terrestrial or remote sensing indicators feature classes AND a field defining the evaluation stratum/group each plot should be considered part of, probably added using \code{attribute.shapefile()}.
#' @param evalstratumfield A string of the name of the field in \code{tdat} that contains the evaluation stratum/group identities. Defaults to \code{"Evaluation.Stratum"}.
#' @return A data frame with the variables \code{"PRIMARYKEY", "PLOTID", "MANAGEMENT.QUESTION", "EVALUATION.STRATUM", "INDICATOR", "VALUE", "EVALUATION.CATEGORY"}
#' @examples
#' benchmark()
#' @export

benchmark <- function(benchmarks, ## The data frame imported with read.benchmarks()
                        tdat, ## The data frame from TerrADat. It needs to already be attributed with evaluation strata
                        evalstratumfield = "Evaluation.Stratum" ## The field in tdat that contains the evaluation strata
){
  ## Sanitization as always
  names(benchmarks) <- stringr::str_to_upper(names(benchmarks))
  ## In case someone didn't read the instructions and fed in an SPDF
  if (class(tdat)[1] == "SpatialPointsDataFrame") {
    tdat <- tdat@data
  }
  tdat.fields.indicators.expected <- c("BareSoilCover_FH", "TotalFoliarCover_FH",
                                       "GapPct_25_50", "GapPct_51_100", "GapPct_101_200", "GapPct_200_plus", "GapPct_25_plus",
                                       "NonInvPerenForbCover_AH", "NonInvAnnForbCover_AH", "NonInvPerenGrassCover_AH", "NonInvAnnGrassCover_AH", "NonInvAnnForbGrassCover_AH", "NonInvPerenForbGrassCover_AH", "NonInvSucculentCover_AH", "NonInvShrubCover_AH", "NonInvSubShrubCover_AH", "NonInvTreeCover_AH",
                                       "InvPerenForbCover_AH", "InvAnnForbCover_AH", "InvPerenGrassCover_AH", "InvAnnGrassCover_AH", "InvAnnForbGrassCover_AH", "InvPerenForbGrassCover_AH", "InvSucculentCover_AH", "InvShrubCover_AH", "InvSubShrubCover_AH", "InvTreeCover_AH",
                                       "SagebrushCover_AH",
                                       "WoodyHgt_Avg", "HerbaceousHgt_Avg", "SagebrushHgt_Avg", "OtherShrubHgt_Avg",
                                       "NonInvPerenGrassHgt_Avg", "InvPerenGrassHgt_Avg",
                                       "InvPlantCover_AH", "InvPlant_NumSp",
                                       "SoilStability_All", "SoilStability_Protected", "SoilStability_Unprotected",
                                       ## Remote sensing values
                                       "HerbLitterCover_FH", "WoodyLitterCover_FH", "EmbLitterCover_FH", "TotalLitterCover_FH", "RockCover_FH", "BiologicalCrustCover_FH", "VagrLichenCover_FH", "LichenMossCover_FH", "DepSoilCover_FH", "WaterCover_FH",
                                       "NonInvPerenForbCover_FH", "NonInvAnnForbCover_FH", "NonInvPerenGrassCover_FH", "NonInvAnnGrassCover_FH", "NonInvSucculentCover_FH", "NonInvShrubCover_FH", "NonInvSubShrubCover_FH", "NonInvTreeCover_FH",
                                       "InvPerenForbCover_FH", "InvAnnForbCover_FH", "InvPerenGrassCover_FH", "InvAnnGrassCover_FH", "InvSucculentCover_FH", "InvShrubCover_FH", "InvSubShrubCover_FH", "InvTreeCover_FH",
                                       "SageBrushCover_FH")

  if (length(tdat.fields.indicators.expected[tdat.fields.indicators.expected %in% names(tdat)]) != length(tdat.fields.indicators.expected)) {
    message("These expected indicators weren't found in the tdat data frame:")
    message(paste(tdat.fields.indicators.expected[!(tdat.fields.indicators.expected %in% names(tdat))], collapse = ", "))
    message("All of these are being dropped from consideration and the remaining indicators are being used.")
  }
  ## Making a tall version of the TerrADat data frame
  ## Indicators listed in order of appearance in TerrADat, line breaks inserted at thematic breaks
  tdat.tall <- eval(parse(text = paste0("gather(tdat, Indicator, Value, ",
                                        paste(tdat.fields.indicators.expected[tdat.fields.indicators.expected %in% names(tdat)],collapse = ", ") %>% stringr::str_replace_all("'", ""),
                                        ")"
  )
  ))

  ## This shouldn't be needed except in weird scenarios, but occasionally you end up with the string <Null> where you shouldn't.
  ## This is likely the result of exporting an attribute table from a geodatabase to a spreadsheet, converting that to a .csv, then reading it in and converting it to an SPDF
  tdat.tall$Value[tdat.tall$Value == "<Null>"] <- NA

  ## It's not clear what this column will be called, so we'll just get it now
  eval.name.benchmarks <- names(benchmarks)[grep(x = names(benchmarks), pattern = "evaluation.group$|^evaluation.stratum", ignore.case = T)]

  ## Strip down benchmarks to just the distinct ones that matter because sometimes the same benchmark appears for multiple reasons?
  benchmarks.distinct <- distinct(benchmarks[, c("MANAGEMENT.QUESTION", eval.name.benchmarks, "INDICATOR.TDAT", "EVALUATION.CATEGORY", "EVAL.STRING.LOWER", "EVAL.STRING.UPPER")])

  ## Merge the tall TerrADat with the benchmark information
  tdat.tall.benched <- merge(x = tdat.tall,
                             y = benchmarks.distinct,
                             by.x = c(names(tdat.tall)[grepl(x = names(tdat.tall), pattern = evalstratumfield, ignore.case = T)], "Indicator"),
                             by.y = c(eval.name.benchmarks, "INDICATOR.TDAT"))

  ## Create parseable evaluation strings
  tdat.tall.benched$EVAL.STRING.LOWER <- paste0(tdat.tall.benched$EVAL.STRING.LOWER, tdat.tall.benched$Value)
  tdat.tall.benched$EVAL.STRING.UPPER <- paste0(tdat.tall.benched$Value, tdat.tall.benched$EVAL.STRING.UPPER)

  ## A parsing function for the lapply()
  parser <- function(x) {
    eval(parse(text = x))
    }
  ## Parse the strings to determing if the value falls within the upper and lower bounds for that benchmark evaluation category
  tdat.tall.benched$meeting <- lapply(tdat.tall.benched$EVAL.STRING.LOWER, parser) %>% unlist() & lapply(tdat.tall.benched$EVAL.STRING.UPPER, parser) %>% unlist()

  names(tdat.tall.benched) <- stringr::str_to_upper(names(tdat.tall.benched))

  names(tdat.tall.benched)[names(tdat.tall.benched) %in% "EVALUATION.GROUP"] <- "EVALUATION.STRATUM"

  ## Because all the benchmark evaluation categories should be mutually exclusive, applying the vector from $meeting should result in one row per indicator per plot
  ## Also restricting this to the relevant columns that are required for the next step
  output <- tdat.tall.benched[tdat.tall.benched$MEETING, c("PRIMARYKEY", "PLOTID", "MANAGEMENT.QUESTION", "EVALUATION.STRATUM", "INDICATOR", "VALUE", "EVALUATION.CATEGORY")] %>%
    filter(!is.na(PRIMARYKEY))
  names(output) <- stringr::str_to_upper(names(output))
  return(output)
}
