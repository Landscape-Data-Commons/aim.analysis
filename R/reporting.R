#' Generating summary reports.
#' @description Generate both a .PDF and .HTML report with \code{RMarkdown}.
#' @param out.path Character string. The folder path to write the output reports into.
#' @param project.name Character string. The name of the project to be used in the title of the report and the filenames it's written to.
#' @param conf.level Numeric. The confidence level expressed as an integer value, e.g. \code{80} if the confidence level is 80\%. Defaults to \code{80}.
#' @param indicator.lut Data frame. The lookup table between the indicator names in TerrADat and the human readable indicator names.
#' @param benchmarks Data frame. The output from \code{read.benchmarks()}.
#' @param analysis Data frame. The data frame \code{"analyses"} from the output of \code{analyze()}.
#' @param cats.to.suppress Character vector. One or more categories to suppress in tables. Defaults to \code{c("Not Meeting")}.
#' @param strata.stats Data frame. The data frame \code{strata.stats} from the output of \code{weight()}.
#' @param reporting.units.spdf Spatial polygons data frame. Used in plotting maps.
#' @param sample.frame.spdf Spatial polygons data frame. Used in plotting maps.
#' @param project.area.spdf Spatial polygons data frame. Used in plotting maps.
#' @param points.benchmarked Data frame. The output from \code{benchmark()}.
#' @param projection  Optional \code{sp::CRS()} argument. Used to convert \code{points.benchmarked} into a spatial points data frame. Only specify if \code{points.benchmarked} has coordinates not from the same projection as TerrADat. Defaults to \code{sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")}.
#' @export

report <- function(out.path,
                   project.name,
                   conf.level = 80,
                   indicator.lut = NULL,
                   benchmarks = NULL,
                   analysis = NULL,
                   cats.to.suppress = c("Not Meeting"),
                   strata.stats,
                   strata.weights,
                   reporting.units.spdf = NULL,
                   sample.frame.spdf,
                   project.area.spdf = NULL,
                   points.benchmarked = NULL,
                   projection = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) {

  if (is.null(indicator.lut)) {
    message("Using default indicator.lut because no alternative was provided.")
    indicator.lut <- data.frame(
      indicator.tdat = c('BareSoilCover_FH', 'TotalFoliarCover_FH', 'GapPct_25_50', 'GapPct_51_100', 'GapPct_101_200', 'GapPct_200_plus', 'GapPct_25_plus', 'NonInvPerenForbCover_AH', 'NonInvAnnForbCover_AH', 'NonInvPerenGrassCover_AH', 'NonInvAnnGrassCover_AH', 'NonInvAnnForbGrassCover_AH', 'NonInvPerenForbGrassCover_AH', 'NonInvSucculentCover_AH', 'NonInvShrubCover_AH', 'NonInvSubShrubCover_AH', 'NonInvTreeCover_AH', 'InvPerenForbCover_AH', 'InvAnnForbCover_AH', 'InvPerenGrassCover_AH', 'InvAnnGrassCover_AH', 'InvAnnForbGrassCover_AH', 'InvPerenForbGrassCover_AH', 'InvSucculentCover_AH', 'InvShrubCover_AH', 'InvSubShrubCover_AH', 'InvTreeCover_AH', 'SagebrushCover_AH', 'WoodyHgt_Avg', 'HerbaceousHgt_Avg', 'SagebrushHgt_Avg', 'OtherShrubHgt_Avg', 'NonInvPerenGrassHgt_Avg', 'InvPerenGrassHgt_Avg', 'InvPlantCover_AH', 'InvPlant_NumSp', 'SoilStability_All', 'SoilStability_Protected', 'SoilStability_Unprotected', 'HerbLitterCover_FH', 'WoodyLitterCover_FH', 'TotalLitterCover_FH', 'RockCover_FH', 'BiologicalCrustCover_FH', 'VagrLichenCover_FH', 'LichenMossCover_FH', 'DepSoilCover_FH', 'WaterCover_FH', 'NonInvPerenForbCover_FH', 'NonInvAnnForbCover_FH', 'NonInvPerenGrassCover_FH', 'NonInvAnnGrassCover_FH', 'NonInvSucculentCover_FH', 'NonInvShrubCover_FH', 'NonInvSubShrubCover_FH', 'NonInvTreeCover_FH', 'InvPerenForbCover_FH', 'InvAnnForbCover_FH', 'InvPerenGrassCover_FH', 'InvAnnGrassCover_FH', 'InvSucculentCover_FH', 'InvShrubCover_FH', 'InvSubShrubCover_FH', 'InvTreeCover_FH', 'SageBrushCover_FH'),
      indicator.name = c('Bare Soil (%)', 'Foliar Cover (%)', 'Percent in Gaps 25-50 cm', 'Percent in Gaps 51-100 cm', 'Percent in Gaps 101-200 cm', 'Percent in Gaps > 200 cm', 'Percent in Gaps > 25 cm', 'Non-invasive Perennial Forb Cover (%, any hit)', 'Non-invasive Annual Forb Cover (%, any hit)', 'Non-invasive Perennial Grass Cover (%, any hit)', 'Non-invasive Annual Grass Cover (%, any hit)', 'Non-invasive Annual Forb/Grass Cover (%, any hit)', 'Non-invasive Perennial Forb/Grass Cover (%, any hit)', 'Non-invasive Succulent Cover (%, any hit)', 'Non-invasive Shrub Cover (%, any hit)', 'Non-invasive Sub-shrub Cover (%, any hit)', 'Non-invasive Tree Cover (%, any hit)', 'Invasive Perennial Forb Cover (%, any hit)', 'Invasive Annual Forb Cover (%, any hit)', 'Invasive Perennial Grass Cover (%, any hit)', 'Invasive Annual Grass Cover (%, any hit)', 'Invasive Annual Forb/Grass Cover (%, any hit)', 'Invasive Perennial Forb/Grass Cover (%, any hit)', 'Invasive Succulent Cover (%, any hit)', 'Invasive Shrub Cover (%, any hit)', 'Invasive Sub-shrub Cover (%, any hit)', 'Invasive Tree Cover (%, any hit)', 'Sagebrush Cover (%, any hit)', 'Average Woody Height (cm)', 'Average Herbaceous Height (cm)', 'Average Sagebrush Height (cm)', 'Averge Non-Sagebrush Shrub Height (cm)', 'Average Non-invasive Perennial Grass Height (cm)', 'Average Invasive Perennial Grass Height (cm)', 'Invasive Plant Cover (%, any hit)', 'Number of Invasive Plant Species', 'Overall Soil Stability Rating', 'Protected Surface Soil Stability Rating', 'Unprotected Surface Soil Stability Rating', 'Between-Canopy Herbaceous Litter Cover (%)', 'Between-Canopy Woody Litter (%)', 'Between-Canopy Herbaceous and Woody Litter (%)', 'Between-Canopy Rock Cover (%)', 'Between-Canopy Biological Crust Cover (%)', 'Between-Canopy Vagrant Lichen Cover (%)', 'Between-Canopy Lichen and Moss Cover (%)', 'Between-Canopy Deposited Soil Cover (%)', 'Between-Canopy Water Cover (%)', 'Non-invasive Perennial Forb Cover (%, first hit)', 'Non-invasive Annual Forb Cover (%, first hit)', 'Non-invasive Perennial Grass Cover (%, first hit)', 'Non-invasive Annual Grass Cover (%, first hit)', 'Non-invasive Succulent Cover (%, first hit)', 'Non-invasive Shrub Cover (%, first hit)', 'Non-invasive Sub-shrub Cover (%, first hit)', 'Non-invasive Tree Cover (%, first hit)', 'Invasive Perennial Forb Cover (%, first hit)', 'Invasive Annual Forb Cover (%, first hit)', 'Invasive Perennial Grass Cover (%, first hit)', 'Invasive Annual Grass Cover (%, first hit)', 'Invasive Succulent Cover (%, first hit)', 'Invasive Shrub Cover (%, first hit)', 'Invasive Sub-shrub Cover (%, first hit)', 'Invasive Tree Cover (%, first hit)', 'Sagebrush Cover (%, first hit)')
    )
  }

  rmarkdown::render(input = paste0(path.package("aim.analysis"), "/markdown/report.Rmd"),
                    output_file = filename.aim(name = project.name, type = "report"),
                    output_dir = out.path)
}


## Function to lookup terradat indicator values in a table and return a real name
## It's called vlookup because it works like that function in Excel.
##  value = the value to pass to the function to lookup
##  table = the lookup table data frame
##  lcol = the lookup column
##  rcol = the return column, or column with the values you want back from the lookup function
vlookup <- function(value,
                    table,
                    lcol,
                    rcol) {
  return(table[table[[lcol]] == value, rcol])
}

## Function to programatically create output bar plots for a given indicator.
## Requires the following inputs:
##  df = data.frame of the cat.analysis results
##  ru = desired level of reporting units for the graph
##  subpop = desired specific reporting unit
##  indicator = indicator to be graphed
indicatorPlot <- function(df,
                          ru,
                          subpop,
                          indicator,
                          mq,
                          threshold = 0.5) {
  plot.data <- dplyr::filter(.data = df,
                             Type == ru,
                             Subpopulation == subpop,
                             Indicator == indicator,
                             MANAGEMENT.QUESTION == mq,
                             Category != "Total")
  ind.realname <- vlookup(indicator,indicator.lut, 1, 2) ## Lookup the pretty name for the indicator
  p <- ggplot2::ggplot(plot.data, ggplot2::aes(x = Category, y = Estimate.P, fill = Category)) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) +
    ggplot2::coord_flip() +
    ggplot2::geom_errorbar(ggplot2::aes(ymax = UCB95Pct.P, ymin = LCB95Pct.P),
                           width = 0.25) +
    ggplot2::scale_fill_brewer(type = "div", palette = "RdYlBu") +
    ggplot2::ylim(0, 100) +
    ggplot2::ggtitle(paste0("Percentage of Reporting Unit by Category: \n", ind.realname)) +
    ggplot2::ylab("Estimated Percent of Reporting Unit Area")
  if (threshold > 0) {
    p <- p + ggplot2::geom_hline(yintercept = threshold, colour = "gold", size = 2)
  }
  return(p)
}

## Function to display table of cat.analysis results. Takes same arguments as indicatorPlot function (minus the threshold argument)
indicatorTable <- function(df,
                           ru,
                           subpop,
                           indicator,
                           mq,
                           conf.level) {
  table.data <- dplyr::filter(.data = df,
                              Type == ru,
                              Subpopulation == subpop,
                              Indicator == indicator,
                              MANAGEMENT.QUESTION == mq,
                              Category != "Total") %>%
    dplyr::select(-(1:4))
  names(table.data) <- c("Category",
                         "# Points",
                         "% Area Estimate",
                         "Std. Error",
                         paste0("Lower ", conf.level,"% CI"),
                         paste0("Upper ", conf.level,"% CI"))
  return(table.data)
}

## Function to create a basic map showing the location of the reporting unit within the study area
##   Requires the following inputs:
##   level = reporting unit level
##   ru = reporting unit name
##   repunits.spdf = spatial polygon data frame for the reporting units
##   prjarea.spdf = spatial polygon data frame for the project area
indicatorMap <- function(level,
                         ru,
                         repunits.spdf,
                         prjarea.spdf,
                         samplepts.spdf) {
  runit <- repunits.spdf[repunits.spdf$Type == level & repunits.spdf$Sbppltn == ru,] # get the reporting unit
  ru.points <- sp::over(samplepts.spdf,
                        runit,
                        returnList = F)
  m <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = fortify(prjarea.spdf),
                          aes(x = long, y = lat),
                          fill = "white",
                          color = "black") +
    ggplot2::coord_map()
  if (level == "Study Area") {
    m <- m +
      ggplot2::geom_polygon(data = fortify(prjarea.spdf),
                            aes(x = long, y = lat, group = group),
                            fill = "darkgray")
  } else {
    m <- m +
      ggplot2::geom_polygon(data = fortify(runit),
                            aes(x = long, y = lat, group = group),
                            fill="darkgray")
  }
  m <- m +
    ggplot2::geom_point(data = ru.points,
                        aes(x = Longitude, y = Latitude, group = ProjectName),
                        color="black") +
    ggplot2::theme(line = element_blank(),
                   text = element_blank(),
                   title = element_blank())
  return(m)
}

## Function to add the estimated landscape proportions to the management objectives table for a reporting unit.
## Function also makes a determination as to whether or not the objective is met
## Requires the following inputs:
##    prop.table = data frame of the management objectives and evaluation categories (that match the cat.analysis input table)
##    analysis.table = cat.analysis input table (i.e., aim.analysis)
##    ru = name of the reporting unit
addLSProp <- function(prop.table,
                      analysis.table,
                      ru) {
  # Get the terradat indicator names that are used in cat.analysis
  prop.table$Indicator <- unlist(lapply(prop.table$indicator.name.alt,
                                        FUN = vlookup,
                                        table = indicator.lut,
                                        lcol = 2,
                                        rcol = 1))

  # Get the estimated proportions from the analysis table for the reporting unit
  a <- dplyr::filter(.data = analysis.table,
                     Subpopulation == ru,
                     Category != "Total") %>%
    dplyr::select(Indicator,
                  Category,
                  NResp,
                  Estimate.P,
                  StdError.P,
                  LCB95Pct.P,
                  UCB95Pct.P)

  # join the estimated proportions into prop.table
  prop.table <- dplyr::left_join(x = prop.table,
                                 y= a,
                                 by = c("Indicator" = "Indicator",
                                        "Evaluation.Category" = "Category"))

  # Calc whether or not objective is met
  prop.table$Objective.Met <- ""
  for (i in 1:nrow(prop.table)) {
    if (!(is.na(prop.table$Required.Proportion[i]) | prop.table$Required.Proportion[i] == "") & !is.na(prop.table$LCB95Pct.P[i])) {
      prop.table$Objective.Met[i] <- objectiveMet(prop.base = prop.table$Required.Proportion[i],
                                                  relation = prop.table$Proportion.Relation[i],
                                                  est.prop = prop.table$Estimate.P[i],
                                                  n = prop.table$NResp[i],
                                                  std.err = prop.table$StdError.P[i],
                                                  conf.level = conf.level)
    }
  }

  return(prop.table)
}


## Function for logic comparing estimated proportions for an indicator to its required landscape proportion.
objectiveMet <- function(prop.base,
                         relation,
                         est.prop,
                         n,
                         std.err,
                         conf.level) {

  # run comparisons first
  if (eval(parse(text = paste(est.prop, "<", prop.base*100)))) { # Is the estimated proportion less than the threshold?
    comp <- "below"
  } else if (eval(parse(text = paste(prop.base*100, "<", est.prop)))) { # Is the estimated prop greater than the threshold?
    comp <- "above"
  } else { # otherwise must be equal
    comp <- "equal"
  }

  # Calc the t statistic for comparison of prop.base to est.prop
  t <- (est.prop - prop.base*100)/std.err
  # determine the probability of a greater t value
  if (n-1>0) {
    p.val <- stats::pt(t, n-1)
  } else { # Trap for bad sample size input.
    p.val <- 999
  }

  #Set up significance level ratings
  if (p.val <= 1-(conf.level/100)) { # If p.val is less than 1-(conf.level/100) - i.e., alpha -, then conclude different from threshold
    sig <- ""
  } else if (p.val < (1-(conf.level/100))*1.5) { # if p.val less than half of alpha, likely different
    sig <- "Likely "
  } else if (p.val < (1-(conf.level/100))*2) { # if p.val less than 1/4 of alpha, likely different
    sig <- "Possibly "
  } else {
    sig <- "At threshold" # else conclude not different than threshold value
  }

  # Compile the result and return
  if (sig == "At threshold") {
    result <- ""
  } else if (relation == ">=") {
    if (comp == "below") {
      result <- "No"
    } else {
      result <- "Yes"
    }
  } else {
    if (comp == "below") {
      result <- "Yes"
    } else {
      result <- "No"
    }
  }
  return(paste0(sig,result))
}
