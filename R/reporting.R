#' Generating summary reports.
#' @description Generate both a .PDF and .HTML report with \code{RMarkdown}.
#' @param out.path Character string. The folder path to write the output reports into.
#' @param project.name Character string. The name of the project to be used in the title of the report and the filenames it's written to.
#' @param indicator.lut Data frame. The lookup table between the indicator names in TerrADat and the human readable indicator names.
#' @param benchmarks Data frame. The output from \code{read.benchmarks()}.
#' @param analysis Data frame. The data frame \code{"analyses"} from the output of \code{analyze()}.
#' @param cats.to.suppress Character vector. One or more categories to suppress in tables. Defaults to \code{c("Not Meeting")}.
#' @param point.weights Data frame. The data frame \code{point.weights} from the output of \code{weight()}.
#' @param strata.weights Data frame. The data frame \code{strata.weights} from the output of \code{weight()}.
#' @param reporting.units.spdf Spatial polygons data frame. Used in plotting maps. This MUST have a field named exactly "Type" containing the type of reporting unit (e.g. "Watershed" or "Study Area") and a field named exactly "Subpopulation" which contains the identity of the reporting unit[s] (e.g. the watersheds "Dickshooter Creek" and "Headwaters Deep Creek").
#' @param sample.frame.spdf Spatial polygons data frame. Used in plotting maps.
#' @param project.area.spdf Spatial polygons data frame. Used in plotting maps.
#' @param points.benchmarked Data frame. The output from \code{benchmark()}.
#' @param projection  Optional \code{sp::CRS()} argument. Used to convert \code{points.benchmarked} into a spatial points data frame. Only specify if \code{points.benchmarked} has coordinates not from the same projection as TerrADat. Defaults to \code{sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")}.
#' @export

report <- function(out.path,
                   project.name,
                   indicator.lut = NULL,
                   benchmarks = NULL,
                   analysis = NULL,
                   cats.to.suppress = c("Not Meeting"),
                   point.weights,
                   strata.weights,
                   reporting.units.spdf = NULL,
                   sample.frame.spdf,
                   project.area.spdf = NULL,
                   points.benchmarked = NULL,
                   projection = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"),
                   extension = "html") {

  fates <- read.csv(paste0(path.package("aim.analysis"), "/inst/defaults/fates.csv"), stringsAsFactors = FALSE)
  point.weights <- merge(point.weights,
                         fates,
                         by.x = "FINAL_DESIG",
                         by.y = "fate.value")
  point.weights$FINAL_DESIG <- point.weights$fate
  point.weights$fate <- NULL

  conf.level <- names(analysis) %>% paste(collapse = "") %>%
    stringr::str_extract(pattern = "[0-9]{2}") %>% as.numeric()

  names(benchmarks) <- stringr::str_to_upper(names(benchmarks))

  if (is.null(indicator.lut)) {
    message("Using default indicator.lut because no alternative was provided.")
    indicator.lut <- data.frame(
      indicator.tdat = c('BareSoilCover_FH', 'TotalFoliarCover_FH', 'GapPct_25_50', 'GapPct_51_100', 'GapPct_101_200', 'GapPct_200_plus', 'GapPct_25_plus', 'NonInvPerenForbCover_AH', 'NonInvAnnForbCover_AH', 'NonInvPerenGrassCover_AH', 'NonInvAnnGrassCover_AH', 'NonInvAnnForbGrassCover_AH', 'NonInvPerenForbGrassCover_AH', 'NonInvSucculentCover_AH', 'NonInvShrubCover_AH', 'NonInvSubShrubCover_AH', 'NonInvTreeCover_AH', 'InvPerenForbCover_AH', 'InvAnnForbCover_AH', 'InvPerenGrassCover_AH', 'InvAnnGrassCover_AH', 'InvAnnForbGrassCover_AH', 'InvPerenForbGrassCover_AH', 'InvSucculentCover_AH', 'InvShrubCover_AH', 'InvSubShrubCover_AH', 'InvTreeCover_AH', 'SagebrushCover_AH', 'WoodyHgt_Avg', 'HerbaceousHgt_Avg', 'SagebrushHgt_Avg', 'OtherShrubHgt_Avg', 'NonInvPerenGrassHgt_Avg', 'InvPerenGrassHgt_Avg', 'InvPlantCover_AH', 'InvPlant_NumSp', 'SoilStability_All', 'SoilStability_Protected', 'SoilStability_Unprotected', 'HerbLitterCover_FH', 'WoodyLitterCover_FH', 'TotalLitterCover_FH', 'RockCover_FH', 'BiologicalCrustCover_FH', 'VagrLichenCover_FH', 'LichenMossCover_FH', 'DepSoilCover_FH', 'WaterCover_FH', 'NonInvPerenForbCover_FH', 'NonInvAnnForbCover_FH', 'NonInvPerenGrassCover_FH', 'NonInvAnnGrassCover_FH', 'NonInvSucculentCover_FH', 'NonInvShrubCover_FH', 'NonInvSubShrubCover_FH', 'NonInvTreeCover_FH', 'InvPerenForbCover_FH', 'InvAnnForbCover_FH', 'InvPerenGrassCover_FH', 'InvAnnGrassCover_FH', 'InvSucculentCover_FH', 'InvShrubCover_FH', 'InvSubShrubCover_FH', 'InvTreeCover_FH', 'SageBrushCover_FH'),
      indicator.name = c('Bare Soil (%)', 'Foliar Cover (%)', 'Percent in Gaps 25-50 cm', 'Percent in Gaps 51-100 cm', 'Percent in Gaps 101-200 cm', 'Percent in Gaps > 200 cm', 'Percent in Gaps > 25 cm', 'Non-invasive Perennial Forb Cover (%, any hit)', 'Non-invasive Annual Forb Cover (%, any hit)', 'Non-invasive Perennial Grass Cover (%, any hit)', 'Non-invasive Annual Grass Cover (%, any hit)', 'Non-invasive Annual Forb/Grass Cover (%, any hit)', 'Non-invasive Perennial Forb/Grass Cover (%, any hit)', 'Non-invasive Succulent Cover (%, any hit)', 'Non-invasive Shrub Cover (%, any hit)', 'Non-invasive Sub-shrub Cover (%, any hit)', 'Non-invasive Tree Cover (%, any hit)', 'Invasive Perennial Forb Cover (%, any hit)', 'Invasive Annual Forb Cover (%, any hit)', 'Invasive Perennial Grass Cover (%, any hit)', 'Invasive Annual Grass Cover (%, any hit)', 'Invasive Annual Forb/Grass Cover (%, any hit)', 'Invasive Perennial Forb/Grass Cover (%, any hit)', 'Invasive Succulent Cover (%, any hit)', 'Invasive Shrub Cover (%, any hit)', 'Invasive Sub-shrub Cover (%, any hit)', 'Invasive Tree Cover (%, any hit)', 'Sagebrush Cover (%, any hit)', 'Average Woody Height (cm)', 'Average Herbaceous Height (cm)', 'Average Sagebrush Height (cm)', 'Averge Non-Sagebrush Shrub Height (cm)', 'Average Non-invasive Perennial Grass Height (cm)', 'Average Invasive Perennial Grass Height (cm)', 'Invasive Plant Cover (%, any hit)', 'Number of Invasive Plant Species', 'Overall Soil Stability Rating', 'Protected Surface Soil Stability Rating', 'Unprotected Surface Soil Stability Rating', 'Between-Canopy Herbaceous Litter Cover (%)', 'Between-Canopy Woody Litter (%)', 'Between-Canopy Herbaceous and Woody Litter (%)', 'Between-Canopy Rock Cover (%)', 'Between-Canopy Biological Crust Cover (%)', 'Between-Canopy Vagrant Lichen Cover (%)', 'Between-Canopy Lichen and Moss Cover (%)', 'Between-Canopy Deposited Soil Cover (%)', 'Between-Canopy Water Cover (%)', 'Non-invasive Perennial Forb Cover (%, first hit)', 'Non-invasive Annual Forb Cover (%, first hit)', 'Non-invasive Perennial Grass Cover (%, first hit)', 'Non-invasive Annual Grass Cover (%, first hit)', 'Non-invasive Succulent Cover (%, first hit)', 'Non-invasive Shrub Cover (%, first hit)', 'Non-invasive Sub-shrub Cover (%, first hit)', 'Non-invasive Tree Cover (%, first hit)', 'Invasive Perennial Forb Cover (%, first hit)', 'Invasive Annual Forb Cover (%, first hit)', 'Invasive Perennial Grass Cover (%, first hit)', 'Invasive Annual Grass Cover (%, first hit)', 'Invasive Succulent Cover (%, first hit)', 'Invasive Shrub Cover (%, first hit)', 'Invasive Sub-shrub Cover (%, first hit)', 'Invasive Tree Cover (%, first hit)', 'Sagebrush Cover (%, first hit)'),
      stringsAsFactors = FALSE
    )
  }

  ## Turn the benchmarked points into an spdf
  points.benchmarked.spdf <- sp::SpatialPointsDataFrame(coords = points.benchmarked[, c("LONGITUDE", "LATITUDE")],
                                                        data = points.benchmarked,
                                                        proj4string = projection)

  #######
  ## Objectives and benchmark information from the Excel template
  points.benchmarked$year <- lubridate::year(points.benchmarked$DATE.VISITED)
  points.benchmarked$yday <- lubridate::yday(points.benchmarked$DATE.VISITED)

  ru.levels <- unique(analysis$Type)


  ## Clean up so that the geometry will display right on the map
  if (!is.null(reporting.units.spdf)) {
    # reporting.units.spdf <- rgeos::gBuffer(reporting.units.spdf,
    #                                      byid = TRUE,
    #                                      width = 0) %>% spTransform(CRSobj = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  }


  # For each benchmark add in the name of the field in TerrADat that corresponds
  benchmarks.sum <- dplyr::group_by(benchmarks, MANAGEMENT.QUESTION, EVALUATION.CATEGORY) %>% dplyr::summarise(
    indicator.name.alt = dplyr::first(INDICATOR),
    Required.Proportion = dplyr::first(REQUIRED.PROPORTION),
    Proportion.Relation = dplyr::first(PROPORTION.RELATION),
    indicator.tdat = dplyr::first(INDICATOR.TDAT),
    Benchmark.Source = dplyr::first(BENCHMARK.SOURCE))

  ######
  ## Sample Design Information (point fate, stratification, study area bdy)
  # Dissolve any additional polygons so we have a single boundary for the project area
  if (!is.null(project.area.spdf)) {
    project.area.spdf$dissolve <- 1
    project.area.spdf <- rgeos::gUnaryUnion(project.area.spdf,
                                            id = project.area.spdf@data$dissolve)
  }

  ## Get the design points info from the analysis script output files
  point.fates <- point.weights %>%
    merge(x = .,
          y = dplyr::distinct(dplyr::select(.data = points.benchmarked,
                                            PRIMARYKEY,
                                            DATE.VISITED)),
          by = "PRIMARYKEY") %>%
    dplyr::mutate(YEAR = lubridate::year(DATE.VISITED)) %>%
    dplyr::group_by(WEIGHT.ID, FINAL_DESIG, YEAR) %>% dplyr::summarize(n = n())
  names(point.fates)[names(point.fates) == "FINAL_DESIG"] <- "variable"

  rmarkdown::render(input = "C:/Users/Nelson/Documents/Projects/aim.analysis/inst/markdown/report.Rmd",
                    #input = paste0(path.package("aim.analysis"), "/markdown/report.Rmd"),
                    output_file = filename.aim(name = project.name, type = "report", extension = extension),
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
                          indicator.lut,
                          mq,
                          threshold = 0.5) {
  plot.data <- dplyr::filter(.data = df,
                             Type == ru,
                             Subpopulation == subpop,
                             Indicator == indicator,
                             MANAGEMENT.QUESTION == mq,
                             Category != "Total")
  plot.data$UCB <- plot.data[[names(plot.data)[grepl(names(plot.data), pattern = "^UCB[0-9]{1,2}Pct\\.P$")]]]
  plot.data$LCB <- plot.data[[names(plot.data)[grepl(names(plot.data), pattern = "^LCB[0-9]{1,2}Pct\\.P$")]]]
  ind.realname <- vlookup(indicator, indicator.lut, 1, 2) ## Lookup the pretty name for the indicator
  p <- ggplot2::ggplot(plot.data, ggplot2::aes(x = Category, y = Estimate.P, fill = Category)) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) +
    ggplot2::coord_flip() +
    ggplot2::geom_errorbar(ggplot2::aes(ymax = UCB,
                                        ymin = LCB),
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
    dplyr::select(Category, NResp, Estimate.P, StdError.P, dplyr::matches(match = "[0-9]{2}Pct.P"))
  names(table.data) <- c("Category",
                         "# Points",
                         "% Area Estimate",
                         "Std. Error",
                         paste0("Lower ", conf.level, "% CI"),
                         paste0("Upper ", conf.level, "% CI"))
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
  runit <- sp::spTransform(repunits.spdf[repunits.spdf$Type == level & repunits.spdf$Subpopulation == ru,],
                           samplepts.spdf@proj4string)
  ru.points <- sp::over(samplepts.spdf,
                        runit,
                        returnList = F)
  ru.points$LONGITUDE <- samplepts.spdf@data$LONGITUDE
  ru.points$LATITUDE <- samplepts.spdf@data$LATITUDE
  ru.points$EVALUATION.CATEGORY <- samplepts.spdf@data$EVALUATION.CATEGORY

  ## Don't ask why this next stretch works. All that matters is that it does.
  prjarea.fortified <- ggplot2::fortify(prjarea.spdf)

  m <- ggplot2::ggplot() +
    ggplot2::coord_map()

  ## This commented out produces nonsense and I have no idea why. It's almost identical to the if{}
  # m <- m +
  #   ggplot2::geom_polygon(data = prjarea.fortified,
  #                         ggplot2::aes(x = long,
  #                                      y = lat,),
  #                         fill = "white",
  #                         color = "black")
  if (level == "Study Area") {
    m <- m +
      ggplot2::geom_polygon(data = prjarea.fortified,
                            ggplot2::aes(x = long,
                                         y = lat,
                                         group = group),
                            fill = "darkgray")
  } else {
    m <- m +
      ggplot2::geom_polygon(data = ggplot2::fortify(runit),
                            ggplot2::aes(x = long, y = lat, group = group),
                            fill="darkgray")
  }
  m <- m +
    ggplot2::geom_point(data = ru.points,
                        ggplot2::aes(x = LONGITUDE,
                                     y = LATITUDE,
                                     group = EVALUATION.CATEGORY,
                                     colour = EVALUATION.CATEGORY),
                        show.legend = FALSE) +
    ggplot2::scale_color_brewer(type = "div",
                                palette ="RdYlBu",
                                guide = FALSE) +
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
                      ru,
                      indicator.lut,
                      conf.level) {
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
                  dplyr::matches(match = "^LCB[0-9]{1,2}Pct\\.P$"),
                  dplyr::matches(match = "^UCB[0-9]{1,2}Pct\\.P$"))

  # join the estimated proportions into prop.table
  prop.table <- dplyr::left_join(x = prop.table,
                                 y= a,
                                 by = c("Indicator" = "Indicator",
                                        "EVALUATION.CATEGORY" = "Category"))

  # Calc whether or not objective is met
  prop.table$Objective.Met <- ""
  for (i in 1:nrow(prop.table)) {
    if (!(is.na(prop.table$Required.Proportion[i]) | prop.table$Required.Proportion[i] == "") &
        !is.na(prop.table[i, names(prop.table)[grepl(names(prop.table), pattern = "^LCB[0-9]{1,2}Pct\\.P$")]])) {
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

num2nom <- function(number,
                    capitalize = FALSE) {
  ones <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  tens <- c("ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
  exceptions <- c("0" = "zero",
                  "11" = "eleven",
                  "12" = "twelve",
                  "13" = "thirteen",
                  "14" = "fourteen",
                  "15" = "fifteen",
                  "16" = "sixteen",
                  "17" = "seventeen",
                  "18" = "eighteen",
                  "19" = "nineteen")
  if (nchar(number) > 2) {
    stop("Sorry, this can handle only one and two digit numbers right now.")
  }

  if (as.character(number) %in% names(exceptions)) {
    output <- unname(exceptions[as.character(number)])
  } else if (nchar(number) == 1) {
    output <- ones[number]
  } else if (nchar(number) == 2) {
    if (substr(number, 2, 2) != "0") {
      output <- paste(tens[as.numeric(substr(number, 1, 1))], ones[as.numeric(substr(number, 2, 2))], sep = "-")
    } else {
      output <- tens[as.numeric(substr(number, 1, 1))]
    }
  }
  if (capitalize) {
    substr(output, 1, 1) <- toupper(substr(output, 1, 1))
  }

  return(output)
}
