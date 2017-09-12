#' Generating summary reports.
#' @description Generate both a .PDF and .HTML report with \code{RMarkdown}.
#' @param out.path Character string. The folder path to write the output reports into.
#' @param project.name Character string. The name of the project to be used in the title of the report and the filenames it's written to.
#' @param indicator.lut Data frame. The lookup table between the indicator names in TerrADat and the human readable indicator names.
#' @param benchmarks Data frame. The output from \code{read.benchmarks()}.
#' @param analysis Data frame. The data frame \code{"analyses"} from the output of \code{analyze()}.
#' @param cats.to.suppress Character vector. One or more categories to suppress in tables. Defaults to \code{c("Not Meeting")}.
#' @param strata.weights Data frame. The data frame \code{strata.weights} from the output of \code{weight()}.
#' @param reporting.units.spdf Spatial polygons data frame. Used in plotting maps. This MUST have a field named exactly "Type" containing the type of reporting unit (e.g. "Watershed" or "Study Area") and a field named exactly "Subpopulation" which contains the identity of the reporting unit[s] (e.g. the watersheds "Dickshooter Creek" and "Headwaters Deep Creek").
#' @param sample.frame.spdf Spatial polygons data frame. Used in plotting maps.
#' @param project.area.spdf Spatial polygons data frame. Used in plotting maps.
#' @param points.benchmarked Data frame. The output from \code{benchmark()}.
#' @param dd.points Spatial points data frame or list of spatial points data frames. This should be the \code{pts} list in the output from \code{read.dd()}.
#' @param daterange.max Optional character string. This must be interpretable by \code{lubridate::as_date()}, e.g. \code{"2016-04-20"}. Only sampling locations visited before this date will be considered. Currently only restricts to year.
#' @param daterange.min Optional character string. This must be interpretable by \code{lubridate::as_date()}, e.g. \code{"2016-04-20"}. Only sampling locations visited after this date will be considered. Currently only restricts to year.
#' @param projection  Optional \code{sp::CRS()} argument. Used to convert \code{points.benchmarked} into a spatial points data frame. Only specify if \code{points.benchmarked} has coordinates not from the same projection as TerrADat. Defaults to \code{sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")}.
#' @param extension Character string. The file extension to write the report to. Defaults to \code{"html"}.
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
                   dd.points,
                   daterange.max = NULL,
                   daterange.min = NULL,
                   projection = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"),
                   extension = "html") {

  # Step one: Grab the confidence level from the analysis variable names
  # We used to ask the user to provide this, but asking for less is better (and more trustworthy)
  conf.level <- names(analysis) %>% paste(collapse = "") %>%
    stringr::str_extract(pattern = "[0-9]{2}") %>% as.numeric()

  # Get the reporting unit levels/types from the analysis data frame
  reporting.unit.levels <- unique(analysis$Type)

  # Get the default indicators lookup table if none is provided
  if (is.null(indicator.lut)) {
    indicator.lut <- indicator.lookup()
    names(indicator.lut) <- toupper(names(indicator.lut))
  }

  # Just sanitizing the names of things
  names(benchmarks) <- toupper(names(benchmarks))

  # Use the default indicator lookup table if none is provided
  if (is.null(indicator.lut)) {
    message("Using default indicator.lut because no alternative was provided.")
    indicator.lut <- data.frame(
      indicator.tdat = c('BareSoilCover_FH', 'TotalFoliarCover_FH', 'GapPct_25_50', 'GapPct_51_100', 'GapPct_101_200', 'GapPct_200_plus', 'GapPct_25_plus', 'NonInvPerenForbCover_AH', 'NonInvAnnForbCover_AH', 'NonInvPerenGrassCover_AH', 'NonInvAnnGrassCover_AH', 'NonInvAnnForbGrassCover_AH', 'NonInvPerenForbGrassCover_AH', 'NonInvSucculentCover_AH', 'NonInvShrubCover_AH', 'NonInvSubShrubCover_AH', 'NonInvTreeCover_AH', 'InvPerenForbCover_AH', 'InvAnnForbCover_AH', 'InvPerenGrassCover_AH', 'InvAnnGrassCover_AH', 'InvAnnForbGrassCover_AH', 'InvPerenForbGrassCover_AH', 'InvSucculentCover_AH', 'InvShrubCover_AH', 'InvSubShrubCover_AH', 'InvTreeCover_AH', 'SagebrushCover_AH', 'WoodyHgt_Avg', 'HerbaceousHgt_Avg', 'SagebrushHgt_Avg', 'OtherShrubHgt_Avg', 'NonInvPerenGrassHgt_Avg', 'InvPerenGrassHgt_Avg', 'InvPlantCover_AH', 'InvPlant_NumSp', 'SoilStability_All', 'SoilStability_Protected', 'SoilStability_Unprotected', 'HerbLitterCover_FH', 'WoodyLitterCover_FH', 'TotalLitterCover_FH', 'RockCover_FH', 'BiologicalCrustCover_FH', 'VagrLichenCover_FH', 'LichenMossCover_FH', 'DepSoilCover_FH', 'WaterCover_FH', 'NonInvPerenForbCover_FH', 'NonInvAnnForbCover_FH', 'NonInvPerenGrassCover_FH', 'NonInvAnnGrassCover_FH', 'NonInvSucculentCover_FH', 'NonInvShrubCover_FH', 'NonInvSubShrubCover_FH', 'NonInvTreeCover_FH', 'InvPerenForbCover_FH', 'InvAnnForbCover_FH', 'InvPerenGrassCover_FH', 'InvAnnGrassCover_FH', 'InvSucculentCover_FH', 'InvShrubCover_FH', 'InvSubShrubCover_FH', 'InvTreeCover_FH', 'SageBrushCover_FH'),
      indicator.name = c('Bare Soil (%)', 'Foliar Cover (%)', 'Percent in Gaps 25-50 cm', 'Percent in Gaps 51-100 cm', 'Percent in Gaps 101-200 cm', 'Percent in Gaps > 200 cm', 'Percent in Gaps > 25 cm', 'Non-invasive Perennial Forb Cover (%, any hit)', 'Non-invasive Annual Forb Cover (%, any hit)', 'Non-invasive Perennial Grass Cover (%, any hit)', 'Non-invasive Annual Grass Cover (%, any hit)', 'Non-invasive Annual Forb/Grass Cover (%, any hit)', 'Non-invasive Perennial Forb/Grass Cover (%, any hit)', 'Non-invasive Succulent Cover (%, any hit)', 'Non-invasive Shrub Cover (%, any hit)', 'Non-invasive Sub-shrub Cover (%, any hit)', 'Non-invasive Tree Cover (%, any hit)', 'Invasive Perennial Forb Cover (%, any hit)', 'Invasive Annual Forb Cover (%, any hit)', 'Invasive Perennial Grass Cover (%, any hit)', 'Invasive Annual Grass Cover (%, any hit)', 'Invasive Annual Forb/Grass Cover (%, any hit)', 'Invasive Perennial Forb/Grass Cover (%, any hit)', 'Invasive Succulent Cover (%, any hit)', 'Invasive Shrub Cover (%, any hit)', 'Invasive Sub-shrub Cover (%, any hit)', 'Invasive Tree Cover (%, any hit)', 'Sagebrush Cover (%, any hit)', 'Average Woody Height (cm)', 'Average Herbaceous Height (cm)', 'Average Sagebrush Height (cm)', 'Averge Non-Sagebrush Shrub Height (cm)', 'Average Non-invasive Perennial Grass Height (cm)', 'Average Invasive Perennial Grass Height (cm)', 'Invasive Plant Cover (%, any hit)', 'Number of Invasive Plant Species', 'Overall Soil Stability Rating', 'Protected Surface Soil Stability Rating', 'Unprotected Surface Soil Stability Rating', 'Between-Canopy Herbaceous Litter Cover (%)', 'Between-Canopy Woody Litter (%)', 'Between-Canopy Herbaceous and Woody Litter (%)', 'Between-Canopy Rock Cover (%)', 'Between-Canopy Biological Crust Cover (%)', 'Between-Canopy Vagrant Lichen Cover (%)', 'Between-Canopy Lichen and Moss Cover (%)', 'Between-Canopy Deposited Soil Cover (%)', 'Between-Canopy Water Cover (%)', 'Non-invasive Perennial Forb Cover (%, first hit)', 'Non-invasive Annual Forb Cover (%, first hit)', 'Non-invasive Perennial Grass Cover (%, first hit)', 'Non-invasive Annual Grass Cover (%, first hit)', 'Non-invasive Succulent Cover (%, first hit)', 'Non-invasive Shrub Cover (%, first hit)', 'Non-invasive Sub-shrub Cover (%, first hit)', 'Non-invasive Tree Cover (%, first hit)', 'Invasive Perennial Forb Cover (%, first hit)', 'Invasive Annual Forb Cover (%, first hit)', 'Invasive Perennial Grass Cover (%, first hit)', 'Invasive Annual Grass Cover (%, first hit)', 'Invasive Succulent Cover (%, first hit)', 'Invasive Shrub Cover (%, first hit)', 'Invasive Sub-shrub Cover (%, first hit)', 'Invasive Tree Cover (%, first hit)', 'Sagebrush Cover (%, first hit)'),
      stringsAsFactors = FALSE
    )
  }

  ## Wrangle dd.points into a single SPDF
  if (length(dd.points) == 1) {
    dd.points <- dd.points[[1]] %>%
      dplyr::select(dplyr::matches("TERRA_PLOT_ID"),
                    dplyr::matches("PLOT_NM"),
                    dplyr::matches("DT_VST"),
                    dplyr::matches("FINAL_DESIG"),
                    dplyr::matches("PANEL")) %>%
      setNames(c("PRIMARYKEY", "PLOTID", "DATE.VISITED", "FINAL_DESIG", "PANEL"))
  } else if (length(dd.points) > 1) {
    dd.points.list <- lapply(dd.points, function(X) {
      X %>%
        dplyr::select(dplyr::matches("TERRA_PLOT_ID"),
                      dplyr::matches("PLOT_NM"),
                      dplyr::matches("DT_VST"),
                      dplyr::matches("FINAL_DESIG"),
                      dplyr::matches("PANEL")) %>%
        setNames(c("PRIMARYKEY", "PLOTID", "DATE.VISITED", "FINAL_DESIG", "PANEL"))
    })
    dd.points <- dd.points.list[[1]]
    for (dd in 2:length(dd.points.list)) {
      dd.points <- rbind(dd.points, dd.points.list[[2]])
    }
  }
  dd.points@data$YEAR <- year.add(pts = dd.points, date.field = "DATE.VISITED", source.field = "PANEL")[["YEAR"]]
  dd.points@data$DATE.VISITED <- lubridate::as_date(dd.points@data$DATE.VISITED)

  ## Turn the benchmarked points into an spdf
  points.benchmarked.spdf <- sp::SpatialPointsDataFrame(coords = points.benchmarked[, c("LONGITUDE", "LATITUDE")],
                                                        data = points.benchmarked,
                                                        proj4string = projection)

  # Getting the strata sampling table trimmed and renamed
  strata.sampling.table <- dplyr::select(.data = strata.weights,
                                         Stratum,
                                         Total.pts,
                                         Observed.pts,
                                         Prop.dsgn.pts.obsrvd,
                                         Area.HA,
                                         Sampled.area.HA,
                                         Weight)
  names(strata.sampling.table) <- c("Design Stratum (Type of Land)",
                                    "# Design Points",
                                    "# Sampled Points",
                                    "Prop. Design Points Sampled",
                                    "Estimated Stratum Area (ha)",
                                    "Stratum Area Sampled",
                                    "Calculated Point Weight (ha/pt)")


  #######
  ## Objectives and benchmark information from the Excel template
  points.benchmarked$year <- lubridate::year(points.benchmarked$DATE.VISITED)
  points.benchmarked$yday <- lubridate::yday(points.benchmarked$DATE.VISITED)


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

  ## Filter out the forbidden date ranges!
  if (!is.null(daterange.max)) {
    dd.points <- dd.points[dd.points$YEAR <= lubridate::year(lubridate::as_date(daterange.max)),]
  }
  if (!is.null(daterange.min)) {
    dd.points <- dd.points[dd.points$YEAR >= lubridate::year(lubridate::as_date(daterange.min)),]
  }

  ## Get the design points info from the analysis script output files
  point.fates <- dd.points@data %>% dplyr::select(.data = .,
                                                  PLOTID,
                                                  FINAL_DESIG,
                                                  YEAR) %>%
    dplyr::group_by(FINAL_DESIG, YEAR) %>%
    dplyr::summarize(n = n())

  fates <- fate.lookup()
  point.fates <- merge(point.fates,
                       fates,
                       by.x = "FINAL_DESIG",
                       by.y = "fate.value")
  point.fates$variable <- point.fates$fate

  ##########################
  ### Making the overview map
  ## Create the study area map - project boundary, sample frame, points sampled (from TerrADat)
  ## Use leaflet.
  bounds.overview <- sp::bbox(project.area.spdf)
  map.overview <- leaflet::leaflet(sample.frame.spdf) %>%
    leaflet::addTiles() %>%
    leaflet::fitBounds(lng1 = bounds.overview[1,1],
                       lat1 = bounds.overview[2,1],
                       lng2 = bounds.overview[1,2],
                       lat2 = bounds.overview[2,2]) %>%
    leaflet::addPolygons(data = sample.frame.spdf,
                         fill = TRUE,
                         stroke = TRUE,
                         color = "tan",
                         fillOpacity = 0.6,
                         weight = 2,
                         group = "BLM Lands") %>%
    leaflet::addPolygons(data = project.area.spdf,
                         fill = FALSE,
                         stroke = TRUE,
                         color = "#222",
                         weight = 3,
                         group = "Study Area") %>%
    leaflet::addPolygons(data = reporting.units.spdf,
                         fill = FALSE,
                         stroke = TRUE,
                         color = "#000",
                         weight = 2,
                         fillOpacity = 0.6,
                         group = "Reporting Units") %>%
    leaflet::addCircleMarkers(data = points.benchmarked.spdf,
                              radius = 4,
                              color = "navy",
                              stroke = FALSE,
                              fillOpacity = 0.8,
                              group = "Monitoring Sites") %>%
    leaflet::addLegend(position = "bottomright",
                       colors = c("#222", "tan", "navy", "#000"),
                       labels = c("Study Area Boundary", "BLM Lands", "Monitoring Sites", "Reporting Units")) %>%
    leaflet::addLayersControl(overlayGroups = c("Monitoring Sites", "BLM Lands", "Reporting Units", "Study Area"),
                              options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  ########################
  ### Figure of sample dates
  # Month start days and names
  months.days <- c(0, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336)
  months.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")

  # Figure itself
  dates.plot <- ggplot2::ggplot(data = dplyr::distinct(dplyr::select(.data = points.benchmarked, PRIMARYKEY, yday, year)),
                                ggplot2::aes(x = yday, fill = factor(year))) +
    ggplot2::geom_histogram(binwidth = 7) +
    ggplot2::xlim(0, 365) +
    ggplot2::geom_vline(xintercept = months.days,
                        color = "white",
                        size = 0.75) +
    ggplot2::scale_x_continuous(breaks = months.days,
                                labels = months.names) +
    ggplot2::ggtitle("Number of monitoring sites where data was collected by week") +
    ggplot2::theme(panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank()) +
    ggplot2::xlab("Week of the year") +
    ggplot2::ylab("Number of Plots") +
    ggplot2::labs(fill = "Year")

  ####################
  ### List of figures of point fates
  fates.plots <- lapply(X = split(point.weights, "REPORTING.UNIT"),
                       FUN = function(X) {
                         point.weights <- X

                         # Filter out unneeded and nonsense values
                         data <- point.weights[toupper(point.weights$FINAL_DESIG) %in% toupper(fates$fate.value[fates$fate != "Unneeded"]),]

                         # Replace the values with the names
                         for (f in unique(data$FINAL_DESIG)) {
                           data$FINAL_DESIG[data$FINAL_DESIG == f] <- fates$fate[toupper(fates$fate.value) %in% toupper(f)]
                         }

                         # Just get a sum
                         total <- dplyr::summarize(.data = dplyr::group_by(.data = data,
                                                                           REPORTING.UNIT),
                                                   total = n())[["total"]]

                         # Calculate percent composition by fate
                         summary <- dplyr::summarize(.data = dplyr::group_by(.data = data,
                                                                             REPORTING.UNIT,
                                                                             FINAL_DESIG),
                                                     pct = n()*100/total)

                         # Sort that to get the fates in the order we want
                         summary <- summary[match(c("Target Sampled", "Non-target", "Inaccessible", "Unknown"), summary$FINAL_DESIG),]

                         # Plot!
                         plot <- ggplot2::ggplot(summary,
                                                 aes(x = REPORTING.UNIT,
                                                     y = pct,
                                                     fill = FINAL_DESIG)) +
                           ggplot2::geom_col(position = "stack") +
                           ggplot2::coord_flip() +
                           ggplot2::scale_fill_manual(name= "Final Point\nDesignation",
                                                      values = c("Unknown" = "#abd9e9",
                                                                 "Non-target" = "#ffffbf",
                                                                 "Inaccessible" = "#fdae61",
                                                                 "Target Sampled" = "#d7191c"),
                                                      breaks = c("Target Sampled", "Non-target", "Inaccessible", "Unknown"),
                                                      drop = FALSE) +
                           ggplot2::geom_text(data = subset(summary,
                                                            pct != 0),
                                              ggplot2::aes(label = pct,
                                                           y = pct),
                                              position = ggplot2::position_stack(vjust = 0.5)) +
                           ggplot2::ylab("Percent of Plots") +
                           ggplot2::xlab("Reporting Unit") +
                           ggplot2::ggtitle(label = "Percentages of Final Point Designations\nWithin Reporting Unit") +
                           ggplot2::theme(aspect.ratio = 1/4,
                                          axis.title.y = element_blank(),
                                          axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          panel.background = element_blank())

                         return(plot)
                       }) %>% setNames(unique(point.weights$REPORTING.UNIT))

  ##############
  ### Summary table of objectives
  # Get and reorder relevant variables from the benchmarks summary data frame
  proportions.df <- dplyr::select(.data = benchmarks.sum,
                                  MANAGEMENT.QUESTION,
                                  Benchmark.Source,
                                  indicator.name.alt,
                                  EVALUATION.CATEGORY,
                                  Required.Proportion,
                                  Proportion.Relation)
  # Add a variable with a string version of the required proportion expression
  proportions.df$LS.Prop <- paste0(proportions.df$Proportion.Relation,
                                   " ",
                                   proportions.df$Required.Proportion*100,
                                   "%")
  # Anywhere that's made of just NA values, replace them with an empty string
  proportions.df$LS.Prop[proportions.df$LS.Prop == "NA NA%"] <- ""

  # Create a list of summary tables
  # Do this by reporting unit within levels/type
  reporting.unit.summary.list <- lapply(reporting.unit.levels,
                                        FUN  = function(X, analysis.df, proportions.df, indicator.lut) {
                                          analysis.level.df <- dplyr::filter(.data = analysis.df,
                                                                             Type == X)

                                          # For each reporting unit in the level
                                          proportions.reporting.unit.df <- lapply(unique(analysis.level.df$Subpopulation),
                                                                                  FUN = function(X, analysis.df, proportions.df, indicator.lut) {
                                                                                    # Figure out if it's meeting
                                                                                    proportions.reporting.unit.df <- addLSProp(prop.table = proportions.df,
                                                                                                                               analysis.table = analysis.df,
                                                                                                                               reporting.unit = X,
                                                                                                                               indicator.lut = indicator.lut)

                                                                                    proportions.reporting.unit.df$CI <- paste("±", (proportions.reporting.unit.df[[grep(names(proportions.reporting.unit.df), pattern = "^UCB[0-9]{1,2}Pct\\.P$")]] - proportions.reporting.unit.df[[grep(names(proportions.reporting.unit.df), pattern = "^LCB[0-9]{1,2}Pct\\.P$")]])/2)

                                                                                    # Get the fields in the right order and drop the ones we don't care about
                                                                                    proportions.reporting.unit.df <- dplyr::select(.data= proportions.reporting.unit.df,
                                                                                                                                   MANAGEMENT.QUESTION,
                                                                                                                                   indicator.name.alt,
                                                                                                                                   EVALUATION.CATEGORY,
                                                                                                                                   LS.Prop,
                                                                                                                                   NResp,
                                                                                                                                   Estimate.P,
                                                                                                                                   CI,
                                                                                                                                   Objective.Met) %>%
                                                                                      # Sort
                                                                                      dplyr::arrange(MANAGEMENT.QUESTION, EVALUATION.CATEGORY)

                                                                                    # Rename to friendly strings
                                                                                    names(proportions.reporting.unit.df) <- c("Monitoring Objective",
                                                                                                                              "Indicator",
                                                                                                                              "Condition Category",
                                                                                                                              "Required Percent",
                                                                                                                              "Number of Plots",
                                                                                                                              "Estimated Percent",
                                                                                                                              "Confidence Interval",
                                                                                                                              "Objective Met?")

                                                                                    # Add the reporting unit
                                                                                    proportions.reporting.unit.df$reporting.unit <- X

                                                                                    return(proportions.reporting.unit.df)
                                                                                  },
                                                                                  analysis.df = analysis.df,
                                                                                  proportions.df = proportions.df,
                                                                                  indicator.lut = indicator.lut)

                                          proportions.reporting.unit.df$level <- X

                                          return(proportions.reporting.unit.df)
                                        },
                                        analysis.df = analysis,
                                        proportions.df = proportions.df,
                                        indicator.lut = indicator.lut)

  if (length(reporting.unit.summary.list) > 1) {
    reporting.unit.summary.df <- dplyr::bind_rows(reporting.unit.summary.list)
  } else {
    reporting.unit.summary.df <- as.data.frame(reporting.unit.summary.list[[1]])
  }

  # Create a list of xtables to display these summaries later
  reporting.unit.summary.xtables <- dplyr::filter(.data = reporting.unit.summary.df,
                                                  !(Category %in% cats.to.suppress)) %>%
    split(x = ., f = list(.$reporting.unit, .$level)) %>%
    lapply(X = .,
           FUN = function(X){
             output <- dplyr::select(.data = X,
                                     -level,
                                     -reporting.unit) %>%
               xtable::xtable(x = .,
                              align = c("l","l","l","l","c","c","c","c","c","c"))

             return(output)
           })
  # Name the list with the appropriate reporting units for future reference
  names(reporting.unit.summary.xtables) <- dplyr::select(.data = reporting.unit.summary.df,
                                                         reporting.unit,
                                                         level) %>% dplyr::distinct() %>% .$reporting.unit

  #################
  ### Benchmark table
  bm.table <- dplyr::select(.data = benchmarks,
                            MANAGEMENT.QUESTION,
                            INDICATOR,
                            BENCHMARK.SOURCE,
                            EVALUATION.STRATUM,
                            EVALUATION.CATEGORY,
                            EVAL.STRING.LOWER,
                            EVAL.STRING.UPPER,
                            INDICATOR.TDAT)
  bm.table$BENCHMARK <- paste(bm.table$EVAL.STRING.LOWER,
                              bm.table$INDICATOR.TDAT,
                              bm.table$EVAL.STRING.UPPER)
  bm.table <- dplyr::select(.data = bm.table,
                            -dplyr::starts_with("eval.string"),
                            -INDICATOR.TDAT) %>% dplyr::arrange(MANAGEMENT.QUESTION, EVALUATION.CATEGORY)  # Drop the unneeded fields and sort by mgt question
  names(bm.table) <- c("Goal/Management Question",
                       "Indicator",
                       "Benchmark Source",
                       "Benchmark Group",
                       "Condition Category",
                       "Benchmark Definition")



  ###############
  ### RENDER

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
##  reporting.unit = desired level of reporting units for the graph
##  subpop = desired specific reporting unit
##  indicator = indicator to be graphed
indicatorPlot <- function(df,
                          reporting.unit.level,
                          reporting.unit.name,
                          indicator,
                          indicator.lut = NULL,
                          mq,
                          threshold = 0.5) {
  plot.data <- dplyr::filter(.data = df,
                             Type == reporting.unit.level,
                             Subpopulation == reporting.unit.name,
                             Indicator == indicator,
                             MANAGEMENT.QUESTION == mq,
                             Category != "Total")
  plot.data$UCB <- plot.data[[names(plot.data)[grepl(names(plot.data), pattern = "^UCB[0-9]{1,2}Pct\\.P$")]]]
  plot.data$LCB <- plot.data[[names(plot.data)[grepl(names(plot.data), pattern = "^LCB[0-9]{1,2}Pct\\.P$")]]]
  ind.realname <- vlookup(indicator, indicator.lut, 1, 2) ## Lookup the pretty name for the indicator
  p <- ggplot2::ggplot(plot.data, ggplot2::aes(x = Category,
                                               y = Estimate.P,
                                               fill = Category)) +
    ggplot2::geom_bar(stat = "identity",
                      width = 0.5) +
    ggplot2::coord_flip() +
    ggplot2::geom_errorbar(ggplot2::aes(ymax = UCB,
                                        ymin = LCB),
                           width = 0.25) +
    ggplot2::scale_fill_brewer(type = "div",
                               palette = "RdYlBu") +
    ggplot2::ylim(0, 100) +
    ggplot2::ggtitle(paste0("Percentage of Reporting Unit by Condition Category: \n", ind.realname)) +
    ggplot2::ylab("Estimated Percent of Reporting Unit Area")

  if (threshold > 0) {
    p <- p + ggplot2::geom_hline(yintercept = threshold, colour = "gold", size = 2)
  }
  return(p)
}

## Function to display table of cat.analysis results. Takes same arguments as indicatorPlot function (minus the threshold argument)
indicatorTable <- function(df,
                           reporting.unit.level,
                           reporting.unit.name,
                           indicator,
                           mq) {

  conf.level <- names(df)[grepl(names(df), pattern = "^LCB[0-9]{1,2}Pct\\.P$")] %>%
    stringr::str_extract(pattern = "[0-9]{1,2}") %>% as.numeric()

  table.data <- dplyr::filter(.data = df,
                              Type == reporting.unit.level,
                              Subpopulation == reporting.unit.name,
                              Indicator == indicator,
                              MANAGEMENT.QUESTION == mq,
                              Category != "Total")

  table.data$CI <- paste("±", (table.data[[grep(names(table.data), pattern = "^UCB[0-9]{1,2}Pct\\.P$")]] - table.data[[grep(names(table.data), pattern = "^LCB[0-9]{1,2}Pct\\.P$")]])/2)

  table.data <- dplyr::select(Category,
                              NResp,
                              Estimate.P,
                              CI,
                              StdError.P)
  names(table.data) <- c("Condition Category",
                         "Number of Points",
                         "Estimated Percent\nof Surveyed Area",
                         "Confidence Interval",
                         "Standard Error")
  return(table.data)
}

## Function to create a basic map showing the location of the reporting unit within the study area
##   Requires the following inputs:
##   level = reporting unit level
##   reporting.unit = reporting unit name
##   repunits.spdf = spatial polygon data frame for the reporting units
##   prjarea.spdf = spatial polygon data frame for the project area
indicatorMap <- function(reporting.unit.level,
                         reporting.unit.name,
                         reporting.units.spdf,
                         prjarea.spdf,
                         samplepts.spdf) {
  runit <- sp::spTransform(reporting.units.spdf[reporting.units.spdf$Type == reporting.unit.level & reporting.units.spdf$Subpopulation == reporting.unit.name,],
                           samplepts.spdf@proj4string)
  reporting.unit.points <- sp::over(samplepts.spdf,
                                    runit,
                                    returnList = F)
  reporting.unit.points$LONGITUDE <- samplepts.spdf@data$LONGITUDE
  reporting.unit.points$LATITUDE <- samplepts.spdf@data$LATITUDE
  reporting.unit.points$EVALUATION.CATEGORY <- samplepts.spdf@data$EVALUATION.CATEGORY

  ## Don't ask why this next stretch works. All that matters is that it does.
  prjarea.fortified <- ggplot2::fortify(prjarea.spdf)

  map <- ggplot2::ggplot() +
    ggplot2::coord_map()

  ## This commented out produces nonsense and I have no idea why. It's almost identical to the if{}
  # m <- m +
  #   ggplot2::geom_polygon(data = prjarea.fortified,
  #                         ggplot2::aes(x = long,
  #                                      y = lat,),
  #                         fill = "white",
  #                         color = "black")
  if (reporting.unit.level == "Study Area") {
    map <- map +
      ggplot2::geom_polygon(data = prjarea.fortified,
                            ggplot2::aes(x = long,
                                         y = lat,
                                         group = group),
                            fill = "darkgray")
  } else {
    map <- map +
      ggplot2::geom_polygon(data = ggplot2::fortify(runit),
                            ggplot2::aes(x = long, y = lat, group = group),
                            fill="darkgray")
  }
  map <- map +
    ggplot2::geom_point(data = reporting.unit.points,
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
  return(map)
}

## Function to add the estimated landscape proportions to the management objectives table for a reporting unit.
## Function also makes a determination as to whether or not the objective is met
## Requires the following inputs:
##    prop.table = data frame of the management objectives and evaluation categories (that match the cat.analysis input table)
##    analysis.table = cat.analysis input table (i.e., aim.analysis)
##    reporting.unit = name of the reporting unit
addLSProp <- function(prop.table,
                      analysis.table,
                      reporting.unit.name,
                      indicator.lut) {
  # Get the terradat indicator names that are used in cat.analysis
  prop.table$Indicator <- unlist(lapply(prop.table$indicator.name.alt,
                                        FUN = vlookup,
                                        table = indicator.lut,
                                        lcol = 2,
                                        rcol = 1))

  conf.level <- names(analysis.table)[grepl(names(analysis.table), pattern = "^LCB[0-9]{1,2}Pct\\.P$")] %>%
    stringr::str_extract(pattern = "[0-9]{1,2}") %>% as.numeric()

  # join the estimated proportions into prop.table
  prop.table <- dplyr::left_join(x = prop.table,
                                 # Get the estimated proportions from the analysis table for the reporting unit
                                 y= dplyr::filter(.data = analysis.table,
                                                  Subpopulation == reporting.unit.name,
                                                  Category != "Total") %>%
                                   dplyr::select(Indicator,
                                                 Category,
                                                 NResp,
                                                 Estimate.P,
                                                 StdError.P,
                                                 dplyr::matches(match = "^LCB[0-9]{1,2}Pct\\.P$"),
                                                 dplyr::matches(match = "^UCB[0-9]{1,2}Pct\\.P$")),
                                 by = c("Indicator" = "Indicator",
                                        "EVALUATION.CATEGORY" = "Category")) %>% dplyr::distinct()

  # Calc whether or not objective is met
  prop.table$Objective.Met <- ""
  for (row in 1:nrow(prop.table)) {
    if (!(is.na(prop.table$Required.Proportion[row]) | prop.table$Required.Proportion[row] == "") &
        !is.na(prop.table[row, names(prop.table)[grepl(names(prop.table), pattern = "^LCB[0-9]{1,2}Pct\\.P$")]])) {
      prop.table$Objective.Met[row] <- objectiveMet(proportion.required = prop.table$Required.Proportion[row],
                                                    relation = prop.table$Proportion.Relation[row],
                                                    estimated.proportion = prop.table$Estimate.P[row]/100,
                                                    n = prop.table$NResp[row],
                                                    std.err = prop.table$StdError.P[row],
                                                    conf.level = conf.level)
    }
  }

  return(prop.table)
}


## Function for logic comparing estimated proportions for an indicator to its required landscape proportion.
objectiveMet <- function(proportion.required,
                         relation,
                         estimated.proportion,
                         n,
                         std.err,
                         conf.level) {
  # Calc the t statistic for comparison of proportion.required to estimated.proportion
  # and determine the probability of a greater t value
  if (n-1>0) {
    p.val <- stats::pt(q = (estimated.proportion - proportion.required)*100/std.err,
                       df = n - 1)
  } else { # Trap for bad sample size input.
    p.val <- 999
  }

  ## Does the relationship evaluate to true?
  result <- if (eval(parse(text = paste(estimated.proportion, relation, proportion.required)))) {"yes"} else {"no"}

  # Set up significance level ratings
  # If p.val is less than 1-(conf.level/100) - i.e., alpha -, then conclude different from threshold
  if (p.val <= 1-(conf.level/100)) {
    sig <- ""
  } else if (p.val < (1-(conf.level/100))*1.5) { # if p.val less than half of alpha, likely different
    sig <- "Likely "
  } else if (p.val < (1-(conf.level/100))*2) { # if p.val less than 1/4 of alpha, likely different
    sig <- "Possibly "
  } else {
    sig <- "Yes" # else conclude not different than threshold value
  }

  # Get rid of the yes/no if at the threshold
  if (sig == "Yes") {
    result <- ""
  }

  # Compile the result and return
  output <- paste0(sig,result)
  substr(output, 1, 1) <- toupper(substr(output, 1, 1))

  return(output)
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
