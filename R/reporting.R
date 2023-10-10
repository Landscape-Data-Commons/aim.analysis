
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
  result <- if (eval(parse(text = paste(estimated.proportion, relation, proportion.required)))) {"yes"}
  else {"no"}

  # Set up significance level ratings
  # If p.val is less than 1-(conf.level/100) - i.e., alpha -, then conclude different from threshold
  if (p.val <= 1-(conf.level/100)) {
    sig <- ""
  } else if (p.val < (1-(conf.level/100))*1.5) { # if p.val less than half of alpha, likely different
    sig <- "Likely "
  } else if (p.val < (1-(conf.level/100))*2) { # if p.val less than 1/4 of alpha, likely different
    sig <- "Possibly "
  } else {
    sig <- "Uncertain" # else conclude not different than threshold value
  }

  # Get rid of the yes/no if at the threshold
  if (sig == "Uncertain") {
    result <- ""
  }

  # Compile the result and return
  output <- trimws(paste0(sig,result))
  substr(output, 1, 1) <- toupper(substr(output, 1, 1))

  return(output)
}

## Convert numeric to nominal
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


#' Plotting Indicator Category Distributions
#' @param analysis.df Data frame. The output from \code{spsurvey::cat.analysis()} sliced to a single combination of the values in Type, Subpopulation, MANAGEMENT.QUESTION, and Indicator.
#' @param benchmarks Data frame. The output from \code{read.benchmarks()}.
#' @param indicator.lut Optional data frame. Defaults to the output from \code{indicator.lookup()} which matches the indicators found in TerrADat.
#' @return A \code{ggplot} point plot with confidence interval bars.
#' @export
plot_indicator_distribution <- function(analysis.df,
                                        benchmarks,
                                        indicator.lut = NULL,
                                        indicator.variable = "INDICATOR.TDAT",
                                        indicator.name = "INDICATOR.NAME") {
  # Just get an easy to use name
  current.data <- analysis.df
  names(current.data) <- toupper(names(current.data))
  # Just sanitizing the names of things
  benchmarks.current <- benchmarks
  names(benchmarks.current) <- toupper(names(benchmarks))
  # Get the human-friendly name of the indicator
  # If there isn't a lookup table already, use the default
  if (is.null(indicator.lut)) {
    indicator.lut <- indicator.lookup()
  }
  names(indicator.lut) <- toupper(names(indicator.lut))
  indicator.variable <- toupper(indicator.variable)
  indicator.name <- toupper(indicator.name)
  if (!all(c(indicator.variable, indicator.name) %in% names(indicator.lut))){
    stop("Both the indicator.variable and indicator.name arguments must be strings which match the names of variables in the indicator lookup table.")
  }
  indicator.name <- indicator.lut[indicator.lut[[INDICATOR.TDAT]] %in% current.data$INDICATOR, indicator.variable]
  # Figure out which categories need to exist for the benchmark
  categories <- unique(dplyr::filter(.data = benchmarks.current,
                                     benchmarks.current$MANAGEMENT.QUESTION %in% current.data$MANAGEMENT.QUESTION,
                                     benchmarks.current$INDICATOR.TDAT %in% current.data$INDICATOR)$EVALUATION.CATEGORY)

  # Make sure that if a category had no points, a row is added for it with the correct values and 0s
  for (category in categories[!(categories %in% current.data$CATEGORY)]) {
    rework <- current.data[1,]
    rework$CATEGORY <- category
    rework[1, 5:(ncol(rework)-1)] <- 0
    current.data <- rbind(current.data, rework)
  }

  # Rename the variables so that we don't need to know the confidence level
  names(current.data)[grepl(names(current.data), pattern = "^UCB[0-9]{2}Pct\\.P$", ignore.case = TRUE)] <- "UCB"
  names(current.data)[grepl(names(current.data), pattern = "^LCB[0-9]{2}Pct\\.P$", ignore.case = TRUE)] <- "LCB"

  # Make the plot!
  current.plot <- ggplot2::ggplot(data = current.data,
                                  ggplot2::aes(x = CATEGORY,
                                               y = ESTIMATE.P,
                                               color = CATEGORY)) +
    # ggplot2::geom_bar(stat = "identity",
    #                   width = 0.5) +
    ggplot2::geom_pointrange(ggplot2::aes(ymax = UCB,
                                          ymin = LCB),
                             size = 1.5,
                             shape = 18) +
    ggplot2::coord_flip() +
    # ggplot2::geom_errorbar(ggplot2::aes(ymax = UCB,
    #                                     ymin = LCB),
    #                        width = 0.25) +
    ggplot2::scale_color_brewer(type = "div",
                               palette = "RdYlBu") +
    ggplot2::ylim(0, 100) +
    ggplot2::ggtitle(paste0("Percentage of Reporting Unit by Condition Category: \n", indicator.name)) +
    ggplot2::ylab("Estimated Percent of Reporting Unit Area")

  # Figure out what the required proportion is
  required.proportion <- unique(dplyr::filter(.data = benchmarks.current,
                                              benchmarks.current$MANAGEMENT.QUESTION %in% current.data$MANAGEMENT.QUESTION,
                                              benchmarks.current$INDICATOR.TDAT %in% current.data$INDICATOR)$REQUIRED.PROPORTION)
  # Remove any NA values that have somehow made their way in and convert to percent
  required.proportion <- required.proportion[!is.na(required.proportion)] * 100
  # If it's empty, that will be assumed to be a 0
  if (length(required.proportion) < 1) {
    required.proportion <- 0
  }

  # Add the line for the required proportion if appropriate
  if (required.proportion > 0) {
    current.plot <- current.plot +
      ggplot2::geom_hline(yintercept = required.proportion,
                          colour = "gold",
                          size = 2)
  }
  return(current.plot)
}

#' Plotting Indicator Category Distributions For Multiple Indicators/Benchmarks/Reporting Units
#' A wrapper for plot_indicator_distribution to apply it across multiple combinations of values in Type, Subpopulation, MANAGEMENT.QUESTION, and Indicator simulataneously.
#' @param analysis.df Data frame. The output from \code{spsurvey::cat.analysis()} sliced to a single combination of the values in Type, Subpopulation, MANAGEMENT.QUESTION, and Indicator.
#' @param benchmarks Data frame. The output from \code{read.benchmarks()}.
#' @param indicator.lut Optional data frame. Defaults to the output from \code{indicator.lookup()} which matches the indicators found in TerrADat.
#' @export
multiplot_indicator_distribution <- function(analysis.df,
                                             benchmarks,
                                             indicator.lut = NULL){
  # Get the analysis output without the "Total" category entries
  analysis.trim <- dplyr::filter(.data = analysis.df,
                                 Category != "Total")

  # Turn that into a list of data frames for unique combinations of Subpopulation, MO, and Indicator
  analysis.split <- split(analysis.trim, interaction(analysis.trim$Type,
                                                     analysis.trim$Subpopulation,
                                                     analysis.trim$MANAGEMENT.QUESTION,
                                                     analysis.trim$Indicator))

  # For each of those data frames, create a data frame with the plot and the identifying fields
  plots.list <- lapply(X = analysis.split,
                       FUN = function(X,
                                      benchmarks,
                                      indicator.lut) {
                         # Just get an easy to use name
                         current.data <- X
                         # Get the human-friendly name of the indicator
                         indicator.name <- indicator.lut$indicator.name[indicator.lut$indicator.tdat %in% current.data$Indicator]
                         # Figure out which categories need to exist for the benchmark
                         categories <- unique(dplyr::filter(.data = benchmarks,
                                                            benchmarks$Management.Question %in% current.data$MANAGEMENT.QUESTION,
                                                            benchmarks$indicator.tdat %in% current.data$Indicator)$Evaluation.Category)

                         # Make sure that if a category had no points, a row is added for it with the correct values and 0s
                         for (category in categories[!(categories %in% current.data$Category)]) {
                           rework <- current.data[1,]
                           rework$Category <- category
                           rework[1, 5:(ncol(rework)-1)] <- 0
                           current.data <- rbind(current.data, rework)
                         }

                         # Rename the variables so that we don't need to know the confidence level
                         names(current.data)[grepl(names(current.data), pattern = "^UCB[0-9]{2}Pct\\.P$")] <- "UCB"
                         names(current.data)[grepl(names(current.data), pattern = "^LCB[0-9]{2}Pct\\.P$")] <- "LCB"

                         # Make the plot!
                         current.plot <- plot_indicator_distribution(current.data,
                                                                     benchmarks)

                         # Create the output data frame with the identifying variables. Just one row!
                         output <- distinct(dplyr::select(.data = current.data,
                                                          Type:Indicator, MANAGEMENT.QUESTION))

                         # Stick the plot into a list and add that to the new variable "plot"
                         # It needs to be in a list because a list is a single object but the plot is 9. Go figure
                         if (nrow(output) == 1) {
                           output[["plot"]] <- list(current.plot)
                           return(output)
                         } else {
                           return(NULL)
                         }

                       }, benchmarks = benchmarks,
                       indicator.lut = indicator.lut)
  # Turn it all into a single data frame to pull the plots from!
  if (length(plots.list) < 2) {
    plots.df <- plots.list[[1]]
  } else {
    plots.df <- dplyr::bind_rows(plots.list)
  }
  return(plots.df)
}

#' Plotting Fate Distributions
#' @param point.weights Data frame. The point weights output from \code{weight()} with a common reporting unit.
#' @param fates Optional data frame. Defaults to the output from \code{fate.lookup()} which matches the plot fates found in sample design databases.
#' @return A \code{ggplot} plot of the fate distribution as a histogram.
#' @export
plot_fate_distribution <- function(point.weights,
                                   fates = NULL) {
  # If there isn't a fate lookup table, use the default
  if (is.null(fates)) {
    fates <- fate.lookup()
  }

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
    # ggplot2::geom_text(data = subset(summary,
    #                                  pct != 0),
    #                    ggplot2::aes(label = paste0(round(pct, digits = 1), "%"),
    #                                 y = pct),
    #                    # position = ggplot2::position_jitter()) +
    #                    position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::ylab("Percent of Plots") +
    ggplot2::xlab("Reporting Unit") +
    ggplot2::ggtitle(label = "Percentages of Final Point Designations\nWithin Reporting Unit") +
    ggplot2::theme(aspect.ratio = 1/6,
                   axis.title.y = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   panel.background = element_blank(),
                   panel.grid = element_blank(),
                   plot.margin = unit(c(0, 0, 0, 0), "cm"),
                   strip.background = element_blank())

  return(plot)
}

