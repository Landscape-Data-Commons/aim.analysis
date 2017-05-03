validate.keys <- function(dd.raw, target.values = c("Target Sampled",
                                                    "TS")){
  ## Initialize the output data frame
  key.errors.df <- data.frame()

  target.values <- c(target.values,
                     "Target Sampled",
                     "TS") %>% unique() %>% str_to_upper()

  ## Check each design database in turn
  for (dd in names(dd.raw$pts)) {
    ## Get the @data slot from the SPDF for the points for this design
    pts.df <- dd.raw$pts[[dd]] %>% .@data
    ## Sanitize the field names
    names(pts.df) <- str_to_upper(names(pts.df))

    ## Grab all the lines where the point was sampled, but there's not a correct PrimaryKey value
    errors.missing.tdat <- pts.df %>%
      filter(FINAL_DESIG %in% target.values,
             !grepl(x = TERRA_TERRADAT_ID, pattern = "^[0-9]{15,24}-[0-9]{1,3}-[0-9]{1,3}$")) %>% .[, c("TERRA_SAMPLE_FRAME_ID", "PLOT_NM", "TERRA_TERRADAT_ID")]
    ## If that turned up anything, then alert the user and add the information about all the points with that error to the output data frame
    if (nrow(errors.missing.tdat) > 0) {
      message(paste0("In ", dd, ", ", nrow(errors.missing.tdat), " points were designated as 'target sampled' but missing a valid PrimaryKey value. See the data frame 'errors' in the output for details."))
      errors.missing.tdat$DD <- dd
      errors.missing.tdat$ERROR <- "Point is designated as 'target sampled' but is missing a valid PrimaryKey value"
      key.errors.df <- rbind(key.errors.df, errors.missing.tdat[, c("DD", "ERROR", "TERRA_SAMPLE_FRAME_ID", "PLOT_NM", "TERRA_TERRADAT_ID")])
    }

    ## Grab all the lines where there's a proper PrimaryKey value but there's not a target designation
    errors.missing.desig <- pts.df %>%
      filter(!(FINAL_DESIG %in% target.values),
             grepl(x = TERRA_TERRADAT_ID, pattern = "^[0-9]{15,24}-[0-9]{1,3}-[0-9]{1,3}$")) %>% .[, c("TERRA_SAMPLE_FRAME_ID", "PLOT_NM", "TERRA_TERRADAT_ID")]
    if (nrow(errors.missing.desig) > 0) {
      message(paste0("In ", dd, ", ", nrow(errors.missing.desig), " points have a valid PrimaryKey value but are not designated as 'target sampled.' See the data frame 'errors' in the output for details."))
      errors.missing.desig$DD <- dd
      errors.missing.desig$ERROR <- "Point has a valid PrimaryKey value but is not designated as 'target sampled'"
      key.errors.df <- rbind(key.errors.df, errors.missing.desig[, c("DD", "ERROR", "TERRA_SAMPLE_FRAME_ID", "PLOT_NM", "TERRA_TERRADAT_ID")])
    }
  }

  if (nrow(key.errors.df) < 1) {
    message("No points designated as 'target sampled' were missing valid TerrADat primary key values and no points with valid primary keys were designated as anything but 'target sampled. This is a good thing and your output should be empty.'")
  }
  return(key.errors.df)
}
