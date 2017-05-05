.onLoad <- function(libname, pkgname){
  ## Because piping within the functios won't work unless this is attached
  library(tidyverse)
  ## If arcgisbinding is installed and R is 32-bit, then it needs to be attached and arc.check_product() run to get it set up
  if ("arcgisbinding" %in% rownames(installed.packages()) & !grepl(x = R.Version()$arch, pattern = "^x86_64")) {
    library(arcgisbinding)
    arc.check_product()
  }
}
