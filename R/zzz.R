.onLoad <- function(libname, pkgname){
  library(tidyverse)
  if ("arcgisbinding" %in% rownames(installed.packages()) & !grepl(x = R.Version()$arch, pattern = "^x86_64")) {
    library(arcgisbinding)
    arc.check_product()
  }
}
