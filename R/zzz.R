.onLoad <- function(libname, pkgname){
  ## Because piping within the functions won't work unless this is attached
  `%>%` <- magrittr::`%>%`
}
