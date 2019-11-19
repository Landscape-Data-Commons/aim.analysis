#' Display the message if criteria are met
#' @description Sometimes you want to display a message not only when certain criteria are met but also only if you've asked a function to be verbose
#' @param string Character string. The text to display.
#' @param criteria Optional logical vector. One or more logical values or statements, e.g. \code{c(length(vector) > 1, is.character(vector))}. These are compared/combined according to \code{criteria_relationship}. Defaults to \code{TRUE}.
#' @param criteria_relationship Character string. The approach to comparing/combining the values in \code{criteria}. Valid options are \code{"all"} (resolves \code{criteria} to \code{TRUE} if all values in \code{criteria} are \code{TRUE}), \code{"any"} (resolves \code{criteria} to \code{TRUE} if any of the values in \code{criteria} are \code{TRUE}), \code{"xor"} (resolves \code{criteria} to \code{TRUE} if only one value in \code{criteria} is \code{TRUE}), and \code{"none"} (resolves \code{criteria} to \code{TRUE} if all values in \code{criteria} are \code{FALSE}). Defaults to \code{"all"}.
#' @param type Character string. Which function to use to relay \code{string}, \code{message()} or \code{warning()}. Valid values are \code{"message"} and \code{"warning"}. Defaults to \code{"message"}.
#' @param verbose Logical value. Intended to take the logical value from the parent function signalling whether or not it should be verbose, \code{string} will only be relayed to the user if this is \code{TRUE}. Defaults to \code{TRUE}.
vmessage <- function(string,
                     criteria = NULL,
                     criteria_relationship = "all",
                     type = "message",
                     verbose = TRUE){
  if (!is.character(string)) {
    string <- as.character(string)
  }

  if (is.null(criteria)) {
    criteria <- TRUE
  }
  if (!is.logical(criteria)){
    stop(paste0("criteria is class ", class(criteria), " but it must be logical"))
  }

  criteria <- switch(criteria_relationship,
                     "all" = {all(criteria)},
                     "any" = {any(criteria)},
                     "xor" = {sum(criteria) == 1},
                     "none" = {sum(criteria) == 0})

  if (criteria & verbose) {
    switch(type,
           "message" = {message(string)},
           "warning" = {warning(string)})
  }
}
