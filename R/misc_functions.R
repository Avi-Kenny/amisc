#' Alias for as.integer function
#'
#' @description Alias for as.integer function; see \code{as.integer}
#'     documentation for more info.
#' @param x object to be coerced or tested
#' @return Integer output
#' @examples
#' In(3!=4)
#' @export
In <- as.integer



#' Logit function
#'
#' @description Computes the logit function
#' @param x Numeric input
#' @return Numeric output
#' @examples
#' logit(0.9)
#' @export
logit <- function(x) { log(x/(1-x)) }



#' Expit function
#'
#' @description Computes the expit function
#' @param x Numeric input
#' @return Numeric output
#' @examples
#' expit(3)
#' @export
expit <- function(x) { 1 / (1+exp(-x)) }



#' Derivative of the logit function
#'
#' @description Computes the derivative of the logit function
#' @param x Numeric input
#' @return Numeric output
#' @examples
#' deriv_logit(0.9)
#' @export
deriv_logit <- function(x) { 1 / (x-x^2) }



#' Derivative of the expit function
#'
#' @description Computes the derivative of the expit function
#' @param x Numeric input
#' @return Numeric output
#' @examples
#' deriv_expit(3)
#' @export
deriv_expit <- function(x) { exp(x) / ((1+exp(x))^2) }



#' Helper function for debugging; prints an integer, timestamp, and optional
#'     message
#'
#' @noRd



#' Debugging checks
#'
#' @description A helper function for debugging that parses and outputs a
#'     message containing a number, a timestamp, and (optionally) a message.
#' @param num A number to be printed (useful to reference a specific place in a
#'     code file
#' @param msg A message to be printed
#' @return NULL
#' @examples
#' chk(1, "code execution began")
#' x <- runif(10)
#' chk(2, "runif completed")
#' y <- rnorm(10)
#' chk(3, "rnorm completed")
#' @export
chk <- function(num, msg="") {
  if (msg=="") {
    str <- paste0("Check ", num, ": ", Sys.time())
  } else {
    str <- paste0("Check ", num, " (", msg, "): ", Sys.time())
  }
  message(str)
}



