 #' APA Syle Table
#'
#' All functions in stringr start with APA_ and take a Object (formula, data.frame, lm, ...)
#' as the first argument.
#'
#' @name APA_
#' @param x An object to be converted into a tidy data.frame or Formula
#' @param data data.frame wenn x eine Formel ist
#' @param digits Nachkommastellen
#' @param caption,note Ueberschrift an Output
#' @param Formula,test,order,decreasing,sig_test,na.action Interne Parameter
#' @param ... extra arguments
#' @return a data.frame or list with data.frame
#' @export
APA_NULL <- function(x, ...) {
    Text("no input")
    invisible(data.frame())
  }
