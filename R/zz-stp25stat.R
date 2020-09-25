#' stp25stat: Functions for Statistical Computations
#'
#'
#' Convert statistical analysis objects from R into APA-Tabls.
#'
#' @import stp25rndr
#' @import stp25output
#' @import stp25formula
#' @import stp25aggregate
#'
"_PACKAGE"




#' Pipe operator
#'
#' See \code{magrittr} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL



 




#' Information uere die verwendeten Methoden
#'
#' @param methode,library,fun  Text
#'
#' @return data.frame()

Info_Statistic <-
  function(methode = "describe",
           library = "base",
           fun = "summary",
           my_methodes = "") {
    Text("Methodes: ",  my_methodes)
    data.frame(
      Methode = methode,
      Library = library,
      Function = fun,
      stringsAsFactors = FALSE
    )
  }