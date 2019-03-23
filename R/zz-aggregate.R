#' @param .data an dplyr
#'
#' @param fun an dplyr default = as.numeric
#' @param stringsAsFactors an data.frame default = FALSE
#' @param ... 
#'
#' @noRd
#' 
dapply1 <-
  function (.data,
            fun = function(x)
              as.numeric(x),
            stringsAsFactors = FALSE,
            ...) {
    if (inherits(.data, "tbl_df"))
      dplyr::tbl_df(plyr::llply(.data, fun, ...)) 
    else
      data.frame(plyr::llply(.data, fun, ...),
                 stringsAsFactors=stringsAsFactors) 
  }



#' Rangreihe transortieren
#'
#' @param x data.frame
#'
#' @return data.frame
#' @examples
#'
#'   DF2 <-   data.frame(
#'   R1 = factor(c("A", "A", "A", "C", "C", "A"),   c("A", "B", "C", "D")),
#'   R2 = factor(c("B", "B", "B", "A", "B", "D"),   c("A", "B", "C", "D")),
#'   R3 = factor(c("C", "C", "C", "B", "A", "B"),   c("A", "B", "C", "D"))
#'   )
#'   transpose(DF2)
#' @noRd
#' 
transpose <- function(x) {
  last <- nrow(x)
  x <- cbind(x, id = 1:last)
  last_column <- ncol(x)
  
  x <- stp25aggregate::Melt2(x,
                             id.vars = last_column,
                             key = "variable",
                             value = "rang")
  
  x <- tidyr::spread(x, rang, variable)
  dapply1(x[-1])
}