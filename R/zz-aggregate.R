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
    if (tibble::is_tibble(.data))
      tibble::as_tibble(plyr::llply(.data, fun, ...))
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
#'   transpose3(DF2)
#' @noRd
#' 
#' 
#' 
transpose3 <- function(x) {
  lvl = levels(x[[1]])
  transposed <- t(apply(x, 1, function(z) {
    trans <- NULL
    for (i in lvl) {
      tr <- which(z == i)
      if (length(tr) == 0)
        tr <- 0
      names(tr) <- i
      trans <- c(trans, tr)
    }
    trans
  }))
  # kontrolle cbind(x, transposed)
  as.data.frame(transposed) 
}





#' htest, xtable, anova
fix_data_frame2 <- function(...) {
  rslt <- data.frame(...)
  rslt[[2]] <- stp25rndr::Format2(rslt[[2]], 2)
  rslt[[ncol(rslt)]] <-
    stp25rndr::rndr_P(rslt[[ncol(rslt)]], FALSE)
  rslt
}