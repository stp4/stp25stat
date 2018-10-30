

#' rndr_CI
#'
#' Formatieren von Konfidenzintervallen.
#'
#' @param ci matrix
#' @param digits Nachkommastellen
#' @param sep,sep_1,sep_2 Seperator
#' @param format an formatC "fg"
#'
#' @return string
#'
rndr_CI2 <-  function(ci,
                      digits = 3,
                      sep =   ",",
                      sep_1 =  "[",
                      sep_2 =  "]",
                      format = "g") {
  cis <-  paste0(
    sep_1,
    stp25rndr:::Format2.default(ci[[1]],
                                digits, format = format),
    sep,
    " ",
    stp25rndr:::Format2.default(ci[[2]],
                                digits, format = format),
    sep_2
  )

  cis[is.na(ci[[1]])] <- NA
  cis
}





#stp25rndr:::Format2.matrix
rndr_percent_ftable <- function(x,
                                count = NULL,
                                digits = options()$stp25$apa.style$prozent$digits[1],
                                percentage_str = options()$stp25$apa.style$prozent$percentage_str,
                                style = options()$stp25$apa.style$prozent$style,
                                null_percent_sign = options()$stp25$apa.style$prozent$null_percent_sign) {

  # cat("\nrndr_percent_ftable")

  x_char <- apply(x, 2, function(y)
    paste0(
      formatC(
        y,
        format = "f",
        digits = digits,
        decimal.mark = getOption("OutDec")
      ),
      percentage_str
    ))


  if (!is.null(count)) {
    if (style == 1)
      res <-
        matrix(paste0(x_char, " (", count, ")"),
               nrow =  nrow(count),
               ncol = ncol(count))
    else
      res <-
        matrix(
          paste0(count, " (", x_char, percentage_str, ")"),
          nrow =  nrow(count),
          ncol = ncol(count)
        )

    if (!is.null(null_percent_sign))
      res[which(count == 0)] <- null_percent_sign

    ans <- stp25output::fix_to_data_frame(count)
    n <- ncol(ans)
    ans[, (n - ncol(res) + 1):n] <- res
  }

  else  {
    if (!is.null(null_percent_sign))
      #   digits<-2
      # 10^-digits
      res[which(x <  10^-digits)] <- null_percent_sign
    ans <- stp25output::fix_to_data_frame(x_char)
  }
  ans

}
