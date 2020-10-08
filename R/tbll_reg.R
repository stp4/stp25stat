#' @rdname Tbll
#' 
#' @description Tbll_reg = APA_Table
#' @export
#'
#' @examples
#' 
#'  lm1 <- lm(breaks ~ wool + tension, data = warpbreaks)
#'  lm2 <- lm(breaks ~ wool * tension, data = warpbreaks)
#'  
#'  # aufruf von APA_Table()
#'  Tbll_reg(
#'   lm1,
#'   lm2,
#'   include.p = FALSE,
#'   include.ci = TRUE,
#'   include.se=FALSE,
#'   caption = "Regression Table"
#' )
#' 
#' # aufruf von APA2() 
#' Tbll_reg(lm1, lm2)
#' 
#' 
Tbll_reg  <- function(...,
                      names = NULL,
                      col_names=NULL,   #c("b", "SE", "p"),
                      digits = NULL,
                      include.b = TRUE,
                      include.se = TRUE,
                      include.odds = FALSE,
                      include.ci = FALSE,
                      include.odds.ci=FALSE,
                      include.p = FALSE,
                      include.stars = if (include.p) FALSE  else TRUE,
                      include.r = TRUE,
                      include.pseudo = FALSE,
                      include.custom = NULL,
                      include.aic = TRUE,
                      include.bic = include.aic,
                      include.gof=TRUE,
                      include.param=TRUE
                      ) {
rslt <- APA_Table(..., 
                  names = names,
                  col_names=col_names, 
                  digits = digits,
                  include.b = include.b,
                  include.se = include.se,
                  include.odds = include.odds,
                  include.ci = include.ci,
                  include.odds.ci=include.odds.ci,
                  include.p = include.p,
                  include.stars = include.stars,
                  include.r = include.r,
                  include.pseudo = include.pseudo,
                  include.custom = include.custom,
                  include.aic = include.aic,
                  include.bic = include.bic,
                  include.gof=include.gof,
                  include.param=include.param,
                  output = FALSE)
rslt
}
