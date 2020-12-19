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
#' # aufruf von APA2() und return list
#' Tbll(lm1, lm2)
#' 
#' 
#' #' lm1 <- lm(breaks ~ wool + tension, data = warpbreaks2)
#' 
#' 
#' Tbll_reg(lm(breaks ~1, data = warpbreaks2),
#'          lm1, lm2)
#'          
Tbll_reg  <- function(...,
                      caption = "",
                      include.label = FALSE,  # geht nicht
                      names = NULL,
                      digits = NULL,
                      include.b = TRUE, include.se = TRUE, include.beta = FALSE,
                      include.ci = FALSE,
                      include.odds = FALSE, include.odds.ci = FALSE,
                      include.p = FALSE, include.stars = if (include.p) FALSE else TRUE,
                      include.r = TRUE, include.pseudo = FALSE,
                      include.aic = TRUE, include.bic = include.aic,
                      include.param = TRUE, include.gof = TRUE,include.custom = NULL
                      ) {
  fit <- list(...)
  if( is.null(names)){
    names <- abbreviate(
    gsub("[~??+\\:=]", "",
         as.character(as.list(sys.call()))[seq_len(length(fit))+1]),
    minlength=7
    )
  }
  
    rslt <- regression_table(
      fit,
      caption = caption, note = "", output = FALSE,
      custom.model.names = names,
      include.param=include.param, include.gof=include.gof, include.custom = include.custom,
      include.b = include.b, include.se = include.se, include.beta = include.beta,
      include.ci = include.ci,
      include.odds = include.odds, include.odds.ci=include.odds.ci,
      include.statistic = FALSE,
      include.p = include.p, include.stars = include.stars,
      include.df = FALSE,
      include.effects = c("ran_pars", "fixed"),
      ci.level = .95, conf.method = "Wald",
      digits=digits, digits.param = 3,  digits.odds = 2, digits.test = 2, digits.beta = 2,
      format = "fg",
      include.r = include.r, include.pseudo = include.pseudo,
      include.rmse = TRUE,
      include.sigma = FALSE,
      include.variance = FALSE,
      include.devianze = FALSE,
      include.loglik = FALSE, include.test = FALSE,
      include.aic = include.aic, include.bic = include.bic,
      include.nobs = TRUE,
      rgroup = c("Parameter", "Goodness of fit"),
      dictionary = c(std.error = "SE", estimate = "b", p.value = "p")
    )
    
    rslt
  }
 

