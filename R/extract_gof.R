#' extract_gof
#'
#' @param x objekt
#' @param include.r,include.pseudo,include.rmse,include.sigma,include.variance,include.devianze,include.loglik,include.aic,include.bic,include.nobs Alles wie bei APA_TAble
#' @param digits nachkommastellen
#' @param ... extra Argumente
#' @return tibble
#' @export
#'
#' @examples
#'
#'
extract_gof <- function(x,
                        include.r = TRUE,include.pseudo = TRUE,
                        include.rmse = TRUE,include.sigma = FALSE,include.variance = FALSE,
                        include.devianze = FALSE,
                        include.loglik = FALSE,
                        include.test=FALSE,
                        include.aic = TRUE,include.bic = include.aic,
                        include.nobs = TRUE,
                        digits = 2,
                        fix_format=FALSE,
                        ...) {
  
  
  res <-  broom::glance(x)
  param <-  "term"
  
  if (include.r | include.pseudo) {
    if( any(names(res) %in% "r.squared")){
      param <- c(param, c("r.squared", "adj.r.squared"))
    }else{  ans_r <- R2(x)
    res <- cbind(res, ans_r)
    param <- c(param, names(ans_r))
    }
  }
  
  
  if (include.aic)
    param <- c(param, "AIC")
  
  if (include.bic)
    param <- c(param, "BIC")
  
  if (include.rmse) {
    param <- c(param, "RMSE")
    res <- cbind(res, RMSE(x)[2])
  }
  
  if (include.loglik)
    param <- c(param, "logLik")
  if (include.devianze)
    param <- c(param, "deviance")
  if (include.sigma)
    param <- c(param, "sigma")
  
  
  if (fix_format) {
    res <- dplyr::tbl_df(
      plyr::llply(res,
                  function(z)
                    formatC(z,
                            digits = digits,
                            format = "f")))
    if (include.test) {
      param <- c(param, "Test")
      res$Test <- "nicht implementiert"
    }
    
    if (include.nobs) {
      param <- c(param, "Obs")
      res$Obs <- formatC(nobs(x), digits = 0, format = "f")
    }
  }
  else{
    res <- round(res, digits = digits)
    if (include.test) {
      param <- c(param, "Test")
      res$Test <- "nicht implementiert"
    }
    if (include.nobs) {
      param <- c(param, "Obs")
      res$Obs <- nobs(x)
    }
  }
  param <- intersect(names(res), param)
  tibble::as_tibble(res[param])
}