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
#' fit1 <- lm(sr ~ pop15 + pop75 + dpi + cut(ddpi, 3), data = LifeCycleSavings)
#'extract_gof2(fit1)
#'extract_gof2(fit1, fix_format = TRUE)
#'
extract_gof <- function(x,
                         include.r = TRUE,
                         include.pseudo = TRUE,
                         include.rmse = TRUE,
                         include.sigma = FALSE,
                         include.variance = FALSE,
                         include.devianze = FALSE,
                         include.loglik = FALSE,
                         include.test = FALSE,
                         include.aic = TRUE,
                         include.bic = include.aic,
                         include.nobs = TRUE,
                         digits = 2,
                         fix_format = FALSE,
                         ...) {
  rslt <-  broom::glance(x)
  param <-  "term"
  
  if (include.r | include.pseudo) {
    if (any(names(rslt) %in% "r.squared")) {
      param <- c(param, c("r.squared", "adj.r.squared"))
    } else{
      ans_r <- R2(x)
      rslt <- cbind(rslt, ans_r)
      param <- c(param, names(ans_r))
    }
  }
  
  
  if (include.aic)
    param <- c(param, "AIC")
  
  if (include.bic)
    param <- c(param, "BIC")
  
  if (include.rmse) {
    param <- c(param, "RMSE")
    rslt <- cbind(rslt, RMSE(x)[2])
  }
  
  if (include.loglik)
    param <- c(param, "logLik")
  if (include.devianze)
    param <- c(param, "deviance")
  if (include.sigma)
    param <- c(param, "sigma")
  
  
  if (fix_format) {
    rslt <- tibble::as_tibble(
      plyr::llply(rslt,
                  function(z) {
                    if (!is.na(z)) {
                      formatC(z, digits = digits, format = "f")
                    } else {
                      ""
                    }
                  }))
    
    if (include.test) {
      param <- c(param, "Test")
      rslt$Test <- "nicht implementiert"
      
    }
    
    if (include.nobs) {
      param <- c(param, "Obs")
      rslt$Obs <- formatC(nobs(x), digits = 0, format = "f")
    }
  }
  else{
    rslt <- tibble::as_tibble(
      plyr::llply(rslt,
                  function(z) {
                    if (!is.na(z)) {
                      round(z, digits = digits )
                    } else {
                      NA
                    }
                  }))
    
    if (include.test) {
      param <- c(param, "Test")
      rslt$Test <- "nicht implementiert"
    }
    if (include.nobs) {
      param <- c(param, "Obs")
      rslt$Obs <- nobs(x)
    }
  }
  param <- intersect(names(rslt), param)
  tibble::as_tibble(rslt[param])
}

