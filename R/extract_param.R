#' @rdname extract
#'
#' @param x Objekt
#' @param include.b,include.beta,include.ci,include.se,include.odds,include.odds.ci,include.statistic,include.p,include.stars,include.df,include.effects,conf.level,conf.method Parameter wie in APA_Tabelle beschrieben.
#' @param ... weitere params
#'
#' @return data.frame tibble  "term","estimate", "beta","std.error" ,"statistic","p.value","stars","conf.high","conf.low","odds","odds.conf.low","odds.conf.high","df","group"
#' @export
#'
#' @examples
#' fit <- lm(Sepal.Width ~ Petal.Length + Petal.Width, iris)
#' broom::tidy(fit)
#' broom::glance(fit)
#'
#' summary(fit)
#' extract_param(fit)
#' APA_Table(fit)
#' APA_Table(fit, type="long")
#'
#'
extract_param  <- function(x,
                          include.b = TRUE,
                          include.se = TRUE,
                          include.beta = FALSE,
                          include.ci = FALSE,

                          include.odds = FALSE,
                          include.odds.ci = if(include.odds & include.ci) TRUE else  FALSE,
                          include.statistic = TRUE,
                          include.p = TRUE,
                          include.stars = FALSE,
                          include.df = FALSE,

                          include.effects = c("ran_pars", "fixed"),

                          include.eta = TRUE,
                          include.sumsq = TRUE,
                          include.meansq = FALSE,

                          conf.int = TRUE,# if (include.ci | include.odds.ci) TRUE else  FALSE ,
                          conf.level = 0.95,
                          conf.method = "Wald",
                          fix_format = FALSE,
                          digits.param = 3,
                          digits.odds = 2,
                          digits.test = 2,
                          digits.beta = 2,
                          format="fg",
                          conf.style.1 = FALSE,
                          ...) {

  if (inherits(x, "aov")) return(extract_param_aov(x,
                                              include.eta ,
                                              include.sumsq ,
                                              include.meansq ,
                                              fix_format,
                                              digits.test,
                                              format="f"))
  param <-  "term"
  res <- NULL
 # Error in broom:tidy
  if (inherits(x, "lmerModLmerTest")) {
    res <- tidy_lmer(
      x,
      effects = include.effects,
      conf.int = conf.int,
      conf.level = conf.level,
      conf.method =  conf.method) 
    res$beta <- NA
    coefs <- res
  }
  else{
     if (inherits(x, "lmerMod")) {
      res <- broom::tidy(
        x,
       # effects =  "fixed",
        conf.int = conf.int,
        conf.level = conf.level,
        conf.method =  conf.method)
      res$p.value <-NA
      res$p.value[which(res$group=="fixed")]<-
        as.vector(lmerTest:::summary.lmerModLmerTest(x)$coefficients[,5])
    } else{
      res <- broom::tidy(
      x,
      effects =  "fixed",
      conf.int = conf.int,
      conf.level = conf.level,
      conf.method =  conf.method)
      res$group <- "fixed"
      res$df <- NA
    }
    
    if (inherits(x, "lme")) {
      my_se <- sqrt(diag(vcov(x)))
      my_coef <- lme4::fixef(x)
      res$conf.low = my_coef - 1.96 * my_se
      res$conf.high = my_coef + 1.96 * my_se
      warning("Eigene Methode: Assuming a normal approximation for the fixed effects (1.96*standard error).")
      }
    coefs <- res
  }


  if (include.b) {
    param <- c(param,  "estimate")
    if (fix_format)
      coefs$estimate <-
        stp25rndr::Format2(res$estimate,
                                    digits = digits.param, format = format)
  }

  if (include.ci) {
      if (fix_format) {
        if (conf.style.1) {
          param <- c(param,   c("conf"))
          coefs$conf <-
             stp25rndr::rndr_CI2(
              res[, c("conf.high", "conf.low")] ,
              digits= digits.param,
              format=format)
        }
        else{
          param <- c(param, c("conf.high", "conf.low"))
          coefs$conf.high <-
            stp25rndr::Format2(res$conf.high,
                                        digits.param, format = format)
          coefs$conf.low <-
            stp25rndr::Format2(res$conf.low,
                                        digits.param, format = format)
        }
      } else {
        param <- c(param,   c("conf.high", "conf.low"))
      }
    }

  if (include.beta & inherits(x, "lm") & !inherits(x, "glm")) {
      param <- c(param, "beta")
      # b <- res$estimate[-1]
      # 
      # sx <- sapply(x$model[-1], function(z) {
      #   if (!is.numeric(z)) {
      #     cat("\nBeta macht bei ", class(z), "keinen Sinn!\n")
      #     z <- as.numeric(z)
      #   }
      #   sd(z, na.rm = TRUE)
      # })
      # sy <- sd(x$model[[1]], na.rm = TRUE)
      # coefs$beta <- c(NA, b * sx / sy)
      # Die obere Funktion rechnet die Interaktionen falsch!

      coefs$beta <- as.vector(c(NA, coef(rockchalk::standardize(x))))
      
      if (fix_format)
        coefs$beta <-
        stp25rndr::Format2(coefs$beta,
                           digits.beta,
                           format = "f")
      
  }
  
  if (include.se) {
    param <- c(param, "std.error")
    if (fix_format)
      coefs$std.error <-
        stp25rndr::Format2(res$std.error,
                           digits.param, format = format)
    
  }
  
  if (include.statistic) {
    if (inherits(x, "glm") & conf.method == "Wald")
      coefs$statistic <- (res$estimate / res$std.error) ^ 2
    
    param <- c(param, "statistic")
    if (fix_format)
      coefs$statistic <- stp25rndr::Format2(res$statistic,
                                            digits.test, format = "f")
  }
  
  if (include.odds & inherits(x, "glm")) {
    param <- c(param, "odds")
    if (fix_format)
      coefs$odds <-
        stp25rndr::rndr_ods(exp(res$estimate),  digits.odds)
    else
      coefs$odds <- exp(res$estimate)
    
    if (coefs[1, 1] == "(Intercept)")
      coefs$odds[1] <- NA
    
  }
  
  if (include.odds.ci & inherits(x, "glm")) {
    coefs$odds.conf.low <-  res$odds.conf.low <- exp(res$conf.low)
    coefs$odds.conf.high <-
      res$odds.conf.high <- exp(res$conf.high)
    
    if (fix_format) {
      if (conf.style.1) {
        param <- c(param, c("odds.conf"))
        coefs$odds.conf <-
          stp25rndr::rndr_CI2(res[, c("odds.conf.low", "odds.conf.high")] ,
                              digits = digits.odds,
                              format = "f")
        if (res[1, 1] == "(Intercept)")
          coefs$odds.conf[1] <-  NA
      }
      else{
        param <- c(param, c("odds.conf.low", "odds.conf.high"))
        coefs$odds.conf.low <-
          stp25rndr::rndr_ods(coefs$odds.conf.low, digits.odds)
        
        coefs$odds.conf.high <-
          stp25rndr::rndr_ods(coefs$odds.conf.high , digits.odds)
        
        
        if (res[1, 1] == "(Intercept)") {
          coefs$odds.conf.low[1] <-  NA
          coefs$odds.conf.high[1]  <- NA
        }
      }
    }
    else{
      param <- c(param, c("odds.conf.low", "odds.conf.high"))
      if (coefs[1, 1] == "(Intercept)") {
        coefs$odds.conf.low[1] <-  NA
        coefs$odds.conf.high[1]  <- NA
      }
      
    }
    
  }
  
  if (include.stars) {
    param <- c(param, "stars")
    coefs$stars <- stp25rndr::rndr_Stars(res$p.value)
  }
  
  if (include.p) {
    param <- c(param, "p.value")
    if (fix_format)
      coefs$p.value <-
        stp25rndr::rndr_P(res$p.value, symbol.leading = c("", "<"))
  }
  
  tibble::as_tibble(coefs[param])
}




#' @description Metode fuer ANOVA-Modelle
#' @rdname extract
#'
extract_param_aov <- function(x,
                              include.eta = TRUE,
                              include.sumsq = TRUE,
                              include.meansq = FALSE,
                              fix_format = FALSE,
                              digits.test = 2,
                              format = "f",
                              ...) {
  param <- "term"
  res <- broom::tidy(x)
  
  if (!include.sumsq) {
    param <- c(param, "sumsq")
    if (fix_format)
      res$sumsq <-
        stp25rndr::Format2(res$sumsq, digits = 2, format = format)
  }
  
  if (!include.meansq) {
    param <- c(param, "meansq")
    if (fix_format)
      res$meansq <-
        stp25rndr::Format2(res$meansq, digits = 2, format = format)
  }
  
  param <- c(param, c("df", "statistic"))
  if (include.eta) {
    if (is(x, "lm") | is(x, "anova")) {
      param <- c(param, c("eta.sq", "eta.sq.part"))
      k <- ncol(res)
      myeta <-  etaSquared2(x, 2, FALSE)
      
      if (nrow(myeta) != nrow(res))
        stop(
          "extract_param_aov mit etaSquared eventuell liefert car:Anova(lm(...)) das richtige Ergebniss"
        )
      res <- cbind(res[,-k], myeta , res[k])
      
      if (fix_format) {
        res$eta.sq <-
          stp25rndr::Format2(res$eta.sq,
                             digits = digits.test, format = format)
        res$eta.sq.part <-
          stp25rndr::Format2(res$eta.sq.part,
                             digits = digits.test,
                             format = format)
      }
    }
  }
  
  param <- c(param, "p.value")
  
  if (fix_format) {
    res$statistic <-
      stp25rndr::Format2(res$statistic, digits = digits.test, format = format)
    
    res$p.value <-
      stp25rndr::rndr_P(res$p.value, symbol.leading = c("", "<"))
    res$df <-
      stp25rndr::Format2(res$df, digits = 0, format = format)
  }
  tibble::as_tibble(res[param])
}

