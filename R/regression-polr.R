#' @rdname APA
#' @export
APA.polr   <- function(x, ...)
 {
  res <- as.data.frame(lmtest::lrtest(x))
  # likelihood ratio tests
  paste0("-LL=" ,
         round(as.numeric(logLik(x)), 1),
         ", ",
         rndr_Chisq(res$Chisq[2], abs(res$Df[2]), res[2, "Pr(>Chisq)", drop =
                                                        TRUE]))

}


#' @rdname APA2
#' @description Ordered Logistic or Probit Regression
#' https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
#' @export
#' @examples
#'
#'  #--- Ordered Logistic or Probit Regression
#'  require(MASS)
#'  # options(contrasts = c("contr.treatment", "contr.poly"))
#'   house.plr <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
#'   APA2(house.plr, note= APA(house.plr))
#'
#'
APA2.polr <- function(x,
                      caption = NULL,
                      note = NULL,
                      include.b = TRUE,
                      include.se = TRUE,
                      include.ci = FALSE,
                      include.odds = TRUE,
                      ...) {
  res <- extract(
    x,
    include.b = include.b,
    include.se = include.se,
    include.ci = include.ci,
    include.odds = include.odds,
    ...
  )

  if (is.null(caption))
    caption <- paste(attr(res, "caption"),
                     "Obs: ", attr(res, "N"))
  if (is.null(note))
    note <- attr(res, "note")

  Output(fix_format(res),
         caption = caption,
         note = note)

  invisible(res)
}






extract.polr <- function(x,
                        digits = 2,
                        include.b = TRUE,
                        include.se = TRUE,
                        include.ci = FALSE,
                        include.odds = TRUE,
                      #  test.my.fun=FALSE,
                        ...){
 # if(test.my.fun) cat("\n   -> extract.polr()")

  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])

  res <- summary(x)

  Intercepts <- names(res$zeta)
  ## store table
  ctable <- coef(res)
  ## calculate and store p values
  colnames(ctable) <- c("b", "se", "t.value")
  p <- pnorm(abs(ctable[, "t.value"]), lower.tail = FALSE) * 2
  ## combined table

  ctable <- data.frame(
    Source = rownames(ctable),
    ctable,
    "p.value" = p,
    stringsAsFactors = FALSE
  )

  n1 <- nrow(ctable)
  n2 <- length(Intercepts)

  coefficients <- ctable[1:(n1 - n2),]
  intercepts  <- ctable[-c(1:(n1 - n2)),]
  source.coef <- coefficients[, 1, drop = FALSE]
  source.interc  <- intercepts[, 1, drop = FALSE]
  b.coef <- coefficients[, 2, drop = FALSE]
  b.interc <- intercepts[, 2, drop = FALSE]
  stat.coef <- coefficients[, 3:5, drop = FALSE]
  stat.interc <- intercepts[, 3:5, drop = FALSE]

  if (include.ci) {
    x <- update(x, Hess = TRUE)
    pr <- profile(x)
    ci <- confint(pr)
    colnames(ci) <- c("low", "upr")
    b.coef <- cbind(b.coef, ci)
    b.interc <- cbind(b.interc,  low = NA, upr = NA)
  }


  if (include.b) {
    source.coef <- cbind(source.coef, b.coef)
    source.interc <- cbind(source.interc, b.interc)
  }

  if (include.odds) {
    if(include.ci){
      source.coef$OR <- rndr_ods(source.coef$b )
      #  ifelse(> 4.6, 100,  round(exp(source.coef$b), 2))
      source.coef$OR.low <-rndr_ods(source.coef$low)
      #  ifelse(source.coef$low > 4.6, 100,  round(exp(source.coef$low), 2))
      source.coef$OR.upr <-rndr_ods(source.coef$upr)
      #  ifelse(source.coef$upr > 4.6, 100,  round(exp(source.coef$upr), 2))

      source.interc$OR <-rndr_ods(source.interc$b)
      #   ifelse(source.interc$b > 4.6, 100,  round(exp(source.interc$b), 2))
      source.interc$OR.low <- NA
      source.interc$OR.upr <- NA
    }
    else{
      source.coef$OR <-  rndr_ods(source.coef$b)

      # ifelse(source.coef$b > 4.6, 100,  round(exp(source.coef$b), 2))
      source.interc$OR <- rndr_ods(source.interc$b)

      #  ifelse(source.interc$b > 4.6, 100,  round(exp(source.interc$b), 2))
    }
  }



  if (include.se) {
    source.coef <- cbind(source.coef, stat.coef)
    source.interc <- cbind(source.interc, stat.interc)
  } else{
    source.coef <- cbind(source.coef, stat.coef[-1, drop = FALSE])
    source.interc <-
      cbind(source.interc, stat.interc[-1, drop = FALSE])
  }

  source.interc$Source <- paste0("Intercept(",  source.interc$Source, ")")

  prepare_output(rbind(source.interc, source.coef),
                 paste0("AV: ", AV),
                 paste0("Model: ", info$family[1]),
                 info$N,
                 info$labels)

}
