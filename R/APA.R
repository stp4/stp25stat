#' APA Style Text-Ausgabe
#'
#' Ausgabe von APA-Style formatiertem Text.
#'
#'
#' @name APA
#' @param x Objekt fit, formula usw
#' @param ... weitere Objekte
#' @return   Character Vector mit einem oder meheren Eintraegen
#' @examples
#'
#' APA(mpg ~ cyl, mtcars)
#' APA(glm(vs ~ mpg, mtcars, family = binomial()))
#' APA(lm(mpg ~ drat + wt + qsec, mtcars))
#' APA(aov(mpg ~ drat + wt + qsec, mtcars))
#' @export
APA <-   function(x,
                  ...) {
  UseMethod("APA")
}

#' @rdname APA
#' @export
#' 
#' 
APA.coxph<- 
function (x, ...){
  gmodel <- broom::glance(x)
  paste0(
    "# Events: ",
    gmodel$nevent,
    "; Global p-value (Log-Rank): ",
    stp25rndr::rndr_P(gmodel$p.value.log),
    " \nAIC: ",
    round(gmodel$AIC, 0),
    "; Concordance Index: ",
    round(gmodel$concordance, 2)
  )
  
}





#' @rdname APA
#' @export
APA.NULL <- function(x,
                     ...) {
    res<- Info_Statistic(
      c("catTest", "conTest", "Wilkox", "Kruskal",
        "ANOVA",
        "T Test"),
      c("stats", "Hmisc", "stats", "stats",
        "car",
        "stats"),
      c(
        "chisq.test",
        "spearman2",
        "wilcox.test",
        "kruskal.test",
        "Anova, type = 3",
        "t.test"
      ), paste(methods("APA"), collapse=", ")
    )
    
"Tests siehe APA2"
}

#' @rdname APA
#' @export
APA.default <- function(x, ...) {
  cat("\nKeine Methode fuer: ", class(x), "\n")
  class(x)[1]
}


#' @rdname APA
#' @export
#' @examples
#'
#' davis <- matrix(
#' c(3,  6,
#'   2, 19),
#' nrow = 2, byrow = TRUE
#' )
#' davis <- as.table(davis)
#' ## Asymptotic Pearson chi-squared test
#' diffusion <- data.frame(
#'   pd = c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46,
#'          1.15, 0.88, 0.90, 0.74, 1.21),
#'   age = factor(rep(c("At term", "12-26 Weeks"), c(10, 5)))
#' )
#' 
#' ## Exact Wilcoxon-Mann-Whitney test
#' ## Hollander and Wolfe (1999, p. 111)
#' ## (At term - 12-26 Weeks)
#' wt <- wilcox_test(pd ~ age, data = diffusion,
#'                   distribution = "exact", conf.int = TRUE)
#' 
#' APA(xt<-chisq_test(davis))
#' 
#' 
#' APA(wt)
APA.QuadTypeIndependenceTest <- function(x, ...) {
  # capture.output(x)[5]
  stp25rndr::rndr_Chisq(coin::statistic(x), 
                        x@statistic@df, 
                        coin::pvalue(x))
}
#' @rdname APA
#' @export
APA.ScalarIndependenceTest <- function(x, ...) {
  stp25rndr::rndr_W(coin::statistic(x), 
                    coin::pvalue(x))
  
}

#' @rdname APA
#' @export
APA.numeric<- function(...) Mean2(...)
#' @rdname APA
#' @export
APA.factor<- function(...) Prozent(...)

#' @rdname APA
#'
#' @param data data.frame
#' @param exclude,max_factor_length an Prozent2default
#'

#' @export
#' @examples
#'
#' \dontrun{
#'
#'  APA( ~ rrs0 + rrs1 + g, hyper)
#'  APA(chol0+chol1 ~ g, hyper) das geht nicht
#'  }
#'
APA.formula <- function(x,
                        data,
                        exclude = NA,
                        max_factor_length = 25,
                        ...) {
  X <- prepare_data2(x, data)
  res <- NULL
  #-Aus Funktion Tabelle(..., APA=TRUE)
  for (i in 1:length(X$measure)) {
    x <- X$data[[X$measure.vars[i]]]
  #  x_NA <- x
   # N    <- length(x)
    x    <- na.omit(x)
    n    <- length(x)

    if (all(is.na(x)))
      X$measure[i] <- "all_NA"

    res1 <- switch(
      X$measure[i],
      numeric = Mean2default(x, X$digits[i], n),
      integer = Mean2default(x, X$digits[i], n),
      factor =  Prozent2default(x, X$digits[i], n, exclude, max_factor_length),
      logical = Prozent2default(x, X$digits[i], n, exclude, max_factor_length),
      freq =    Prozent2default(x, X$digits[i], n, exclude, max_factor_length),
      mean =    Mean2default(x, X$digits[i], n),
      median =   Median2default(x, X$digits[i], n),
      multi =    Multi2default(x, X$digits[i], n),

      c(lev = "NA", n = "NA", m = "NA")
    )
    m <- as.character(res1$m)
    if (length(m) == 1)
      names(m) <- X$measure.vars[i]
    else
      names(m) <- paste(X$measure.vars[i], res1$lev, sep = "_")
    res <- c(res, m)

  }
  res

}
#' @rdname APA
#' @description APA.lm F-Test aus lm und Anova
#' @param include.r APA.lm: R-Squar
#' @export
APA.lm <- function(x, include.r = TRUE) {
  if (any(class(x) == "aov"))
    x <- lm(x)
  fitSummary <- summary(x)
  fstats <- fitSummary$fstatistic
  pValue <-  stats::pf(fstats[['value']],
                       fstats[['numdf']],
                       fstats[['dendf']], lower.tail = FALSE)
  if (include.r)
    rndr_lm(fstats[['value']] ,
            fstats[['numdf']],
            fstats[['dendf']],
            pValue,
            fitSummary$r.squared,
            fitSummary$adj.r.squared)
  else
    rndr_F(fstats[['value']] ,
           fstats[['numdf']],
           fstats[['dendf']],
           pValue)
  
}
