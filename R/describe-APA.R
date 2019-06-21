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
APA2.psychobject <- function(x, ...){ x$text }


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
