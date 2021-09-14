#' APA_CI
#' 
#' 
#' Confidence Intervals for Binomial Proportions
#' 
#' The Wilson interval, which is the default, was introduced by Wilson (1927)
#' and is the inversion of the CLT approximation to the family of equal tail tests of p = p0. The Wilson interval is
#' recommended by Agresti and Coull (1998) as well as by Brown et al (2001).
#' 
#' 
#' Confidence Intervals for Multinomial Proportions
#' 
#' Sison, C.P and Glaz, J. (1995) Simultaneous confidence intervals
#' and sample size determination for multinomial proportions.
#' Journal of the American Statistical Association, 90:366 - 369.
#' 
#' @param x data.frame or vector
#' @param ... an stp25formula::prepare_data2
#' @param caption,note,output an Output
#' @param conf.level,sides,method an DescTools::BinomCI
#' @param digits Nachkommastellen
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' #' require(stpvers)
#' 
#' set.seed(234)
#' n <- 3 * 100
#' 
#' g = gl(3, n / 3, labels = c("Control", "Treat A", "Treat B"))
#' g2 <- g[sample.int(n)]
#' levels(g2) <- c("male", "female", "female")
#' data <- data.frame(g = g, g2 = g2,
#'                    x = rnorm(n))[sample.int(n)[1:78], ]
#' APA_CI(x <- data$g2)
#' 
#' APA_CI(data, g, g2)
#' 
#' 
APA_CI <- function(x,
                   ...,
                   caption = "Confidence Intervals",
                   note = paste("conf.level = ", conf.level),
                   output = which_output(),
                   conf.level = 0.95,
                   digits = 1,
                   sides = "two.sided",
                   method) {
  res <- NULL

  if (is.numeric(x)) {
    res <-
      data.frame(
        Item = "x",
        N = length2(x),
        Statistics = Meanci2(x, digits = digits, conf.int = conf.level)
      )
  } else if (is.factor(x)) {
    if (nlevels(x) > 2) {
      res <- ci_factor(x, digits = digits, conf.level, sides, "sisonglaz")
    } else{
      res <- ci_binom(x, digits = digits, conf.level, sides, "wilson")
    }
  } else if (is.data.frame(x) | inherits(x, "formula")) {
    X <- stp25formula::prepare_data2(x, ...)
    if (!is.null(X$group.vars))
      stop("Gruppen sind noch nicht Implementiert!")

    for (i in seq_len(length(X$measure.vars))) {
      x <- X$data[[X$measure.vars[i]]]
      if (is.factor(x)) {
        if (nlevels(x) > 2) {
          re <- ci_factor(x, X$digits[i], conf.level, sides, "sisonglaz")
        } else {
          re <- ci_binom(x, X$digits[i], conf.level, sides, "wilson")
        }
        names(re)[1:2] <- c("Item", "N")
        r <- 1
        names(r) <- X$row_name[i]
        re <- stp25tools::add_row_df(re, r)
      } else {
        if (X$measure[i] == "median")
          re <-
            data.frame(
              Item = X$row_name[i],
              N = length2(x),
              Statistics = Medianci2(x, X$digits[i], conf.int = conf.level)
            )
        else
          re <-
            data.frame(
              Item = X$row_name[i],
              N = length2(x),
              Statistics = Meanci2(x, X$digits[i], conf.int = conf.level)
            )
      }
      res <-  rbind(res, re)
    }
  }

  res <- prepare_output(tibble::as_tibble(res),
                        caption, note)
  Output(res, output = output)
  invisible(res)
}


#' @rdname APA_CI
ci_binom <- function(x,
                     digits,
                     conf.level,
                     sides = "two.sided",
                     method = "wilson") {
  xtab <- table(x)
  xci <-
    DescTools::BinomCI(
      xtab,
      n = sum(xtab),
      conf.level = conf.level,
      sides = sides,
      method = method
    )
  xtab <- as.data.frame(xtab)
  xtab$Statistics <-
    rndr_percent_CI(xci[, 1] * 100,
                    xci[, 2] * 100,
                    xci[, 3] * 100,
                    digits)

  xtab
}
#' @rdname APA_CI
ci_factor <- function(x,
                      digits,
                      conf.level,
                      sides = "two.sided",
                      method = "sisonglaz") {
  xtab <- table(x)
  xci <-
    DescTools::MultinomCI(xtab,
                          conf.level = conf.level,
                          sides = sides,
                          method = method)
  xtab <- as.data.frame(xtab)
  xtab$Statistics <-
    rndr_percent_CI(xci[, 1] * 100,
                    xci[, 2] * 100,
                    xci[, 3] * 100,
                    digits)

  xtab
}





rndr_percent_CI <-
  function(x,
           low,
           upr,
           digits = default_stp25("digits", "prozent"),
           prc = "% ",
           sep = ", ",
           sep_1 = "[",
           sep_2 = "]") {
    if (which_output() == "latex")
      prc <- "prc"

    paste0(
      stp25rndr::Format2(x, digits),
      prc,
      sep_1,
      stp25rndr::Format2(low, digits),
      prc,
      sep,
      stp25rndr::Format2(upr, digits),
      prc,
      sep_2
    )
  }
#' @rdname APA_CI
#' @export
#' 
Prop_Test2 <- function(..., output = FALSE) {
  APA_CI(..., output = output)
}

Prop_Test <- function(...) {
  APA_CI(...)
}