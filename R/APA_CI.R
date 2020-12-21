#' Confidence Intervals
#'
#' @param x data.frame, formula oder vecto
#' @param ... an prepare Data
#' @param caption,note,output an Output()
#' @param n,p not used
#' @param sides,method an DescTools::MultinomCI()

#'
#' @return
#' @export
#'
#' @examples
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
#'                    x = rnorm(n))[sample.int(n)[1:78],]
#' APA_CI(x <- data$g2)
#'
#' APA_CI(data, g, g2)
APA_CI <- function(x,
                   ...,
                   caption = "Confidence Intervals",
                   note = paste("conf.level = ", conf.level),
                   output = which_output(),
                   conf.level = 0.95,
                   digits = 1,
                   sides="two.sided",
                   method,
                   n = NULL,
                   p = NULL
                   ) {
  res <- NULL
 
  if (is.numeric(x)) {
    res <-
      data.frame(
        Item = "x",
        N = length2(x),
        Statistics = Meanci2(x, digits = digits, conf.int = conf.level))
  } else if (is.factor(x)) {
    if (nlevels(x) > 2) {
      res <- ci_factor(x, digits = digits, conf.level, sides, "sisonglaz")
    } else{
      res <- ci_binom(x, digits = digits, conf.level, sides, "wilson")
    }
  } else if (is.data.frame(x) | inherits(x, "formula")) {
      X <- stp25formula::prepare_data2(x, ...)
      if (!is.null(X$group.vars)) stop("Gruppen sind noch nicht Implementiert!")
      
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
          
          if(X$measure[i] == "median")
            re <-
              data.frame(
                Item = X$row_name[i],
                N = length2(x),
                Statistics = Medianci2(x, X$digits[i], conf.int = conf.level))
            else
          re <-
            data.frame(
              Item = X$row_name[i],
              N = length2(x),
              Statistics = Meanci2(x, X$digits[i], conf.int = conf.level))
        }
        res <-  rbind(res, re)
      }
  }
  
  res <- prepare_output(tibble::as_tibble(res),
                        caption, note)
  Output(res, output = output)
  invisible(res)
}

#' Confidence Intervals for Binomial Proportions
#' 
#' The Wilson interval, which is the default, was introduced by Wilson (1927) 
#' and is the inversion of the CLT approximation to the family of equal tail tests of p = p0. The Wilson interval is 
#' recommended by Agresti and Coull (1998) as well as by Brown et al (2001).
#' 
ci_binom <- function(x, digits, conf.level, 
                     sides = "two.sided",
                     method = "wilson"
                     ) {
  xtab <- table(x)
  xci <-
    DescTools::BinomCI(xtab, 
                       n = sum(xtab), 
                       conf.level = conf.level,
                       sides=sides,
                       method= method)
  xtab <- as.data.frame(xtab)
  xtab$Statistics <-
    rndr_percent_CI(xci[, 1] * 100, 
                    xci[, 2] * 100, 
                    xci[, 3] * 100, 
                    digits)
  
  xtab
}

#' Confidence Intervals for Multinomial Proportions
#' 
#' Sison, C.P and Glaz, J. (1995) Simultaneous confidence intervals 
#' and sample size determination for multinomial proportions. 
#' Journal of the American Statistical Association, 90:366-369.
#' 
ci_factor <- function(x, digits,  conf.level,
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


#   94% [89%, 97%] 


rndr_percent_CI <-
  function(x,
           low,
           upr,
           digits = default_stp25("digits", "prozent"),
           prc = "% ",
           sep = ", ",
           sep_1 = "[",
           sep_2 = "]") {
    
    if(which_output()=="latex") prc<- "prc"
    
    paste0(
      stp25rndr::Format2(x, digits),
      prc,
      sep_1,
      stp25rndr::Format2(low, digits),prc,
      sep,
      stp25rndr::Format2(upr, digits),prc,
      sep_2
    )
  }

#
# require(stpvers)
#
# set.seed(234)
#
# smokers  <- c(83, 90, 129, 70)
# patients <- c(86, 93, 136, 82)
# n <- 3 * 100
#
# g = gl(3, n / 3, labels = c("Control", "Treat A", "Treat B"))
# g2 <- g[sample.int(n)]
# levels(g2) <- c("male", "female", "female")
# data <- data.frame(group = g, sex = g2,
#                    x = rnorm(n))[sample.int(n)[1:78],]
# #APA_CI(x <- data$g2)
#
# APA_CI(data, group, sex, x)







#' @rdname APA_CI
#' @export
Prop_Test2 <- function(..., output = FALSE) {
  APA_CI(..., output = output)
}

Prop_Test <- function(...) {
  APA_CI(...)
}

# # @rdname Prop_Test
# # @export
# Prop_Test2 <-
#   function(x,
#            ...,
#            caption = "Test of Equal or Given Proportions",
#            note = "",
#            output = which_output()) {
#     # Prop.Test2
#
#     res <-  Prop_Test.default(x, ...)
#     Output(res[c(1, 2, 6)],
#            caption, paste(note, "probabilities =", res$p[1]),
#            output = output)
#     invisible(res)
#   }
#
#
#
# # @rdname Prop_Test
# # @export
# Prop_Test.default<- function(x,
#                              p=1/nlevels(x),
#                              n=length(na.omit(x)),
#                              conf.level=.95,
#                              digits = 0,
#                              include.ci=TRUE,
#                              ...){
#   if (length(x)<=0) return("NaN")
#   res <- as.data.frame(tab <- table(x))
#   res$Prz <- as.data.frame(prop.table(tab))$Freq
#
#   res <- cbind(res,
#                t(
#                  sapply(res$Freq,
#                         function(x){
#                           ci<-prop.test(as.integer(x),
#                                         n, p,
#                                         conf.level=conf.level)$conf.int
#                           names(ci) <- c("low", "hig")
#                           ci})
#                ))
#
#
#   res$Statistics <- paste0( Format2(res$Prz*100, digits = digits), "% CI=[",
#                             Format2(res$low*100, digits = digits), " - ",
#                             Format2(res$hig*100, digits = digits) , "]")
#   res$n <- ""
#   names(res)[1] <- "Characteristics"
#   res$n[1] <- n
#   res$p<- p
#
#   res
#
#   # data.frame(Characteristics="dummy", n=1, Statistics="dummy")
# }
#
