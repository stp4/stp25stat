






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
#'
#' smokers  <- c(83, 90, 129, 70)
#' patients <- c(86, 93, 136, 82)
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
                   note = paste( "conf.level = ", conf.level),
                   output = which_output(),
                   conf.level = 0.95,
                   n = NULL,
                   p = NULL,
                   digits=1,
                   sides = c("two.sided"),
                   method = c("sisonglaz")) {
  res <- NULL
  
  
  
  if (is.numeric(x)) {
    res <-
      data.frame(
        Item = "x",
        N = length2(x),
        Statistics = Meanci2(x, digits=digits, conf.int=conf.level)
      )
  }
  else if (is.factor(x)) {
    if (nlevels(x) > 2) {
      res <- ci_factor(x, digits=digits, sides, method, conf.level)
    } else{
      res <- ci_binom(x, digits=digits, conf.level)
    }
  }
  else if (is.data.frame(x) |
           is_formula2(x)) {
    X <- stp25formula::prepare_data2(x, ...)
    
    for (i in seq_len(length(X$measure.vars)) ) {
      x <- X$data[[X$measure.vars[i]]]
      if (is.factor(x)) {
        if (nlevels(x) > 2) {
          re <- ci_factor(x, X$digits[i], sides, method,conf.level)
          
          
        } else{
  
            re <- ci_binom(x, X$digits[i], conf.level)
          
        }
        names(re)[1:2] <- c("Item", "N")
        r <- 1
        names(r) <- X$row_name[i]
        re <- stp25output::add_row_df(re, r)
        
      } else{
        re <-
          data.frame(
            Item = X$row_name[i],
            N = length2(x),
            Statistics = Meanci2(x, X$digits[i], conf.int=conf.level)
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


ci_binom <- function(x, digits,conf.level) {
  xtab <- table(x)
  xci <- DescTools::BinomCI(xtab, n = sum(xtab), conf.level=conf.level)
  xtab <- as.data.frame(xtab)
  xtab$Statistics <-
    rndr_percent_CI(xci[, 1]* 100,xci[, 2] * 100,xci[, 3] * 100, digits)
  
  xtab
}

ci_factor <- function(x, digits, sides, method, conf.level) {
  xtab <- table(x)
  xci <- DescTools::MultinomCI(xtab, conf.level=conf.level, sides = sides, method = method)
  xtab <- as.data.frame(xtab)
  xtab$Statistics <-
    rndr_percent_CI(xci[, 1]* 100,xci[, 2] * 100,xci[, 3] * 100, digits)
  
  
  xtab
}

rndr_percent_CI <-
  function(x, low, upr,
           digits = default_stp25("digits", "prozent"),
           prc="% ", sep = ", ", sep_1 = "[", sep_2 = "]") {
    paste0(
      stp25rndr::Format2(x, digits),
      prc,
      sep_1,
      stp25rndr::Format2(low, digits),
      sep,
      stp25rndr::Format2(upr, digits),
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
#' @param x data.frame-Objekt
#' @param p Probability 0.50
#' @param n anzahl
#' @param conf.level Conf-Intervall 95
#' @param digits Nachkomnmastellen
#' @return data.frame
#' @export
#' @examples
#' set.seed(234)
#' n<-999
#' library(BayesianFirstAid)
#' smokers  <- c( 83, 90, 129, 70 )
#' patients <- c( 86, 93, 136, 82 )
#'
#'
#' g =gl(3, n/3, labels = c("Control", "Treat A", "Treat B"))
#' g2<- g[sample.int(n)]
#' levels(g2)<- c("male", "female", "female")
#' data<- data.frame(g=g, g2=g2,
#'                   x=rnorm(n) )[sample.int(n)[1:78],]
#' #Prop_Test2(data$g)
#' x<-as.data.frame(table(data$g))$Freq
#  bayes.prop.test(x, rep(nrow(data), nlevels(data$g)), p=1/3)
#'
#' APA2(~g+g2, data, type="freq.ci")
#' APA2(g~g2, data, type="freq.ci")
Prop_Test <- function(..., output=FALSE){
APA_CI(..., output=output)
}

Prop_Test <- function(...){
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
