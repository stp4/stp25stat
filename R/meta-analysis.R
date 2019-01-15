#' Metaanalysis
#'
#' Types Of Effects
#' An effect"could be almost any aggregate statistic of interest:
#' Mean, Mean difference, Mean change
#' Risk ratio, Odds ratio, Risk difference Incidence rate, Prevalence,
#' Proportion Correlation
#'
#'
#' @param x meta-Objekt
#' @param output  an Autput
#' @param ... an  APA2
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' # library(meta)
#' # require(stpvers)
#' data2<- GetData("
#'                 Nr            author  Ne    Me    Se Nc    Mc    Sc
#'                 1    Blashki(75%150)  13  6.40  5.40 18 11.40  9.60
#'                 2     Hormazabal(86)  17 11.00  8.20 16 19.00  8.20
#'                 3   Jacobson(75-100)  10 17.50  8.80  6 23.00  8.80
#'                 4        Jenkins(75)   7 12.30  9.90  7 20.00 10.50
#'                 5     Lecrubier(100)  73 15.70 10.60 73 18.70 10.60
#'                 6        Murphy(100)  26  8.50 11.00 28 14.50 11.00
#'                 7          Nandi(97)  17 25.50 24.00 10 53.20 11.20
#'                 8      Petracca(100)  11  6.20  7.60 10 10.00  7.60
#'                 9       Philipp(100) 105 -8.10  3.90 46 -8.50  5.20
#'                 10     Rampello(100)  22 13.40  2.30 19 19.70  1.30
#'                 11       Reifler(83)  13 12.50  7.60 15 12.50  7.60
#'                 12       Rickels(70)  29  1.99  0.77 39  2.54  0.77
#'                 13     Robertson(75)  13 11.00  8.20 13 15.00  8.20
#'                 14      Rouillon(98)  78 15.80  6.80 71 17.10  7.20
#'                 15           Tan(70)  23 -8.50  8.60 23 -8.30  6.00
#'                 16 Tetreault(50-100)  11 51.90 18.50 11 74.30 18.50
#'                 17      Thompson(75)  11  8.00  8.10 18 10.00  9.70")
#'
#' print(meta::metacont(
#'   Ne,  Me,  Se,  Nc,  Mc,  Sc,
#'   sm = "SMD",
#'   data = data2,
#'   subset = 2
#' ),
#' digits = 2)
#'
#'
#' mc1 <- meta::metacont(Ne, Me, Se, Nc, Mc, Sc,
#'                 data=data1,
#'                 studlab=author)
#' # round(c(mc1$TE.fixed, mc1$seTE.fixed^2), 4)
#'  #meta::forest(mc1)
#'
#' APA2(mc1)
#' 
#' 
#' #Power calculations for the general linear model
#' #?pwr.f2.test
#' x <- pwr::pwr.f2.test(
#'   u = 6,
#'   f2 = .4,
#'   sig.level = 0.05,
#'   power = 0.80
#' )
#' res<- APA(x)
#' 
#' meta::metacont(n.e=10, mean.e=2.34, sd.e=1.14, 
#' n.c=20, mean.c=3.43, sd.c=1.01,  sm="SMD")
#' meta::metabin(10, 20, 15, 20, sm = "OR")
#' 
Metaanalysis <- function(x,
                         output = which_output(),
                         ...) {
  if (inherits(x, "meta")) {
    APA2(x, output = output, ...)
  }
  else {
    cat("\nBis jetzt nur fuer meta implemeniert!\n")
  }
}


#' @rdname APA2
#' @description APA-Methode fuer pwr::pwr.f2.test
#' @export
APA.power.htest <- function(x, ... , output = which_output()) {
  res <-  c(
    df = x$u,
    n = round(x$v + 0.5, 0),
    effect.size = round(x$f2, 2),
    sig.lev = round(x$sig.level, 3),
    power = round(x$power, 2)
  )
  if (!is.logical(output))
    paste(paste(names(res), "=", res), collapse = "; ")
  else
    invisible(res)
}


#' extract_meta
#'
#' Extract results from  R package meta.
#'
#' @param x meta-Objekt
#' @param caption,note,digits an Output
#'
#' @return data.frame
extract_meta <- function(x,
                         ...) {
  UseMethod("extract_meta")
}


extract_meta.default <- function(x,
                         ...) {
  data.frame(class =  class(x)[1])
}


#' @rdname extract_meta
#' @description Meta-regression Meta-Analysis via Linear (Mixed-Effects) Models rma.uni {metafor}

extract_meta.metareg <- function(x,
                                 ...) {
  data.frame(class =  class(x)[1])
}

#' @rdname extract_meta
#' @description Meta-analysis of binary outcome data
#' Risk ratio (sm="RR")
#' Odds ratio (sm="OR")
#' Risk difference (sm="RD")
#' Arcsine difference (sm="ASD")

extract_meta.metabin <- function(x,
                                 ...) {
  data.frame(class =  class(x)[1])
}


#' @rdname extract_meta
#' @description Meta-analysis of continuous outcome data
#' mean difference (argument sm="MD")
#' standardised mean difference (sm="SMD")
#' ratio of means (sm="ROM")
extract_meta.metacont <- function(x,
                                  digits = 2) {
  data.frame(
    Study = x$studlab,
    estimate =   stp25rndr::Format2(x$TE, digits),
    SE =   stp25rndr::Format2(x$seTE, digits),
    CI = stp25rndr::rndr_CI(cbind(x$lower, x$upper), digits = digits),
    stringsAsFactors = FALSE
  )
}




#' @rdname APA2
#' @description APA-Methode fuer meta
#' @export
#' @param include.fixed,include.random Fixed effect and random effects model:
#' @param include.sub Subgruppen-Analyse
#' @param include.total Zusammenfassung als Gesamtwert
APA2.meta <- function(x,
                      caption = "Meta-analysis",
                      note = paste("estimate:", x$sm, x$method.smd),
                      include.table = TRUE,
                      include.fixed = FALSE,
                      include.random = TRUE,
                      include.sub = TRUE,
                      include.total = TRUE,
                      include.heterogeneity = TRUE,
                      digits = 2,
                      output = which_output(),
                      ...) {
  res <- list(
    tabel = NULL,
    sub.fixed = NULL,
    sub.random = NULL,
    effect.model = NULL,
    heterogeneity = NULL
  )
  
  if (include.table) {
    res$tabel <- extract_meta(x, digits = digits)
    Output(prepare_output(res$tabel, caption = caption, note = note),
           output = output)
  }
  
  if (include.sub & !is.null(x$bylab)) {
    if (include.fixed)
      res$sub.fixed <- with(
        x,
        data.frame(
          levels = bylevs,
          k = k.w,
          estimate =   stp25rndr::Format2(TE.fixed.w, digits),
          SE =   stp25rndr::Format2(seTE.fixed.w, digits),
          CI = stp25rndr::rndr_CI(cbind(lower.fixed.w, upper.fixed.w), digits = digits),
          Z =   stp25rndr::Format2(zval.fixed.w, 2),
          p.value =  stp25rndr::rndr_P(pval.fixed.w, FALSE),
          
          stringsAsFactors = FALSE
        )
      )
    Output(res$sub.fixed,
           "Results for subgroups (fixed effect model)",
           output = output)
    if (include.random)
      res$sub.random <- with(
        x,
        data.frame(
          levels = bylevs,
          k = k.w,
          estimate =   stp25rndr::Format2(TE.random.w, digits),
          SE =   stp25rndr::Format2(seTE.random.w, digits),
          CI = stp25rndr::rndr_CI(cbind(lower.random.w, upper.random.w), digits = digits),
          Z =   stp25rndr::Format2(zval.random.w, 2),
          p.value =  stp25rndr::rndr_P(pval.random.w, FALSE),
          
          stringsAsFactors = FALSE
        )
      )
    Output(res$sub.random,
           "Results for subgroups (random effects model)",
           output = output)
  }
  
  if (include.total) {
    res$effect.model <- with(
      x,
      data.frame(
        Source = c("Fixed effect model", "Random effects model"),
        estimate = stp25rndr::Format2(c(TE.fixed , TE.random), digits),
        SE =   stp25rndr::Format2(c(seTE.fixed, seTE.random), digits),
        CI = stp25rndr::rndr_CI(cbind(
          c(lower.fixed, lower.random),
          c(upper.fixed, upper.random)
        ), digits = digits),
        Z =   stp25rndr::Format2(c(zval.fixed, zval.random), 2),
        p.value =  stp25rndr::rndr_P(c(pval.fixed, pval.random), FALSE),
        
        stringsAsFactors = FALSE
      )
    )
    
    caption <-
      paste("Results number of studies combined: k =", x$k)
    if (include.fixed & include.random)
      Output(res$effect.model, caption, output = output)
    if (!include.fixed & include.random)
      Output(res$effect.model[-1,], caption, output = output)
    if (include.fixed & !include.random)
      Output(res$effect.model[-2,], caption, output = output)
    
  }
  
  if (include.heterogeneity) {
    res$heterogeneity <-
      with(summary(x),
           data.frame(
             Source = c("Quantifying heterogeneity", "Test of heterogeneity"),
             value = c(
               paste0(
                 "tau=", stp25rndr::Format2(tau ^ 2, 2),
                 "; H=", stp25rndr::Format2(H$TE, 2),
                 " ", stp25rndr::rndr_CI(cbind(H$lower, H$upper), digits =2), 
                 "; I=", stp25rndr::Format2(I2$TE * 100, 1),
                 "% ", stp25rndr::rndr_CI(cbind(I2$lower * 100, I2$upper *
                                            100), digits = 1)
               ),
               paste0(
                 "Q=", stp25rndr::Format2(Q, 2),
                 "; d.f=",stp25rndr::Format2(df.Q, 0),
                 "; ",stp25rndr::rndr_P(meta:::pvalQ(Q,  df.Q))
               )
             ),
             stringsAsFactors = FALSE
           ))
    Output(res$heterogeneity, caption = "heterogeneity", output = output)
    
  }
  invisible(res)
  
}
