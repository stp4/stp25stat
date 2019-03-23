#' stp25stat: Functions for Statistical Computations
#'
#'
#' Convert statistical analysis objects from R into APA-Tabls.
#'
#' @import stp25rndr
#' @import stp25output
#' @import stp25formula
#' @import stp25aggregate
#'
"_PACKAGE"




#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL



# 
# 
# @importFrom caret confusionMatrix
# @importFrom pscl pR2
# @importFrom stats IQR aggregate median na.omit qt quantile sd
# @importFrom Hmisc smean.cl.normal Cs spearman2 rcorr
# @importFrom psych describe alpha skew kurtosi
# @importFrom plyr llply
# @importFrom effects allEffects
# @importFrom reshape2 colsplit dcast
# @importFrom HH brewer.pal.likert
# @importFrom broom tidy
# @importFrom rms lrm
# @importFrom lazyeval lazy_dots
# @importFrom sjstats eta_sq
# @importFrom magrittr %>%
# @export
#magrittr::`%>%`

# @importFrom Hmisc Cs
# @export
#Hmisc::Cs

# @importFrom car contr.Treatment
# @export
#car::contr.Treatment
# @importFrom car contr.Sum
# @export
#car::contr.Sum
# @importFrom car contr.Helmert
# @export
#car::contr.Helmert


# 
# 
# stp25aggregate, 
# stp25output,
# stp25formula,
# stp25rndr,
# ,
# stp25data,
# Hmisc,
# stats,
# utils,
# psych,
# plyr,
# effects,
# reshape2,
# car,
# broom,
# HH,
# rms,
# lazyeval,
# magrittr,
# coin,
# sjstats,
# lmerTest,
# texreg,
# caret,
# pscl








 




#' Information uere die verwendeten Methoden
#'
#' @param methode,library,fun  Text
#'
#' @return data.frame()

Info_Statistic <-
  function(methode = "describe",
           library = "base",
           fun = "summary",
           my_methodes = "") {
    Text("Methodes: ",  my_methodes)
    data.frame(
      Methode = methode,
      Library = library,
      Function = fun,
      stringsAsFactors = FALSE
    )
  }