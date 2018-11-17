

#' @param .data an dplyr
#'
#' @param fun an dplyr default = as.numeric
#' @param stringsAsFactors an data.frame default = FALSE
#' @param ... 
#'
#' @noRd
#' 
#' 
#' 
#' 
dapply1 <-
  function (.data,
            fun = function(x)
              as.numeric(x),
            stringsAsFactors = FALSE,
            ...) {
    if (inherits(.data, "tbl_df"))
      dplyr::tbl_df(plyr::llply(.data, fun, ...)) 
    else
      data.frame(plyr::llply(.data, fun, ...),
                 stringsAsFactors=stringsAsFactors) 
  }







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
#' @importFrom caret confusionMatrix
#' @importFrom pscl pR2
#' @importFrom stats IQR aggregate median na.omit qt quantile sd
#' @importFrom Hmisc smean.cl.normal Cs spearman2 rcorr
#' @importFrom psych describe alpha skew kurtosi
#' @importFrom plyr llply
#' @importFrom effects allEffects
#' @importFrom reshape2 colsplit dcast
#' @importFrom HH brewer.pal.likert
#' @importFrom broom tidy
#' @importFrom rms lrm
#' @importFrom lazyeval lazy_dots
#' @importFrom sjstats eta_sq
#'
"_PACKAGE"

# 
# 

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
# stpvers,
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