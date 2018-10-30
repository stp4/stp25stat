#' stp25stat: Functions for Statistical Computations
#'
#'
#' Convert statistical analysis objects from R into APA-Tabls.
#'
#' To learn more about dplyr, start with the vignettes:
#' `browseVignettes(package = "dplyr")`
#'
#' @import stp25rndr
#' @import stp25output
#' @import stp25formula
#' @import stp25aggregate
#' @importFrom Hmisc smean.cl.normal
#' @importFrom Hmisc Cs
#' @importFrom Hmisc spearman2
#' @importFrom Hmisc rcorr
#' @importFrom psych describe
#' @importFrom psych alpha
#' @importFrom psych skew
#' @importFrom psych kurtosi
#' @importFrom plyr llply
#' @importFrom effects allEffects
#' @importFrom reshape2 colsplit
#' @importFrom reshape2 dcast
#' @importFrom HH brewer.pal.likert
#' @importFrom broom tidy
#' @importFrom rms lrm
#' @importFrom lazyeval lazy_dots
#' @importFrom sjstats eta_sq
#'
"_PACKAGE"



#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom Hmisc Cs
#' @export
Hmisc::Cs

#' @importFrom car contr.Treatment
#' @export
car::contr.Treatment
#' @importFrom car contr.Sum
#' @export
car::contr.Sum
#' @importFrom car contr.Helmert
#' @export
car::contr.Helmert

