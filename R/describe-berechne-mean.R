#' Aggregate
#' 
#' Wird in Tabelle(), 
#' Tbll_desc_long() und
#' APA_Correlation()
#' benutzt.
#' 
#' Interne Funktion um Mittelwerte/Freq zu berechnen. 
#' 
#' @noRd
#' @param x  measure.vars
#' @param type "auto_long"
#' @param fm formel
#' @param digits Nachkommastellen
#' @param by,measure,measure.name aus prepare Formula
#' @param fun Function an plyr::ddply
#'
berechne_all <- function(data,
                         x,
                         by = "1",
                         measure,
                         type = "1",
                         fun = function(x)length(na.omit(x)),
                         fm = NULL,
                         digits = stp25rndr::default_stp25("digits", "mittelwert")[1],
                         measure.name = NULL
) {
  
  
  mdn <- function() {
    aggregate(
      fm,
      data,
      FUN = function(x) {
        if (type == "auto_long")
          rndr_median_range(
            median(x, na.rm = TRUE),
            ifelse(length(x) > 2, IQR(x, na.rm = TRUE), NA),
            min(x, na.rm = TRUE),
            max(x, na.rm = TRUE),
            digits = digits
          )
        else
          
          rndr_median(median(x), ifelse(length(x) > 2, IQR(x, na.rm = TRUE), NA), digits = digits)
        
      }
    )
  }
  
  mn <- function() {
    aggregate(
      fm,
      data,
      FUN = function(x) {
        if (type == "auto_long")
          rndr_mean_range(
            mean(x, na.rm = TRUE),
            ifelse(length(x) > 2, sd(x, na.rm = TRUE), NA),
            min(x, na.rm = TRUE),
            max(x, na.rm = TRUE),
            digits = digits
          )
        else
          
          rndr_mean(mean(x, na.rm = TRUE), 
                    ifelse(length(x) > 2, sd(x, na.rm = TRUE), NA), 
                    digits=digits)
      }
    )
  }
  
  frq <- function() {
    aggregate(
      fm,
      data ,
      FUN = function(x) {
        r <- table(x)
        paste(r, collapse = "/")
      }
    )
  }
  
  lgcl <- function() {
    #   if(length(na.omit(data[all.vars(fm)[1]]))<0){
    aggregate(
      fm,
      data ,
      FUN = function(x) {
        x <- factor(x)
        r <- table(x)
        paste(r, collapse = "/")
      }
    )
    #  }
    #  else {
    #    r<- data.frame(x="", stringsAsFactors=FALSE)
    #    names(r)<- all.vars(fm)[1]
    #   r
    #  }
  }
  
  custom_fun <- function() {
    res <-  aggregate(fm, data, FUN = fun, simplify = TRUE)
    
    if (is.matrix(res[[ncol(res)]])) {
      measure.name <<- NULL
      cbind(res[-ncol(res)],  res[[ncol(res)]])
    } else
      res
  }
  
  emty <- function() {
   data.frame(x = "", stringsAsFactors = FALSE)
  }
    
  if (is.null(fm)) {
    fm <- stp25formula::make_formula(x, by)
  }
  
  res <- switch (
    measure,
    factor = frq() ,
    numeric = mn(),
    median = mdn(),
    integer = mn(),
    mean = mn(),
    units=mn(), ## library(units)
    custom_fun = custom_fun(),
    logical = lgcl(), 
    header = emty(),
    emty()
  )
  
  if (!is.null(measure.name))
    names(res)[ncol(res)] <- measure.name[1]

  res
}
