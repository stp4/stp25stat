#' Prozent
#'
#' Berechnung der Prozent
#'
#' @param x vector
#' @param digits nachkommastellen digits = 0,
#' @param n Anzahl
#' @param exclude,useNA an table
#' @param max_factor_length lange Eintraege kuerzen
#' @param return_data_frame,is_true_false  intern
#'
#' @return
#' @export
#'
#' @examples
#'
#'  x<- gl(2, 8, labels = c("Control", "Treat"))
#' x[1]<- NA
#' stp25stat:::Prozent2default(x)
#' Prozent(x )
#' Prozent(x, useNA ="always")
#'
#' stp25stat:::Multi2default(x)
#' stp25stat:::Prozent_multi(x)
#'
#'
#' x<- as.numeric(x)
#' stp25stat:::Prozent2default(x)
#' Prozent(x)
#'
#' x<- ifelse(x==1, TRUE, FALSE)
#'
#' stp25stat:::Prozent2default(x)
#' Prozent(x)
#' Prozent(x, useNA ="always")
#'
#'
#' # in Tabelle() verwendet
#'
#' set_my_options(prozent = list(digits = 1, style = 2, null_percent_sign= "."))
#' Prozent2default(factor(c(1,2,3,3,3,5), 1:5))
#' x <- c(1, 1, 1, 0, 0, 0, 1, 1)
#'  set_my_options(prozent=list(include_name=FALSE))
#'  stp25stat:::Multi2default(x)
#'  stp25stat:::Prozent2default(x)
#'
Prozent <- function (x,
                     digits = 0,
                     exclude = if (useNA == "no")
                       c(NA, NaN),
                     useNA = "no",
                     max_factor_length = 25) {
  Prozent2default(
    x,
    digits = digits,
    exclude = exclude,
    useNA = useNA,
    max_factor_length = max_factor_length,
    return_data_frame = FALSE
  )
  
}

#' @rdname Prozent
#' @param n intern
#' @param exclude geh noch nicht
#' @param max_factor_length lange Einträge kuerzen
Prozent2default <-
  function(x,
           digits = 0,
           n = length(x),
           exclude = if (useNA == "no") c(NA, NaN),
           max_factor_length = 25,
           useNA = "no",
           return_data_frame = TRUE,
           is_true_false = FALSE
           ) 
{
    if (is.factor(x)) {
      ans <- table(x, exclude = exclude, useNA = useNA)
      if (length(ans) > max_factor_length) {
        naLev <- levels(x)[-(1:max_factor_length)]
        Text("NA = ", paste(naLev, collapse = ", "))
        x <-  factor(x, levels(x)[1:max_factor_length], exclude = NULL)
        x <- addNA(x)  #- addNA modifies a factor by turning NA into an extra level
        ans <- table(x)
        
      }
    } else if (is.logical(x)) {
      x <- factor(x, c(TRUE, FALSE), c("true", "false"))
      is_true_false <- TRUE
      ans <- table(x, exclude = exclude, useNA = useNA)
    } else {
      xt <- factor(x)
      
      if (nlevels(xt) > max_factor_length)
        stop("class = ", class(xt), " nlevels = ", nlevels(xt))
      else
        ans <- table(xt, exclude = exclude, useNA = useNA)
    }
    
    
    if (n == 0) {
      rslt <- ""
      ans <- rep(NA, nlevels(x))
      names(ans) <- levels(x)
    } else {
      rslt <-
        rndr_percent(as.vector(prop.table(ans)) * 100, as.vector(ans))
    }
    
    
    if (return_data_frame) {
      rslt <-
        data.frame(
          lev = names(ans),
          n = c(n, rep("", length(ans) - 1)),
          m = as.vector(rslt),
          stringsAsFactors = FALSE
        )
      rslt <- if (!is_true_false)  rslt else rslt[1, ]
    } else{
      names(rslt) <- names(ans)
      rslt <- if (!is_true_false) rslt else rslt[1]
    }
    
    rslt
  }






#' @rdname Prozent
#' @export
Prozent_multi <- function(x,
                          digits = 0,
                          use.level = 1) {
  if (is.logical(x)) {
    res <- Prozent(x, digits)
  } else if (is.factor(x)) {
    res <-
      Prozent(ifelse(x == levels(x)[use.level], TRUE, FALSE), digits)
    
  } else if (is.numeric(x)) {
    res <- Prozent(ifelse(x ==  use.level, TRUE, FALSE), digits)
  }
  else
    (stop(class(x)))
  
  res
}

#' @rdname Prozent
#' @param use.level welcher level wir gezaelt
#' @param include.level mit yes in labels?
Multi2default <- function(x,
                          digits = 0,
                          n = length(x),
                          use.level = 1,
                          include.level = get_my_options()$apa.style$prozent$include_name) {
  if (is.null(include.level))
    include.level <- TRUE
  
  
  
  if (is.logical(x)) {
    res <- Prozent2default(x, digits, n)
  } else if (is.factor(x)) {
    res <-
      Prozent2default(ifelse(x == levels(x)[use.level], TRUE, FALSE), digits, n)
    res$lev <- levels(x)[use.level]
  } else if (is.numeric(x)) {
    res <-
      Prozent2default(ifelse(x ==  use.level, TRUE, FALSE), digits, n)
    res$lev <- use.level
  }
  else
    (stop(class(x)))
  
  if (!include.level)
    res$lev <- ""
  
  
  res
}
