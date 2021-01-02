#'  Mean 
#'
#' Lagemasse Berechnen
#' 
#' @name mean2
#' @export
#' @examples
#' 
#' mean2(rnorm(100))
#' sd2(rnorm(100))
#' CI(rnorm(100))
#'
#'
#' n<-30
#' df<- data.frame(x=rnorm(n, 5,1.2), y=rpois(n,.7))
#' df
#' x<- rnorm(30)
#' 
#' stp25rndr::default_stp25("digits", "mittelwert")
#' get_my_options()$apa.style$mittelwert$mean.style
#' mean2(x)
#' Mean2(x)
#' Mean2default(x)
#' 
#' Mean2(x, digits=3, style=2)
#' df %>% Tabelle(x,y )
#' df %>% Tabelle(x,y, APA=TRUE)
#' df %>% Tabelle(x,y, fun = function(x) Mean2(x))
#' 
#' df %>% Summarise(x,y, fun = function(x) c(n=length(x), mean=Mean2(x)))
#' 
#' 
mean2 <-
  function(x, na.rm = TRUE) {
    mean(make_numeric(x), na.rm = na.rm)}
 
  
make_numeric <- function(x) {
  if (is.numeric(x))
    return(x)
  else if (is.factor(x)) {
    if (nlevels(x) == 2)
      as.numeric(x) - 1
    else
      as.numeric(x)
  }
  else {
    warning("Die Werte sind eventuel falsch class=", class(x), "!")
    stp25tools::as_numeric(x)
  }
}

#' @rdname mean2
#' @export
#' 
Mean2 <- function(x, ...){UseMethod("Mean2")}


#' @rdname mean2
#' 
Mean2.formula <-  function(x, 
                           data, 
                           ...) {
  if (length(x) == 2) {
    rslt <- c(NULL)
    for (i in all.vars(x)) {
      rslt <- c(rslt, Mean2(data[, i]))
    }
  } else {
    rslt <-  aggregate(x, data, FUN = Mean2.default)
    apply(rslt, 1, function(x) {
      paste(x, collapse = " = ")
    })
  }
  rslt
}

#' @rdname mean2
#' 
Mean2.default <- function(x, ...) {
  if (length(x) <= 0) return("NaN")
  
  if (is.vector(x) | is.numeric(x)) {
    calc_mean(x, ...)
  } else if (is.data.frame(x)) {
    if (ncol(x) == 1) {
      calc_mean(x[, 1], ...)
    }
    else{
      unlist(lapply(as.data.frame(x), calc_mean, ...))
    }
  } else{
    cat("Unbekanter Datentype", class(x))
    return("NaN")
  }
}



#' @rdname mean2
#'
#' @param x vector
#' @param digits nachkomastellen (2)
#' @param n laenge des vectors
#' @param style lang oder Kurz
#' @param unit Einheiten
#'
calc_mean <-  function(x,
                       digits = stp25rndr::default_stp25("digits", "mittelwert") ,
                       n = length(x),
                       style = get_my_options()$apa.style$mittelwert$mean.style,
                       unit=NULL) {
  
  if(all(is.na(x))) return(NaN)
  
  # handle units
  if (!is.numeric(x)) {
    x <- make_numeric(x)
  }  else if (inherits(x, "units")) {
    gr <- c("[", "]")  
    unit <- paste0(gr[1], as.character(attr(x, "units")), gr[2])
    x <- units::drop_units(x)
  }
  
  # calc and format
  if (is.null(style)) {
    rndr_mean(mean(x, na.rm = TRUE), 
              ifelse(n > 2, sd(x, na.rm = TRUE), NA), 
              digits=digits,
              unit=unit)
  }
  else if (style == "1") {
    rndr_mean(mean(x, na.rm = TRUE), 
              ifelse(n > 2, sd (x, na.rm = TRUE), NA), 
              digits=digits,
              unit=unit)
  }
  else if (style == "2" |
           style == "long") {
    rndr_mean_range(
      mean(x, na.rm = TRUE),
      ifelse(n > 2, sd (x, na.rm = TRUE), NA),
      min(x, na.rm = TRUE),
      max(x, na.rm = TRUE),
      digits = digits,
      unit=unit
    )
 
    
  } 
  else if (style == "3" |
           style == "CI") {
    
    Meanci2(x, digits=digits)
 
    
  } 
  
  
  else {
    rndr_mean(mean(x), 
              ifelse(n > 2, sd (x), NA), 
              digits=digits,
              unit=unit)
  }
}
 
#' @noRd
calc_median <-
  function(x,
           digits = 2,
           n = length(x),
           style = get_my_options()$apa.style$mittelwert$median.style,
           unit=NULL) {
    
    if(all(is.na(x))) return(NaN)
    
    if (!is.numeric(x)) {  
      x <- make_numeric(x)
    } else if (inherits(x, "units")) {
      gr <- c("[", "]") # = units_options("group")
      unit <- paste0(gr[1], as.character(attr(x, "units")), gr[2])
      x <- units::drop_units(x)
    } 
    
    if (is.null(style)) {
      rndr_median_quant(quantile(x, na.rm = TRUE), 
                        digits=digits,
                        unit=unit)
    }
    else if (style == 1) {
      rndr_median_quant(quantile(x, na.rm = TRUE), 
                        digits=digits,
                        unit=unit)
    }
    else if (style == "IQR" | style == "IRQ") {
      rndr_median(median(x), 
                  ifelse(n > 2, IQR(x), NA), 
                  digits=digits,
                  unit=unit)
    }
    else if (style == "2" | style == "long") {
      rndr_median_range(
        median(x, na.rm = TRUE),
        IQR(x, na.rm = TRUE),
        min(x, na.rm = TRUE),
        max(x, na.rm = TRUE),
        digits = digits,
        unit=unit
      )
    }
    
    else if(style=="3"| style=="CI"){
      Medianci2(x, digits=digits)
    }
    
    else {
      rndr_median_quant(quantile(x, na.rm = TRUE),
                        digits=digits,
                        unit=unit)
    }
  }





#' @rdname mean2
#' 
Mean2default <- function(x,
                         digits = 2,
                         n = length(x),
                         style = get_my_options()$apa.style$mittelwert$mean.style,
                         include.level = get_my_options()$apa.style$mittelwert$include_name
) {
  if (is.null(include.level))   mylevel <- "(mean)"
  else if (include.level) mylevel <- "(mean)"
  else mylevel <- ""
  
  data.frame(
    lev = mylevel,
    n = as.character(n),
    m =   calc_mean(x, digits, n, style),
    stringsAsFactors = FALSE
  )
}





#' @rdname mean2
#' @export
median2 <-
  function(x, na.rm=TRUE, ... ) { median(make_numeric(x), na.rm = na.rm) }


#' @rdname mean2
#' @export
Median2 <- function(x, ...) {
  UseMethod("Median2")
}


#' @rdname mean2
Median2.formula<-  function(x, data, ...){
  if(length(x) == 2){
    rslt<- c(NULL)
    for (i in all.vars(x) ){
      rslt<- c(rslt, Median2(data[,i]))
    }
  } else {
    rslt <-  aggregate(x, data, FUN=Median2.default)
    apply(rslt, 1, function(x) {
      paste(x, collapse=" = ")
    })}
  
  rslt
}

#' @rdname mean2
Median2.default<- function(x, ...) {
  
  if (length(x) <= 0)
    return("NaN")
  
  if (is.vector(x) | is.numeric(x)) {
    calc_median(x, ...)
  } else if (is.data.frame(x)) {
    if (ncol(x) == 1) {
      calc_median(x[, 1], ...)
    }
    else{
      unlist(lapply(as.data.frame(x), calc_median, ...))
    }
  } else{
    cat("Unbekanter Datentype", class(x))
    return("NaN")
  }
  
  
}
 



#' @rdname mean2
Median2default <- function(x,
                           digits = 2,
                           n = length(x),
                           style = get_my_options()$apa.style$mittelwert$median.style,
                           include.level = get_my_options()$apa.style$mittelwert$include_name
) {
  if (is.null(include.level))
    mylevel <- "(median)"
  else if (include.level)
    mylevel <- "(median)"
  else
    mylevel <- ""
  
  
  data.frame(
    lev = mylevel,
    n = as.character(n),
    m = calc_median(x, digits, n, style),
    stringsAsFactors = FALSE
  )
}


#' @rdname mean2
#' @export
sd2 <-  function(x, na.rm=TRUE){ sd(make_numeric(x), na.rm=na.rm)}

#' @rdname mean2
#' @export
CI <- function (x, ci = 0.95, na.rm=TRUE, ...) {
  x <- make_numeric(x)
  
  a <- mean(x, na.rm=na.rm)
  s <- sd(x, na.rm=na.rm)
  n <- length(na.omit(x))
  error <- qt(ci + (1 - ci)/2, df = n - 1) * s/sqrt(n)
  return(c(upper = a + error, mean = a, lower = a - error))
}


#' @rdname mean2
#' @export
length2 <- function(x, 
                    na.rm=FALSE) {
  if (na.rm) sum(!is.na(x))
  else       length(x)
}






#' @rdname mean2
#' @export
#' @examples
#'
#' Meanci2(c(1,1,1,1,2,2,4,4,5,5,5))
#'
Meanci2<- function(x, digits=2, ...){
  
  if (length(x)<=0) return("NaN")
  x <- make_numeric(x)
  x <- na.omit(x)
  res <- Hmisc::smean.cl.normal(x, ...)
  
  stp25rndr::rndr_mean_CI(res[1],
                          cbind(res[2], res[3]), digits=digits[1])
  
}


#' @rdname mean2
#' @export
#' @examples
#'
#' Medianci2(c(1,1,1,1,2,2,4,4,5,5,5))
#'
Medianci2<- function(x, digits=2, ...){
  
  if (length(x)<=0) return("NaN")
  x <- make_numeric(x)
  
  x <- na.omit(x)
  res <- Hmisc::smedian.hilow(x, ...)
  
 # if(is.null(digits)) digits <- stp25rndr::countDigits(signif(res[1], 4))
  
  stp25rndr::rndr_mean_CI(res[1],
                          cbind(res[2], res[3]), digits=digits[1])
  
}