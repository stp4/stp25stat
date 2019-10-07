# roxygen   16-10-2018



#' berechne: berechne Mittelwerte
#'
#' Die Lagemasse werden ueber die Standard-Funktionen berechnet unterschied ist nur dass
#' Faktoren zu Zahlen transformiert werden und das \code{na.rm=TRUE} gesetzt wird.
#' CI = Hmisc::smean.cl.normal
#'
#'
#' @return  ein dataframe Objekt oder ein Character-String
#' @param ... alles weitere
#' @export

berechne <- function(...) {
  UseMethod("berechne")
}


#' @rdname berechne
#' @param na.rm NAs
#' @param conf.interval,ci Grenzen der Konfidenzintervalle CIs
#' @param .drop anplyr::ddply
#' @export
#'
#' @examples
#' # erlaubt:  varana %>% berechne(4, 5, by= ~geschl )
#' #  berechne(hyper, "chol0" )
#' #  names(hyper)
#' #  hyper %>% berechne(chol0,chol1,chol6,chol12, by=~med+g)
berechne.data.frame <- function(data,
                                ...,
                                by = "1",
                                type = 1,
                                na.rm = TRUE,
                                conf.interval = .95,
                                .drop = TRUE) {
  measure <-
    sapply(lazyeval::lazy_dots(...), function(x) {
      as.character(x[1])
    })

  meAsNum <- grep("^[[:digit:]]", measure)
  if (length(meAsNum) != 0) {
    measure[meAsNum] <- names(data[as.numeric(measure[meAsNum])])
  }

  if (is_formula2(by))
    by <- all.vars(by)


  res <- NULL
  for (i in measure) {
    res <- rbind(
      res,
      berechneMean(
        data,
        i,
        by,
        na.rm = na.rm,
        conf.interval = conf.interval,
        .drop = .drop
      )
    )
  }
  res$variable <- factor(res$variable, unique(res$variable))

  res
}

#' @rdname berechne
#' @param data Objekt
#' @param measurevar Namen der Variablen
#' @export
berechneMean <- function(data = NULL,
                         measurevar,
                         by = NULL,
                         na.rm = TRUE,
                         conf.interval = .95,
                         .drop = TRUE) {
  Text("berechneMean: Achtung die Funktion wird bals geloescht!")
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  if (length(measurevar) != 1)
    return(measurevar)

  datac <- plyr::ddply(
    data,
    by,
    .fun = function(xx, col) {
      c(
        variable = NA,
        N    = length2(xx[[col]], na.rm = na.rm),
        mean = mean   (xx[[col]], na.rm = na.rm),
        sd   = sd     (xx[[col]], na.rm = na.rm),
        min  = min    (xx[[col]], na.rm = na.rm),
        max  = max    (xx[[col]], na.rm = na.rm)
      )
    },
    measurevar,
    .drop = .drop
  )

  # Rename the "mean" column
  #  datac <- plyr::rename(datac, c("mean" = measurevar))
  datac$se <-
    datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval / 2 + .5, datac$N - 1)
  datac$ci <- datac$se * ciMult
  datac$ci.low <-    datac$mean - datac$ci
  datac$ci.hig <-    datac$mean + datac$ci
  datac$variable <- GetLabelOrName(data[measurevar])
  return(datac)
}


#' @rdname berechne
#' @description
#' Interne Funktion
#' berechne_all(data,x,...):
#' interne Funktion um Mittelwerte/Freq zu berechnen. Wird in Tabelle benutzt.
#'
#' @param x  measure.vars
#' @param type mean, median
#' @param fm formel
#' @param digits Nachkommastellen
#' @param by,measure,measure.name aus prepare Formula
#' @param fun Function an plyr::ddply
#'
berechne_all <- function(data,
                         x,
                         by = "1",
                         measure,
                         type ,
                         fun = function(x)length(na.omit(x)),
                         fm = NULL,
                         digits = stp25rndr::default_stp25("digits", "mittelwert")[1],
                         measure.name = NULL
) {
  
  cat("\n")
  # print(x)
  # print(head(data))
  # print(by)
  # print(measure)
  # print(fm)
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
          #calc_median(x, digits)

      }
    )
  }

  mn <- function() {
#cat("in mn \n")
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
          
         # calc_mean(x, digits = digits)
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
  if(length(na.omit(data[all.vars(fm)[1]]))<0){
    aggregate(
      fm,
      data ,
      FUN = function(x) {
        x<- factor(x)
        r <- table(x)
        paste(r, collapse = "/")
      }
    )}
    else {
      r<- data.frame(x="", stringsAsFactors=FALSE)
      names(r)<- all.vars(fm)[1]
      r
    }
  }
  
  
  
  
  custom_fun <- function() {
    res <-  aggregate(fm, data, FUN = fun, simplify = TRUE)

    if (is.matrix(res[[ncol(res)]])) {
      measure.name <<- NULL
      cbind(res[-ncol(res)],  res[[ncol(res)]])
    } else
      res
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
    logical = lgcl(), # Zwischen-Ueberschrift
    NA
  )

  if (!is.null(measure.name))
    names(res)[ncol(res)] <- measure.name[1]

  res
}


# Mittelwert und Median ---------------------------------------------------


#' @rdname berechne
#' @param median.style,mean.style intern in calc_median
#'
calc_median <-
  function(x,
           digits = 2,
           n = length(x),
           median.style = get_my_options()$apa.style$mittelwert$median.style,
           unit=NULL) {
    
    if (!is.numeric(x)) {x <- as.numeric(x)}
    else if (inherits(x, "units")) {
        gr <- c("[", "]") # = units_options("group")
        unit <- paste0(gr[1], as.character(attr(x, "units")), gr[2])
        x <- units::drop_units(x)
      } 
    
    if (is.null(median.style)) {
      rndr_median_quant(quantile(x, na.rm = TRUE), 
                        digits=digits,
                        unit=unit)
    }
    else if (median.style == 1) {
      rndr_median_quant(quantile(x, na.rm = TRUE), 
                        digits=digits,
                        unit=unit)
    }
    else if (median.style == "IQR" | median.style == "IRQ") {
      rndr_median(median(x), 
                  ifelse(n > 2, IQR(x), NA), 
                  digits=digits,
                  unit=unit)
    }
    else if (median.style == "2" | median.style == "long") {
      rndr_median_range(
        median(x, na.rm = TRUE),
        IQR(x, na.rm = TRUE),
        min(x, na.rm = TRUE),
        max(x, na.rm = TRUE),
        digits = digits,
        unit=unit
      )
    }
    else {
      rndr_median_quant(quantile(x, na.rm = TRUE),
                        digits=digits,
                        unit=unit)
    }
  }


#stp25rndr::default_stp25("mean.style", "mittelwert")
#stp25output::get_my_options()$apa.style$mittelwert$mean.style


#' @rdname berechne
calc_mean <-  function(x,
                       digits = stp25rndr::default_stp25("digits", "mittelwert") ,
                       n = length(x),
                       mean.style = get_my_options()$apa.style$mittelwert$mean.style,
                       unit=NULL) {
  
  if (!is.numeric(x)) {x <- as.numeric(x)}
  else if (inherits(x, "units")) {
      gr <- c("[", "]") # = units_options("group")
      unit <- paste0(gr[1], as.character(attr(x, "units")), gr[2])
      x <- units::drop_units(x)
    }

      
  
 
  if (is.null(mean.style)) {
     rndr_mean(mean(x, na.rm = TRUE), 
                 ifelse(n > 2, sd(x, na.rm = TRUE), NA), 
                 digits=digits,
                 unit=unit)
 
    
  }
  else if (mean.style == "1") {
    rndr_mean(mean(x, na.rm = TRUE), 
              ifelse(n > 2, sd (x, na.rm = TRUE), NA), 
              digits=digits,
              unit=unit)
  }
  else if (mean.style == "2" |
           mean.style == "long") {
    rndr_mean_range(
      mean(x, na.rm = TRUE),
      ifelse(n > 2, sd (x, na.rm = TRUE), NA),
      min(x, na.rm = TRUE),
      max(x, na.rm = TRUE),
      digits = digits,
      unit=unit
    )
  } else {
    rndr_mean(mean(x), 
              ifelse(n > 2, sd (x), NA), 
              digits=digits,
              unit=unit)
  }
  
  
}


#' @rdname berechne
#' @export
Mean2default <- function(x,
                         digits = 2,
                         n = length(x),
                         mean.style = get_my_options()$apa.style$mittelwert$mean.style,
                         include.level = get_my_options()$apa.style$mittelwert$include_name
) {
  if (is.null(include.level))
    mylevel <- "(mean)"
  else if (include.level)
    mylevel <- "(mean)"
  else
    mylevel <- ""


 # m <- if (is.numeric(x))
 #   calc_mean(x, digits, n, mean.style)
 # else
 #   calc_mean(as.numeric(x), digits, n, mean.style)

  data.frame(
    lev = mylevel,
    n = as.character(n),
    m =   calc_mean(x, digits, n, mean.style),
    stringsAsFactors = FALSE
  )
}


#' @rdname berechne
Median2default <- function(x,
                           digits = 2,
                           n = length(x),
                           median.style = get_my_options()$apa.style$mittelwert$median.style,
                           include.level = get_my_options()$apa.style$mittelwert$include_name
) {
  if (is.null(include.level))
    mylevel <- "(median)"
  else if (include.level)
    mylevel <- "(median)"
  else
    mylevel <- ""
  #style=IQR  quantile rndr_median_range
#  m <-
 #   if (is.numeric(x))
 #     calc_median(x, digits, n, median.style)
 # else
 #   calc_median(as.numeric(x), digits, n, median.style)

  data.frame(
    lev = mylevel,
    n = as.character(n),
    m = calc_median(x, digits, n, median.style),
    stringsAsFactors = FALSE
  )
}


#' @rdname berechne
Multi2default <- function(x,
                          digits = 0,
                          n = length(x),
                          use.level = 1) {
  if (is.factor(x) & nlevels(x) == 2) {
    firstLevel <- levels(x)[use.level]
    x <-
      factor(ifelse(x == firstLevel, firstLevel, 0), c(firstLevel, 0))
  }
  else if (is.logical(x)) {
    x <-  factor(x)
  }
  else if (is.numeric(x) | is.integer(x)) {
    x <- factor(ifelse(x == use.level, 1, 0), 1:0)
  } else {
    return(data.frame(
      lev = "",
      n = n,
      m = "n.a.",
      stringsAsFactors = FALSE
    ))
  }

  Prozent2default(x, digits, n)[1,]
}


#' @rdname berechne
#' @return Vector
#' @export
#' @examples
#' mean2(rnorm(100))
#' sd2(rnorm(100))
#' CI(rnorm(100))
#'
mean2 <-
  function(x, na.rm = TRUE, ...) {
    #- test_that
    mean_factor <- function(x, na.rm = TRUE) {
      if (nlevels(x) == 2)
        mean(as.numeric(x), na.rm = na.rm) - 1
      else
        mean(as.numeric(x), na.rm = na.rm)
    }
    if (is.numeric(x))
      mean(x, na.rm = na.rm)
    else if (is.factor(x)) {
      warning("Die Mittelwerte wurden durch Transformation auf Numeric berechnet!")
      mean_factor(x, na.rm = na.rm)
    }
    else {
      warning("Die Mittelwerte sind eventuel Falsch!")
      mean_factor(factor(x), na.rm = na.rm)
    }
  }


#' @rdname berechne
#' @export
median2 <-
  function(x, na.rm=TRUE, ... ){  #- test_that
    #  cat(str(x))
    mean_factor<- function(x, na.rm){
      # cat(levels(x),"\n")
      if(nlevels(x)==2)    median(as.numeric(x), na.rm=na.rm) -1
      else   median(as.numeric(x), na.rm=na.rm)
    }


    if(is.numeric(x)) median(x, na.rm=na.rm)
    else if(is.factor(x)) {
      warning("Die Mittelwerte wurden durch Transformation auf Numeric berechnet!")
      mean_factor(x, na.rm=na.rm)
    }
    else {
      warning("Die Mittelwerte sind eventuel Falsch!")
      mean_factor(factor(x), na.rm=na.rm)

    }
  }


#' @rdname berechne
#' @export
sd2 <-  function(x, na.rm=TRUE, ...){
  if(is.numeric(x)) sd(x, na.rm=na.rm)
  else {
    warning("Die Mittelwerte wurden durch Transformation auf Numeric berechnet!")
    sd(as.numeric(x), na.rm=na.rm)
  }
}

#' @rdname berechne
#' @export
CI <- function (x, ci = 0.95, na.rm=TRUE, ...) {
  #-- stolen from Rmisc
  a <- mean(x, na.rm=na.rm)
  s <- sd(x, na.rm=na.rm)
  n <- length(na.omit(x))
  error <- qt(ci + (1 - ci)/2, df = n - 1) * s/sqrt(n)
  return(c(upper = a + error, mean = a, lower = a - error))
}


# StdErr <- function (x, ...)
#   {
#   x<- na.omit(x)
#   a <- mean(x)
#   s <- sd(x)
#   n <- length(x)
#   error <- (s/sqrt(n))
#   return(c(upper = a + error, mean = a, lower = a - error))
# }



#' @rdname berechne
#' @export
length2 <- function(x, 
                     na.rm=FALSE) {
  if (na.rm) sum(!is.na(x))
  else       length(x)
}


# Formatierte Mittelwerte aus data.frames ---------------------------------



#' @rdname berechne
#' @export
Mean2 <- function(x, 
                  ...){UseMethod("Mean2")}


#' @rdname berechne
#' @export
Mean2.formula <-  function(x, 
                           data, 
                           ...) {
  if (length(x) == 2) {
    strg <- c(NULL)
    for (i in all.vars(x)) {
      strg <- c(strg, Mean2(data[, i]))
    }
  } else {
    strg <-  aggregate(x, data, FUN = Mean2.default)
    apply(strg, 1, function(x) {
      paste(x, collapse = " = ")
    })
  }
  strg
}

#' @rdname berechne
#' @export
Mean2.default <- function(x, digits = NULL, ...) {

  if (length(x) <= 0)
    return("NaN")
  
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



#' @rdname berechne
#' @export
Median2 <- function(x, ...) {
  UseMethod("Median2")
}


#' @rdname berechne
#' @export
Median2.formula<-  function(x, data, ...){
  if(length(x) == 2){
    strg<- c(NULL)
    for (i in all.vars(x) ){
      strg<- c(strg, Median2(data[,i]))
    }
  } else {
    strg <-  aggregate(x, data, FUN=Median2.default)
    apply(strg, 1, function(x) {
      paste(x, collapse=" = ")
    })}

  strg
}



#' @rdname berechne
#' @export
Median2.default<- function(x, digits = NULL,
                           median.style=get_my_options()$apa.style$mittelwert$median.style,
                           ...) {
  # if (length(x) <= 0)
  #   return("NaN")
  # if (is.vector(x) | is.numeric(x)) {
  #   if (!is.numeric(x))
  #     x <- as.numeric(x)
  # 
  #   if (median.style == "IQR") {
  #     if(is.null(digits))  rndr_median(median(x), ifelse(length(x) > 2, IQR(x), NA))
  #     else rndr_median(median(x), ifelse(length(x) > 2, IQR(x), NA), digits=digits)
  #   } else {
  #     if(is.null(digits))   rndr_median_quant(quantile(x, na.rm = TRUE))
  #     else  rndr_median_quant(quantile(x, na.rm = TRUE), digits=digits)
  #   }
  # 
  #   # rndr_median_quant(quantile(x, na.rm = TRUE), digits, ...)
  # } else if (is.data.frame(x)) {
  #   if (ncol(x) == 1) {
  #     Median2(x[, 1])
  #   }
  #   else{
  #     unlist(lapply(as.data.frame(x), Median2))
  #   }
  # } else{
  #   cat("Unbekanter Datentype", class(x))
  #   return("NaN")
  # }
  
  
  
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


#' @rdname berechne
#' @export
#' @examples
#'
#' Meanci2(c(1,1,1,1,2,2,4,4,5,5,5))
#'
Meanci2<- function(x, digits=NULL, ...){

  if (length(x)<=0) return("NaN")
  if(!is.numeric(x)) x <- as.numeric(x)

  #N <- length(x)
  x <- na.omit(x)
 # n <- length(x)
  res <- Hmisc::smean.cl.normal(x, ...)

  if(is.null(digits)) digits <- stp25rndr::countDigits(signif(res[1], 4))

  stp25rndr::rndr_mean_CI(res[1],
                          cbind(res[2], res[3]), digits=digits[1])

}



#' @rdname berechne
#' @description
#' Prozent:
#' Interne Funktion um Prozent fuer die Tabellen zu berechnen.
#' @param continuous,breaks,labels fuer cut
#' @param count_factor für multi
#' @param retur_tabel intern
#'
#' @return Vector
#' @export
Prozent <- function (x,
                     digits = 1,
                     continuous = 3,
                     breaks = NULL,
                     labels = NULL,
                     count_factor = c("yes", "ja", "T", "TRUE", 1),
                     retur_tabel = FALSE) {

  if (length(x) <= 0)
    return("NaN") #-- fuer Melt2

  calc_factor <- function(x) {
    if (length(x) <= 0) {
      ans <-  if (retur_tabel)
        0
      else
        rndr_percent(0, 0)
      names(ans) <-  names(x)
    } else{
      fq <- table(x)
      prc <- prop.table(fq) * 100
      ans <- if (retur_tabel)
        fq
      else
        rndr_percent(prc, fq)
      names(ans) <- row.names(fq)
    }
    ans
  }


  if (is.factor(x)) {
    calc_factor(x)
  }
  else  if (is.logical(x) |
            is_all_0_1((x))) {
    rndr_percent(mean(x, na.rm = TRUE) * 100,  sum(x, na.rm = T))
  }
  else if (is.numeric(x)) {
    xf <- factor(x)

    if (nlevels(xf) > 7)
      xf <- cut(x, quantile(x, na.rm = TRUE))
    calc_factor(xf)
  }
  else {
    # hier kommt alles aus Recast2 an weil recast Character weitergibt
    xf <- factor(x)
    lvls <- levels(xf)
    n <- nlevels(xf)
    if (n == 2 |  n == 1) {
      if (any(tolower(lvls) %in% c("ja", "yes", "true", "1", "nein", "no", "false", "0"))) {
        x <- ifelse(tolower(x) %in%  c("ja", "yes", "true", "1"), 1, 0)
        if (retur_tabel) {
          table(x)
        }
        else {
          rndr_percent(mean(x, na.rm = TRUE) * 100, sum(x, na.rm = TRUE))
        }
      } else if (n == 2) {
        x <- 2 - as.numeric(xf)   # erster Faktor wird gez?hlt
        if (retur_tabel)  {
          table(x)
        }
        else {
          res <-
            rndr_percent(mean(x, na.rm = TRUE) * 100, sum(x, na.rm = TRUE))
          paste(lvls[1], res)#-- namen zu Variable
        }
      } else {
        "nur eine Factorstufe"
      }
    } else{
      "mehr als 2 Faktorstufen"
    }

  }
}



#' @rdname berechne
#' @param n intern
#' @param exclude geh noch nicht
#' @param max_factor_length lange Einträge kuerzen
#' @examples
#' #set_my_options(prozent = list(digits = 1,
#' #style = 2,
#' #null_percent_sign= "."))
#' #Prozent2default(factor(c(1,2,3,3,3,5), 1:5))
#'
Prozent2default <-
  function(x,
           digits = 0,
           n = length(x),
           exclude = NA,
           max_factor_length = 25) {
    if (!is.factor(x))
      x <- factor(x)

  #  x_NA <- x
  #  N    <- length(x)

    if (n == 0) {
      result <- ""
      ans <- rep(NA, nlevels(x))
      names(ans) <- levels(x)
    } else {
      ans <- table(x, exclude = exclude)

      if (length(ans) > max_factor_length) {
        naLev <- levels(x)[-(1:max_factor_length)]
        Text("NA = ", paste(naLev, collapse = ", "))
        x <-
          factor(x, levels(x)[1:max_factor_length], exclude = NULL)
        x <-
          addNA(x)
        #- addNA modifies a factor by turning NA into an extra level
        N <- length(x)
        n <- length(x)
        ans <- table(x)
      }

      result <-
        rndr_percent(as.vector(prop.table(ans)) * 100, as.vector(ans))

    }
    data.frame(
      lev = names(ans),
      n = c(n, rep("", length(ans) - 1)),
      m = as.vector(result),
      stringsAsFactors = FALSE
    )
  }


#' @rdname berechne
#' @description Prozent2 wird in APA.formula errate_statistik2 verwendet
#' freq = Prozent2(x_NA, exclude, digits=digits.percent, rtrn = "df", max_factor_length = max_factor_length)
Prozent2APA <- function(x,
                        exclude = NA,
                        digits = 1,
                        max_factor_length = 35,
                        ...) {
  Non_Factor_To_Factor <- function(x) {
    if (is.logical(x)) {
      x <- factor(x, c(TRUE, FALSE))
    } else if (is.numeric(x)) {
      if (is_all_0_1(x))
        x <- factor(x, c(0, 1))
      else{
        x <- as.numeric(x)
        xf <- factor(x)
        if (nlevels(xf) > 7)
          x <- cut(x, quantile(x, na.rm = TRUE))
        else
          x <- xf
      }
    } else
      x <- rep(NA, length(x))
    x
  }

  if (!is.factor(x))
    x <- Non_Factor_To_Factor(x)

  x_NA <- x  # --mit na
  x    <- na.omit(x)
  n    <- length(x)

  if (n == 0) {
    result <- ""
    ans <- rep(NA, nlevels(x_NA))
    names(ans) <- levels(x_NA)
  } else {
    if (is.null(exclude))
      x <- x_NA


    ans <- table(x, exclude = exclude)

    # seltener fall das sehr viele levels vorhanden sind
    if (length(ans) > max_factor_length) {
      naLev <- levels(x)[-(1:max_factor_length)]
      Text("NA = ", paste(naLev, collapse = ", "))

      x <- factor(x, levels(x)[1:max_factor_length], exclude = NULL)
      x <-
        addNA(x)  #- addNA modifies a factor by turning NA into an extra level
      n <- length(x)
      ans <- table(x)
    }

    result <-
      rndr_percent(prop.table(ans) * 100, ans,  digits = digits)
  }

  data.frame(
    Characteristics = names(ans),
    n = c(n, rep("", length(ans) - 1)),
    Statistics = result,
    stringsAsFactors = FALSE
  )
}


#' @rdname berechne
#' @param use.level welcher level wir gezaelt
#' @param include.level mit yes in labels?
# x <- c(1, 1, 1, 0, 0, 0, 1, 1)
# # set_my_options(prozent=list(include_name=FALSE))
# # Multi2default(x)
# # Prozent2default(x)
Multi2default <- function(x,
                          digits = 0,
                          n = length(x),
                          use.level = 1,
                          include.level = get_my_options()$apa.style$prozent$include_name) {
  if (is.null(include.level))
    include.level <- TRUE

  if (is.factor(x) & nlevels(x) == 2) {
    firstLevel <- levels(x)[use.level]
    x <-
      factor(ifelse(x == firstLevel, firstLevel, 0), c(firstLevel, 0))
  }
  else if (is.logical(x)) {
    x <-  factor(x)
  }
  else if (is.numeric(x) | is.integer(x)) {
    x <- factor(ifelse(x == use.level, 1, 0), 1:0)
  } else {
    return(data.frame(
      lev = "",
      n = n,
      m = "n.a.",
      stringsAsFactors = FALSE
    ))
  }

  res <- Prozent2default(x, digits, n)[1,]
  if (!include.level)
    res$lev <- ""
  res

}

