# "Sun Nov 11 06:28:19 2018"
#  APA_Reliability euinfacher Test



#' Reliability und Cronbach-Alpha
#'
#' \code{Reliability()} Reliabilitaets Analyse mit Cronbach-Alpha + Mittelwerte
#'
#' @details
#' Deutsch
#' Um die interne Konsistenz zu bestimmen, wurde Cronbachs Alpha für die Subskala positiver Affekt (insgesamt zehn Fragen) berechnet. Die interne Konsistenz war hoch, mit Cronbachs Alpha = .89 für positiven Affekt.
#'
#' English
#' For reliability analysis, Cronbach’s alpha was calculated to assess the internal consistency of the subscale for positive affect, which consists of ten questions. The internal consistency of the questionnaire is satisfying, with Cronbach’s alpha for positive affect = .89.
#'
#' @name Reliability
#' @param ... an psych
#' @return \code{Reliability()} gibt eine Liste mit den Resultaten und den transformierten Daten
#' "data", "range", "labels", "keys", "psych",
#'  "item_statistik", "Alpha", "index", "Items","n",
#'  "M", "SD","Skew", "Kurtosi", "shapiro"
#'
#'  psych: ist psych::alpha
#'
#'  index: ist dabei der Mittelwert-Index
#'
#'  data: Daten Umcodiert.
#'
#'  keys:  Umcodiert.
#' @export
#' @examples
#'
#'
#' require(stp25data)
#' n<-200
#' x<- runif(n, min = 1, max = 5)
#' set.seed(0815)
#' data<- data.frame( x1 = x+rnorm(n),
#'                    x2 = x+rnorm(n),
#'                    x3 = x+rnorm(n),
#'                    x4 = x+rnorm(n),
#'                    x5 = 6-x+rnorm(n),
#'                    x6 = x+rnorm(n),
#'                    x7 = x+rnorm(n))
#'
#' Reliability(data)
#' Reliability(data, revcoded=TRUE)
#' Reliability(data, check.keys=TRUE)
#' Reliability(data, revcoded=5)
#' Reliability(data, revcoded="x5")
#'
#' library(lavaan)
#' population.model <- '
#' Fachinteresse =~ F1+F2+F3+F4+F5
#' Soziale.Einbindung =~ S1+S2+S3+S4
#' Relevanz.Inhalte=~ R1+R2+R3+R4
#' Kompetenzerleben =~ K1+K2+K3+K4
#' Autonomieerleben=~ A1+A2+A3+A4+A5+A6
#' Motivierungsqualitaet=~ M1+M2+M3+M4
#' '
#'
#' # generate data
#' set.seed(1234)
#' DF <- simulateData(population.model, sample.nobs=60 )
#' DF[1,10]<- NA
#' DF<-stp25aggregate::dapply2(DF, function(x) cut(x, 5, 1:5))
#'
#' DF<-stp25aggregate::dapply2(DF)
#' DF[,1]<- 6-DF[,1]
#' DF[,3]<- 6-DF[,3]
#'
#'
#' Fachinteresse <- Reliability( DF[ , Cs(F1,F2,F3,F4,F5)], check.keys=TRUE)
#' APA_Reliability( DF[ , Cs(F1,F2,F3,F4,F5)], check.keys=TRUE)
#'
#'
#' APA(Reliability(~F3+F2+F10+F11, fkv, check.keys =TRUE))
#'
#'
#' # ALPHA
#' Alpha( Fachinteresse)
#' Distanz <-  Reliability(~F3+F2+F10+F11, fkv, check.keys =TRUE)
#'
#' Distanz %>% Alpha()
#' Alpha(Distanz)
#'
#' Verarbeitung <- Reliability(~ F5+F16+F22+F9+F26+F6+F35+F33+F12+F34+F4, fkv, check.keys =TRUE)
#' Coping <- Reliability(~ F7+F8+F17+F14+F15+F18+F19+F1+F13+F20, fkv, check.keys =TRUE)
#' Vertrauen <- Reliability(~ F28+F27+F31+F29, fkv, check.keys =TRUE)
#' Religion <- Reliability(~F21+F25+F30+F23+F24, fkv, check.keys =TRUE)
#' Distanz <- Reliability(~F3+F2+F10+F11, fkv, check.keys =TRUE)
#'
#'
#' Alpha(Verarbeitung, Coping, Vertrauen, Religion, Distanz)
Reliability <- function(...) {
  UseMethod("Reliability")
}







#' Tbll_reliability
#'
#' @param ... Data, Formula
#' @param caption,note  an prepare
#' @param include.label Labels 
#' @param include.item_statistic Item Statistic
#' @param include.scale_statistic  Scale Statistic
#' @param revcoded,na.rm,max.level,min.level an Umkodieren
#' @param digits Digits
#'
#' @return list ore data.frame
#' @export
#'
#' @examples
#'
#'   Tbll_reliability(data.frame(
#'     x = 1:10,
#'     y = 2:11,
#'     z = 12:3,
#'     w = rnorm(10)
#'   ),
#'   x,
#'   y,
#'   z,
#'   w,
#'   revcoded = 3)
#' 
#'  
Tbll_reliability <-
  function(...,
           caption = "Reliability",
           note = "",
           include.label = TRUE,
           include.item_statistics = TRUE,
           include.scale_statistics = TRUE,
           include.cronbachs.alpha =TRUE,
           include.inter.item.correlation =FALSE,
           # an Umcodieren
           revcoded = FALSE,
           
           na.rm = TRUE,
           max.level = NA,
           min.level = NA,
           
           digits = 2) {
    X <- stp25formula::prepare_data2(...)
    n <- length(X$measure.vars)
    if (!include.label)
      X$row_name <- X$measure.vars
    
    if (length(X$measure.vars) > 1)   {
      rslt <- Reliability (
        X$data[X$measure.vars],
        revcoded = revcoded,
        check.keys = FALSE,
        max.level = max.level,
        min.level = min.level,
        type = "mean",
        na.rm = TRUE
      )
      
      
      
      item <-
        data.frame(
          Items = paste0(rslt$labels, ifelse(rslt$keys < 0, " (-)", "")),
          n = rslt$item_statistik$n,
          M = stp25rndr::Format2(rslt$item_statistik$m, 2),
          SD = stp25rndr::Format2(rslt$item_statistik$sd, 2),
          "Alpha if Item Deleted" = stp25rndr::Format2(rslt$psych$item.stats$r.drop, 2)
        )
      aplha_statistik <- with(
        rslt,
        data.frame(
          Items = Items,
          n = n,
          M = stp25rndr::Format2(M, 2),
          SD = stp25rndr::Format2(SD, 2),
          
          Range = paste(stp25rndr::Format2(range, 2), collapse = "; "),
          Skew = stp25rndr::Format2(Skew, 2),
          Kurtosi = stp25rndr::Format2(Kurtosi, 2) ,
          "Shapiro Test" = shapiro
        )
      )
      
      if (include.cronbachs.alpha)
        aplha_statistik$Alpha = 
        stp25rndr::Format2(rslt$Alpha, 2)
      if (include.inter.item.correlation)
        aplha_statistik$inter.item.correlation =    
        stp25rndr::Format2(performance::item_intercor(rslt$data), 2)
      
      
      item_statistics =
        prepare_output(
          item,
          caption = paste("Itemstatistiken", caption),
          note = note,
          N = n
        )
      scale_statistics  =
        prepare_output(
          aplha_statistik ,
          caption = paste("Item-Mittelwerte", caption),
          note = note,
          N = n
        )
      if (include.item_statistics & include.scale_statistics)
        return(list(item_statistics = item_statistics, scale_statistics =
                      scale_statistics))
      else if (include.item_statistics)
        return(item_statistics)
      else
        return(scale_statistics)
    }
    else{
      x <- as.numeric(X$data[[1]])
      res_shapiro <- stats::shapiro.test(x)
      return(
        prepare_output(
          data.frame(
            Items = 1,
            n  = length(na.omit(x)),
            M  = stp25rndr::Format2(mean(x, na.rm = na.rm), digits),
            SD = stp25rndr::Format2(sd(x, na.rm = na.rm), digits),
            Range = paste(stp25rndr::Format2(range(x, na.rm = na.rm), digits), collapse = "; "),
         #   Alpha = "n.a.",
            Skew    = stp25rndr::Format2(psych::skew(x, na.rm = na.rm), digits),
            Kurtosi = stp25rndr::Format2(psych::kurtosi(x , na.rm = na.rm), digits),
            shapiro = stp25rndr::rndr_shapiro(res_shapiro$statistic, res_shapiro$p.value)
          )
        ),
        caption = paste("Item-Mittelwerte", caption),
        note = note,
        N = n
      )
    }
  }




#' @rdname Reliability
#' @export
APA.stp25_reliability <- function(x, ...) {
  paste("Alpha = ", Format2(x$Alpha, 2))
}

#' @rdname Reliability
#' @export
Reliability2 <- function(...) {
  APA_Reliability(...)
}

#' @rdname Reliability
#' @export
APA2.stp25_reliability <- function(x,
                                   caption = "",
                                   note = "",
                                   output=which_output(),
                                   ...) {
item <-
    data.frame(
      Items = paste0(x$labels, ifelse(x$keys < 0, " (-)", "")),
      n = x$item_statistik$n,
      M = Format2(x$item_statistik$m, 2),
      SD = Format2(x$item_statistik$sd, 2),
      "Alpha if Item Deleted" = Format2(x$psych$item.stats$r.drop, 2)
    )
  aplha_statistik <- with(
    x,
    data.frame(
      Items = Items,
      n = n,
      M = Format2(M, 2),
      SD = Format2(SD, 2),
      Alpha = Format2(Alpha, 2),
      Range = paste(Format2(range, 2), collapse = "; "),
      Skew = Format2(Skew, 2),
      Kurtosi = Format2(Kurtosi, 2) ,
      "Shapiro Test" = shapiro
    )
  )
  x$item_statistics <-
    prepare_output(item,
                   caption = paste("Itemstatistiken", caption),
                   note = note)
  x$aplha_statistics <-
    prepare_output(
      aplha_statistik ,
      caption = paste("Item-Mittelwerte", caption),
      note = note
    )
  Output(x$item_statistics, output=output, ...)
  Output(x$aplha_statistics, output=output, ...)

  invisible(x)

}


#' @rdname Reliability
#' @export
APA_Reliability <- function(...,
                            caption = "",
                            note = "",
                            output=which_output()
                            ) {
  APA2.stp25_reliability(Reliability(...),
                         caption,
                         note,
                         output)
}


#' @rdname Reliability
#' @export
print.stp25_reliability <- function(x,
                                    ...) {
  cat("\nnames: ", paste(names(x), collapse = ", "), "\n")
  cat("\nAlpha: ")
  print(x$Alpha)

}


#' @rdname Reliability
#' @param name Slalen namen
#' @export
Reliability.data.frame <- function(data,
                                   name = NULL,
                                   ...) {
  if (is.null(name)) {
    name <- grap_call_name(data)
    if (length(name) == 0)
      name <- "Skale"
  }
  res <- Reliability.default(data, ...)
  res$name <- name
  res
}

#' @rdname Reliability
#' @param data data.frame mit den Daten
#' @export
Reliability.formula <- function(x,
                                data,
                                name = "Skale",
                                ...) {

  data <- Formula_Data(x, data)$Y_data
  res <- Reliability.default(data, ...)
  res$name <- name
  res
}


#' @rdname Reliability
#' @description  \code{Appha} Cronbach-Alpha Werte extrahieren
#'
#'  Cronbachs Alpha oder einfach nur α ist ein Maß für die interne Konsistenz einer Skala.
#'
#'  > .9	Exzellent
#'
#'  > .8	Gut / Hoch
#'
#'  > .7	Akzeptabel
#'
#'  > .6	Fragwürdig
#'
#'  > .5	Schlecht / Niedrig
#'
#'  < .5	Inakzeptabel
#'
#' Quelle http://statistikguru.de/spss/reliabilitaetsanalyse/auswerten-und-berichten-2.html
#'
#' @export

Alpha <- function(...,
                  type = 1,
                  names = NULL ) {
  if (is.null(names)) {
    names <- as.list(sys.call())[-1]
    nms <-
      which(names(names) %in% c("caption" , "note", "output", "type", "names"))
    if (length(nms > 0))
      names <-  paste(names[-nms])
    else
      names <-  paste(names)
  }
    
  skalen <- list(...)

  result <- NULL
  for (i in skalen) {
    result <- rbind(result, fix_alpha(i))
  }
  result$Source <- names
  result
}

#' @rdname Reliability
#' @export

Alpha2 <- function(...,
                   caption = "",
                   note = "",
                   output = which_output(),
                   type = 1,
                   names = NULL) {

  if (is.null(names)) {
    names <- as.list(sys.call())[-1]
    nms <-
      which(names(names) %in% c("caption" , "note", "output", "type", "names"))
    if (length(nms > 0))
      names <-  paste(names[-nms])
    else
      names <-  paste(names)
  }
  
  result <- Alpha(..., type = type, names = names )
  
  Output(result,
         caption = caption,
         note = note,
         output = output)
  
  invisible(result)
}





#' @rdname Reliability
#' @param max.level,max.level an aggregate
#' @param revcoded position zum umcodieren. Kann entweder nummer oder name oder TRUE sein.
#' @param type Aggregatfunktion fuer die Skala (mean, median und trimmed)
#' @param na.rm Fehlende Werte
#' @param check.keye aus psych wenn \code{check.keye=TRUE} 
#' gesetzt wird werden die Daten automatisch umkodiert
#' @param ... an psych::alpha()
Reliability.default <- function(x,
                                revcoded = FALSE,
                                check.keys = FALSE,
                                max.level = NA,
                                min.level = NA,
                                type = "mean",
                                na.rm = TRUE,
                                ...) {
  data <- stp25aggregate:::transform_to_numeric(x)
  #- data ist jetzt eine Liste mit # list(data, range , label)
  result <-
    item_statistik(data, revcoded, check.keys, ...)
  # list(data, range , label, keys, list(alpha) , Alpha)
  result <-
    skala_statistik(result, type, na.rm)
  # list(data, range , label, keys, list(alpha) , Alpha, index, n...schapiro)

  class(result) <- c("stp25_reliability", class(result))

  result
}



 
#' Item Statistik
#' 
#' @noRd
item_statistik <- function(data, #Data ist Liste
                           revcoded,
                           check.keys = TRUE,
                           ...) {
  # Zum erzeugen einer Liste mit den Daten und labels
  if (is.data.frame(data))
    data <- transform_to_numeric(data)
  else if (!is.list(data))
    return(class(data))
  else{

  }
  #Text(revcoded)
  if (is.numeric(revcoded) | is.character(revcoded)) {
    data$data <- stp25aggregate:::Umcodieren(data$data,
                            revcoded,
                            max.level = data$range[2],
                            min.level = data$range[1])
    data$keys <-  if (is.numeric(revcoded))
      ifelse(1:ncol(data$data) %in% revcoded, -1, 1)
    else
      ifelse(names(data$data) %in% revcoded, -1, 1)
    #  Text(data$keys)

    data$psych <- psych::alpha(data$data, check.keys = FALSE, ...)
  }
  else if (isTRUE(revcoded) | isTRUE(check.keys)) {
    alp_check <- psych::alpha(data$data, check.keys = TRUE, ...)#
    data$keys <- alp_check$keys
    if (any(alp_check$keys == -1)) {
      data$data <-
        stp25aggregate:::Umcodieren(
          data$data,
          which(alp_check$keys == -1),
          max.level = data$range[2],
          min.level = data$range[1]
        )
      data$psych <-
        psych::alpha(data$data, check.keys = FALSE, ...)
    } else{
      data$psych <- alp_check
    }
  }
  else{
    data$keys <- rep(1, ncol(data$data))
    data$psych <-
      psych::alpha(data$data, check.keys = check.keys, ...)
    #-- data bleiben gleich
  }

  #- library Pych Version: 1.4.3
  #- Date:	 2014--March--25
  #- liefert falsche n (bei n>120 wird 122 ausgegeben)
  data$item_statistik <-
    list(
      m = sapply(data$data, mean, na.rm = T),
      sd = sapply(data$data, sd, na.rm =
                    T),
      n = sapply(data$data, function(x)
        length(na.omit(x)))
    )


  data$Alpha <- as.numeric(data$psych$total$raw_alpha)

  data
}


 
#' Helper Funktion Mittelwerte
#' 
#' @noRd
skala_statistik <- function(data,
                            type = c("mean", "median", "trimmed", "sum"),
                            na.rm = TRUE) {
  if (is.data.frame(data))
    data <- transform_to_numeric(data)
  else if (!is.list(data))
    return(class(data))
  type <- match.arg(type)
 

  data$index <- apply(
    data$data,
    1,
    FUN = function(x) {
      switch(
        type,
        mean = mean(x, na.rm = na.rm),
        median = median(x, na.rm = na.rm),
        trimmed = mean(x, trim = .1, na.rm = na.rm),
        "NAs"
      )
    }
  )

  res_shapiro <- stats::shapiro.test(data$index)
  data$Items <- ncol(data$data)
  # data$N  <- nrow(data)
  data$n  <- length(na.omit(data$index))
  data$M  <- mean(data$index, na.rm = TRUE)
  data$SD <- sd(data$index, na.rm = TRUE)

  data$Skew    <- psych::skew(data$index, na.rm = TRUE)
  data$Kurtosi <- psych::kurtosi(data$index , na.rm = TRUE)

  data$shapiro <- rndr_shapiro(res_shapiro$statistic,res_shapiro$p.value)

  data
  }

#' psych::alpha zu data.frame
#' 
#' @noRd
fix_alpha <- function(x) {
  with(
    x,
    data.frame(
      Source = name,
      Items = Items,
      n = n,
      M = Format2(M, 2),
      SD = Format2(SD, 2),
      Alpha = Format2(Alpha, 2),
      Range = paste(Format2(range, 2), collapse = "; "),
      Skew = Format2(Skew, 2),
      Kurtosi = Format2(Kurtosi, 2) ,
      "Shapiro Test" = shapiro,
      stringsAsFactors = FALSE
    )
  )
}

