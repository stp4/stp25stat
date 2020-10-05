#' Tabelle
#'
#' Einfache deskriptive Tabelle verwendet werden.
#' Die Funktion arbeitet Intern mit \code{aggregate} bzw. mit  berechne_default()
#' also aggregate(formula, data,FUN).
#'
#' @name Tabelle
#' @return Tabelle: data.frame oder list mit data.frame
#' Tabelle2: HTML
#' @author Wolfgang Peter
#' @export
#' @examples
#'
#' library(dplyr)
#' library(tidyr)
#' #names(varana)
#' #  set_my_options(mittelwert=list(median.style="Quantil")) # ++IQR
#' require(stp25data)
#' varana2 <- varana %>%
#'   gather(Zeit, Merkfgk, m1:m4) %>%
#'   mutate(Zeit = factor(Zeit, Cs(m1, m2, m3 , m4), Cs(t0, t1, t2, t3))) %>%
#'   stp25aggregate::Label(Merkfgk = "Merkfaehigkeit")
#'
#' Tabelle(Merkfgk ~ Zeit, varana2)
#'
#' varana2 %>% Tabelle(Merkfgk, by =  ~ Zeit)
#' varana %>% Tabelle(m1, m2, m3 , m4)
#'
#' 
#' #varana %>% Tabelle(
#' #  4:7,
#' #  by =  ~ geschl,
#' #  fun = function(x)
#' #    c(
#' #      n = length(na.omit(x)),
#' #      m = mean(x),
#' #      sd = sd(x)
#' #   )
#' #)
Tabelle <- function(...,
                    output = FALSE) {
  UseMethod("Tabelle")
}



#' @rdname Tabelle
#' @description Tabelle2: Tabelle(...) %>% Output()
#' @export
#' @examples
#'
#'  get_my_options()$apa.style$mittelwert$include_name
#'
#' varana2 %>% Tabelle2(Merkfgk, by=~ Zeit)
#'
Tabelle2 <- function(...,
                     output = which_output()) {
  x <- Tabelle(...)
  Output(x, output=output)
  invisible(x)
}

#' @rdname Tabelle
#' @export
Tabelle.NULL <- function(){
  Info_Statistic(
    c("catTest", "conTest", "Wilkox", "Kruskal",
      "ANOVA",
      "T Test"),
    c("stats", "Hmisc", "stats", "stats",
      "car",
      "stats"),
    c(
      "chisq.test",
      "spearman2",
      "wilcox.test",
      "kruskal.test",
      "Anova, type = 3",
      "t.test"
    ), paste(methods("Tabelle"), collapse=", ")
  )
}

#' @rdname Tabelle
#'
#' @param ...   Die auszuwertenden Variablen  sex, age="mean", usw
#' @param type 1 oder 2  2=auto_lang 44.56 (SD 18.10, range 25.00 to 70.00) 1= auto_kurz 44.56 (18.10)
#' @param formula An dcast Gruppe ~ .id ist zum Zeilen und Spalten vertauschen
#' @param fun Eigene Function am Berechne
#' @param digits Kommastellen
#' @param caption,note Uberschrift an Output
#'
#' @param test,include.test  Signifikanz Test include.test
#'   "wilcox.test","u.test",  "kruskal.test","h.test",
#'   "chisq.test","t.test",  "aov", "anova",
#'   "SPSS", "Hmisc"
#'   "shapiro.test" "KS.test"
#  Kolmogorov-Smirnov-Anpassungstest mit SPSS
#' @param na.action,exclude an Formula
#'
#' @param include.n,include.nr,include.total Anzahl ausgeben
#' @param exclude.level Wenn ein Factor zwei Levels hat kann einer Ausgeschlossen werden exclude.level ="no" bei levels = c("yes", "no")
#' @param max_factor_length Fehler bei langen Faktoren abfangen
#'
#' @param APA APA2 Style TRUE/FALSE
#' @param   stat  fuer type="describe" c("vars","n","mean","sd" ,"median","trimmed","mad","min","max","range","skew","kurtosis","se" ,
#'
#' @export
Tabelle.default <- function(...,
                            formula = NULL,
                            fun = NULL,
                            type = c(
                              "2", 
                              "1", 
                              "freq","freq.ci",
                              "mean", "median", "ci","cohen.d","effsize",
                              "multiresponse",
                            #  "describe",
                              "correlation",
                              "custom_fun"),
                            caption = "Charakteristik",
                            note = "",

                            digits = NULL,
                            APA = FALSE,
                            test = FALSE,
                            na.action = na.pass,
                            exclude = NA,

                            include.n = TRUE,
                            include.nr = FALSE,
                            include.total = FALSE,

                            include.test = test,
                            exclude.level=NULL,

                            max_factor_length = 35,
                            order = FALSE,
                            measure.name = "value"#,
                            #describe = c("n", "mean", "sd", "min", "max")
                          ) {
  type <-  match.arg(type, several.ok = TRUE)
  type<- type[1]

  # if ("describe" %in% type)
  #   return(
  #     Describe2(
  #       ...,
  #       by = by,
  #       caption = caption,
  #       note = note,
  #       stat = describe,
  #       output = output,
  #       digits = if (is.null(digits)) 2 else digits
  #     )
  #   )


  if(!is.null(fun)) type <- "custom_fun"

  if(type=="correlation"){
    stop("Benutze Bitte die Funktion APA_Correlation()!")
  } else if(type=="multiresponse") APA <- TRUE

  if (APA) {
    errate_statistik3(
      ...,
      type = type,
      caption = caption,
      note = note,
      na.action = na.action,
      exclude = exclude,
      include.n = include.n,
      include.nr = include.nr,
      include.total = include.total,
      include.test = include.test,
      exclude.level=exclude.level,
      max_factor_length = max_factor_length,
      order = order
    )
    
    # prepare_output(
    #   Tbll_desc(
    #     ...,
    #     type = type,
    #     na.action = na.action,
    #     exclude = exclude,
    #     include.n = include.n,
    #     include.nr = include.nr,
    #     include.total = include.total,
    #     include.test = include.test,
    #     exclude.level = exclude.level,
    #     max_factor_length = max_factor_length
    #   ),
    #   caption = caption,
    #   note = note
    # )
 
  }
  else
  {
    if (is.null(formula)) {
    res <-  calculate_tabelle2(
        prepare_data2(...),
        type = type,
        # nur ein Type erlaubt
        caption = caption,
        note = note,
        fun = fun,
        digits = digits,
        measure.name = measure.name
      )
    if(names(res)[1] == ".id")
      names(res)[1] <- "Item"
    res
    
    }
    else {
      res <- calculate_tabelle2(
        prepare_data2(...),
        type = type[1],
        caption = caption,
        note = note,
        fun = fun,
        digits = digits,
        measure.name = measure.name
      )
      
      res <- reshape2::dcast(res,
                             formula, function(x) {
                               paste(x, collapse = "|")
                             })
      if(names(res)[1] == ".id")
        names(res)[1] <- "Item"
   
      prepare_output(res, caption, note)
    }
  }
}



calculate_tabelle2 <- function(X,
                               type,
                               caption,
                               note,
                               fun = NULL,
                               digits = digits,
                               measure.name = "value") {
  res <- NULL
  
  if (type[1] == "2") {
    type <- "auto_long"
  }
  else if (type[1] == "1") {
    type <- "auto_kurz"
  }
  else if (type[1] == "custom_fun") {
    X$measure <- rep("custom_fun", length(X$measure))
  }
  else{
    if (type %in% c("mean", "median", "factor", "freq")) {
      type[which(type == "freq")] <- "factor"
      if (length(type) != length(X$measure))
        X$measure  <- rep(type[1], length(X$measure))
      else
        X$measure  <-  type
    } else
      warning("Der gewaehlte type ", type[1], " ist nicht implementiert.")
    type <- "auto_kurz"
  }
  
  
  for (i in seq_len(length(X$measure))) {
    if (X$measure[i] == "factor") {
      if (!is.factor(X$data[[X$measure.vars[i]]])) {
        X$data[[X$measure.vars[i]]] <- factor(X$data[[X$measure.vars[i]]])
        warning("Konvertiere die Variable ", X$measure.vars[i], " zu Factor!")
      }
      X$row_name[i] <- paste0(X$row_name[i], " (",
                              paste0(levels(X$data[[X$measure.vars[i]]]),
                                     collapse = "/"), ")")
    }
    else if (X$measure[i] == "mean") {
      X$row_name[i] <- paste0(X$row_name[i], " (mean)")
    }
    else if (X$measure[i] == "median") {
      X$row_name[i] <- paste0(X$row_name[i], " (median)")
    }
    else if (X$measure[i] == "logical") {
      X$data[[X$measure.vars[i]]] <-
        factor(X$data[[X$measure.vars[i]]], c(TRUE, FALSE))
      X$measure[i] <- "factor"
      X$row_name[i] <- paste0(X$row_name[i], " (",
                              paste0(levels(X$data[[X$measure.vars[i]]]),
                                     collapse = "/"), ")")
    }
    
    res[[X$measure.vars[i]]] <-
      berechne_all(
        X$data,
        X$measure.vars[i],
        X$by,
        X$measure[i],
        type,
        fun = fun,
        digits = if (is.null(digits)) X$digits[i] else digits,
        measure.name = measure.name
      )
  }
  
  df <- plyr::ldply(res)
  df[, 1] <- factor(df[, 1], names(X$row_name), X$row_name)
  
  prepare_output(df, caption, note, nrow(X$data), NA)
}

 