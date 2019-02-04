#' Tabelle
#'
#'  Einfache deskriptive Tabelle die in medizinischen Arbeiten
#' verwendet werden.
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
#' @description Tabelle2:  html-Output Tabelle(...) %>% Output()
#' @export
#' @examples
#'
#' varana2 %>% Tabelle2(Merkfgk, by=~ Zeit)
#'
Tabelle2 <- function(...,
                     output = TRUE) {
  x <- Tabelle(...)
  Output(x)
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
#' @param type 1 oder 2 1 ist kurzes Format 2 int lang
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
#'
#' @param max_factor_length Fehler bei langen Faktoren abfangen
#'
#' @param APA APA2 Style TRUE/FALSE
#'
#' @export
Tabelle.default <- function(...,
                            formula = NULL,
                            fun = NULL,
                            type = c(
                              "2", # "auto_lang" ## 44.56 (SD 18.10, range 25.00 to 70.00)
                              "1", # "auto_kurz" ## 44.56 (18.10)
                             # "auto",
                              "freq","freq.ci",
                              "mean", "median", "ci","cohen.d","effsize",
                              "multiresponse",
                              "describe", "correlation",
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

                          #  include.p = TRUE,
                           # include.stars = FALSE,
                          #  include.mean=FALSE,  # fuer Correlation
                          #  corr_test = "pearson",
                          #  cor_diagonale_up = TRUE,
                            max_factor_length = 35,
                          #  order = FALSE,
                          #  decreasing = FALSE,
                         #   useconTest = FALSE,
                           # normality.test = FALSE
                         measure.name = "value"
                          ) {
  type <-  match.arg(type, several.ok = TRUE)[1]

  if(!is.null(fun)) type <- "custom_fun"

  if(type=="correlation"){
    stop("Benutze Bitte die Funktion APA_Correlation()!")
  }

  if (APA) {

  #  cat("\ninAPA=TRUE")
    errate_statistik3(
      ...,
      type = type,        # "multiresponse"
      caption = caption,
      note = note,
    #  digits = digits,
    #  test = test,
      na.action = na.action,
      exclude = exclude,

      include.n = include.n,
      include.nr = include.nr,
      include.total = include.total,
      include.test = include.test, # "wilcox.test","u.test",  "kruskal.test","h.test",
                                   # "chisq.test","t.test",  "aov", "anova",
                                   # "SPSS", "Hmisc"
                                   #         "shapiro.test" "KS.test"
                                  #  Kolmogorov-Smirnov-Anpassungstest mit SPSS
   #   include.p = include.p,
   #   include.stars = include.stars,
    #  include.mean=include.mean,
   #   corr_test = corr_test,
    #  cor_diagonale_up = cor_diagonale_up,
      max_factor_length = max_factor_length#,
    #  order = order,
    #  decreasing = decreasing,
   #   useconTest = useconTest,
    #  normality.test = normality.test
    )
  }
  else
  {
    if (is.null(formula)) {
      calculate_tabelle2(
        prepare_data2(...),
        type = type,
        # nur ein Type erlaubt
        caption = caption,
        note = note,
        fun = fun,
        digits = digits,
        measure.name = measure.name
      )
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
      prepare_output(reshape2::dcast(res,
                                     formula, function(x) {
                                       paste(x, collapse = "|")
                                     }),
                     caption, note)
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
  res <- NULL #ergebnis liste

  if(type[1] =="2") type <- "auto_long"
  else if( type[1] == "1")  type <- "auto_kurz"
  else if( type[1] == "custom_fun")  X$measure <- rep("custom_fun", length(X$measure))
  else{

    if(type %in% c("mean", "median", "factor", "freq")){
      type[which(type=="freq")] <- "factor"

    if(length(type) != length(X$measure)) X$measure  <- rep(type[1], length(X$measure))
    else X$measure  <-  type
    } else warning("Der gewaehlte type ", type[1], " ist nicht implementiert.")

    type <- "auto_kurz"
  }


  for (i in seq_len(length(X$measure))) {
    #-- Labels ----------------------------------
    if (X$measure[i] == "factor"){
      if( !is.factor(  X$data[[X$measure.vars[i]]])  ) {

        X$data[[X$measure.vars[i]]] <- factor(X$data[[X$measure.vars[i]]])
        warning("Konvertiere die Variable ", X$measure.vars[i], " zu Factor!")
      }
      X$row_name[i] <- paste0(X$row_name[i], " (",
                                paste0(levels(X$data[[X$measure.vars[i]]]), collapse = "/"), ")")}
    else if (X$measure[i] == "mean")
        X$row_name[i] <- paste0(X$row_name[i], " (mean)")
    else if (X$measure[i] == "median")
        X$row_name[i] <- paste0(X$row_name[i], " (median)")


    res[[X$measure.vars[i]]]  <-
      berechne_all(
        X$data,
        X$measure.vars[i],
        X$by,
        X$measure[i],
        type,
        fun = fun,
        digits =  if (is.null(digits))  X$digits[i] else digits,
        measure.name = measure.name
      )
  }

  df <- plyr::ldply(res)
  df[, 1] <- factor(df[, 1], names(X$row_name), X$row_name)

  prepare_output(df, caption, note, nrow(X$data), NA)
}





# Mittelwerte -------------------------------------------------------------

#' @rdname Tabelle
#' @export
Tabelle.lmerModLmerTest <- function(x,
                                    digits = 2,
                                    fun = function(x) {
                                      c(
                                        n = length(x),
                                        M = mean(x, na.rm = TRUE),
                                        SD = sd(x, na.rm = TRUE)
                                      )
                                    },
                                    ...) {
  Tabelle.lm(x, digits, fun)
}



#' @rdname Tabelle
#' @export
Tabelle.glm <- function(x,
                        digits = 2,
                        fun = function(x) {
                          c(n = length(x),
                            M = mean(x, na.rm = TRUE))
                        },
                        ...)
{
  Tabelle.lm(x, digits, fun)
}



#' @rdname Tabelle
#' @export
Tabelle.lm <- function(x,
                       digits = 2,
                       fun = function(x) {
                         c(
                           n = length(x),
                           M = mean(x, na.rm = TRUE),
                           SD = sd(x, na.rm = TRUE)
                         )
                       },
                       ...) {
  res_list <- NULL
  myeff <- effects::allEffects(x)

  for (i in names(myeff)) {
    info <- model_info(myeff[[i]])
    ans <- aggregate_effect(myeff[[i]], info$y, info$x, fun)

    AV <- ifelse(is.na(info$labels[info$y]),
                 info$y, info$labels[info$y])

    ans <-
      data.frame(plyr::llply(ans, function(x)
        if (is.numeric(x))
          round(x, digits)
        else
          x),
        stringsAsFactors = FALSE)

    res_list[[i]] <- prepare_output(ans,
                                    paste0("AV: ", AV), "",
                                    info$N,  info$labels)
  }
  res_list
}


aggregate_effect <- function(eff,
                             y,
                             x,
                             fun = function(x)
                               length(x)) {
  fm <- formula(paste(y, "~", paste(x, collapse = "+")))
  df <- eff$data
  #-- Faktoren fuer N berechnung vorbereiten
  for (j in names(eff$variables)) {
    if (!eff$variables[[j]]$is.factor)
      df[, j] <- cut(df[, j], length(eff$variables[[j]]$levels))
  }

  res <- try(aggregate(fm, df, fun, drop = FALSE))
  if (class(res) == "try-error")
    data.frame(NULL)  #  wegen ncol im weiteren progammverlauf
  else
    do.call(data.frame, res)
}



# Describe <-  function(x, ...) {
#   fix_format(broom::tidy(x))
# }



#' @rdname Tabelle
#' @description Describe2: workaraond fuer  psych::describe()
#' @param stat result von psych kann "n", "mean", "sd",
#' "median", "trimmed", "mad", "min" ,
#' "max", "range", "skew", "kurtosis" ,"se"
#' @export
Describe2 <- function(...,
                    output = FALSE) {
  UseMethod("Describe2")
}

#' @rdname Tabelle
#' @export
Describe2.data.frame<-function(...){
  
  cat("\n Noch nicht implementiert!\n")
  data.frame(NULL)
}

#' @rdname Tabelle
#' @export
Describe2.formula <- function(x,
                              data,
                              by = NULL,
                              caption = "",
                              note = "",
                              stat = c("n", "mean", "sd", "min", "max"),
                              output = which_output(),
                              digits = 2,
                              ...) {
  vars <- which(names(data) %in% all.vars(x))
  stat <- c(
    "vars",
    "n",
    "mean",
    "sd" ,
    "median",
    "trimmed",
    "mad",
    "min",
    "max",
    "range",
    "skew",
    "kurtosis",
    "se" ,
    stat
  )
  stat <- unique(stat[duplicated(stat)])
  
  if (is.null(by)) {
    data <- data[vars]
    result <-  as.data.frame(psych::describe(data),
                             stringsAsFactors = FALSE)
    # stat <- c(names(result), stat)
    # stat <- unique(stat[duplicated(stat)])
    result <- cbind(Item = GetLabelOrName(data), result)
    
    res <- result[c("Item", stat)]
    res[-1] <- Format2(res[-1], digits = digits)
    } else{
    names_groups <- which(names(data) %in% all.vars(by))
    groups <- data[names_groups]
    if(ncol(groups)>1){
      groups<- interaction(groups,  sep = " / ")
    }
    
    data <- data[vars]
    results_list <- psych::describeBy(data, groups)
    result <- res <- NULL
    
    for (i in   names(results_list)) {
      r1 <- as.data.frame(results_list[[i]],
                          stringsAsFactors = FALSE)
      
      r1 <- cbind(Item = GetLabelOrName(data), Group = i, r1)
      result <- rbind(result, r1)
    }
        res <- result[c("Item", "Group", stat)]
    res[-c(1:3)] <- Format2(res[-c(1:3)], digits = digits)
  }
  
  
  
  res  <-
    prepare_output(res,
                   caption,
                   note,
                   nrow(data))
  
  Output(res, output = output)
  
  invisible(result)
}
