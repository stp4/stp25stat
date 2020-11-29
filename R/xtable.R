#' APA_Xtabs
#' 
#' Die Funktion Formatiert xtabs() mit NxM und NxMxO Tabellen.
#' Sig.-Tests die Berechnung erfolgt  mit assocstats.
#' 
#' @export

APA_Xtabs <-   function(x, ...) {
  UseMethod("APA_Xtabs")
}
#' @rdname Tabelle
#' @export
APA_Xtabs.NULL <- function() {
  Info_Statistic(
    c(
      "include.chisq",
      "include.fisher",
      "include.correlation",
      "include.diagnostic"
    ),
    c("vcd", "stats", "vcd", "caret"),
    c(
      "assocstats",
      "fisher.test",
      "assocstats",
      "confusionMatrix"
      
    ),
    paste(methods("APA_Xtabs"), collapse = ", ")
  )
}

#' @rdname APA_Xtabs
#' @export
APA_Xtabs.glm <- function(x,
                          caption = "",
                          output = stp25output::which_output(),
                          thresh = 0.5,
                          ...) {
  x <- Klassifikation(x, thresh, caption)$xtab
  APA2(x, output = output, ...)
}



#' @rdname APA_Xtabs
#' 
#' @param data = data.frame
#' @param labels Beschriftung mit labels
#' @param addNA,exclude,drop.unused.levels An xtabs() default = FALSE
#' 
#' @export
#' @examples 
#' 
#' df<- data.frame(A = c(1,0,0,1,0,1,0,1,1,0,0,0,0,1,1),
#' B = c(0,0,1,0,1,0,1,1,1,1,1,1,1,0,0)
#' )
#' 
#' 
#' APA_Xtabs(
#'   ~ A + B,
#'   df,
#'   caption = "2x2 Tabelle zur Destimmung der Kennwerte",
#'   include.percent = FALSE,
#'   include.test = TRUE,
#'   include.diagnostic = TRUE
#' )
#' 
APA_Xtabs.formula <- function(x,
                              data = NULL,
                              caption = "", note="",
                              output = stp25output::which_output(),
                              labels = TRUE,
                              addNA = FALSE,
                              exclude = if (!addNA)  c(NA, NaN) ,
                              drop.unused.levels = FALSE,
                              ...) {
 
  x_tab <- stats::xtabs(x, data,
                    addNA = addNA,
                    exclude = exclude,  
                    drop.unused.levels = drop.unused.levels)
  
  if (is.logical(labels)) {
    if (labels) {
      dnn <- dimnames(x_tab)
      names(dnn) <-
        stp25aggregate::get_label( data[all.vars(x)], include.units=FALSE )
      dimnames(x_tab) <- dnn
    }
  } else if (is.character(labels)) {
    dnn <- dimnames(x_tab)
    names(dnn)[1:length(labels)] <- labels
    dimnames(x_tab) <- dnn
  } else if (is.list(labels)) {
    dimnames(x_tab) <- labels
  }
  
  APA2.xtabs(x_tab, 
             caption = caption, 
             output = output, 
             note = note,
             ...)
}

#' @rdname APA_Xtabs
#' @export
APA_Xtabs.xtabs <- function(x, ...) {
  APA2.xtabs(x, ...)
}


#' @rdname APA_Xtabs
#' @export
APA_Xtabs.default <- function(x, ...) {
  Text("Keine Methode fuer ", class(x), " vorhanden.")
}


#' APA.xtabs: Chi-Quadrat aus Kreuztabellen
#'
#' @rdname APA
#' @export
APA.xtabs <- function(x, ... ) {
  x<-summary(x)
  rndr_Chisq(x$statistic, x$parameter, x$p.value)
}


#' @rdname APA
#' @export
APA.table <- function(x, ...) APA.xtabs(x, ...)



#' @rdname APA2
#' 
#' @description APA2.loglm: Orginal MASS::print.loglm
#' @export
#' @examples
#'
#' # Fit Log-Linear Models by Iterative Proportional Scaling
#' library(MASS)
#'
#' fit<-loglm(~ Type + Origin, xtabs(~ Type + Origin, Cars93))
#' APA2(fit)
APA2.loglm <- function(x,
                       caption = "Likelihood",
                       note = "",
                       output = stp25output::which_output(),
                       col_names = NULL,
                       ...) {
  ts.array <- 
    rbind(c(x$lrt, x$df,
           if (x$df > 0L) 1 - pchisq(x$lrt, x$df) else 1
          ),
          c(x$pearson, x$df,
            if (x$df > 0L) 1 - pchisq(x$pearson, x$df) else 1))
  
  dimnames(ts.array) <- list(c("Likelihood Ratio",
                               "Pearson"),
                             c("Chi2", "Df", "p.value"))
  res <-
    prepare_output(
      fix_data_frame2(
        Test = rownames(ts.array), ts.array)
      , caption, note)

  Output(res, output=output, note=note)
  invisible(res)
}

#' @rdname APA2
#' @export
#' @examples
#' #-- APA2.summary.table
#'
#' a <- letters[1:3]
#' APA2(summary(table(a, sample(a))))
#' 
APA2.summary.table <- function(x, ...) {
  r <- data.frame(
    Chisq =    Format2(x$statistic, 2),
    df = x$parameter,
    p =  rndr_P(x$p.value, FALSE)
  )
  r <- prepare_output(r)
  Output(r, ...)
  invisible(r)
}


#' @rdname APA2
#' @export
#' @examples
#' #-- APA2.summary.table
#'
#' a <- letters[1:3]
#' APA2(summary(table(a, sample(a))))
#' 
APA.summary.table <- function(x, ...) {
  Text(paste0(
    "Chisq(df=", x$parameter, ")=",
    Format2(x$statistic, 2), ", ", rndr_P(x$p.value)))
}


#' @rdname APA_Xtabs
#' @export
#'
APA2.table <- function(...) APA2.xtabs(...)




#' @rdname APA_Xtabs
#'
#' @param digits Nachkommastellen
#' @param x xtabs-Objekt oder Formel
#' @param caption,note,output  an Output 
#' @param include.total,include.total.columns,include.total.sub,include.total.rows Zeilen Prozenz usw
#' aber wie mit den Margins gerechnet 
#' @param include.percent,include.count,include.tabel ausgabe
#' @param margin,add.margins flexible einstellung der Prozent
#' @param include.test,test,include.prop.chisq,include.chisq,include.fisher die Tests
#' @param include.correlation 
#' @param include.diagnostic,include.sensitivity,prevalence ascostat
#' @param ... not used
#' 
#' @return list("xtab","fisher_test","diagnostic.test")
#' 
#' @export
#' 
#' 
#' @examples 
#' 
#' 
#' 
#' data(infert, package = "datasets")
#' infert$case  <- factor(infert$case ,1:0, c("case", "control") )
#' 
#' infert$spontaneous <- factor(infert$spontaneous)
#' infert$induced2    <- factor(infert$induced==0)
#' 
#' tab_1<- xtabs(~  case, infert)
#' tab_2x2<- xtabs(~ induced2 + case, infert)
#' tab_3x2<- xtabs(~ induced + case, infert)
#' tab_3x3<- xtabs(~ induced + education, infert)
#' tab_3x3x2<- xtabs(~ induced + education+case, infert)
#' 
#' #APA2(summary(tab_3x3x2))
#' 
#' (APA2(tab_1, include.test=TRUE, output=FALSE))
#' (APA2(tab_2x2, include.test=TRUE, output=FALSE))
#' (APA2(tab_3x2, include.test=TRUE, output=FALSE))
#' (APA2(tab_3x3, include.test=TRUE, output=FALSE))
#' (APA2(tab_3x3x2, include.test=TRUE, output=FALSE))
#' 
APA2.xtabs  <- function(x,
                        caption = "" ,
                        note = "",
                        output = stp25output::which_output(),
                        digits = options()$stp25$apa.style$prozent$digits[1],
                        
                        include.total = FALSE,
                        include.total.columns = FALSE,
                        include.total.sub = FALSE,
                        include.total.rows = FALSE,
                        
                        include.percent = TRUE,
                        include.count = TRUE,
                        include.tabel =TRUE,
                        
                        margin = NULL,
                        add.margins = NA,
                        
                        include.prop.chisq = FALSE,
                        include.chisq = FALSE,
                        include.fisher = FALSE,
                        # include.mcnemar = FALSE, include.resid = FALSE,
                        # include.sresid = FALSE,include.asresid = FALSE,
                        test = FALSE,
                        include.test=any(c(test, 
                                           include.fisher,
                                           include.chisq, 
                                           include.prop.chisq)),
                      
                        include.correlation = FALSE,
                        include.sensitivity = FALSE,
                        include.diagnostic = include.sensitivity,
                        prevalence = NULL,
                        ...) {
  #- param --------------------------------
  res <- list()
  dim_x <- dimension(x)
  mrgn <-
    get_margins(x,margin, add.margins,
                include.total,
                include.total.columns,
                include.total.sub,
                include.total.rows)
  #- xtabl ------------------------------
  res$xtab <- prepare_output(
    format_xtab(
      x,
      margin=mrgn$prop,
      add.margins =mrgn$add,
      include.count,
      include.percent,
      digits = digits,
      dim_x=dim_x),caption = caption)
  if(include.tabel) Output(res$xtab, output = output, note = note)
  
  #- sig-test -------------------------------
  if (include.test) {
    include.chisq.sumary<- FALSE
   if(!any(include.fisher,include.chisq, include.prop.chisq)){
       dm <- dim(x)
       ldm <-  length(dm)
       if (ldm == 1)  include.prop.chisq<-TRUE
       else if (ldm == 2 & prod((dm - 1)) == 1)  include.fisher <- TRUE
       else if (ldm == 2)  include.chisq <-TRUE
       else include.chisq.sumary <- TRUE
   }
    if (include.prop.chisq ) {
      Text("Funktion  Proportion noch nicht fertig. Daher bitte APA(binom.test(tab_1)) verwenden.")
      res$prop.chisq<- NULL
    } 
     else if ( include.fisher & dim_x==1) {
      fisher_test <- fisher.test(x)
      res$fisher_test <- prepare_output(
        data.frame(
          OR  = stp25rndr::Format2(fisher_test$estimate),
          CI  = stp25rndr::rndr_CI(matrix(fisher_test$conf.int, ncol = 2)),
          p   = stp25rndr::rndr_P(fisher_test$p.value),
          stringsAsFactors = FALSE
        ), caption = "Fisher's Exact Test")
      Output(res$fisher_test, output = output)
    }
    else if( include.chisq & dim_x==2){
      chisq_tests <-  vcd::assocstats(x)
      res$chisq_tests <- prepare_output(
        data.frame(
          Test = rownames(chisq_tests$chisq_tests),
          Chi2 = stp25rndr::Format2(chisq_tests$chisq_tests[, 1], 2),
          df   = stp25rndr::Format2(chisq_tests$chisq_tests[, 2], 0),
          p    = stp25rndr::rndr_P(chisq_tests$chisq_tests[, 3]),
          stringsAsFactors = FALSE
        ),
        caption = "Chi-Squared Test")
      Output(res$chisq_tests, output = output)
    }
    else if( include.chisq.sumary ){
      # hier gibt es noch eine spezifikation
      res$chisq_tests <- APA2(summary(x), output=FALSE)
       Output(res$chisq_tests, output = output)
    }
     else {
       res$chisq_tests <- APA2(summary(x), output=FALSE)
       Output(res$chisq_tests, output = output)
     }
  }
  #- Korrelation und Diagnostic ------------
  if (include.correlation) {
    corr_test <-  vcd::assocstats(x)
    res$corr_test <- prepare_output(data.frame(
      Test = c("Phi-Coefficient",
               "Contingency Coefficient",
               "Cramer's V"),
      r = stp25rndr::Format2(
        c(corr_test$phi,
          corr_test$contingency,
          corr_test$cramer),
        3
      ),
      stringsAsFactors = FALSE
    ),
    caption = "Correlation Test")
    Output(res$corr_test, output = output)
  }
  
  if (include.diagnostic & dim_x == 1) {
    diagnostic.test <-
      prepare_output(Klassifikation(x, prevalence = prevalence)$statistic,
                     caption = caption)
    res$diagnostic.test <- diagnostic.test
    Output(diagnostic.test, output = output)
  }
  invisible(res)
}

 




#' main function for xtabs
#' 
#' @noRd
#' @param x xtabs Tabelle
#' @param margin,add.margins welche Margins
#' @param include.count,include.percent Include
#' @param digits Komastellen
#' @param dim_x was fuer eine Tabelle kommt 
#'
format_xtab <- function(x,
                        margin = NULL,
                        add.margins = NA,
                        include.count = TRUE,
                        include.percent = TRUE,
                        digits = 0,
                        dim_x = dimension(x))  {
  if (dim_x > 0) {
    if (!is.null(add.margins)) {
      f_count <- ftable(addmargins(x, add.margins))
      f_percent <-
        ftable(addmargins(prop.table(x, margin) * 100,
                          add.margins))
    } else{
      f_count <- ftable(x)
      f_percent <-
        ftable(prop.table(x, margin) * 100)
    }
    if (include.count & include.percent) {
      rndr_percent_ftable(f_percent, f_count, digits = digits)
    }
    else if (!include.percent)  {
     stp25tools::fix_to_df(f_count)
    }
    else{
      rndr_percent_ftable(f_percent, digits = digits)
    }
  }
  else{
    f_count <- x
    f_percent <-
      (prop.table(x) * 100)
    r <-  stp25tools::fix_to_df(f_count)
    if (include.count & include.percent) {
      r[1,] <- rndr_percent(f_percent, f_count, digits = digits)
    }
    else if (include.percent)  {
      r[1,] <- rndr_percent(f_percent, digits = digits)
    }
    r
  }
}




#' get_margins
#' 
#' get_margins erlaubt margin="sex"
#'
#' @return list
#'
#' @examples
#' x <- xtabs( ~ education + induced + case, infert)
#'
#' stp25stat:::get_margins(x,
#'             margin = c("education"),
#'             add.margins = NULL )
#'
get_margins <- function(x,
                        margin = NULL,
                        add.margins = NA,
                        include.total = FALSE,
                        include.total.columns = FALSE,
                        include.total.sub = FALSE,
                        include.total.rows = FALSE,
                        use_margin = !any(c(
                          include.total,
                          include.total.columns,
                          include.total.sub,
                          include.total.rows
                        ))) {
  if (use_margin) {
    margin <- position_margin(x, margin)
    
    if (!is.null(add.margins)) {
      if (is.na(add.margins[1])) {
        if (is.null(margin)) {
          add.margins <- NULL
        }
        else {
          add.margins <- setdiff(seq_along(dim(x)), margin)
          if (length(add.margins) == 0)
            add.margins <- NULL
        }
      }
      else {
        add.margins <- position_margin(x, add.margins)
      }
    }
    list( add = add.margins, prop = margin)
  }
  else {
    which_margin(
      dim(x),
      include.total,
      include.total.columns,
      include.total.sub,
      include.total.rows)
  }
}


#' helper
#' 
#' @param mydim dimensionan der Tabelle
#' @param include.total,include.total.columns,include.total.sub,include.total.rows Include
#' @noRd
#' 
which_margin <- function(mydim,
                         include.total = FALSE,
                         include.total.columns = include.total,
                         include.total.sub = include.total,
                         include.total.rows = include.total) {
  mylength <- length(mydim)
  margin <-
    if (mylength == 2) {
      if (include.total)
        list(add = seq_along(mydim), prop = NULL)
      else if (include.total.columns &
               !include.total.rows)
        list(add = mylength, prop = 1)
      else if (include.total.rows &
               !include.total.columns)
        list(add = 1, prop = 2)
      else
        list(add = NULL, prop = NULL)
    }
  else{
    if (include.total)
      list(add = seq_along(mydim), prop = NULL)
    else if (include.total.columns &
             !include.total.rows &
             !include.total.sub)
      list(add = mylength, prop = 1:2)
    else if (include.total.rows  &
             !include.total.columns  &
             !include.total.sub)
      list(add = 1, prop = 2:3)
    else if (include.total.sub &
             !include.total.rows  &
             !include.total.columns)
      list(add = 2, prop = c(1, 3))
    else if (include.total.rows  &
             include.total.sub &
             !include.total.columns)
      list(add = 1:2, prop = 3)
    else if (include.total.rows  &
             include.total.columns  &
             !include.total.sub)
      list(add = c(1, 3), prop = c(2))
    else
      list(add = NULL, prop = NULL)
  }
  margin
}



#' copy from base::sweep
#' 
#' @param x Tabelle
#' @param margin Margin
#' @noRd
position_margin <- function(x, margin) {
  if (is.character(margin)) {
    dn <- dimnames(x)
    if (is.null(dnn <- names(dn)))
      stop("'x' must have named dimnames")
    margin <- match(margin, dnn)
    if (anyNA(margin))
      stop("not all elements of 'margin' are names of dimensions")
  }
  margin
}


#' dimension
#'
#' @param x xtabs-Objekt
#'
#' @return integer
#'
#' @examples
#' #' tab_1<- xtabs(~  case, infert)
#' tab_2x2<- xtabs(~ induced2 + case, infert)
#' tab_3x2<- xtabs(~ induced + case, infert)
#' tab_3x3<- xtabs(~ induced + education, infert)
#' tab_3x3x2<- xtabs(~ induced + education+case, infert)
#' 
#' stp25stat:::dimension(tab_1)
#' stp25stat:::dimension(tab_2x2)
#' stp25stat:::dimension(tab_3x2)
#' stp25stat:::dimension(tab_3x3)
#' stp25stat:::dimension(tab_3x3x2)
dimension <- function(x) {
  dm <- dim(x)
  ldm <-  length(dm)
  if (ldm == 1)  0
  else if (ldm == 2 & prod((dm - 1)) == 1)  1
  else if (ldm == 2)  2  
  else ldm
}
