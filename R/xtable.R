#' @rdname APA_
#' @description \strong{Kreuztabellen} cross-tabulation, APA_Xtabs, APA2.xtabs :
#' Die Funktion Formatiert xtabs() mit NxM und NxMxO Tabellen.
#'
#' \itemize{
#'  \item{"Haufigkeit/Prozent 1"}{
#'    Die Prozent werden ueber include.percent mit margin erstellt. Der Parameter
#'    add.margins wird automatisch vergeben.
#'    include.total, include.total.columns, include.total.sub, include.total.rows, include.percent,
#'    include.count.
#'    Feineinstellungen erfolgt ueber \code{margin = 2}).
#'    }
#'  \item{"Sensitivitaets Analyse"}{Nur bei 2x" Tabellen ueber test=TRUE}
#'  \item{"Sig.-Tests"}{Bei 2x" Tabellen Fischer sonst Chi-test.
#'  Die Berechnung erfolgt hier mit \link{assocstats}.
#'  Weiter Einstellungen sind Correlationen, Pearson, Kontingentkoeffizient
#'  berechnet alternativ steht auch  der Phi-Coefficient}
#'
#' }
#'
#' @export

APA_Xtabs <-   function(x, ...) {
  UseMethod("APA_Xtabs")
}

# APA ---------------------------------------------------------------------




#' \code{APA.xtabs}: Chi-Quadrat aus Kreuztabellen
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


# APA2 --------------------------------------------------------------------
#' @rdname APA2
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
  #-- Orginal MASS::print.loglm
  ts.array <- rbind(c(x$lrt, x$df,
                      if (x$df > 0L)
                        1 - pchisq(x$lrt, x$df)
                      else
                        1),
                    c(x$pearson, x$df,
                      if (x$df > 0L)
                        1 - pchisq(x$pearson, x$df)
                      else
                        1))
  dimnames(ts.array) <- list(c("Likelihood Ratio",
                               "Pearson"),
                             c("Chi2", "Df", "p.value"))
  res <-
    prepare_output(fix_data_frame2(Test = rownames(ts.array), ts.array), caption, note)

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
APA2.summary.table <- function(x, ...) {
  Text(paste0(
    "Chisq(df=",
    x$parameter,
    ")=",
    Format2(x$statistic, 2),
    ", p=",
    rndr_P(x$p.value)
  ))
}


#' @rdname APA2
#' @export
#'
APA2.table <- function(...) APA2.xtabs(...)


#' @rdname APA2
#'
#' @param caption,note,output,col_names,print_col,labels an Output
#' @param digits Nachkommastellen
#' @param test,type fischer chi usw
#' @param include.total,include.total.columns,include.total.sub,include.total.rows Zeilen Prozenz usw
#' @param include.margins sollen überhaupt Summen (margins) ausgegeben werden die Prozent werden aber wie mit den Margins gerechnet
#' @param include.percent,include.count ausgabe
#' @param margin,add.margins alternative zu include.total
#'
#' @return list(xtab, test)
#' @export
APA2.xtabs  <- function(x,
                        caption = "" ,
                        note = "",
                        output = stp25output::which_output(),
                      #  col_names = NULL,
                       # print_col = NULL,
                        digits = NULL,
                        # test = FALSE,
                        # type = c("0",
                        #          "fischer",
                        #          "odds",
                        #          "sensitivity",
                        #          "chisquare",
                        #          "correlation",
                        #          "r"),
                        include.total = FALSE,
                        include.total.columns = FALSE,
                        include.total.sub = FALSE,
                        include.total.rows = FALSE,
                        include.percent = TRUE,
                        include.count = TRUE,
                        include.margins = TRUE,
                        margin = NA,
                        add.margins = NA,
                        
                        
                        # include.prop.chisq = TRUE,
                        # include.chisq = FALSE,
                         include.correlation = FALSE,
                        # include.fisher = FALSE,
                        # include.mcnemar = FALSE,
                        # include.resid = FALSE,
                        # include.sresid = FALSE,
                        # ## include.asresid = FALSE,
                         include.test=FALSE,
                         include.sensitivity = FALSE,
                         prevalence = NULL,
                        # 
                         # test = include.sensitivity | include.prop.chisq |
                         #  include.chisq | include.fisher | include.mcnemar ,
                        
                     #   labels = NULL,
                        #stp25aggregate::GetLabelOrName()
                        ...) {
  res <- list(xtab=NULL)
#  type <- match.arg(type, several.ok = TRUE)


  res$x_tab   <- prepare_output(
    Xtabelle(
      x,
      include.total,
      include.total.columns,
      include.total.sub,
      include.total.rows,
      include.margins,
      margin,
      add.margins,
      include.count,
      include.percent,
      digits =   if (is.null(digits))
        options()$stp25$apa.style$prozent$digits[1]
      else
        digits
    ),
    caption = caption
  )
  Output(res$x_tab, 
         output = output,
         note=note)
  
 
  #  print(class(res))
  if (include.test) {
    Text("Funktion  include.test  noch nicht fertig")
    
    dimension <- length(dimnames(x))
    
    
    print(dimension)
   # Text(dimension)
    
    if (dimension == 1) {
      # Proportion
      Text("Funktion  Proportion noch nicht fertig")
    } else if (dimension == 2 & length(x) == 4) {
      Text("Funktion  fisher_test noch nicht fertig")
      fisher_test <- fisher.test(x)
      fisher_test <- prepare_output(
        data.frame(
          OR  = stp25rndr::Format2(fisher_test$estimate),
          CI  = stp25rndr::rndr_CI(matrix(fisher_test$conf.int, ncol=2)),
          p   = stp25rndr::rndr_P(fisher_test$p.value),
          stringsAsFactors = FALSE
        ),
        caption = "Fisher's Exact Test"
      )
      Output(res$fisher_test,
             output = output)
    }
    else{
      Text("Funktion  chisq_tests noch nicht fertig")
      res$chisq_tests <-  vcd::assocstats(xtabs)
      res$chisq_tests <- prepare_output(
        data.frame(
          Test = rownames(stat$chisq_tests),
          Chi2 = stp25rndr::Format2(stat$chisq_tests[, 1], 2),
          df   = stp25rndr::Format2(stat$chisq_tests[, 2], 0),
          p    = stp25rndr::rndr_P(stat$chisq_tests[, 3]),
          stringsAsFactors = FALSE
        ),
        caption = "Chi-Squared Test"
      )
      
      Output(res$chisq_tests,
             output = output)
    }
  }
  
 if( include.correlation) {
   corr_test <-  vcd::assocstats(xtabs)
   res$corr_test <- prepare_output(data.frame(
     Test = c("Phi-Coefficient",
              "Contingency Coefficient",
              "Cramer's V"),
     r = stp25rndr::Format2(c(corr_test$phi,
                   corr_test$contingency,
                   corr_test$cramer), 3), stringsAsFactors = FALSE
   ),
   caption = "Correlation Test")
Output(res$corr_test,
       output = output)


 }

  if (include.sensitivity) {
    diagnostic.test <-
      prepare_output(Klassifikation(x,
                                    prevalence = prevalence)$statistic,
                     caption = caption)
    res[["diagnostic.test"]] <- diagnostic.test
    Output(diagnostic.test,
           output = output)
  }
  
     

  invisible(res)
}






#' @rdname APA_
#' @export
APA_Xtabs.glm <- function(x,
                          caption = "",
                          output = stp25output::which_output(),
                          thresh = 0.5,
                          ...) {
  x <- Klassifikation(x, thresh, caption)$xtab
  APA2(x, output = output, ...)

}



#' @rdname APA_
#' @param addNA,exclude,drop.unused.levels An xtabs() default = FALSE
#' @export
#' @examples 
#' 
#'  DF <- GetData(
#' "
#' GoldStandart Schnell.Test Anzahl
#' positiv positiv 124
#' positiv negativ 9
#' negativ positiv 20
#' negativ negativ 305 ",
#' Tabel_Expand = TRUE,
#' id.vars = 1:2,
#' output = FALSE
#' )
#' 
#' 
#' 
#' DF <- transform(
#'   DF,
#'   GoldStandart = factor(GoldStandart, rev(levels(DF$GoldStandart))),
#'   Schnell.Test = factor(Schnell.Test, rev(levels(DF$Schnell.Test)))
#' )
#' 
#' DF<- Label(DF, GoldStandart="Krank Covid-19", Schnell.Test= "Schnell Test GTV8")
#' N<- nrow(DF)
#' 
#' 
#' xtb <- xtabs( ~ Schnell.Test + GoldStandart  , DF)
#' 
#' 
#' APA_Xtabs(xtb)
#' 
#' APA_Xtabs( ~ Schnell.Test  + GoldStandart,
#'            DF,
#'            caption = "2x2 Tabelle zur Destimmung der Kennwerte",
#'            include.percent = FALSE,
#'            include.total=TRUE)
APA_Xtabs.formula <- function(x,
                              data = NULL,
                              caption = "",
                              note="",
                              output = stp25output::which_output(),
                              labels = TRUE,
                              addNA = FALSE,
                              exclude = if (!addNA)  c(NA, NaN) ,
                              drop.unused.levels = FALSE,
                              
                              # include.prop.chisq = TRUE,
                              # include.chisq = FALSE,
                              # include.correlation = FALSE,
                              # include.fisher = FALSE,
                              # include.mcnemar = FALSE,
                              # include.resid = FALSE,
                              # include.sresid = FALSE,
                              # ## include.asresid = FALSE,
                              # include.sensitivity = FALSE,
                              # 
                              # test = include.prop.chisq |
                              #   include.chisq | include.fisher | include.mcnemar,
                              # 
                              ...) {
  fm_x <- x
  x <- stats::xtabs(
    x,
    data,
    addNA = addNA,
    exclude = exclude,
    drop.unused.levels = drop.unused.levels
  )
  
  if (is.logical(labels)) {
    if (labels) {
      dnn <- dimnames(x)
      names(dnn) <-
        stp25aggregate::GetLabelOrName(data[all.vars(fm_x)])
      dimnames(x) <- dnn
    }
  } else if (is.character(labels)) {
    dnn <- dimnames(x)
    names(dnn)[1:length(labels)] <-
      labels
    dimnames(x) <- dnn
  } else if (is.list(labels)) {
    dimnames(x) <- labels
  }
  
  #print(x)
  # noch nicht fertig
  # 
  # CST <- NULL  #chisq.test
  # 
  # if (include.chisq | include.correlation) {
  #   # chisq.test(x ) #Pearson's Chi-squared test with Yates
  #   # chisq.test(x, correct = FALSE) #Pearson's Chi-squared test
  #   # summary(x) # Test for independence of all factors
  #   #
  #   CST <- chisq.test(x, correct = FALSE)
  # }
  # 
  # if (include.resid) {
  #   if (is.null(CST))
  #     CST <- chisq.test(x, correct = FALSE)
  #   resid <-  formatC(CST$observed - CST$expected,
  #                     digits = 2,
  #                     format = "f")
  # }
  # 
  # if (include.sresid) {
  #   if (is.null(CST))
  #     CST <- chisq.test(x, correct = FALSE)
  #   sresid  <-  formatC(CST$residual , digits = 2,
  #                       format = "f")
  # }
  # 
  # if (include.mcnemar) {
  #   McN <- mcnemar.test(x, correct = FALSE)
  # }
  
  cat("\n vor APA")
  APA2.xtabs(x, 
             caption = caption, 
             output = output, 
             note=note,
             ...)
}

#' @rdname APA_
#' @export
APA_Xtabs.xtabs <- function(x, ...) {
  APA2.xtabs(x, ...)
}
# 
# # @rdname APA_
# # @export
# APA_Xtabs.data.frame <- function(data = NULL,
#                                  formula,
#                                  caption = "",
#                                  output = stp25output::which_output(),
#                                  labels = FALSE,
#                                  ...) {
#   APA_Xtabs.formula(formula, data, caption, output, labels, ...)
# }

#' @rdname APA_
#' @export
APA_Xtabs.default <- function(x, ...) {
  Text("Keine Methode fuer ", class(x), " vorhanden.")
}




# Test Chi und Fisher ---------------------------------------------------------------------

# 
# fisher_Statistik <- function(x, digits = 2) {
#   fisher <- fisher.test(x)
#   
#   
#   res <- data.frame(
#     OR  = Format2(fisher$estimate, digits),
#     CI  = rndr_CI(as.vector(fisher$conf.int)),
#     p   = rndr_P(fisher$p.value)
#   )
#   names(res) <- c("OR", "95% CI" , "p-Value")
# 
#   res
# }

# chisq_Statistik <- function(xtabs, type,
#                             x = summary(xtabs),
#                             dins = length(dimnames(xtabs))
#                             ) {
#   stat <- vcd::assocstats(xtabs)
#   ans<- list()
# 
#   if ("chisquare" %in% type)
#    {
# 
#     ans[["Chisq"]] <- data.frame(
#               Test = rownames(stat$chisq_tests),
#               Chi2 = Format2(stat$chisq_tests[, 1], 2),
#               df   = Format2(stat$chisq_tests[, 2], 0),
#               p    = rndr_P(stat$chisq_tests[, 3]))}
# 
#  if (any(c("correlation", "r") %in% type))
#   ans[["Correlation"]] <- data.frame(
#         Test = c("Phi-Coefficient",
#                  "Contingency Coefficient",
#                  "Cramer's V"),
#         r = Format2(c(stat$phi,
#                       stat$contingency,
#                       stat$cramer), 3)
#       )
#   ans
# }





# interne Functions --------------------------------------



which_margin <- function(mydim, #x,
                         include.total = FALSE,
                         include.total.columns = include.total,
                         include.total.sub = include.total,
                         include.total.rows = include.total) {
  #mydim <- dim(x)
  
  # cat("\n in which_margin\n")
  # print(paste(include.total,
  #             include.total.columns,
  #             include.total.sub,
  #             include.total.rows))
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


# main function for xtabs
Xtabelle <- function(x,
                     include.total = FALSE,
                     include.total.columns = FALSE,
                     include.total.sub = FALSE,
                     include.total.rows = FALSE,
                     include.margins=TRUE,
                     margin = NA,
                     add.margins = NA,
                     count = TRUE,
                     percent = TRUE,

                     digits = 0)  {
 # cat("\n in Xtabelle dim = ")
 # print(dim(x))
 # cat("\n Input: ")
  # print(paste(include.total,
  #             include.total.columns,
  #             include.total.sub,
  #             include.total.rows))
  # cat("\n")
  mrgn <-
    which_margin(dim(x),
                 include.total,
                 include.total.columns,
                 include.total.sub,
                 include.total.rows)
 
  if (!is.na(margin))
    mrgn$prop <- margin
  if (!is.na(add.margins))
    mrgn$add <- add.margins


  if (include.margins & (!is.null(mrgn$add))) {
    f_count <- ftable(addmargins(x, mrgn$add))
    f_percent <-
      ftable(addmargins(prop.table(x, mrgn$prop) * 100, mrgn$add))
  } else{
    f_count <- ftable(x)
    f_percent <-
      ftable(prop.table(x, mrgn$prop) * 100)
  }


  if (count & percent) {

    rndr_percent_ftable(f_percent, f_count, digits=digits)
  }
  else if (!percent)  {
    stp25output::fix_to_data_frame(f_count)
    }
  else{
   rndr_percent_ftable(f_percent, digits=digits)
  }

}




# test_xtabl_NxMxO <- function(x, type, output, ...) {
#   list(likelihood.test = NULL)
# }

# 
# test_xtabl_NxM <- function (x, type, output, ...) {
#   ans <- chisq_Statistik(x, type = type)
#   for (i in names(ans))
#     Output(
#       ans[[i]],
#       caption = i,
#       fix_colnames = FALSE,
#       output = output
#     )
#   ans #liste mit Test
# }

# test_xtabl_2x2 <- function(x, type, output, lvs = c("+", "-"), ...) {
#  
#   res <- list(fisher=NULL, sensitivity=NULL)
#   # type kann mehr sein  #-- c("fischer", "odds","sensitivity", "chisquare" )
#   if ("fischer" %in%  type) {
#  
#     x_fisher <- prepare_output(fisher_Statistik(x),
#                                caption = "Fisher's Exact Test ")
# 
#     
#     Output(x_fisher,  output = output)
#     res$fisher <- x_fisher
#  
#   }
#   if ("sensitivity" %in% type) {
#   
#     x_diagnostic <- prepare_output(Klassifikation.xtabs(x, lvs),
#                                    caption = "Sensitivity Test")
#    
#     Output(x_diagnostic$statistic, output = output)
# 
#     res$sensitivity <- x_diagnostic
#   }
#   res
# }

