#' @title Kaplan Maier
#' @name Kaplan_Meier
#' @description Summary -Funktion für Kaplan-Maier
#' Beispiel unter
#' (see  \link[stp25data]{hkarz} )
#'
#' \subsection{Mediane}{
#'    \code{m <- Surv(Time, status) ~ 1}
#'
#'    \code{res <- survfit(m, DF)}
#'
#'    \code{ APA2(res, caption="Kaplan-Meier")}
#'    }
#' \subsection{survival rate at a certain time }{
#' \code{APA2(summary(res, times=c(5, 10, 15)))}
#' }
#'
#'
#' @return A \code{\link[tibble]{tibble}} with counted tagged NA values.
#' @examples
#'
#' #'### Magenkarzinom ###
#' require(stp25output)
#' mkarz <-stp25aggregate::GetData("C:/Users/wpete/Dropbox/3_Forschung/1 Statistik/BspDaten/SPSS/_Buehl/MKARZ.SAV")
#'
#' # Text("Buehl Seite 553", style=3)
#' #Text("Die Datei mkarz.sav ist ein Datensatz mit 106 Patientan
#' #     mit Magenkarzinom über einen Zeitraum von 5 Jahren")
#' #require(stp25data)
#' mkarz %>% Tabelle2(survive="median", status, lkb)
#' mkarz$status<- ifelse(mkarz$status=="tot", 1, 0)
#'
#' #Head("Kaplan-Meier estimator without grouping", style=3)
#' #Text("
#' #     m0 <- Surv(survive, status) ~ 1
#' #     res0<- survfit(m0, mkarz)
#' #
#' #     ")
#' m0 <- Surv(survive, status) ~ 1
#' res0<- survfit(m0, mkarz)
#' APA2(res0)
#' #windows(8,4)
#' #par(mfrow=c(1,2))
#' #plot( res0 , ylab="Hazard", mark.time = T)
#' #plot( res0, fun="cumhaz",  ylab="Cumulative Hazard" )
#' #SaveData(caption="plot: mkarz")
#'
#'
#' APA2(summary(res0, times= c(5, 10,12,17, 20, 60)),
#' percent=TRUE,
#' #Statistik Anfordern und ander Schreibweise
#' include=c( time ="time", n.risk ="n.risk", 
#'            n.event ="n.event", surv = "survival",
#'            lower = "lower 95% CI",upper ="upper 95% CI"),
#' caption="Kaplan-Meier" )
#' #'
#' m1 <- Surv(survive, status) ~ lkb
#' res1<- survfit(m1, mkarz)
#' fit1<- coxph(m1, mkarz)
#' logrank1<- survdiff(m1, mkarz)
#' model_info(logrank1)
#' APA2(res1, caption="Kaplan-Meier")
#' APA2(logrank1)
#' APA2(coxph(m1,mkarz))
#'
NULL



#' @rdname APA
#' @description APA.survfit  Mediane berechnen.
#' @export
APA.survfit <- function(x, ...) {
  if (length(names(x)) > 11) {
    # Workaround fuer unterschiedlichen Output
    # Mediane berechnen
    mdn <- fix_to_data_frame(summary(x)$table)
    mdn <- if (ncol(mdn) == 10)
      cbind(mdn[, c(1, 2, 5)], mdn[, c(8:10)])
    else
      cbind(mdn[, c(1, 4)], mdn[, c(7:9)])
    unlist(mdn["median"])
  }
}


#' @rdname APA
#' @description APA.survdiff Log-Rank-Test  berechnen.
#' @export
APA.survdiff <- function(x) {
  df <- length(x$n) - 1
  p.val <- 1 - pchisq(x$chisq, df)
  paste0("Log Rank-Test ",
         rndr_X(x$chisq, df), ", ",
         rndr_P(p.val))
}


# stp25data::mkarz
#' @rdname APA2
#' @export
APA2.summary.survfit <- function(x,
                                 digits = NULL,
                                 # an fix_format()
                                 percent = FALSE,
                                 include = c(
                                   time = "time",
                                   n.risk = "n.risk",
                                   n.event = "n.event",
                                   surv = "survival",
                                   std.err = "std.err",
                                   lower = "lower 95% CI",
                                   upper = "upper 95% CI"
                                 ),
                                 ...) {
  #cat("\nin APA2.summary.survfit")
  #fit <- survfit((time=time,event=death)~group)
  #surv.prob <- summary(fit,time=c(0,10,20,30))$surv
  if (is.null(names(include))) {
    vars <- vars_names <- include
  }
  else{
    vars <-  names(include)
    vars_names <- as.character(include)
  }

  result <- as.data.frame(x[vars])
  if (percent) {
    result$surv <- result$surv * 100
    result$lower <- result$lower * 100
    result$upper <- result$upper * 100
  }

  colnames(result) <- vars_names
  result <-  fix_format(result,  exclude = 1:3, digits = digits)
  if ("strata" %in% names(x))
    result <- cbind(Source = x$strata,
                    result)

  Output(result, ...)
  invisible(result)
}

#' @rdname APA2
#' @export
APA2.survfit <- function(x,
                         caption = "NULL",
                         note = "",
                         type = 1,
                         # 1ist Mediane 2 Mediane + Tabelle
                         digits = 2,
                         ...) {
  if (length(names(x)) > 11) {
    # Workaround fuer unterschiedlichen Output
    # Mediane berechnen
    mdn <- fix_to_data_frame(summary(x)$table)

    if (!is.null(digits)) {
      cat(names(mdn))
      mdn["median"] <- round(mdn["median"], digits)
      mdn["0.95LCL"] <- round(mdn["0.95LCL"], digits)
      mdn["0.95UCL"] <- round(mdn["0.95UCL"], digits)
    }
    mdn <- if (ncol(mdn) == 10)
      cbind(mdn[, c(1, 2, 5)], mdn[, c(8:10)],
            Mean = rndr_mean(mdn[, 6], mdn[, 7]))
    else
      cbind(mdn[, c(1, 4)], mdn[, c(7:9)],
            Mean = rndr_mean(mdn[, 5], mdn[, 6]))
    Output(mdn,
           caption = paste("Survival Mean ", caption),
           note = note)

    if (type != 1)
      Output(with(
        x,
        data.frame(
          Time = time,
          Risk = n.risk,
          Event = n.event,
          Survival = Format2(surv, 2),
          SE = Format2(std.err, 2)
        )
      ), caption = paste("Survival Table ", caption))
  }
  else{
    cat(
      "Hier besser folgend kodieren: 0=alive, 1=dead. Other choices are TRUE/FALSE (TRUE = death)."
    )
    return(x)
  }
}




#' @rdname APA2
#' @export
APA2.survdiff <- function(x,
                          caption = "Test Survival Curve Differences",
                          note = "") {
  df <- length(x$n) - 1
  p.val <- 1 - pchisq(x$chisq, df)

  Output(
    data.frame(
      Source = c(names(x$n), "Overall"),
      N = as.integer(c(x$n, sum(x$n))),
      Observed = as.integer(c(x$obs, sum(x$obs))),
      Expected = c(round(x$exp, 1), NA)
    ),
    caption,
    note = APA.survdiff(x)
  )
}

#' @rdname APA2
#' @export
APA2.coxph<- function(x,
                      caption="",
                      note="",
                      ...){
  sfit<- summary(x)  #survival:::summary.coxph
  res<- rbind(
    "Wald test"=  fix_format_p(sfit$waldtest) ,
    "Score (logrank) test"= fix_format_p(sfit$sctest),
    "Likelihood ratio test"= fix_format_p(sfit$logtest ) ,

    "Concordance"= c(paste0(Format2(sfit$concordance[1],2)
                      , " (SE=", Format2(sfit$concordance[1],2),")"), NA, NA),
    "Rsquare"= c(Format2(sfit$rsq[1],2), NA, NA),
    "AIC"= c(Format2(AIC(x),2), NA, NA),
    "BIC"= c(Format2(BIC(x),2), NA, NA)
  )
  
  
  Output(fix_to_data_frame(res), caption, note)
}

#'  fix_irgendwas fix_format_p ist nur hier in Verwendung
#'  fix_format_p sucht automatisch nach den p-Werten die meist an der Letzten stelle sind
#'  und gibt einen Vector-String mit der LAenge drei aus.
#'  Nicht zu verwechseln mit \code{rndr_P()}
#'
#'  fix_format_p Input(F, df,und p)  Output: (test, df, p.value)
#'
#'

fix_format_p <- function(x,
                         df1 = NULL,
                         df2 = null,
                         p = NULL) {
 ## stp25rndr::rndr_P( p.value, symbol.leading = c("", "<"))
  
  if (is.vector(x)) {
    if (length(x == 3))
      c(Format2(x[1], 2), x[2], stp25rndr::rndr_P(x[3], symbol.leading = c("", "<")))
    else if (length(x == 4))
      c(Format2(x[1], 2), paste(x[2], ", ", x[3]), stp25rndr::rndr_P(x[4], symbol.leading = c("", "<")))
  }
  else  {NULL}
}
