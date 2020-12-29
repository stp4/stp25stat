#' @title Kaplan Maier
#' 
#' @name Kaplan_Meier
#' @description Summary-Funktion fuer Kaplan-Maier
#'
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
#' @return data.frame
#' @examples
#'
#' ### Magenkarzinom ###
#' require(stpvers)
#' require(survival)
#' mkarz <-stp25aggregate::GetData("C:/Users/wpete/Dropbox/3_Forschung/1 Statistik/BspDaten/SPSS/_Buehl/MKARZ.SAV")
#'
#' # Text("Buehl Seite 553", style=3)
#' #Text("Die Datei mkarz.sav ist ein Datensatz mit 106 Patientan
#' #     mit Magenkarzinom ueber einen Zeitraum von 5 Jahren")
#' #require(stp25data)
#' mkarz %>% Tabelle2(survive[median], status, lkb)
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
#'      percent=TRUE,
#'      #Statistik Anfordern und ander Schreibweise
#'      include=c( time ="time", n.risk ="n.risk",
#'                 n.event ="n.event", surv = "survival",
#'                 lower = "lower 95% CI",upper ="upper 95% CI"),
#'      caption="Kaplan-Meier" )
#'
#' m1 <- Surv(survive, status) ~ lkb
#' res1<- survfit(m1, mkarz)
#' APA2(res1, caption="survfit: Compute a survival Curve for Censored Data (survival)")
#'
#'
#' fit1<- coxph(m1, mkarz)
#' logrank1<- survdiff(m1, mkarz)
#' model_info(logrank1)
#'
#' APA2(logrank1)
#' APA2(coxph(m1,mkarz))
#'
NULL


#' @rdname Kaplan_Meier
#' @description APA.survfit  Mediane berechnen.
#' @export
#' @examples
#'
#' APA.survfit(survfit(Surv(futime, fustat) ~ ecog.ps, data = ovarian)  )
#'
#'
APA.survfit <- function(x, ...) {
  if (length(names(x)) > 11) {
    # Workaround fuer unterschiedlichen Output
    # Mediane berechnen
    mdn <-  stp25tools::fix_to_df(summary(x)$table)
    mdn <- if (ncol(mdn) == 10)
      cbind(mdn[, c(1, 2, 5)], mdn[, c(8:10)])
    else
      cbind(mdn[, c(1, 4)], mdn[, c(7:9)])
    unlist(mdn["median"])
  }
}


#' @rdname Kaplan_Meier
#' @export
#' @examples
#'
#' APA2.survfit(survfit(Surv(futime, fustat) ~ ecog.ps, data = ovarian))
#' APA2.survfit(survfit(Surv(futime, fustat) ~ ecog.ps, data = ovarian), type=2)
#' #ovarian$fustat.r <- 1 - ovarian$fustat
#' #APA2.survfit(survfit(Surv(futime, fustat.r) ~ ecog.ps, data = ovarian))
#'
APA2.survfit <- function(x,
                         caption = "",
                         note = "",
                         output = which_output(),
                         
                         type = 1,
                         # 1ist Mediane 2 Mediane + Tabelle
                         digits = 2,
                         ...) {
  rslt <- extract_survfit(x, digits)
  Output(
    rslt$median,
    caption = paste("Survival Mean ", caption),
    note = note,
    output = output
  )
  
  if (type != 1)
    Output(rslt$table,
           caption = paste("Survival Table ", caption),
           output = output)
  invisible(rslt)
}


#' @rdname Kaplan_Meier
#' @description APA.survdiff Log-Rank-Test  berechnen.
#' @export
#' @examples
#'
#' APA.survdiff(survdiff(Surv(futime, fustat) ~ ecog.ps, data = ovarian))
#'
APA.survdiff <- function(x) {
  df <- length(x$n) - 1
  p.val <- 1 - pchisq(x$chisq, df)
  paste0("Log Rank-Test ",
         stp25rndr::rndr_X(x$chisq, df),
         ", ",
         stp25rndr::rndr_P(p.val))
}


#' @rdname Kaplan_Meier
#' @export
#' @examples
#'
#' APA2.summary.survfit(summary(
#' survfit(Surv(futime, fustat) ~ ecog.ps, data = ovarian),
#' time=c(180, 365)))
#'
#'
#' Summary_Surv(
#'   survfit(Surv(futime, fustat) ~ ecog.ps, data = ovarian),
#'   times = c(180, 365), percent =TRUE
#' )
#'
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
  rslt <- extract_summary_survfit(x, digits, percent, include)
  
  stp25output::Output(rslt, ...)
  invisible(rslt)
}


#' @rdname Kaplan_Meier
#' @export
#' @examples
#'
#' APA2.survdiff(survdiff(Surv(futime, fustat) ~ ecog.ps, data = ovarian))
#'
APA2.survdiff <- function(x,
                          caption = "Test Survival Curve Differences",
                          note = "") {
  Output(extract_survdiff(x),
         caption,
         note = APA.survdiff(x))
}


#' @rdname Kaplan_Meier
#' @export
#'
#' @examples
#' APA2.coxph(coxph(Surv(futime, fustat) ~ ecog.ps, data = ovarian) )
#' APA2.coxph(coxph(Surv(futime, fustat) ~ ecog.ps, data = ovarian),
#'  include.param=TRUE, include.test = FALSE)
#'  Coxph_Test(Surv(futime, fustat) ~ ecog.ps, data = ovarian)
#'
APA2.coxph <- function(x,
                       caption = "",
                       note = "",
                       output = which_output(),
                       include.param = FALSE,
                       include.test = TRUE,
                       ...) {
  prm <- NULL
  rslt <- NULL
  if (include.test) {
    rslt <- extract_coxph_test(x)
    Output(rslt,
           caption = caption,
           note = note,
           output = output)
  }
  if (include.param) {
    prm <- extract_coxph_param(x)
    if (include.test)
      rslt <- list(test = rslt, param = prm)
    else
      rslt <- prm
    
    Output(prm,
           caption = caption,
           note = note,
           output = output)
  }
  invisible(rslt)
}


#' @rdname Kaplan_Meier
#'
#' @description  APA2_coxph_param Kopie von survival:::print.coxph
#' @export
#'
#' @examples
#'
#' APA2_coxph_param(coxph(Surv(futime, fustat) ~ ecog.ps, data = ovarian))
#'
APA2_coxph_param <-
  function (x,
            caption = "",
            note = "",
            include.ci = TRUE,
            include.se = FALSE,
            output = which_output()){
    res <- extract_coxph_param(x)
    
    Output(res,
           caption = caption,
           note = note,
           output = output)
    invisible(res)
  }


#' @rdname Kaplan_Meier
#' @export
#'
Coxph_Test <- function(...,
                       data,
                       names = NA,
                       caption = "Wald test",
                       note = "",
                       output = which_output()) {
  ms <- list(...)
  i <- 0
  res <- NULL
  for (m in ms) {
    i <- 1 + i
    fit_coxph <- coxph(m, data)
    test_coxph <-
      APA2.coxph(
        fit_coxph,
        include.param = FALSE,
        include.test = TRUE,
        output = FALSE
      )$test[c(1, 2, 4)]
    names(test_coxph)[2:3] <-
      paste0(names[i], c("_Test", "_P Value"))
    
    if (is.null(res))
      res <- test_coxph
    else
      res <- cbind(res, test_coxph[2:3])
  }
  
  Output(res,
         caption = caption,
         note = note,
         output = output)
  
  invisible(res)
}



#' @rdname Kaplan_Meier
#'
#' @description  Summary_Surv(): Summary of a Survival Curve survival::survfit- Objekt
#' @param times Zeit
#' @param percent Formatierung
#' @param names_time,cleanup_names Header
#' @export
#'
#' @examples
#'
#' Summary_Surv(
#'   survfit(Surv(futime, fustat) ~ ecog.ps +rx , data = ovarian),
#'   times = c(180, 360),
#'   percent = TRUE
#'   #, cleanup_names=TRUE
#' )
#'
#' summary(survfit(Surv(futime, fustat) ~ ecog.ps +rx , data = ovarian), times = c(180, 360))
#'
Summary_Surv <- function(...) {
  UseMethod("Summary_Surv")
}



#' @rdname Kaplan_Meier
#' @export
#'
Summary_Surv.list <- function(fits,
                              times = 1,
                              digits = 0,
                              percent = FALSE,
                              names_time = c("n (% [95% CI])", "n (est [95% CI])"),
                              caption = "",
                              note = "",
                              output = which_output()) {
  result <- NULL
  for (i in fits) {
    res <- Summary_Surv(
      i,
      times,
      digits,
      percent,
      names_time,
      cleanup_names = FALSE,
      caption,
      note,
      output = FALSE
    )
    
    result <- rbind(result, res)
    
    
  }
  result
}


#' @rdname Kaplan_Meier
#' @export
#'
Summary_Surv.survfit <-
  function(x,
           times = 1,
           digits = 0,
           percent = FALSE,
           names_time = c("n (% [95% CI])", "n (est [95% CI])"),
           cleanup_names = FALSE,
           caption = "",
           note = "",
           output = which_output()) {
    sum_surv <- summary(x, times)
    
    n <- cbind(Source = names(x$strata), N = x$n)
    
    include <- c(
      time = "time",
      n.risk = "n.risk",
      n.event = "n.event",
      surv = "survival",
      std.err = "std.err",
      lower = "lower 95% CI",
      upper = "upper 95% CI"
    )
    vars <-  names(include)
    result <- as.data.frame(sum_surv[vars])
    
    
    if (percent) {
      result$surv <- result$surv * 100
      result$lower <- result$lower * 100
      result$upper <- result$upper * 100
      names_time <- names_time[1]
      prc <- "% "
    }
    else{
      names_time <- names_time[2]
      prc <- " "
    }
    
    result$value <-
      #ifelse(
      # result$n.even == 0,
      # result$n.even,
      paste0(
        result$n.even,
        " (",
        rndr_percent_CI(
          result$surv,
          result$lower,
          result$upper,
          prc = prc,
          digits = digits
        ),
        ")"
      )
    # )
    
    if ("strata" %in% names(sum_surv)) {
      result <- cbind(Source = sum_surv$strata, result)
    }
    
    result <-
      stp25aggregate::Wide(result[order(result$time), c("Source", "time", "value")], time, value)
    
    result <- merge(n, result, by = "Source", all.x = TRUE)
    
    if (cleanup_names) {
      sour_name <- str_split(result$Source, "=")
      result$Source <- sapply(sour_name, "[", 2)
      names(result)[1] <- sour_name[[1]][1]
    }
    
    result <- tibble::as_tibble(result)
    
    names(result)[3:ncol(result)] <-
      paste0(names_time, "_",   names(result)[3:ncol(result)])
    # print(str(result))
    # result <- stp25stat:::dapply1(result,
    #                   function(x) {
    #                     if (is.factor(x))
    #                       as.character(x)
    #                     else
    #                       x
    #                   })
    
    
    Output(result, caption = caption, output = output)
    invisible(result)
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
      c(Format2(x[1], 2),
        x[2],
        stp25rndr::rndr_P(x[3], symbol.leading = c("", "<")))
    else if (length(x == 4))
      c(Format2(x[1], 2),
        paste(x[2], ", ", x[3]),
        stp25rndr::rndr_P(x[4], symbol.leading = c("", "<")))
  }
  else  {
    NULL
  }
}
