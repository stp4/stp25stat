#' @title Kaplan Maier
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
    mdn <- stp25output::fix_to_data_frame(summary(x)$table)
    mdn <- if (ncol(mdn) == 10)
      cbind(mdn[, c(1, 2, 5)], mdn[, c(8:10)])
    else
      cbind(mdn[, c(1, 4)], mdn[, c(7:9)])
    unlist(mdn["median"])
  }
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
         stp25rndr::rndr_X(x$chisq, df), ", ",
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
  result <-  stp25output::fix_format(result,  exclude = 1:3, digits = digits)
  if ("strata" %in% names(x))
    result <- cbind(Source = x$strata,
                    result)
  
  stp25output::Output(result, ...)
  invisible(result)
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
                         caption = "NULL", output=which_output(),
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
           note = note,
           output=output)
    
    
    
    tab<- with(
      x,
      data.frame(
        Time = time,
        Risk = n.risk,
        Event = n.event,
        Survival = Format2(surv, 2),
        SE = Format2(std.err, 2)
      )
    )
    
    if (type != 1)
      Output(tab, 
             caption = paste("Survival Table ", caption),
             output=output)
  }
  else{
    cat(
      "Hier besser folgend kodieren: 0=alive, 1=dead. Other choices are TRUE/FALSE (TRUE = death)."
    )
    return(x)
  }
  
  invisible(list(median=mdn,
                 table=tab))
  
  
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
  tst <- NULL
  prm <- NULL
  
  if (include.test) {
    sfit <- summary(x)  #survival:::summary.coxph
    tst <- rbind(
      "Wald test" =  fix_format_p(sfit$waldtest) ,
      "Score (logrank) test" = fix_format_p(sfit$sctest),
      "Likelihood ratio test" = fix_format_p(sfit$logtest) ,
      "Concordance" = c(paste0(Format2(sfit$concordance[1], 2), " (SE=", Format2(sfit$concordance[1], 2),")"), NA, NA),
      "Rsquare" = c(Format2(sfit$rsq[1], 2), NA, NA),
      "AIC" = c(Format2(AIC(x), 2), NA, NA),
      "BIC" = c(Format2(BIC(x), 2), NA, NA)
    )
    colnames(tst)[3]<- "p.value" 
  #  print(tst)
    tst <- fix_to_data_frame(tst)
    
    Output(tst,
           caption = caption,
           note = note,
           output = output)
  }
  if (include.param) {
    prm <- APA2_coxph_param(x, output = FALSE)
    Output(prm,
           caption = caption,
           note = note,
           output = output)
  }
  invisible(list(test = tst, param = prm))
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
            include.ci=TRUE,
            include.se=FALSE,
            output = which_output())
  {
    coef <- x$coefficients
    se <- sqrt(diag(x$var))
    
    cnames <- names(coef)
    if (is.null(coef) | is.null(se))
      stop("Input is not valid")
    if (is.null(x$naive.var)) {
      tmp <- cbind(coef,
                   exp(coef),
                   se,
                   coef / se,
                   pchisq((coef / se) ^ 2,
                          1, lower.tail = FALSE))
      dimnames(tmp) <- list(names(coef),
                            c("coef", "HR", "se(coef)", "z", "p"))
    }
    else {
      nse <- sqrt(diag(x$naive.var))
      tmp <-
        cbind(coef,
              exp(coef),
             # nse,
              se,
              coef / se,
              pchisq((coef / se) ^ 2,
                     1, lower.tail = FALSE))
      dimnames(tmp) <- list(names(coef),
                            c("coef", "HR", #"se(coef)", 
                              "robust se", 
                              "z", "p"))
    }
    
    res <- fix_format(tmp, digits = c(2, 2, 2, 2, 3))
    res <- cbind(Source = cnames, res)
   # names(res)[3] <- "HR"
    if(!include.se) res<- res[-4]
    if(include.ci){
      ci<- stp25rndr::rndr_CI(exp(confint(x)))
      res<- cbind(res[1:3], CI.HR=ci, res[4:ncol(res)])
      
    }
    
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
    names(test_coxph)[2:3] <- paste0(names[i], c("_Test", "_P Value"))
    
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
      stp25aggregate::Wide(
        result[order(result$time), c("Source", "time", "value")], time, value)
    
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
      c(Format2(x[1], 2), x[2], stp25rndr::rndr_P(x[3], symbol.leading = c("", "<")))
    else if (length(x == 4))
      c(Format2(x[1], 2), paste(x[2], ", ", x[3]), stp25rndr::rndr_P(x[4], symbol.leading = c("", "<")))
  }
  else  {NULL}
}

#' rendere percent and CIs
#'
#' @param x,low,upr Wert, Lower und Upper
#' @param digits Nachkommastellen
#' @param prc,sep,sep_1,sep_2 Formatierungs-Symbole
#' @return string

rndr_percent_CI <-
  function(x, low, upr,
           digits = 0,
           prc="% ", sep = ", ", sep_1 = "[", sep_2 = "]") {
    paste0(
      stp25rndr::Format2(x, digits),
      prc,
      sep_1,
      stp25rndr::Format2(low, digits),
      sep,
      stp25rndr::Format2(upr, digits),
      sep_2
    )
  }
