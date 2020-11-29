

#' Table for Survival Objects
#'
#' @param ... survfit, survdiff, coxph
#' @param caption heading
#'
#' @return data.frame or list with data.frames
#' @export
#'
#' @examples
#' 
#' # require(stpvers)
#' require(survival)
#'  
#' fit0 <- survfit(Surv(futime, fustat) ~ 1, data = ovarian)
#' fit1 <- survfit(Surv(futime, fustat) ~ rx, data = ovarian)
#' fit2 <- survfit(Surv(futime, fustat) ~ rx + resid.ds, data = ovarian)
#' #summary(fit1)
#' Tbll(fit0)
#' Tbll(fit1)
#' Tbll(fit2)
#' 
#' Tbll(fit1, include.survival = TRUE)
#' 
#' survdiff(Surv(futime, fustat) ~ rx, data = ovarian) %>% Tbll()
#' 
#' coxph(Surv(futime, fustat) ~ rx, data = ovarian) %>% Tbll()
#' 
#' cfit1<-coxph(Surv(futime, fustat) ~ rx +resid.ds, data = ovarian)
#' cfit2 <-coxph(Surv(futime, fustat) ~ rx, data = ovarian)
#' 
#' Tbll(cfit1, cfit2, include.param=FALSE)
#' 
#' 
Tbll_surv <- function(..., caption="") {
  tbll_extract(..., caption)
}






#' @param include.survival Tabelle mit allen Ueberlebenszeiten
#'
#' @rdname Tbll_surv
tbll_extract.survfit <- function(x,
                                 caption = "Median", 
                                 include.survival=FALSE,
                                 digits = 2) {
  rslt <- extract_survfit(x, digits)
  rslt$median <- prepare_output(rslt$median, caption=caption, note="")
  
  if(include.survival) rslt
  else  rslt$median
}


#' @param include.se Standardfehler
#' @param include.ci  Konfidenzintervall
#'
#' @rdname Tbll_surv
tbll_extract.summary.survfit <- function(x,
                                         caption="Summary of a Survival Curve",
                                         digits = NULL,
                                         include.se=FALSE,
                                         include.ci=TRUE) {
  include <- c(time = "time", n.risk = "n.risk", n.event = "n.event", surv = "survival")
  if(include.se) include<- append(include, c(std.err = "std.err"))
  if(include.ci) include<- append(include, c(lower = "lower 95% CI", upper = "upper 95% CI"))
  rslt <- extract_summary_survfit(x, digits=digits, percent=FALSE, include=include)
  prepare_output(rslt, caption=caption, note="")
}

#' @rdname Tbll_surv
tbll_extract.survdiff <- function(x,
                                  caption = "Test Survival Curve Differences") {
  prepare_output( extract_survdiff(x),
                  caption=caption,
                  note=APA(x)
  )
}


#' @param include.param Regrssionstabelle
#' @param include.test  Wald-Test
#'
#' @rdname Tbll_surv
tbll_extract.coxph <- function(x,
                               caption = "Cox proportional hazards regression model",
                               include.param = FALSE,
                               include.test = TRUE) {
  rslt <- NULL
  prm <- NULL
  if (include.test) {
    rslt <- extract_coxph_test(x)
  }
  if (include.param) {
    prm <- prepare_output(extract_coxph_param(x), caption=caption)
    if(include.test) rslt<- list(test = rslt, param = prm)
    else rslt <- prm
  }
  rslt
}
