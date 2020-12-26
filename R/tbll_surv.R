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



#' @rdname Tbll_surv
#' @description  Univariate Cox regression
#'
#'  stolen from www.sthda.com/english/wiki/cox-proportional-hazards-model
#' 
#'  regression coefficients = the hazard (risk) 
#'   
#'  Hazard ratios = effect size of covariates exp(coef) 
#' 
#' 
#' @param ... an prepare_data2
#' @param note  an Output
#'
#' @return data.frame
#' @export
#'
#' @examples
#' 
#' 
#'   tbll_coxph_uni(lung, age, sex, 
#' ph.karno, ph.ecog, wt.loss, 
#' by = ~ time + status)
#' 
#' 
#' # The variables sex, age and ph.ecog have highly statistically significant 
#' # coefficients, while the coefficient for ph.karno is not significant.
#' # 
#' # age and ph.ecog have positive beta coefficients, while sex has a negative 
#' # coefficient. Thus, older age and higher ph.ecog are associated with poorer 
#' # survival, whereas being female (sex=2) is associated with better survival.
#' 
#' 
tbll_coxph_uni <-
  function(x, ...,
           caption = "Univariate Cox regression",
           note = "Regression beta coefficients, effect sizes (given as hazard ratios).
           Each factor is assessed through separate univariate Cox regressions.",
           
           include.se = FALSE,
           include.ci = TRUE,
           include.z.test = FALSE,
           include.wald.test = TRUE,
           digits = 2) {
 
    if (inherits(x, "formula")) {
      vrs <- all.vars(x)
      covariates <-   vrs[-c(1:2)]
      Surv_vars <-
        paste0('Surv(', vrs[1], ", ", vrs[2], ')~')
      X <- list(data = list(...)[[1]])
    }
    else{
      X <-
        stp25formula::prepare_data2(x, ...) # c("age", "sex",  "ph.karno", "ph.ecog", "wt.loss")
      covariates <-   X$measure.vars
      Surv_vars <-
        paste0('Surv(', paste0(X$group.vars, collapse = ', '), ')~')
    }
    
    univ_formulas <- sapply(covariates,
                            function(x)
                              as.formula(paste0(Surv_vars, x)))
    
    univ_models <-
      lapply(univ_formulas, function(x) {
        coxph(x, data = X$data)
      })
    
    univ_results <- lapply(univ_models,
                           function(x) {
                             x<- summary(x)
                             rslt <- tbll_extract.summary.coxph(
                               x,
                               include.test =  include.z.test,
                               include.se = include.se,
                               include.ci = include.ci,
                               digits = digits
                             )
                             
                             if (include.wald.test)
                               dplyr::bind_cols(
                                 rslt,
                                 wald.test =
                                   stp25rndr::Format2(x$wald["test"], digits = 2),
                                 p.value =
                                   stp25rndr::rndr_P(x$wald["pvalue"], FALSE)
                               )
                             else rslt
                           })
    
    univ_results<- do.call(rbind.data.frame, univ_results)
    
    prepare_output(
      univ_results,
      caption = caption,
      note = note
    )
  }



#' @param include.survival Tabelle mit allen Ueberlebenszeiten
#' @param ...  an summary.surfit times, censored = FALSE, scale = 1, extend=FALSE
#'
#' @rdname Tbll_surv
tbll_extract.survfit <- function(x,
                                 caption = "Median", 
                                 include.survival=FALSE,
                                 include.se=FALSE,
                                 include.ci=TRUE,
                                 digits = 2,
                                 ...) {
  rslt <- extract_survfit(x, digits=digits, ...)

  rslt$median <- prepare_output(rslt$median, caption=caption, note="")
 
  if( !include.se ) rslt$table <- 
    rslt$table[-(ncol(rslt$table)-2)]
   if( !include.ci )
  rslt$table <- 
    rslt$table[-c((ncol(rslt$table)-1), ncol(rslt$table) )]
  
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



#' summary surv
#' 
#' extract_survfit
#'
#' @param x surv
#' @param digits 
#' @param ...  an summary.surfit times, censored = FALSE, scale = 1, extend=FALSE
#' 
#' 
#' @noRd
#' 
extract_survfit <- function(x,
                            digits = 2,
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
  
  xs<- summary(x, ...)
  mdn <-xs$table
  if (is.null(names(mdn))) {
    mdn <- as.matrix(mdn)
    mdn  <-   data.frame(
      Source =  rownames(mdn), #   sapply(strsplit(rownames(mdn), "="), "[", 2),
      median = stp25rndr::Format2(mdn[, "median"], digits),
      low.ci = stp25rndr::Format2(mdn[, "0.95LCL"], digits),
      up.ci = stp25rndr::Format2(mdn[, "0.95UCL"], digits),
      Mean = stp25rndr::rndr_mean(mdn[, "*rmean"], mdn[, "*se(rmean)"], digits =
                                    digits)
    )
    
    tab <- extract_summary_survfit(xs, digits, percent, include)
    
  }
  else
  {
    mdn  <-   data.frame(
      Source = "Null",
      median = stp25rndr::Format2(mdn[["median"]], digits),
      low.ci = stp25rndr::Format2(mdn[["0.95LCL"]], digits),
      up.ci = stp25rndr::Format2(mdn[["0.95UCL"]], digits),
      Mean = stp25rndr::rndr_mean(mdn[["*rmean"]], mdn[["*se(rmean)"]], digits =
                                    digits)
    )
    
    tab <-  extract_summary_survfit(xs, digits, percent, include)
    
  }
  list(median = mdn, table = tab)
}



extract_summary_survfit <- function(x,
                                    digits = NULL,
                                    percent = FALSE,
                                    include = c(
                                      time = "time",
                                      n.risk = "n.risk",
                                      n.event = "n.event",
                                      surv = "survival",
                                      std.err = "std.err",
                                      lower = "lower 95% CI",
                                      upper = "upper 95% CI"
                                    )) {
  if (is.null(names(include))) {
    vars <- vars_names <- include
  }
  else{
    vars <-  names(include)
    vars_names <- as.character(include)
  }
  rslt <- as.data.frame(x[vars])
  
  if (percent) {
    rslt$surv  <- rslt$surv * 100
    rslt$lower <- rslt$lower * 100
    rslt$upper <- rslt$upper * 100
  }
  
  colnames(rslt) <- vars_names
  rslt <-
    stp25output::fix_format(rslt, exclude = 1:3, digits = digits)
  if ("strata" %in% names(x))
    rslt <- cbind(Source = x$strata, rslt)
  
  rslt
}



extract_survdiff <- function(x) {
  data.frame(
    Source = c(names(x$n), "Overall"),
    N = as.integer(c(x$n, sum(x$n))),
    Observed = as.integer(c(x$obs, sum(x$obs))),
    Expected = c(round(x$exp, 1), NA)
  )
}



extract_coxph_test <- function(x) {
  sfit <- summary(x)
  Concordance <- paste0(
    stp25rndr::Format2(sfit$concordance[1], 2),
    " (SE=",
    stp25rndr::Format2(sfit$concordance[2], 2),
    ")"
  )
  tst <- cbind(data.frame(
    Source = c(
      "Wald test",
      "Score (logrank test)",
      "Likelihood ratio test",
      "Rsquare","AIC","BIC")
  ),
  rbind(
    sfit$waldtest,
    sfit$sctest,
    sfit$logtest ,
    c(sfit$rsq[1], NA, NA),
    c(AIC(x),  NA, NA),
    c(BIC(x), NA, NA)
  ))
  
  rbind(fix_format(tst), c("Concordance", Concordance, NA, NA))
}



#' @rdname Tbll_surv
tbll_extract.summary.coxph <-
  function(x,
           include.se = FALSE,
           include.ci = TRUE,
           include.test = TRUE,
           digits = 2)
  {
    rslt <- data.frame(
      Source = row.names(x$coefficients),
      beta = stp25rndr::Format2(x$coefficients[, 1], digits = digits)
    )
    if (include.se)
      rslt <-
        cbind(rslt, se = stp25rndr::Format2(x$coefficients[, 3], digits = digits))
    
    rslt <-
      cbind(rslt, HR = stp25rndr::Format2(x$coefficients[, 2], digits = 2))
    
    if (include.ci)
      rslt <-
      cbind(rslt, CI = stp25rndr::rndr_CI(x$conf.int[, c("lower .95", "upper .95")]))
    if (include.test)
      rslt <- cbind(
        rslt,
        z.test = stp25rndr::Format2(x$coefficients[, 4], digits = digits),
        p.value = stp25rndr::rndr_P(x$coefficients[, 5], FALSE)
      )
    
    tibble::as_tibble(rslt)
  }


extract_coxph_param <-
  function (x,
            include.se = FALSE,
            include.ci = TRUE,
            include.test = TRUE,
            digits = 2) {
    tbll_extract.summary.coxph(
      summary(x),
      include.se = include.se,
      include.ci = include.ci,
      include.test = include.test,
      digits = digits
    )
    
  }







# tbll_extract.summary.coxph <-
#   function(x,
#            digits = 2)
#   {
#     p.value <-
#       stp25rndr::rndr_P(x$coef[, 5], FALSE)
#     
#     
#     z.test <-
#       stp25rndr::Format2(x$coef[, 4], digits = digits)
#     beta <-
#       signif(x$coef[, 1], digits = digits)
#     #coeficient beta
#     HR <-
#       signif(x$coef[, 2], digits = digits)
#     #exp(beta)
#     HR.confint.lower <-
#       signif(x$conf.int[, "lower .95"], digits)
#     
#     HR.confint.upper <-
#       signif(x$conf.int[, "upper .95"], digits)
#     HR <- paste0(HR, " (",
#                  HR.confint.lower, "-", HR.confint.upper, ")")
#     
#     res <-
#       data.frame(
#         covariates = names(beta),
#         beta = beta,
#         "HR" = HR,
#         z.test = z.test,
#         p.value = p.value
#       )
#     #    names(res) <- c("beta", "HR (95% CI for HR)", "wald.test",         "p.value")
#     res
#     
#   }



# extract_coxph_param <-
#   function (x,
#             include.se = FALSE,
#             include.ci = TRUE) {
#     coef <- x$coefficients
#     se <- sqrt(diag(x$var))
#     
#     cnames <- names(coef)
#     if (is.null(coef) | is.null(se))
#       stop("Input is not valid")
#     
#     if (is.null(x$naive.var)) {
#       tmp <- cbind(coef,
#                    exp(coef),
#                    se,
#                    coef / se,
#                    pchisq((coef / se) ^ 2,
#                           1, lower.tail = FALSE))
#       dimnames(tmp) <- list(names(coef),
#                             c("coef", "HR", "se(coef)", "z", "p"))
#     }
#     else {
#       nse <- sqrt(diag(x$naive.var))
#       tmp <-
#         cbind(coef,
#               exp(coef),
#               se,
#               coef / se,
#               pchisq((coef / se) ^ 2,
#                      1, lower.tail = FALSE))
#       dimnames(tmp) <- list(names(coef),
#                             c("coef", "HR", "robust se", "z", "p"))
#     }
#     
#     res <- fix_format(tmp, digits = c(2, 2, 2, 2, 3))
#     res <- cbind(Source = cnames, res)
#     
#     if (!include.se)
#       res <- res[-4]
#     if (include.ci) {
#       ci <- stp25rndr::rndr_CI(exp(confint(x)))
#       res <- cbind(res[1:3], CI.HR = ci, res[4:ncol(res)])
#     }
#     
#     res
#   }
# 
# tbll_coxph_uni <-
#   function(..., 
#            caption = "Univariate Cox regression",
#            note = "Regression beta coefficients, effect sizes (given as hazard ratios). 
#            Each factor is assessed through separate univariate Cox regressions.") {
#     X <-
#       stp25formula::prepare_data2(...) # c("age", "sex",  "ph.karno", "ph.ecog", "wt.loss")
#     covariates <-   X$measure.vars
#     Surv_vars <-
#       paste0('Surv(', paste0(X$group.vars, collapse = ', '), ')~')
#     univ_formulas <- sapply(covariates,
#                             function(x)
#                               as.formula(paste0(Surv_vars, x)))
#     
#     univ_models <-
#       lapply(univ_formulas, function(x) {
#         coxph(x, data = X$data)
#       })
#     
#     univ_results <- lapply(univ_models,
#                            function(x) {
#                              x <- summary(x)
#                              
#                              # summary(x)$coef
#                              #       coef  exp(coef)  se(coef)   z      Pr(>|z|)
#                              #  sex  -0.53 0.58       0.16       -3.17  0.0014  
#                              #
#                              # summary(x)$conf.int
#                              #     exp(coef) exp(-coef) lower .95 upper .95
#                              # sex 0.58      1.70       0.42      0.81 
#                              
#                              p.value <-
#                                stp25rndr::rndr_P(x$wald["pvalue"], FALSE)
#                              wald.test <-
#                                stp25rndr::Format2(x$wald["test"], digits = 2)
#                              beta <-
#                                signif(x$coef[1], digits = 2)
#                              #coeficient beta
#                              HR <-
#                                signif(x$coef[2], digits = 2)
#                              #exp(beta)
#                              HR.confint.lower <-
#                                signif(x$conf.int[, "lower .95"], 2)
#                              HR.confint.upper <-
#                                signif(x$conf.int[, "upper .95"], 2)
#                              HR <- paste0(HR, " (",
#                                           HR.confint.lower, "-", HR.confint.upper, ")")
#                              res <- c(beta, HR, wald.test, p.value)
#                              names(res) <-
#                                c("beta", "HR (95% CI for HR)", "wald.test",
#                                  "p.value")
#                              res
#                              #return(exp(cbind(coef(x),confint(x))))
#                            })
#     
#     
#     res <- t(as.data.frame(univ_results, check.names = FALSE))
#     prepare_output(
#       cbind(covariates = row.names(res), as.data.frame(res)),
#       caption = caption,
#       note = note
#     )
#   }
# 
# 
# 
# 




