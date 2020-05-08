#' @rdname APA2
#' @description
#' Ausgabe von Regressions Tabelle nach der APA-Style vorgabe. Die Funktion
#' ist eine Kopie von  texreg aggregate.matrix.
#' @param custom.model.names Namen ner Modelle
#' @param include.custom liste mit Statistiken für Gofs also zB F-Tests
#' @param include.se,include.ci,include.odds SE, 95-Ci, OR noch nicht fertig
#' @param include.ftest,include.loglik  noch nicht fertig
#' @param include.r,include.pseudo pseudo R
#' @param include.aic,include.bic geht nur zusammen
#' @param ci.level Ci default 95 Prozent
#' @param rgroup Zwischen Beschriftung
#'
#' @return invisible data.frame und Output mit html/knit oder Text.
#' @export
#'
#' @examples
#'
#' library(lmerTest)
#'
#' fit1 <- lm(chol0 ~  ak + rrs0 + med + g, hyper)
#' fit2 <- glm(chol0 ~ med +   ak +   g + rrs0 , hyper, family = poisson())
#' fit3 <- lmerTest::lmer(chol0 ~ rrs0 + med +  ak  +  (1|g) , hyper )
#' fits <- list(fit1, fit2, fit3)
#'
#' APA2(fits,
#'           custom.model.names=c("lm", "glm", "lmer"),
#'           digits= list(c(1,2,3,4,5,6,7),
#'                        c(1,2,3,4,5,6,7),
#'                        c(1,2,3,4,5,6)),
#'           include.custom=list(
#'                        Wald=c("F(1)=245", "F(2)=245","F(3)=245"),
#'                        Chi=c("X(4)=2.45", "X(5)=24.5","X(6)=24.5")))
#'
#'

APA2.list <- function(...) APA2_list(...)

 
#' APA2
#'
#' @param x lm object.
#' @param include.ci Confidence interval
#' @param include.effect Text zu Effect_Size
#' @export
#'
#' @examples
#'
#' library(psycho)
#'
#'
#'  df <- psycho::affective  # Load a dataset from the psycho package
#'  #df <- standardize(df)  # Standardize all numeric variables
#'
#'  fit <- lm(Age ~ Salary, data=df)  # Fit a Bayesian linear model
#'  # results <- analyze(fit)  # Format the output
#'  #APA2(results )
#'
#'
#'
#'  library(lmerTest)
#'  fit <- lmerTest::lmer(Sepal.Length ~ Sepal.Width + (1|Species), data=iris)
#'
#'  #results <- analyze(fit)
#'  #APA2(results)
APA2.psychobject <- function(x,
                             caption = "",
                             note = NULL,
                             # paste("contrasts: ", paste(options()$contrasts, collapse=", ")),
                             include.ci = FALSE,
                             include.effect=FALSE,
                             output = stp25output::which_output(),
                             ...) {
 # class(x)

  res <-
    fix_format(summary(x),
               pattern_pval = "p",
               pattern_est = c("SE", "SE.std"))

  if (!include.ci) {
    ci <- which(names(res) %in% c("CI_lower", "CI_higher"))
    res <- res[-ci]

  }

  if(!include.effect){
    eff <- which(names(res) == "Effect_Size")
    res <- res[-eff]

  }
  if (is.null(note)) {

    r2s <- x$values$model
    note <- ""
    for (i in names(r2s)) {
      note <- paste(note, i, "=", rndr_r(r2s[[i]], FALSE))
      if(names(r2s)[1] == i) note<- paste0(note, ",")
    }
    note
  }
  res <-  prepare_output(res, caption, note)

  Output(res, output = output)
  invisible(res)
}














  #APA2.aov()


#' @rdname APA2
#' @param anova_type  bei lme:  "F"  F-werte (wie SPSS) oder Chi (car::Anova)
#' @export
#'
#'
APA2.lme <- function(...){
  APA2.lmerMod(...)
}


# APA2.lme <- function(x,
#                      caption = "" ,
#                      note = "",
#                      output = stp25output::which_output(),
#                      col_names = NULL,
#                      type = "III",
#                      anova = TRUE,
#                      anova_type = "F",
#                      ...) {
#   Anov <- NULL
#   goodnes <- NULL
#   fit_param <- NULL
#   fit_sum <- summary(x)
#   fit_param <- as.data.frame(fit_sum$tTable)  ##  xtTab
#   names(fit_param)[1] <- c("Estimate")
#
#   if (anova_type == "F") {
#     #cat(Anov)
#     #  cat("Verwende die Funktion anova stadt car::Anova\n")
#     #Text("Fehler in den Factorstufen es duerfen keine leeren Factoren vorkommen")
#     #-- Unbekannter aufgrung von anzahl an Factorstufen
#     Anov <- anova(x)
#     Anov <- as.data.frame(Anov)
#
#     goodnes <- cbind(
#       Obs = fit_sum$dims[["N"]],
#       round(R2(x), 2) ,
#       BIC = round(fit_sum$BIC, 2),
#       logLik = round(c(fit_sum$logLik), 2)
#     )
#
#   } else{
#     Anov <- car::Anova(x, type = type)
#
#     goodnes <- cbind(
#       Obs = fit_sum$dims[["N"]],
#       round(r.squared(fit)[, 4:6], 2) ,
#       BIC = round(fit_sum$BIC, 2),
#       logLik = round(c(fit_sum$logLik), 2)
#     )
#   }
#
#   fit_param <- prepare_output(
#     fix_data_frame2(Source = rownames(fit_param), fit_param),
#     caption = paste("Regression Model:", caption),
#     note = note
#   )
#
#   Anov <-
#     prepare_output(
#       fix_data_frame2(Source = rownames(Anov), Anov),
#       caption = paste("ANOVA:", caption),
#       note = note
#     )
#
#   goodnes <-
#     prepare_output(goodnes,
#                    caption = paste("Goodness-of-fit", caption),
#                    note = "R-Quadrat entspricht Marginal und Conditional")
#
#   Output(fit_param, output=output)
#   Output(goodnes, output=output)
#
#   if (anova)
#     Output(fit_param, output=output)
#
#   invisible(list(
#     param = fit_param,
#     anova = Anov,
#     gof = goodnes
#   ))
# }




#' @rdname APA2
#' @export
APA2.lmerMod <- function(x,
                         caption = NULL,
                         note = NULL,
                         output = stp25output::which_output(),
                         col_names = NULL,
                         include.b = TRUE,
                         include.se = TRUE,
                         include.ci = FALSE,
                         include.odds = FALSE,
                         include.odds.ci =  include.ci,
                         include.statistic = TRUE,
                         include.p = TRUE,
                         include.stars = FALSE,
                         include.r = TRUE,
                         include.pseudo = include.r,
                         include.test = FALSE,
                         ci.level = .95,
                         conf.method = "Wald",
                         test.my.fun = FALSE,conf.style.1 =TRUE,
                         digits = 2,
                         ...) {
  if (test.my.fun){
    cat("\n    -> APA2.lmerMod() \n Input: ")
    print(class(x))
    cat("\n")
    }
  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])

  if (is.null(note)) {
    note <-  paste0("Model: ", info$family[1])
    if (include.test)
      note <- paste(note, APA(x, FALSE))
    if (include.r) {
      if (test.my.fun)
        cat("\n       include.r: R( )")
      r2 <- R2(x)
      note <-  paste(note, "\npseudo r-squared:", rndr_r2pseudo(r2))
    }
  }
  if (is.null(caption))
    caption <-  paste0("AV: ", AV, " Obs: ", info$N)


  coefs <- extract_param(
    x,
    include.b = include.b,
    include.se = include.se,
    include.beta = FALSE,
    include.ci = include.ci,
    include.odds = include.odds,
    include.odds.ci =  include.odds.ci,
    include.statistic = include.statistic,
    include.p = include.p,
    include.stars = include.stars,
    ci.level = ci.level,
    conf.method = conf.method,
    fix_format = TRUE, conf.style.1 = conf.style.1
  )

  res <- prepare_output(coefs,
                        caption, note, info$N, info$labels)

  if (!is.logical(output))
    Output(res, output = output, col_names = col_names)

  invisible(res)

}


 
 


GOF_LMER <- function(x){

  res <- lmerTest::summary(x)
  # goodnes <- cbind(
  #   Obs = res$devcomp$dims["N"],
  #   round(r.squared.merMod(x)[, 4:6], 2),
  #   # BIC = round(res$BIC,2),
  #   logLik = round(c(as.numeric(res$logLik)), 2),
  #   REML =  round(res$devcomp$cmp["REML"], 2)
  # )


  # goodnes <-  prepare_output(goodnes,
  #                            caption = paste("Goodness-of-fit", caption),
  #                            note = "R-Quadrat entspricht Marginal und Conditional")



  res
}
