#' GOF 
#'  
#'  
#' Im Folgenden die wichtige Voraussetzungen der Regressionsanalyse 
#' 
#' Das Modell ist linear in den Parametern.
#' Die Residuen sind normalverteilt und im Mittel sind die Residuen null.
#' Die Zahl der Beobachtungen muss größer sein als die Zahl der zu schätzenden Parameter: n > k.
#'  
#' \strong{APA_Validation}
#' Validation of Linear Models Assumptions
#' (Testing Linear Regression Models)
#' 
#' \itemize{
#' \item{loglik} Likelihood Ratio Test The log-likelihood from the intercept-only restricted model.
#' -2LL: The LL (log-likelihood from the fitted model)
#' llhNull	(The log-likelihood from the intercept-only restricted model),
#' G2	(Minus two times the difference in the log-likelihoods)
#'
#' \item{Autocorrelation} Durbin-Watson test for autocorrelation of disturbances. Ist nur bei Zeitreihendaten sinnvoll.
#' 
#' \item{Homogeneity of Variances} Levene Computes Levene's test for homogeneity of variance across groups.
#'  Bartlett Test of Homogeneity of Variances
#' 
#' \item{Heteroskedasticity} Breusch-Pagan test against heteroskedasticity.
#' 
#' \item{VIF} variance inflation factor. VIF values over 5 are troubling, should probably investigate anything over 2.5.
#' 
#' \item{Residual} RMSE values should be low (<0.5 and <0.3, respectively).
#'  SigmaResidual standard error  When the residual standard error is exactly 0 then the model fits the data perfectly (likely due to overfitting)
#' \item{ R-Quadrats } Cox und Snell R2: [ 0.2 = akzeptabel, 0.4 = gut ] 
#' Nagelkerke R2: [ 0.2 = akzeptabel, 0.4 = gut, 0.5 = sehr gut] 
#' McFaddens R2: [ 0.2 = akzeptabel, 0.4 = gut ] (see pR2)
#' }
#' @param x model- fit
#' @param include.ftest,include.loglik  F-sratistik
#' @param include.heteroskedasticity Breusch-Pagan test
#' @param include.r,include.pseudo R-Quadrat
#' @param include.durbin  autocorrelation
#' @param include.levene  homogeneity of variance across groupssiehe T-Test
#' @param include.bartlett Homogeneity of Variances siehe T-Test
#' @param include.vif noch nicht Implementiert
#' @param include.sigma,include.rmse  RMSE Extract Residual Standard Deviation Sigma
#' @export
#'
#' @examples 
#' 
#' hkarz$Lai <- factor(hkarz$lai, 0:1, Cs(.neg, .pos))
#' fit2 <- glm(gruppe ~ tzell + Lai, hkarz, family = binomial)
#' fit1<-lm(score ~ grade + treatment + stdTest, schools)
#' APA_Validation(x) 
#' APA_Validation(fit1,fit2, include.pseudo = TRUE, include.r = TRUE, include.loglik = TRUE,
#'                include.rmse = TRUE)
#'                
#'                

APA_Validation<- function(...,
                          include.ftest = TRUE,include.loglik = FALSE,include.minus.LL = include.loglik,
                          include.pseudo = TRUE, include.r = include.pseudo,  
                          include.heteroskedasticity = TRUE,
                          
                          include.durbin = TRUE,
                          include.levene = FALSE,
                          include.bartlett = FALSE,
                          
                          
                          include.multicollin = FALSE,
                          include.vif = FALSE,
                          
                         # include.autocorrelation =FALSE
                          
                          include.sigma = FALSE,
                          include.rmse = FALSE,
                          include.aic = TRUE, include.bic=include.aic,include.residual = TRUE,
                          include.normality = TRUE,
                          
                          
                          include.deviance = TRUE,
                          
                          caption = "Testing Regression Models",
                          note="",
                          names = NULL,
                          output = which_output()
)
{
  custom_model_names <- function() {
    if (length(myfits) == 1) { 
      ""
    }else{
      if (is.null(names)) paste0("(", 1:length(myfits), ")")  else
        names     
    }
  }
  
  myfits <- list(...)
  
  if (is(myfits[[1]], "list")) {
    myfits <- myfits[[1]]  # hier kommt ein fit_with-Objekt
    if (is.null(names))
      names <- names(myfits)
  }
  n <- length(myfits)
  
  custom.model.names <- custom_model_names()
  
  res <- exptact_gof(myfits[[1]],
                         include.ftest,include.loglik,include.minus.LL,
                         include.r, 
                         include.heteroskedasticity,  
                         include.durbin,
                         include.levene,
                         include.bartlett,
                         include.vif,
                         include.sigma, include.rmse,include.aic,
                         include.bic,include.residual,
                         include.normality,
                         include.multicollin,
                         include.deviance
  )
  
  if (n > 1) {
    for (i in 2:n) {
      res <- cbind(
        res,
        exptact_gof(
          myfits[[i]],
          include.ftest,include.loglik,include.minus.LL,
          include.r,
          include.heteroskedasticity,  
          include.durbin,
          include.levene,
          include.bartlett,
          include.vif,include.sigma, include.rmse,include.aic,
          include.bic,include.residual,
          include.normality,
          include.multicollin
          ,include.deviance
        )[2]
      )
    }
    names(res) <- c("Test", custom.model.names)
  }

  
  res <- prepare_output(res, caption, note)
  
  Output(res, output=output)
  invisible(res) 
} 



 





#' Kopien von require(sjstats)
#'  p.val < 0.05 => "Non-normality
#' @noRd
test_normality <- function(x) {
  # bei  sjstats ist im orginal stats::rstandard ich verwende aber resid
  APA( stats::shapiro.test(stats::resid(x)) )
}





#' @rdname APA_Validation
#' @description 
#' Multi-collinearity diagnostics
#'
#' Conducts a series of checks for multicollinearity.
#' 
#' 
#' Bei perfekter Multikollinearität lässt sich das Modell nicht lösen. 
#' 
#' @param include.cor Bivariate Correlations
#' @export
#' 
APA_Multicollinearity <-
  function(x,
           caption = "Test for Multicollinearity ",
           note = "",
           include.vif = TRUE,
           include.cor = TRUE,
           output = which_output()) {
    # from package 'rockchalk' mcDiagnose
    # cat("Bivariate Correlations for design matrix \n")
    mm <- model.matrix(x)[,-1] ## data, omit intercept
    res_cor <-
      prepare_output(
        Format2(cor(mm[, ]), digits=2),
        caption = paste(caption, "Bivariate Correlations for design matrix"),
        notes = notes
      )
    
    res_vif <-  VIF(x)
    
    res_vif <- prepare_output(
      data.frame(Source =  names(res_vif),
                 VIF = Format2(as.vector(res_vif), 2)),
      caption = paste(caption, "VIF"),
      notes = notes
    )
    
    
    
    if (include.vif)
      Output(res_vif,
             output = output)
    
    if (include.cor)
      Output(res_cor,
             output = output)
    
    invisible(list(vif = res_vif,
                   cor = res_cor))
  }




#' Kopien von require(sjstats)
#' @noRd
test_multicollin <- function(x) {
  
  res<-""

    # check for autocorrelation
    ts <- sqrt(car::vif(x)) > 2
    
    if (any(ts)) {
      mp <- paste(sprintf("%s", names(ts)), collapse = ", ")
      res<-  paste0("Multicollinearity detected for following predictors: ", mp)
    } else {
      res <- "No multicollinearity detected."
    }
    

  res
}

#' Kopien von require(sjstats)
#' @noRd
test_nonconstvar <- function(model) {
  sumry <- summary(model)
  
  residuals <- stats::residuals(model, type = "pearson")
  S.sq <- stats::df.residual(model) * (sumry$sigma) ^ 2 / sum(!is.na(residuals))
  
  .U <- (residuals ^ 2) / S.sq
  mod <- lm(.U ~ fitted.values(model))
  
  SS <- stats::anova(mod)$"Sum Sq"
  RegSS <- sum(SS) - SS[length(SS)]
  Chisq <- RegSS / 2
  
  stats::pchisq(Chisq, df = 1, lower.tail = FALSE)
}



#' @rdname APA_Validation
#' @description APA_Autocorrelation: 
#' Der Durbi-Watson (DW) 
#' Test verwendet die geschaetzten Residuen um auf 
#' Autokorrelation erster Ordnung zu testen.
#' 
#' Der DW d Test weist folgendes Intervall auf: 0<d<4
#' 0:  Extrem positive Autokorrelation 
#' 4:  Extrem negative Autokorrelation. 
#' 2:  Keine Autokorrelation
#' 
#' 
#' Der Breusch-Godfrey (BG) Test ist flexibler,
#' da er auf Autokorrelation hoeherer Ordnung testet.
#' @export
 
APA_Autocorrelation <- function(x,  order = 1, ...) {
  list(
    bg = list(name = "Breusch-Godfrey" ,
              stat = lmtest::bgtest(x,  order = order)),
    dw = list(name = "Durbin-Watson", car::durbinWatsonTest(x, ...))
  )
}


#' @rdname APA_Validation
#' @description APA_Heteroscedasticity: mit (lmtest::bptest) Breusch-Pagan test against heteroskedasticity
#' @export

APA_Heteroscedasticity <- function(x...) {
  lmtest::bptest(x)
}

#' @rdname APA_Validation
#' @description  APA_Durbin_Watson:  APA_Durbin_Watson(fit, max.lag=1, simulate=TRUE, reps=1000,
#' method=c("resample","normal"),
#' alternative=c("two.sided", "positive", "negative")): Durbin-Watson Test for Autocorrelated Errors. 
#'   Kopie der Funktion car::durbinWatsonTest
#'   
#' @export
#'
#' @examples
#' 
#' # APA_Durbin_Watson
#' x<-lm(score ~ grade + treatment + stdTest, schools)
#' APA2(car::durbinWatsonTest(x))
#' DW_Test2(x)
#' 
#' lmtest::dwtest(x)
#' car::durbinWatsonTest(x)
#' 
APA_Durbin_Watson<- function(x,
                             caption = "Durbin-Watson Test for Autocorrelated Errors",
                             note =NULL, ...){
  dw<-car::durbinWatsonTest(x,...) 
  APA2.durbinWatsonTest(dw, caption=caption, note=note)
}


#' @rdname APA_Validation
#' @description APA2.durbinWatsonTest: Methode für car::durbinWatsonTest Kopie von car:::print.durbinWatsonTest
#' @export

APA2.durbinWatsonTest <-
  function(x,
           caption = "Durbin-Watson Test for Autocorrelated Errors",
           note =NULL,
           ...) {
    max.lag <- length(x$dw)
    result <- if (is.null(x$p))
      cbind(
        lag = 1:max.lag,
        Autocorrelation = x$r,
        `D-W Statistic` = x$dw
      )
    else cbind(lag = 1:max.lag, Autocorrelation = x$r, `D-W Statistic` = x$dw, 
               `p-value` = x$p)
    rownames(result) <- rep("", max.lag)
    
    note <- paste(" Alternative hypothesis: rho", 
                  if (max.lag > 1) "[lag]"
                  else "", 
                  c(" != ", " > ", " < ")[which(x$alternative == c("two.sided", "positive", "negative"))], "0", sep = "")
    
    result <- prepare_output(fix_format(data.frame(result)),
                             caption = caption, note = note)
    Output(result, ...)
    invisible(result)
  }



