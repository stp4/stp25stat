#' @param x Objekt lm, glm, lmer
#' @param include.ftest lm
#' @param include.loglik,include.deviance,include.minus.LL glm
#' @param include.r lm and glm
#' @param include.heteroskedasticity,include.durbin,include.levene,include.bartlett lm
#' @param include.vif lm
#' @param include.sigma,include.rmse  lm and glm
#' @param include.aic,include.bic lm and glm
#' @param include.residual  lm and glm
#' @param include.normality,include.multicollin lm and glm
#' @param ...  nicht benutzt
#'
#' @rdname extract
#' @export
#' 
#' @return vector
#' 
#' @description regressions diagnostic
#' 
#' 
exptact_gof <- function(x,
                            include.ftest=TRUE, include.loglik=FALSE, include.minus.LL=include.loglik,
                            include.r=FALSE, #include.pseudo=FALSE,
                            include.heteroskedasticity = TRUE,
                            include.durbin = TRUE,
                            include.levene = FALSE,
                            include.bartlett = FALSE,
                            include.vif=FALSE,
                            include.sigma=FALSE,
                            include.rmse=FALSE,
                            include.aic=TRUE, include.bic = TRUE,
                            include.residual=TRUE,
                            include.normality=TRUE,
                            include.multicollin=include.vif,
                            
                            include.deviance=TRUE,
                            
                            #include.obs=TRUE,
                            ...
) {
  mdlnf <- model_info(x)
  type_glm  <- inherits(x, "glm")
  type_lm   <- inherits(x, "lm") & (!type_glm)
  type_lmer <- inherits(x, "lmerModLmerTest")
  
  xs <- if(type_lmer) summary(x) else NULL
  res <- data.frame(Test ="Obs", 
                    statistic = Format2(mdlnf$N, 0),
                    stringsAsFactors=FALSE)
  
  if (include.ftest) {
    res <-
      rbind(res,
            c(Test = "F-Statistic",
              statistic = ifelse(type_lm,
                                 APA(x,include.r = FALSE), NA)))
  }
  
  if (include.loglik) {
    if (inherits(x, "glm")) {
      res <- rbind(res,
                   c(Test = "Likelihood Ratio Test",
                     statistic =   APA(x)))
      
      if (include.minus.LL) {
        minus_ll <- pscl::pR2(x)[1:3]
        minus_ll[1:2] <- minus_ll[1:2] * (-2)
        minus_ll <- Format2(minus_ll, 2)
        res <-
          rbind(res,
                c(
                  Test = "-2LL",
                  statistic =  paste0("LL=", minus_ll[1],
                                      ", LL-Null=", minus_ll[2],
                                      ", G2=", minus_ll[3])
                ))
        
      }
    } else{
      res <- rbind(res,
                   c(Test = "Likelihood Ratio Test",
                     statistic =   NA))
      
      if (include.minus.LL) {
        res <-
          rbind(res,
                c(Test = "-2LL", statistic = NA))
      }
    }
  }
  
  if (include.deviance) {
    res <- rbind(res,
                 c(Test = "Deviance Residuals",
                   statistic =   Format2(deviance(x,REML=FALSE), 1)))
    
  }

  if (include.r) {
    # res <-rbind(res,
    #             c(Test = "R-Squared",
    #               statistic = ifelse( type_lm,
    #                                   rndr_r2(R2(x)),
    #                                   ifelse( type_glm | type_lmer,
    #                                           rndr_r2pseudo(R2(x)),
    #                                           NA))))
    
    
    r_sqrt <- R2(x)
    if( type_lm ) names(r_sqrt) <- c("R2", "adj. R2")
    res <- rbind(res,
                 cbind(
                   Test = names(r_sqrt),
                   statistic = as.character(Format2(r_sqrt, digits = 2))
                 ))
    
  }
  
  if (include.heteroskedasticity) {
    res <-
      rbind(res,
            c(Test = "Heteroskedasticity (Breusch-Pagan)",
              statistic = ifelse( type_lm |type_glm, APA(lmtest::bptest(x)), NA )
            ))
  }
  
  if (include.durbin) {
    res <-
      rbind(res,
            c(Test = "Autocorrelation (Durbin-Watson)",
              statistic = ifelse( type_lm |type_glm, APA(lmtest::dwtest(x)), NA )
            ))
  }
  
  if (include.levene) {
    levi <- car::leveneTest(x$model[, 1], x$model[, 2])
    levi <-  rndr_F(levi[1, 2],  levi[1, 1],  levi[2, 1],  levi[1, 3])
    
    res <- rbind(res,
                 c(Test = "Homogeneity of Variances (Levene's)",
                   statistic = levi))
  }
  
  if (include.bartlett) {
    res <-
      rbind(res,
            c(Test = "Homogeneity of Variances (Bartlett)",
              statistic =  APA(bartlett.test(x$model[,1], x$model[,2]))))
    
  }
  
  if (include.normality) {
    res <-
      rbind(res,
            c(Test = "Shapiro-Wilk normality test",
              statistic =  test_normality(x)))
  }
  
  if (include.multicollin) {
    res <-
      rbind(res,
            c(Test = "Autocorrelation (VIF)",
              statistic =  test_multicollin(x)))
  }
  
  if (include.aic) {
    res <-
      rbind(res,
            c(Test = "AIC",
              statistic = Format2(AIC(x),1)  ))
    
  }
  
  if (include.bic) {
    res <-
      rbind(res,
            c(Test = "BIC",
              statistic = Format2(BIC(x),1)  ))
    
  }
  
  if (include.rmse) {
    res <-
      rbind(res,
            c(Test =  "RMSE",
              statistic = Format2(RMSE(x)[1,2],2)
            ))
  }
  
  if (include.sigma) {
    res <-
      rbind(res,
            c(Test = "Sigma",
              statistic = ifelse( type_lm |type_glm, Format2(RMSE(x)[1,1],2),
                                  ifelse( type_lmer,  Format2(xs$sigma, 2)))
              
            ))
  }
  
  if (include.residual) {
    res <-
      rbind(res,
            c(Test = "Var: Residual",
              statistic = ifelse( type_lm |type_glm, Format2(RMSE(x)[1,1]^2,2),
                                  ifelse( type_lmer,  Format2(xs$sigma^2, 2)))
              
            ))
  }
  
  # Test                                statistic
  # 2                         F-Statistic                   F(6, 276)=2.14, p=.049
  # 3                  Deviance Residuals                                     31.3
  # 4                           
  # 5  Heteroskedasticity (Breusch-Pagan)                       BP(6)=4.63, p=.592
  # 6     Autocorrelation (Durbin-Watson)                          DW=2.28, p=.984
  # 7         Shapiro-Wilk normality test                           W=0.96, p<.001
  # 8                                 AIC                                    195.9
  # 9                                 BIC                                    225.0
  # 10                      Var: Residual                                     0.11
  # 1                                 Obs                                      283
  
  rbind(res[-1,], res[1,]) 
  
}  
  




# extract_gof
#
# @param x objekt
# @param include.r,include.pseudo,include.rmse,include.sigma,include.variance,include.devianze,include.loglik,include.aic,include.bic,include.nobs Alles wie bei APA_TAble
# @param digits nachkommastellen
#extract_gof <- function(x,
#                          include.r = TRUE,
#                          include.pseudo = TRUE,
#                          include.rmse = TRUE,
#                          include.sigma = FALSE,
#                          include.variance = FALSE,
#                          include.devianze = FALSE,
#                          include.loglik = FALSE,
#                          include.test = FALSE,
#                          include.aic = TRUE,
#                          include.bic = include.aic,
#                        #  include.nobs = TRUE,
#                        #  digits = 2,
#                        #  fix_format = FALSE,
#                          ...) {
#   
#   rslt <-
#     test_regression(
#       x,
#       include.ftest = include.test,
#       include.minus.LL = include.loglik,
#       include.r = any(c(include.r, include.pseudo)),
#       include.heteroskedasticity = FALSE,
#       include.durbin = FALSE,
#       include.levene = FALSE,
#       include.bartlett = FALSE,
#       include.sigma = include.sigma,
#       include.rmse = include.rmse,
#       include.aic = include.aic,
#       include.bic = include.bic,
#       include.residual = FALSE,
#       include.normality = FALSE,
#       include.multicollin = FALSE,
#       include.deviance = include.devianze
#     )
#  
#   names <-  rslt[[1]]
#   rslt <- as.data.frame(as.matrix(t(rslt[-1])))
#   colnames(rslt) <- names
#   
#  
#   tibble::as_tibble(rslt)
# 
# }  
  
  # rslt <-  broom::glance(x)
  # param <-  "term"
  # 
  # if (include.r | include.pseudo) {
  #   if (any(names(rslt) %in% "r.squared")) {
  #     param <- c(param, c("r.squared", "adj.r.squared"))
  #   } else{
  #     ans_r <- R2(x)
  #     rslt <- cbind(rslt, ans_r)
  #     param <- c(param, names(ans_r))
  #   }
  # }
  # 
  # 
  # if (include.aic)
  #   param <- c(param, "AIC")
  # 
  # if (include.bic)
  #   param <- c(param, "BIC")
  # 
  # if (include.rmse) {
  #   param <- c(param, "RMSE")
  #   rslt <- cbind(rslt, RMSE(x)[2])
  # }
  # 
  # if (include.loglik)
  #   param <- c(param, "logLik")
  # if (include.devianze)
  #   param <- c(param, "deviance")
  # if (include.sigma)
  #   param <- c(param, "sigma")
  # 
  # 
  # if (fix_format) {
  #   rslt <- tibble::as_tibble(
  #     plyr::llply(rslt,
  #                 function(z) {
  #                   if (!is.na(z)) {
  #                     formatC(z, digits = digits, format = "f")
  #                   } else {
  #                     ""
  #                   }
  #                 }))
  #   
  #   if (include.test) {
  #     param <- c(param, "Test")
  #     rslt$Test <- "nicht implementiert"
  #     
  #   }
  #   
  #   if (include.nobs) {
  #     param <- c(param, "Obs")
  #     rslt$Obs <- formatC(nobs(x), digits = 0, format = "f")
  #   }
  # }
  # else{
  #   rslt <- tibble::as_tibble(
  #     plyr::llply(rslt,
  #                 function(z) {
  #                   if (!is.na(z)) {
  #                     round(z, digits = digits )
  #                   } else {
  #                     NA
  #                   }
  #                 }))
  #   
  #   if (include.test) {
  #     param <- c(param, "Test")
  #     rslt$Test <- "nicht implementiert"
  #   }
  #   if (include.nobs) {
  #     param <- c(param, "Obs")
  #     rslt$Obs <- nobs(x)
  #   }
  # }
  # param <- intersect(names(rslt), param)
  #   tibble::as_tibble(rslt[param])
    
  # # A tibble: 1 x 6
  # r.squared adj.r.squared   AIC   BIC  RMSE   Obs
  #     <dbl>         <dbl> <dbl> <dbl> <dbl> <int>
  #      0.04          0.02  196.  225.  0.33   283
  

