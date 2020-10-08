#' extract (Tidy)
#'
#' @rdname extract
#' @name extract
#'
#' @param x Objekt
#' @param ... weitere Objekte nicht benutzt
#' @return   data.frame mit dbl 
#' Im Attribut N sind die Stichprobengroesse
#' und notes
#'
extract <- function(x, ...) {
  UseMethod("extract")
}



#' @rdname extract
extract.polr <- function(x,
                        digits = 2,
                        include.b = TRUE,
                        include.se = TRUE,
                        include.ci = FALSE,
                        include.odds = TRUE,

                        ...){

  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
  
  res <- summary(x)
  
  Intercepts <- names(res$zeta)
  ## store table
  ctable <- coef(res)
  ## calculate and store p values
  colnames(ctable) <- c("b", "se", "t.value")
  p <- pnorm(abs(ctable[, "t.value"]), lower.tail = FALSE) * 2
  ## combined table
  
  ctable <- data.frame(
    Source = rownames(ctable),
    ctable,
    "p.value" = p,
    stringsAsFactors = FALSE
  )
  
  n1 <- nrow(ctable)
  n2 <- length(Intercepts)
  
  coefficients <- ctable[1:(n1 - n2),]
  intercepts  <- ctable[-c(1:(n1 - n2)),]
  source.coef <- coefficients[, 1, drop = FALSE]
  source.interc  <- intercepts[, 1, drop = FALSE]
  b.coef <- coefficients[, 2, drop = FALSE]
  b.interc <- intercepts[, 2, drop = FALSE]
  stat.coef <- coefficients[, 3:5, drop = FALSE]
  stat.interc <- intercepts[, 3:5, drop = FALSE]
  
  if (include.ci) {
    x <- update(x, Hess = TRUE)
    pr <- profile(x)
    ci <- confint(pr)
    colnames(ci) <- c("low", "upr")
    b.coef <- cbind(b.coef, ci)
    b.interc <- cbind(b.interc,  low = NA, upr = NA)
  }
  
  
  if (include.b) {
    source.coef <- cbind(source.coef, b.coef)
    source.interc <- cbind(source.interc, b.interc)
  }
  
  if (include.odds) {
    if(include.ci){
      source.coef$OR <- rndr_ods(source.coef$b )
      #  ifelse(> 4.6, 100,  round(exp(source.coef$b), 2))
      source.coef$OR.low <-rndr_ods(source.coef$low)
      #  ifelse(source.coef$low > 4.6, 100,  round(exp(source.coef$low), 2))
      source.coef$OR.upr <-rndr_ods(source.coef$upr)
      #  ifelse(source.coef$upr > 4.6, 100,  round(exp(source.coef$upr), 2))
      
      source.interc$OR <-rndr_ods(source.interc$b)
      #   ifelse(source.interc$b > 4.6, 100,  round(exp(source.interc$b), 2))
      source.interc$OR.low <- NA
      source.interc$OR.upr <- NA
    }
    else{
      source.coef$OR <-  rndr_ods(source.coef$b)
      
      # ifelse(source.coef$b > 4.6, 100,  round(exp(source.coef$b), 2))
      source.interc$OR <- rndr_ods(source.interc$b)
      
      #  ifelse(source.interc$b > 4.6, 100,  round(exp(source.interc$b), 2))
    }
  }
  
  
  
  if (include.se) {
    source.coef <- cbind(source.coef, stat.coef)
    source.interc <- cbind(source.interc, stat.interc)
  } else{
    source.coef <- cbind(source.coef, stat.coef[-1, drop = FALSE])
    source.interc <-
      cbind(source.interc, stat.interc[-1, drop = FALSE])
  }
  
  source.interc$Source <- paste0("Intercept(",  source.interc$Source, ")")
  
  prepare_output(rbind(source.interc, source.coef),
                 paste0("AV: ", AV),
                 paste0("Model: ", info$family[1]),
                 info$N,
                 info$labels)
  
}


#' @rdname extract
extract.default <- function(x, ...) {
  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
  
 
  if (inherits(x, "lme"))
    df <- broom.mixed::tidy(x)
  else
    df <- broom::tidy(x)
  
  if (any(class(x) %in% "merModLmerTest")) {
    df <- cbind(df[1:(ncol(df) - 1)], p.value = NA, df[ncol(df)])
    p_value <- lmerTest::summary(x)$coefficients[, 5]
    df$p.value[1:length(p_value)] <- p_value
  }
  
  attr(df, "caption") =  paste0("AV: ", AV)
  attr(df, "note") = paste0("Model: ", info$family[1])
  attr(df, "N") = info$N
  attr(df, "labels") = info$labels
  
  df
}






#' @rdname extract
extract.anova <- function(... ){
  extract_param(...,
                fix_format = FALSE)
}
 

#' @description ANOVA - Methode ueber broom::tidy
#' @param include.eta  Eta Quadrat
#' @param include.sumsq,include.meansq  Quadrat- Summen
#'
#' @rdname extract
extract.aov <- function(... ){
  extract_param(...,
                fix_format = FALSE)
}
#' @description Regression - Methode ueber basr::summary (lm und glm)
#' @param include.b Estimate
#' @param include.se  SE Standardfehler
#' @param include.beta  standartisiertes beta
#' @param include.ci,ci.level  95-CI mit ci-Level
#'
#' @rdname extract

extract.lm <- function(... ){
  extract_param(...,
                fix_format = FALSE)
}

#' @rdname extract
#' @param rr RR Relatives Risiko
#' @param include.b.ci,include.odds,include.rr.ci 95 Konfidenzintervalle
extract.glm <- function(... ){
  # res <- APA2.glm(..., output=output)
  # res
  
  extract_param(...,
                fix_format = FALSE)
}




# extract.aov <- function(x,
#                        include.eta = TRUE,
#                        include.sumsq = TRUE,
#                        include.meansq = FALSE, test.my.fun=FALSE,
#                        ...){
#   if(test.my.fun) cat("\n   -> extract.aov()")
#   info <- model_info(x)
#   AV <-
#     ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
#   res <- broom::tidy(x)
# 
#   if (include.eta) {
#     if( is(x, "lm") | is(x, "anova") ){
#     k <- ncol(res)
#     res <-
#       cbind(res[, -k], etaSquared2(x, 2, FALSE), res[k])
#     }else cat("\nWarnung: include.eta geht nur bei aov\n")
#   }
#   if (!include.sumsq){
#     res <-  res[, names(res) != "sumsq"]
#   }
#   if (!include.meansq){
#     res <-  res[, names(res) != "meansq"]
#   }
#   prepare_output(res,
#                  paste0("AV: ", AV),
#                  paste0("Model: ", info$family[1]),
#                  info$N,
#                  info$labels)
# }



# extract.lm <- function(x,
#                       include.b = TRUE,
#                       include.se = TRUE,
#                       include.beta = FALSE,
#                       include.ci = FALSE,
#                       include.r = TRUE,
#                       include.ftest = FALSE,
#                       ci.level = .95,
#                       test.my.fun=FALSE,
#                       ...
#                       ){
#  
#   info <- model_info(x)
#   AV <- ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
#   note <-  paste0("Model: ", info$family[1])
#   if (include.ftest)
#     note <- paste(note, APA(x, FALSE))
#   if (include.r) {
#     r2 <- R2(x)
#     note <- paste(note, "\nr-squared:", rndr_r2(r2))
#   }
# 
#   prepare_output(extract_param(x,
#                                  include.b,
#                                  include.se,
#                                  include.beta,
#                                  include.ci,
#                                  ci.level=ci.level, 
#                                  ...),
#                  paste0("AV: ", AV),
#                  note=note,    #  paste0("Model: ", info$family[1]),
#                  info$N,
#                  info$labels)
#  
# }





# extract.glm <- function(x,
#                        include.b = TRUE,
#                        include.se = TRUE,
#                        include.ci = FALSE,
#                        include.odds = TRUE,
#                        include.odds.ci= include.ci,
#                        include.rr=FALSE,
#                        include.rr.ci=include.ci,
#                        include.b.ci = include.ci,
#                        include.test = "lrt",
#                        #"wald"  bei SPSS wird der Wald-Test verwendet, ich verwende den LRT
#                        include.r = TRUE,
#                        include.pseudo = include.r,
# 
#                        include.loglik = TRUE,
#                        ci.level = .95,digits = 2,
#                        test.my.fun=FALSE,
#                         ...
#                        ){
#   info <- model_info(x)
# 
#   note <-  paste0("Model: ", info$family[1])
#   if (include.loglik)
#     note <- paste(note, APA(x))
#   if (include.pseudo) {
#     r2 <- R2(x)
#     note <-
#       paste(note, "\npseudo r-squared:", rndr_r2pseudo(r2))
#   }
# 
# 
#   AV <-
#     ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
# 
#   coefs <- data.frame(summary(x)$coef)
#   names(coefs) <- c("b", "SE", "LR.Test", "p.value")
# 
#   if ( include.test != "lrt"){
# 
#  #   car::Anova(x, type = "III", test.statistic = "Wald")
#     coefs$LR.Test <- (coefs$b/coefs$SE)^2
#     names(coefs)[3]<- "Wald"
#   }
# 
#   include.intercept <- rownames(coefs)[1] == "(Intercept)"
# 
#   if (include.ci) {
#     # likelihood ratio test  oder  Wald test
#     if (include.test == "lrt")
#       cis <- confint(x, level = ci.level)
#     else
#       cis <- confint.default(x, level = ci.level)
# 
#     if (!is.matrix(cis))
#       cis <- matrix(cis, ncol = 2)
# 
#   }
# 
#   # SPSS gibt keine CIs hier aus
#   if (include.b & include.b.ci ) {
#     coefs <- cbind(coefs[, 1, drop = FALSE],
#                    CI = rndr_CI(cis),
#                    coefs[,-1, drop = FALSE])
# 
#     if (include.intercept)
#       coefs$CI[1] <- ""
#   } else {
#     coefs <- coefs
#   }
# 
#   if (include.odds) {
#     if (include.odds.ci) {
#       coefs$OR <- rndr_ods(as.vector(exp(coefs[, 1])))
#       coefs$OR.CI <- rndr_ods_CI(exp(cis))
# 
#       if (include.intercept)  {
#         coefs$OR[1] <- NA
#         coefs$OR.CI[1] <- ""
#       }
#     }
#     else{
#       coefs$OR <- rndr_ods(as.vector(exp(coefs[, 1])))
#       if (include.intercept)
#         coefs$OR[1] <- NA
#     }
#   }
# 
# 
# 
#   # Relaltv-Risk   include.rr
#   if (include.rr) {
#     rr <-  sjstats::odds_to_rr(x)
#     rr <- data.frame(RR = rndr_ods(rr[, 1]),
#                      RR.CI = rndr_ods_CI(rr[, 2:3]))
# 
#     if (include.intercept)
#       rr[1, 1:2] <- NA
# 
#     if (include.rr.ci)
#       coefs <- cbind(coefs, rr)
#     else
#       coefs <- cbind(coefs, rr[, 1])
#   }
# 
#   if (!include.se) {
#     coefs <-  coefs[, colnames(coefs) != "SE"]
#   }
# 
#   if (!include.b) {
#     coefs <-  coefs[,  colnames(coefs) != "b"]
#   }
# 
#   prepare_output(
#     fix_format(cbind(source = rownames(coefs), coefs)),
#     paste0("AV: ", AV),
#     note=note,
#     info$N,
#     info$labels
#   )
# }






# Alte Version kommentar siehe unten

#' @rdname extract
extract.merModLmerTest <- function(x,
                      # custom.model.names = NULL,
                      # digits = 2,
                      include.b = TRUE,
                      include.se = TRUE,
                      include.beta = FALSE,
                      # include.eta = TRUE,
                      include.ci = FALSE,
                      include.odds = FALSE,
                      # include.variance = TRUE,
                      #  include.r = TRUE,
                      #  include.ftest = FALSE,
                      #  include.aic = TRUE,
                      #  include.bic = include.aic,
                      ci.level = .95,
                      ...) {
  #cat("\n In extract.merModLmerTest \n")
  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
  coefs <- lmerTest::summary(x)$coefficients
  #lmerTest:::summary.lmerModLmerTest(x)
  stat.coef <- coefs[,-1, drop = FALSE]

  if (include.ci) {
    cis<-  confint(x, level = ci.level)
    k <- which(rownames(cis) == "(Intercept)")


    b.coef <- cbind(coefs[, 1, drop = FALSE],
                 cis[k:nrow(cis), ]
                 )
  } else {
    b.coef <- coefs[, 1, drop = FALSE]
  }

  res<-
  if (include.odds) {
    odds <-
      apply(b.coef, 2, function(x)
        ifelse(x > 4.6, 100, round(exp(x), 2)))

    if(ncol(b.coef==1))
    colnames(odds) <-  "OR"
    else   colnames(odds) <- c("OR", "low", "upr")


  #  print(b.coef)
  #  print(odds)
    b.coef <- cbind(b.coef, odds)
   }

#  if (include.beta)  cat("\nBeta macht bei ", class(x), "keinen Sinn!\n")

  res <- cbind(b.coef, stat.coef)
    colnames(res)[ncol(res)] <- "p.value"

  if (!include.se) {
    res <-  res[, colnames(res) != "Std. Error"]
  }

  if (!include.b) {
    res <-  res[, colnames(res) != "Estimate"]
  }


  prepare_output(data.frame(Source= rownames(res), res, stringsAsFactors = FALSE),
                 paste0("AV: ", AV),
                 paste0("Model: ", info$family[1]),
                 info$N,
                 info$labels)
}



#' @rdname extract
#' @description lmerTest::lmer returns an object of class 'lmerModLmerTest' (previously 'merModLmerTest')
#' to clarify that 'lmerModLmerTest' extends  'lmerMod' – not 'merMod'. The merMod class includes generalized
#' and nonlinear mixed models and lmerTest is only designed for linear mixed models.
extract.lmerModLmerTest<- function(x,
                                  # custom.model.names = NULL,
                                  # digits = 2,
                                  include.b = TRUE,
                                  include.se = TRUE,
                                  include.beta = FALSE,
                                  # include.eta = TRUE,
                                  include.ci = FALSE,
                                  include.odds = FALSE,
                                  # include.variance = TRUE,
                                  #  include.r = TRUE,
                                  #  include.ftest = FALSE,
                                  #  include.aic = TRUE,
                                  #  include.bic = include.aic,
                                  ci.level = .95,
                                 
                                  ...){
 
  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
  res <- tidy_lmer(
    x,
    effects = c("ran_pars", "fixed"),
    scales = NULL,
    ## c("sdcor",NA),
    ran_prefix = NULL,
    conf.int = include.ci,
    conf.level = ci.level,
    conf.method = "Wald"
  )

  if (include.beta)  cat("\nBeta macht bei ", class(x), "keinen Sinn!\n")

 

 
    if (include.odds) {
      res$OR <- c(NA, round(exp(res$estimate[-1],4)))
      if(include.ci){
        
         res$OR.conf.low <- c(NA, round(exp(res$conf.low[-1],4)))
         res$OR.conf.high <- c(NA, round(exp(res$conf.high[-1],4)))
       }
    }


  if (!include.se) res <- res[, colnames(res) != "std.error"]
  if (!include.b) res <-res[, colnames(res) != "estimate"]

  prepare_output(res,
                 paste0("AV: ", AV),
                 paste0("Model: ", info$family[1]),
                 info$N,
                 info$labels)
}





#' Fix tidy_lmer
#' https://github.com/tidymodels/broom/issues/309
#' @param x Objekt
#' @param effects,scales,ran_prefix,conf.int,conf.level,conf.method Param antidy
#' @param ... extra params
#'
#' @return data.frame tibble
tidy_lmer <- function(x,
                      effects = c("ran_pars", "fixed"),
                      scales = NULL,
                      ## c("sdcor",NA),
                      ran_prefix = NULL,
                      conf.int = FALSE,
                      conf.level = 0.95,
                      conf.method = "Wald",
                      ...) {
 # cat("\nin tidy_lmer\n")
  effect_names <- c("ran_pars", "fixed", "ran_modes")
  if (!is.null(scales)) {
    if (length(scales) != length(effects)) {
      stop("if scales are specified, values (or NA) must be provided ",
           "for each effect")
    }
  }
  if (length(miss <- setdiff(effects, effect_names)) > 0)
    stop("unknown effect type ", miss)
  base_nn <-
    c("estimate", "std.error", "df", "statistic", "p.value")
  ret_list <- list()
  if ("fixed" %in% effects) {
    # return tidied fixed effects rather than random
    ret <- stats::coef(summary(x))
    
    # p-values may or may not be included
    nn <- base_nn[1:ncol(ret)]
    
    if (conf.int) {
      cifix <- confint(x, parm = "beta_", method = conf.method, ...)
      ret <- data.frame(ret, cifix)
      nn <- c(nn, "conf.low", "conf.high")
    }
    if ("ran_pars" %in% effects || "ran_modes" %in% effects) {
      ret <- data.frame(ret, group = "fixed")
      nn <- c(nn, "group")
    }
    ret_list$fixed <-
      fix_data_frame(ret, newnames = nn)
  }
  if ("ran_pars" %in% effects) {
    if (is.null(scales)) {
      rscale <- "sdcor"
    } else
      rscale <- scales[effects == "ran_pars"]
    if (!rscale %in% c("sdcor", "vcov"))
      stop(sprintf("unrecognized ran_pars scale %s", sQuote(rscale)))
    ret <- as.data.frame(lme4::VarCorr(x))
    ret[] <- lapply(ret, function(x)
      if (is.factor(x))
        as.character(x)
      else
        x)
    if (is.null(ran_prefix)) {
      ran_prefix <- switch(rscale,
                           vcov = c("var", "cov"),
                           sdcor = c("sd", "cor"))
    }
    pfun <- function(x) {
      v <- na.omit(unlist(x))
      if (length(v) == 0)
        v <- "Observation"
      p <- paste(v, collapse = ".")
      if (!identical(ran_prefix, NA)) {
        p <- paste(ran_prefix[length(v)], p, sep = "_")
      }
      return(p)
    }
    
    rownames(ret) <- paste(apply(ret[c("var1", "var2")], 1, pfun),
                           ret[, "grp"], sep = ".")
    
    ## FIXME: this is ugly, but maybe necessary?
    ## set 'term' column explicitly, disable fix_data_frame
    ##  rownames -> term conversion
    ## rownames(ret) <- seq(nrow(ret))
    
    if (conf.int) {
      ciran <- confint(x, parm = "theta_", method = conf.method, ...)
      ret <- data.frame(ret, ciran)
      nn <- c(nn, "conf.low", "conf.high")
    }
    
    
    ## replicate lme4:::tnames, more or less
    ret_list$ran_pars <-
      fix_data_frame(ret[c("grp", rscale)],
                     newnames = c("group", "estimate"))
  }
  if ("ran_modes" %in% effects) {
    ## fix each group to be a tidy data frame
    
    nn <- c("estimate", "std.error")
    re <- lme4::ranef(x, condVar = TRUE)
    getSE <- function(x) {
      v <- attr(x, "postVar")
      setNames(as.data.frame(sqrt(t(
        apply(v, 3, diag)
      ))),
      colnames(x))
    }
    fix <- function(g, re, .id) {
      newg <-
        fix_data_frame(g, newnames = colnames(g), newcol = "level")
      # fix_data_frame doesn't create a new column if rownames are numeric,
      # which doesn't suit our purposes
      newg$level <- rownames(g)
      newg$type <- "estimate"
      
      newg.se <- getSE(re)
      newg.se$level <- rownames(re)
      newg.se$type <- "std.error"
      
      data.frame(rbind(newg, newg.se),
                 .id = .id,
                 check.names = FALSE)
      ## prevent coercion of variable names
    }
    
    mm <- do.call(rbind, Map(fix, coef(x), re, names(re)))
    
    ## block false-positive warnings due to NSE
    type <- spread <- est <- NULL
    mm %>% tidyr::gather(term, estimate, -.id, -level, -type) %>%
      tidyr::spread(type, estimate) -> ret
    
    ## FIXME: doesn't include uncertainty of population-level estimate
    
    if (conf.int) {
      if (conf.method != "Wald")
        stop("only Wald CIs available for conditional modes")
      
      mult <- qnorm((1 + conf.level) / 2)
      ret <- transform(
        ret,
        conf.low = estimate - mult * std.error,
        conf.high = estimate + mult * std.error
      )
    }
    
    ret <- dplyr::rename(ret, group = .id)
    ret_list$ran_modes <- ret
  }
  return(plyr::rbind.fill(ret_list))
}



#' Ensure an object is a data frame, with rownames moved into a colum
#'
#' @param x data.frame or matrix
#' @param newnames new column names, not including the rownames
#' @param newcol the name of the new rownames column
#'
#' @return a data.frame, with rownames moved into a column and new column names assigned

fix_data_frame <- function (x,
                            newnames = NULL,
                            newcol = "term") {
  # .Deprecated(msg = "This function is deprecated as of broom 0.7.0 and will be removed from a future release. Please see tibble::as_tibble().")
  # if (!is.null(newnames) && length(newnames) != ncol(x)) {
  #   stop("newnames must be NULL or have length equal to number of columns")
  # }
  # if (all(rownames(x) == seq_len(nrow(x)))) {
  #   ret <- data.frame(x, stringsAsFactors = FALSE)
  #   if (!is.null(newnames)) {
  #     colnames(ret) <- newnames
  #   }
  # }
  # else {
  #   ret <- data.frame(...new.col... = rownames(x), unrowname(x),
  #                     stringsAsFactors = FALSE)
  #   colnames(ret)[1] <- newcol
  #   if (!is.null(newnames)) {
  #     colnames(ret)[-1] <- newnames
  #   }
  # }
  # as_tibble(ret)
  if (!is.null(newnames) ) colnames(x) <- newnames
  tibble::as_tibble(x, rownames = "term")
  
}


