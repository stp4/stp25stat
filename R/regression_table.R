#' Formatierte Regressionstabelle
#'
#' Formatiert Listen mit Modellen zu Dataframs.
#'  APA2_list = regression_table
#'
#' @param x Regressionsobjekt
#' @param caption,note,output,custom.model.names,rgroup An Output
#' @param include.param,include.gof,include.custom Was soll ausgegeben werden
#' @param include.b,include.beta,include.ci,include.odds,include.se,include.statistic,include.odds.ci,include.p,include.stars an extract_coef
#' @param include.effects,conf.level,conf.method an extract_coef
#' @param digits.param,digits.odds,digits.test,digits.beta,formatan extract_coef
#' @param include.df,include.r,include.pseudo,include.rmse,include.sigma,include.variance,include.devianze,include.loglik,include.aic,include.bic,include.nobs,include.test An exptact_goff()
#' @param ...  nicht benutzt
#'
#' @return data.frame
#'
#' @noRd
#' 
regression_table <-
  function (x,
            caption = "" ,
            note = "",
            output = stp25output::which_output(),
            custom.model.names = NULL,
            include.param = TRUE,
            include.gof = TRUE,
            include.custom = NULL,
            include.b = TRUE,
            include.se = TRUE,
            include.beta = FALSE,
            include.ci = FALSE,
            include.odds = FALSE,
            include.odds.ci = FALSE,
            include.statistic = FALSE,
            include.p = FALSE,
            include.stars = TRUE,
            include.df = FALSE,
            include.effects = c("ran_pars", "fixed"),
            conf.level = 0.95,
            conf.method = "Wald",
            digits = NULL,
            digits.param = 3,
            digits.odds = 2,
            digits.test = 2,
            digits.beta = 2,
            format = "fg",
            include.r = TRUE,
            include.pseudo = TRUE,
            include.rmse = TRUE,
            include.sigma = FALSE,
            include.variance = FALSE,
            include.devianze = FALSE,
            include.loglik = FALSE,
            include.test = FALSE,
            include.aic = TRUE,
            include.bic = include.aic,
            include.nobs = TRUE,
            rgroup = c("Parameter", "Goodness of fit"),
            dictionary = c(std.error = "SE",
                           estimate = "b",
                           p.value = "p"),
            col_names = NULL,
            ...)
  {
    n <- length(x)
    coefs <- list()
    gofs <- list()
    result <- NULL
    first_nam <- ""
 
    if (is.null(custom.model.names) |
        length(custom.model.names) != n)
      custom.model.names <- paste0("m", 1:n)
    
 
    #-- param ----------------------------------
    for (i in seq_along(x)) {
      if (!is.null(digits)) {
        format <- "f"
        if (is.list(digits)) {
          digits.param = digits[[i]]
        #  digits.odds = digits[[i]]
        } else{
          digits.param = digits
        #  digits.odds = digits
        }
      }
      model <- extract_param(
        x[[i]],
        include.b = include.b,
        include.se = include.se,
        include.beta = include.beta,
        include.ci = include.ci,
        
        include.odds = include.odds,
        include.odds.ci = include.odds.ci,
        include.statistic = include.statistic,
        include.p = include.p,
        include.stars = include.stars,
        include.df = include.df,
        
        include.effects = include.effects,
        conf.int = TRUE ,
        conf.level = conf.level,
        conf.method = conf.method,
        
        digits.param = digits.param,
        digits.odds = digits.odds,
        digits.test = digits.test,
        digits.beta = digits.beta,
        format = format,
        fix_format = TRUE,
        conf.style.1 = TRUE
      
      )
      
      if (include.stars) {
        pos_star <-  grep("stars", names(model))
        model[[2]] <- paste0(unlist(model[[2]]), model[[pos_star]])
        model <- model[-pos_star]
      }
      
      names(model) <- sapply(names(model),
                             function(y) if (y %in% names(dictionary)) dictionary[y] else y,
                             USE.NAMES = FALSE)
      first_nam <- names(model)[2]
      coefs[[custom.model.names[i]]] <- model
    }
    
    
    if (n > 1) {
      coefs <-  stp25tools::list_to_df(coefs)
    } else
      coefs <- coefs[[1]]
    
    #-- gof ----------------------------------
    if (include.gof) {
      for (i in seq_along(x)) { 
 
        model <- exptact_gof(
            x[[i]],
            include.ftest = include.test,
            include.minus.LL = include.loglik,
            include.r = any(c(include.r, include.pseudo)),
            include.heteroskedasticity = FALSE,
            include.durbin = FALSE,
            include.levene = FALSE,
            include.bartlett = FALSE,
            include.sigma = include.sigma,
            include.rmse = include.rmse,
            include.aic = include.aic,
            include.bic = include.bic,
            include.residual = FALSE,
            include.normality = FALSE,
            include.multicollin = FALSE,
            include.deviance = include.devianze
          )
        
        names(model)  <-c("term", first_nam)
        gofs[[custom.model.names[i]]] <- model
      }

      
      if (n > 1) {
        gofs <- stp25tools::list_to_df(gofs, last = "Obs")
      } else
        gofs <- gofs[[1]]
 
      
     
      if (!is.null(include.custom)) {
        if (inherits(include.custom, "data.frame")) {
          names(include.custom) <- names(gofs)
          gofs <- dplyr::bind_rows(gofs,
                                   tibble::as_tibble(include.custom))
        }  else if (inherits(include.custom, "list")) {
          include.custom <- t(as.data.frame(include.custom))
          include.custom <-
            cbind(include.custom = rownames(include.custom), include.custom)
          colnames(include.custom) <- names(gofs)
          include.custom <- tibble::as_tibble(include.custom)
          
          gofs <- dplyr::bind_rows(gofs, include.custom)
        }
        else {
          warning(" Bei include.custom sind nur data.frames oder listen erlaubt.")
        }
      }
      
      if (include.param & include.gof) {
        n.rgroup <- nrow(coefs)
        result <-
          prepare_output(
            dplyr::bind_rows(coefs, gofs),
            caption,
            note,
            N = NULL,
            include.n = NULL,
            rgroup = rgroup,
            n.rgroup = n.rgroup
          )
        
      } else if (!include.param) {
        rgroup <- n.rgroup <- NULL
        result <-
          prepare_output(
            gofs,
            caption,
            note,
            N = NULL,
            include.n = NULL,
            rgroup = rgroup,
            n.rgroup = n.rgroup
          )
        
      }
      
    } else {
      rgroup <- n.rgroup <- NULL
      result <-
        prepare_output(
          coefs,
          caption,
          note,
          N = NULL,
          include.n = NULL,
          rgroup = rgroup,
          n.rgroup = n.rgroup
        )
      
    }
    
    if (!is.logical(output)) {
      Output(
        result,
        output = output,
        col_names = col_names,
        rgroup = rgroup,
        n.rgroup = n.rgroup
      )
    }
    
    
    invisible(result)
  }
