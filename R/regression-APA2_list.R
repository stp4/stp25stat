#' Formatierte Regressionstabelle
#'
#' Formatiert Listen mit Modellen zu Dataframs.
#'
#' @param x Regressionsobjekt
#' @param caption,note,output,custom.model.names,rgroup An Output
#' @param include.param,include.gof,include.custom Was soll ausgegeben werden
#' @param include.b,include.beta,include.ci,include.odds,include.se,include.statistic,include.odds.ci,include.p,include.stars an extract_coef
#' @param include.effects,conf.level,conf.method an extract_coef
#' @param digits.param,digits.odds,digits.test,digits.beta,formatan extract_coef
#' @param include.df,include.r,include.pseudo,include.rmse,include.sigma,include.variance,include.devianze,include.loglik,include.aic,include.bic,include.nobs,include.test An extract_gof()
#' @param ...  nicht benutzt
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' fit1 <- lm(chol0 ~ rrs0 + med, hyper)
#' fit2 <- lm(chol0 ~ rrs0 + med + ak, hyper)
#' fit3 <- lm(chol0 ~ ak + med + rrs0 , hyper)
#' fit4 <- lmerTest::lmer(chol0 ~ rrs0 + med +  ak  +  (1 | g) , hyper)
#'
#' coefs <- APA2_list(
#'   list(fit1,
#'        fit2,
#'        fit3, fit4),
#'   include.beta = TRUE,
#'   include.custom =
#'     data.frame(term = "M",  "A", "B", "C", "D")
#' )
#'
#' # fit1 <- lm(chol0 ~  ak + rrs0 + med + g, hyper)
#' #
#' #
#' # x1 <- stp25stat:::APA2_list(
#' #   fits,
#' #   custom.model.names = c("lm", "glm", "lmer"),
#' #   digits = list(c(1, 2, 3, 4, 5, 6, 7),
#' #                 c(1, 2, 3, 4, 5, 6, 7),
#' #                 c(1, 2, 3, 4, 5, 6)),
#' #   include.custom = list(
#' #     Wald = c("F(1)=245",
#' #              "F(2)=241",
#' #              "F(3)=242"),
#' #     Chi = c("X(4)=2.45", "X(5)=24.5", "X(6)=24.5")
#' #   ),
#' #   include.pseudo = FALSE,
#' #   output = FALSE
#' # )
#'
APA2_list <-
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
      coefs <- list_to_df(coefs)
    } else
      coefs <- coefs[[1]]
    
    #-- gof ----------------------------------
    if (include.gof) {
      for (i in seq_along(x)) { 
      #  cat("\n in gof i=", i, "\n")
        model <- t(
          extract_gof(
            x[[i]],
            include.r = include.r,
            include.pseudo = include.pseudo,
            include.rmse = include.rmse,
            include.sigma = include.sigma,
            include.variance = include.variance,
            include.devianze = include.devianze,
            include.loglik = include.loglik,
            include.test = include.test,
            include.aic = include.aic,
            include.bic = include.bic,
            include.nobs = include.nobs,
            fix_format = TRUE
          )
        )
        
        model <-  tibble::tibble(term = rownames(model),
                                 model = model[, 1])
        names(model)[2] <- first_nam
        gofs[[custom.model.names[i]]] <- model
      }
   #   cat("\n    gof complet")
    #   print(gofs)
      
      if (n > 1) {
        gofs <- list_to_df(gofs, last = "Obs")
      } else
        gofs <- gofs[[1]]
     # cat("\n    after list_to_df\n")
      
     
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
          prepare_output(dplyr::bind_rows(coefs, gofs),
                         caption, note, N = NULL, include.n = NULL)
        
      } else if (!include.param) {
        result <-
          prepare_output(gofs, caption, note, N = NULL, include.n = NULL)
        rgroup <- n.rgroup <- NULL
      }
      
    } else {
      result <-
        prepare_output(coefs, caption, note, N = NULL, include.n = NULL)
      rgroup <- n.rgroup <- NULL
    }
    
    if (!is.logical(output)) {
      Output(
        result,
        output = output,
        col_names = col_names,
        rgroup = rgroup,
        n.rgroup = n.rgroup)
    }
    invisible(result)
  }













# APA2_list <-
#   function (x,
#             caption = "" ,
#             note = "",
#             output = stp25output::which_output(),
#             custom.model.names = NULL,
#             include.param = TRUE, include.gof = TRUE,
#             include.custom = NULL,
#             include.b = TRUE, include.se = TRUE,
#             include.beta = FALSE,
#             include.ci = FALSE, include.odds = FALSE, include.odds.ci = FALSE,
#             include.statistic = FALSE,
#             include.p = FALSE, include.stars = TRUE,
#             include.df = FALSE,
#             include.effects = c("ran_pars", "fixed"),
#             conf.level = 0.95, conf.method = "Wald",
#             digits=NULL,
#             digits.param = 3, digits.odds = 2, digits.test = 2, digits.beta = 2,
#             format="fg",
#             include.r = TRUE, include.pseudo = TRUE,
#             include.rmse = TRUE, include.sigma = FALSE, include.variance = FALSE,
#             include.devianze = FALSE,
#             include.loglik = FALSE,
#             include.test = FALSE,
#             include.aic = TRUE, include.bic = include.aic,
#             include.nobs = TRUE,
#             rgroup = c("Parameter", "Goodness of fit"),
#            
#             dictionary = c(std.error = "SE",
#                             estimate = "b",
#                             p.value = "p"),
#             col_names=NULL,
#             ...
#             )
#   {
#     n <- length(x)
#     coefs <- NULL
#     gofs <- NULL
#     result <- NULL
#     mySep <- "__"
#     n_param <- NULL
#
#
#
#     if (is.null(custom.model.names) |
#         length(custom.model.names) != n)
#       custom.model.names <- paste0("Model ", 1:n)
#     custom.model.names.s <- paste0(mySep, custom.model.names)
#
#      #-- Extrahieren ----------------------------------
#     for (i in seq_len(n)) {
#        if(!is.null(digits)){
#         format <- "f"
#         if(is.list(digits)){
#           digits.param = digits[[i]]
#           digits.odds = digits[[i]]
#         }else{
#           digits.param = digits
#           digits.odds = digits
#         }
#       }
#       model <-  extract_param(
#         x[[i]],
#         include.b = include.b,
#         include.se = include.se,
#         include.beta = include.beta,
#         include.ci = include.ci,
#
#         include.odds = include.odds,
#         include.odds.ci = include.odds.ci,
#         include.statistic = include.statistic,
#         include.p = include.p,
#         include.stars = include.stars,
#         include.df = include.df,
#
#         include.effects = include.effects,
#         conf.int = TRUE ,
#         conf.level = conf.level,
#         conf.method = conf.method,
#
#         digits.param = digits.param,
#         digits.odds = digits.odds,
#         digits.test = digits.test,
#         digits.beta = digits.beta,
#         format=format,
#         fix_format = TRUE,
#         conf.style.1 = TRUE,
#         ...
#       )
#
#
#       if (include.stars) {
#
#
#      pos_star <-  grep("stars", names(model) )
#         model[[2]] <- paste0(unlist(model[[2]]), model[[pos_star]])
#         model <- model[-pos_star]
#       }
#
#      names(model) <- sapply(names(model),
#                              function(y) if (y %in% names(dictionary)) dictionary[y] else  y,
#                              USE.NAMES = FALSE)
#
#       if (i == 1) {
#         coefs <- model
#         coef.order <- unique(model$term)
#       }
#       else {
#         coef.order <- unique(c(coef.order, model$term))
#         coefs <- merge(
#           coefs, model,
#           by = 1, all = TRUE, suffixes = c("", custom.model.names.s[i])
#         )
#       }
#       n_param[i] <- ncol(model) - 1 #auszaehlen an parametern
#     }
#
#
#     if (n > 1) {
#       n_names <-
#         stringr::str_split(names(coefs)[-1], mySep, simplify = TRUE)
#       suffix <- n_names[, 2]
#       param <-  n_names[, 1]
#       suffix[which(suffix == "")] <- custom.model.names[1]
#       names(coefs)[-1] <-  paste0(suffix, "_", param)
#       coefs <- coefs[order(match(coefs$term, coef.order)),]
#     }
#
#     if (include.gof) {
#      gofs- list()
#       for (i in seq_along(i)) {
#         model <- t(
#           extract_gof(
#             x[[i]],
#             include.r = include.r,
#             include.pseudo = include.pseudo,
#             include.rmse = include.rmse,
#             include.sigma = include.sigma,
#             include.variance = include.variance,
#             include.devianze = include.devianze,
#             include.loglik = include.loglik,
#             include.test = include.test,
#             include.aic = include.aic,
#             include.bic = include.bic,
#             include.nobs = include.nobs,
#             fix_format = TRUE
#           )
#         )
#
#        # model <- tibble::tibble(term = rownames(model),
#         #                       model = model[,1])
#
#
#         gofs[[paste0("m", i)]] <- tibble::tibble(term = rownames(model),
#                                                       model = model[, 1])
#
#
#
#
#
#        # # gof_new[[i]]<-model
#        #  if (i == 1) {
#        #    gofs <- model
#        #    gofs.order <- unique(model$term)
#        #  }
#        #  else {
#        #    gofs.order <- unique(c(gofs.order, model$term))
#        #    gofs <- merge(
#        #      gofs,
#        #      model,
#        #      by = 1,
#        #      all = TRUE,
#        #      suffixes = c("",  paste0("_", i))
#        #    )
#        #  }
#
#
#
#       }
#    #  print(list_to_df(gof_new))
#     #  print(gof_new)
#     #  print(gofs)
#
#      gofs<- list_to_df(gofs, last = "Obs")
#     #  gofs <- gofs[order(match(gofs$term, gofs.order)),]
#
#    #   print(gofs)
#       if (!is.null(include.custom)) {
#
#         if (inherits(include.custom, "data.frame")) {
#         #  print(names(gofs))
#
#           names(include.custom) <- names(gofs)
#           gofs <-  rbind(gofs, include.custom)
#         }  else if (inherits(include.custom, "list")) {
#           gofs <-  rbind(gofs,
#                          cbind(
#                            term = names(include.custom),
#                            matrix(
#                              unlist(include.custom),
#                              nrow = length(include.custom),
#                              byrow = TRUE,
#                              dimnames = list(names(include.custom), names(gofs)[-1])
#                            )
#                          ))
#         }
#       }
#
#       n_row <- nrow(gofs)
#       j <- 1
#       for (i in seq_len(n)) {
#         n_col <- n_param[i] - 1
#         j <-  j + 1
#         empty_gofs <-
#           tibble::as_tibble(matrix(rep(NA, n_row * n_col),
#                                    nrow = n_row),
#                             .name_repair = "minimal")
#         gofs <- append(unclass(gofs), empty_gofs, after = j)
#         j <- n_col + j
#       }
#
#       gofs <- dplyr::bind_cols(gofs)
#       names(gofs) <- names(coefs)
#
#       if (include.param) {
#         result <-
#           prepare_output(rbind(coefs, gofs), caption, note, include.n = NULL)
#         n.rgroup <- nrow(coefs)
#       }
#       else
#       {
#         result <- prepare_output(gofs,
#                                  caption, note, include.n = NULL)
#         rgroup <- n.rgroup <- NULL
#       }
#     } else if (include.param) {
#       result <- prepare_output(coefs, caption, note, include.n = NULL)
#
#       rgroup <- n.rgroup <- NULL
#     }
#     else{
#       result <- NULL
#     }
#
#     if (!is.logical(output)) {
#       Output(
#         result,
#         output = output,
#         col_names = col_names,
#         rgroup = rgroup,
#         n.rgroup = n.rgroup
#       )
#     }
#     invisible(result)
# }
