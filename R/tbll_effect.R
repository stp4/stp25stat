#' Effect Displays
#' Workaraund fuer effect {effects}
#'
#' @param x,formula    fit  und effecte
#' @param ... param an effect transformation = list(link = log, inverse = exp)
#' @param caption,include.se,include.ci,include.n,digits an Output
#'
#' @return data.frame ore  list with data.frame
#' @export
#'
#' @examples
#' 
#' require(effects)
#'  # require(stpvers)
#'  
#' mod <- lm(prestige ~ type * (education + income) + women, Prestige)
#' Tbll_effect(mod, ~ type * education + women)
#'
#' DF <- data.frame(
#'   
#'   y=c(12, 15, 23, 44, 11, 44, 49, 27, 25,  8, 
#'       11, 10, 28, 15, 34, 20, 31, 55, 15, 34, 
#'       47, 11, 27,  9, 12, 15,  7, 15, 42, 14,
#'       19, 24, 20, 10, 38, 28, 36,  9, 31),
#'   iq =c(91,  95, 103, 116,  88, 116, 118, 106, 105,  82, 
#'         88,  87, 107,  95, 111, 100, 109, 120,  95, 111, 
#'         117,  88, 106,  85,  91,  95,  81,  95, 115,  94,  
#'         99, 104, 100,  87, 113, 107, 112,  84, 109 )
#' )
#' DF$log.y =  log(DF$y) 
#' 
#' 
#' (fit.linear <- lm(y ~ iq, DF))
#' (fit.log.y <- lm(log.y ~ iq, DF))
#' (fit.model.log<- lm(log(y)~iq, DF))
#' 
#' p1 <-
#'   plot(effect("iq", fit.linear , 
#'               partial.residuals = TRUE), 
#'        main = "y (linear)")
#' 
#' p2 <- 
#'   plot(effect("iq", fit.log.y, 
#'               partial.residuals = TRUE), 
#'        main = "log")
#' 
#' p3 <- plot(effect("iq",  fit.log.y,
#'                   partial.residuals = TRUE,
#'                   transformation =  list(link =  log,  inverse = exp)),
#'            main = "log + trans")
#' 
#' p4 <- plot(effect("iq", fit.model.log , 
#'                   partial.residuals = TRUE), 
#'            main = "log(y)")
#' 
#' p5 <-   plot(effect("iq", fit.model.log , 
#'                     partial.residuals = TRUE,
#'                     transformation =  list(link =  log,  inverse = exp)), 
#'              main = "log(y) + trans")
#' 
#' require(cowplot)
#' plot_grid(p1,p4, p5,  p2, p3, ncol = 3)
#' 
#' cbind(
#'   Tbll(effect("iq", fit.linear), include.ci=FALSE),
#'   log.y=Tbll(effect("iq", fit.log.y), include.ci=FALSE)[[2]],
#'   log.y.trans= Tbll(effect("iq", fit.log.y,   
#'                            transformation = list(link = log, inverse = exp)), include.ci=FALSE)[[2]],
#'   model.log=Tbll(effect("iq", fit.model.log), include.ci=FALSE)[[2]],
#'   model.log.trans= Tbll(effect("iq", fit.model.log,   
#'                                transformation =  list(link = log, inverse = exp)), include.ci=FALSE)[[2]]
#' )

Tbll_effect <-
  function(x,
           formula = NULL,
           ...,
           caption = "Predictor Effects",
           include.se = FALSE,
           include.ci = TRUE,
           include.n = FALSE,
           digits = 2) {
 
    if (is.null(formula))
      Tbll(
        effects::allEffects(x, ...),
        include.se = include.se,
        include.ci = include.ci,
        include.n = include.n,
        digits = digits
      )
    else {
      
      formula <- gsub(" ", "", strsplit(as.character(formula), "\\+")[[2L]])
   
      if (length(formula) == 1) {
        Tbll(
          effects::effect(formula, x, ...),
          include.se = include.se,
          include.ci = include.ci,
          include.n = include.n,
          digits = digits
        )
      } else{
        rslt <- list()
        for (i in  formula)
          rslt[[i]] <- Tbll(
            effects::effect(i, x, ...),
            include.se = include.se,
            include.ci = include.ci,
            include.n = include.n,
            digits = digits
          )
        rslt
      }
    }
  }



#' @rdname Tbll
#' @export
#' @examples
#'
#' require(effects)
#' fit1 <-
#'   lm (Sepal.Length ~ Sepal.Width * Species + Petal.Width, data = iris)
#' fit2 <-
#'   lm (log(Sepal.Length) ~ Sepal.Width * Species + Petal.Width, data = iris)
#'
#' x1 <- allEffects(fit1)
#' x2 <- allEffects(fit2,
#'                  transformation = list(link = log, inverse = exp))
#' x3 <- effect("Petal.Width",  fit1)
#' x4 <- effect("Petal.Width",  fit2,
#'              transformation = list(link = log, inverse = exp))
#' Tbll(x1)
#' Tbll(x3)
#' Tbll(x2)
#' Tbll(x4)
#' 
tbll_extract.eff <- function(x,  ...) { tbll_extract_eff(x, ...) }

#' @rdname Tbll
#' @export
#'
tbll_extract.efflist <- function(x, ...) { tbll_extract_eff(x, ...) }  




#' @param x,i Werte ueber die for schleife
#' 
#' @noRd
#' 
#' extract_n
extract_n <- function (x, i) {
  if (inherits(x, "eff"))
    x <- list(i = x)
  y <- names(x[[i]]$data)[1L]
  
  fm <-
    formula(paste0(y, "~", paste0(names(x[[i]]$variables), collapse = "+")))
  
  var_is_factor <-
    lapply(x[[i]]$variables, function(z)
      z$is.factor)
  var_source <- lapply(x[[i]]$variables, function(z)
    z$levels)
  for (j in names(var_source)) {
    if (!var_is_factor[[i]])
      x[[i]]$data[[j]] <-
        cut(x[[i]]$data[[j]], length(var_source[[j]]))
  }
  rslt_n <- aggregate(
    fm,
    x[[i]]$data,
    FUN = function(n)
      length(n),
    drop = FALSE
  )
  rslt_n[[ncol(rslt_n)]]
}

#' @param x objekt
#'
#' @param type fuer transform "response", "link"
#' 
#' @noRd
#'  
#' effects_as.data.frame.eff
#' 
#' orginal geht nicht  "Tue Apr 20 11:53:58 2021" primär habe ich 
#' x$transformation$inverse ergaenzt
#' 
effects_as.data.frame.eff <-
  function (x,
          #  row.names = NULL,
           # optional = TRUE,
            type = c("response", "link")
          ) {
    type <- match.arg(type)
    linkinv <- if (is.null(x$link$linkinv))
      I
    else
      x$link$linkinv
    linkmu.eta <- if (is.null(x$link$mu.eta))
      function(x)
        NA
    else
      x$link$mu.eta
    xx <- x$x
    for (var in names(xx)) {
      if (is.factor(xx[[var]])) {
        xx[[var]] <- addNA(xx[[var]])
      }
    }
    x$x <- xx
    
    result <- switch(type,
                     response = {
                       data.frame(
                         x$x,
                         fit = x$transformation$inverse(x$fit),
                         se = x$transformation$inverse(x$fit) * x$se,
                         lower = x$transformation$inverse(x$lower),
                         upper = x$transformation$inverse(x$upper)
                       )
                     }, link = {
                       data.frame(
                         x$x,
                         fit = x$fit,
                         se = x$se,
                         lower = x$lower,
                         upper = x$upper
                       )
                     })
    attr(result, "type") <- type
    result
  }



#' @param x objekt
#'
#' @param caption ueberschrift
#' @param include.fit,include.se,include.ci,include.n include
#' @param include.format Zahl oder Text
#' @param digits Nachkomastellen
#' @param type fuer transform "response", "link"
#' @param ... nicht benutzt abfangen von zn note
#'
#' @noRd
#'  
#'  tbll_extract_eff
#'  
#'  das ist die eigendliche Funktion die sowol efflist als auch eff  aufloest.
#'  
tbll_extract_eff <-
  function(x,
           caption = "",
           include.fit = TRUE,
           include.se = FALSE,
           include.ci = TRUE,
           include.n = FALSE,
           include.format = TRUE,
           digits = 2,
           type = c("response", "link"),
           ...)  {
 
    type <- match.arg(type)
    rslt <- NULL
    if (inherits(x, "eff")) {
      rslt[[1]] <- effects_as.data.frame.eff(x, type = type)
    }
    else{
      rslt <-  lapply(x, effects_as.data.frame.eff, type = type)
    }
    
    if (!include.format)
      return(if (length(rslt) == 1)
        rslt[[1]]
        else
          rslt)
    
    for (i in seq_along(rslt)) {
      if (include.fit & include.ci) {
        rslt[[i]]$value <-
          stp25rndr::rndr_mean_CI(rslt[[i]]$fit, cbind(rslt[[i]]$lower, rslt[[i]]$upper), digits = digits)
        note <- "mean [95%-CI]"
      }
      else if (include.fit & include.se) {
        rslt[[i]]$value  <-
          stp25rndr::rndr_mean(rslt[[i]]$fit,  rslt[[i]]$se, digits)
        note <- "mean (SE)"
      }
      else if (include.fit) {
        rslt[[i]]$value <-
          stp25rndr::Format2(rslt[[i]]$fit, digits = digits)
        note <- "mean"
      }
      else {
        return(rslt[[i]])
      }
      
      
      if (include.n) {
        rslt[[i]]$value <- paste0("(", extract_n(x, i), ") ", rslt[[i]]$value)
      }
      
      rslt[[i]] <-  rslt[[i]][-(1:4 + (ncol(rslt[[i]]) - 1 - 4))]
      if (ncol(rslt[[i]]) == 2)
        rslt[[i]] <-
        stp25stat::prepare_output(rslt[[i]], caption = caption, note = note)
      else {
        rslt[[i]] <- stp25stat::prepare_output(
          tidyr::pivot_wider(rslt[[i]],
                             names_from = 2,
                             values_from = "value"),
          caption = caption,
          note = note
        )
      }
    }
    if (length(rslt) == 1)
      rslt[[1]]
    else
      rslt
  }




#' @rdname Effsize
#' @export
APA2.eff <- function(x,
                     caption = "Effekte: ",
                     note = "",
                     output = stp25output::which_output(),
                     ...) {
  res <- tbll_extract(x, caption =  caption, ...)
  
  Output(res, output = output)
  
  invisible(res)
}


#' @rdname Effsize
#' @export
#' @examples
#'
#' #' # Effekte / Mittelwerte
#' fit1 <- lm(chol0 ~  ak + rrs0 + med + g, hyper)
#' Tabelle2(fit1, digits = 2)  # mean SD
#'
#' eff <- effects::allEffects(fit1)
#'
#' APA2(eff)
#'
APA2.efflist <- function(x,
                         caption = "Effekte: ",
                         note = "",
                         output = stp25output::which_output(),
                         ...) {
  res <- tbll_extract(x, caption =  caption, ...)
  for (i in names(res)) {
    Output(res[[i]],
           caption=paste(caption, i), 
           output=output)
  }
  invisible(res)
}
  
  
#APA2.eff
# if (names(x)[2] %in% "formula") {
#   efflist <- list(Effect = x)
#   APA2.efflist(efflist, ...)
# }
# else
#   "Weis nich was das ist?"

#APA2.efflist
#   if (is.null(type)) {
#     if (include.fit)
#       type <- "fit"
#     if (include.se)
#       type <- c(type, "se")
#     if (include.ci)
#       type <- c(type, "lower", "upper")
#     if (include.n)
#       type <- c("N", type)
#     type <- setdiff(c("N", "fit", "se", "lower", "upper"), type)
#   }
#   else{
#     type <- setdiff(c("N", "fit", "se", "lower", "upper"), type)
#   }
#   
#   res <- fix_eff_to_df(x, caption =  caption,
#                        note = note)
#   for (i in names(res)) {
#     
#     # cat("\n", i ,"\n")
#     spalte = which(names(res[[i]]) %in% type)
#     
#     
#     Output(fix_format(res[[i]][-spalte], digits = digits),
#            caption=paste(caption, i), output=output)
#   }
#   invisible(res)
# }


# fix_eff_to_df <- function(x, caption, note ) {
#   res_list <- NULL
#   for (i in names(x)) {
#     info <- model_info(x[[i]])
#     AV <- ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
#     ans <- as.data.frame(x[[i]])
#     n<- ncol(ans)
#     
#     ans[1:(n-4) ] <- lapply(ans[1:(n-4)], as.character)
#     
#     myN <- aggregate_effect(x[[i]], info$y, info$x)
#     #- aggregate verwirft Leere Eintraege
#     if (nrow(ans) == nrow(myN)) {
#       ans$N <- Format2(myN[, ncol(myN)], 0)
#       attr(ans, "note") = ""
#     }
#     else{
#       ans$N <- NA
#       attr(ans, "note") = "Warnung: Die Stichprobe ist relativ klein sodass die Anzahl nicht berechnet werden kann."
#     }
#     attr(ans, "caption") =  paste0("AV: ", AV)
#     attr(ans, "N") = info$N
#     attr(ans, "labels") = info$labels
#     
#     
#     
#     res_list[[i]] <- ans
#   }
#   res_list
# }




# 
# tbll_extract_eff <-
#   function(x,
#            caption = "",
#            include.fit = TRUE,
#            include.se = FALSE,
#            include.ci = TRUE,
#            include.n = FALSE,
#            include.format = TRUE,
#            digits = 2,
#            type = c("link", "response"),
#           
#            ...)
#   {
#     type <- match.arg(type)
#     rslt <- NULL
#     if (inherits(x, "eff")) rslt[[1]] <- effects:::as.data.frame.eff(x, type = type)
#     else rslt <- effects:::as.data.frame.efflist(x, type = type)
#     
#     if (!include.format)
#       return(if (length(rslt) == 1)
#         rslt[[1]]
#         else
#           rslt)
#     
#     for (i in seq_along(rslt)) {
#       if (include.fit & include.ci) {
#         rslt[[i]]$value <-
#           stp25rndr::rndr_mean_CI(rslt[[i]]$fit, cbind(rslt[[i]]$lower, rslt[[i]]$upper), digits = digits)
#         note <- "mean [95%-CI]"
#       }
#       else if (include.fit & include.se) {
#         rslt[[i]]$value  <-
#           stp25rndr::rndr_mean(rslt[[i]]$fit,  rslt[[i]]$se, digits)
#         note <- "mean (SE)"
#       }
#       else if (include.fit) {
#         rslt[[i]]$value <-
#           stp25rndr::Format2(rslt[[i]]$fit, digits = digits)
#         note <- "mean"
#       }
#       else {
#         return(rslt[[i]])
#       }
#       
#       
#       if (include.n) {
#         rslt[[i]]$value <- paste(extract_n(x, i),  rslt[[i]]$value)
#       }
#       
#       rslt[[i]] <-  rslt[[i]][-(1:4 + (ncol(rslt[[i]]) - 1 - 4))]
#       if (ncol(rslt[[i]]) == 2)
#         rslt[[i]] <-
#         stp25stat::prepare_output(rslt[[i]], caption = caption, note = note)
#       else {
#         rslt[[i]] <- stp25stat::prepare_output(
#           tidyr::pivot_wider(rslt[[i]],
#                              names_from = 2,
#                              values_from = "value"),
#           caption = caption,
#           note = note
#         )
#       }
#     }
#     if  (length(rslt)==1)   rslt[[1]]
#     else rslt
#   }
# 
#  
# tbll_extract_eff(eff1, transformation = trs)

#tbll_extract(eff2)



# 
# ectract_n <- function(x) {
#   y <- names(x$data)[1L]
#   fm <- formula(paste0(y, "~", paste0(names(x$variables), collapse = "+")))
#   
#   var_is_factor <- lapply(x$variables, function(z)
#     z$is.factor)
#   var_source <- lapply(x$variables, function(z)
#     z$levels)
#   var_length <- sapply(x$variables, function(z)
#     length(z$levels))
#   
#   for (i in names(var_source)) {
#     if (!var_is_factor[[i]])
#       x$data[[i]] <- cut(x$data[[i]] , length(var_source[[i]]))
#   }
#   res <- aggregate(
#     fm,
#     x$data,
#     FUN = function(x)
#       length(x),
#     drop = FALSE
#   )
#   
#   if (length(var_length) > 1)
#     cbind(var_source[1], as.data.frame(array(
#       res[[ncol(res)]],
#       dim = var_length,
#       dimnames = var_source
#     )))
#   else
#     data.frame(var_source,  res[[ncol(res)]])
# }
# 
# 
# 








# 
# #effects:::Effect.lm
# efffffff <-
#   function (focal.predictors,
#             mod,
#             xlevels = list(),
#             fixed.predictors,
#             vcov. = vcov,
#             se = TRUE,
#             residuals = FALSE,
#             quantiles = seq(0.2,
#                             0.8, by = 0.2),
#             x.var = NULL,
#             ...,
#             given.values,
#             typical,
#             offset,
#             confint,
#             confidence.level,
#             partial.residuals,
#             transformation)
# 
#   {
#     if (is.numeric(xlevels)) {
#       if (length(xlevels) > 1 || round(xlevels != xlevels))
#         stop("xlevels must be a single whole number or a list")
#       form <- Effect.default(NULL, mod)
#       terms <- attr(terms(form), "term.labels")
#       predictors <- all.vars(parse(text = terms))
#       xlevs <- list()
#       for (pred in predictors) {
#         xlevs[[pred]] <- xlevels
#       }
#       xlevels <- xlevs
#     }
#     if (!missing(partial.residuals))
#       residuals <- partial.residuals
#     partial.residuals <- residuals
#     if (missing(transformation))
#       transformation <-
#       list(link = family(mod)$linkfun,
#            inverse = family(mod)$linkinv)
#     if (missing(fixed.predictors))
#       fixed.predictors <- NULL
#     fixed.predictors <- applyDefaults(
#       fixed.predictors,
#       list(
#         given.values = NULL,
#         typical = mean,
#         apply.typical.to.factors = FALSE,
#         offset = mean
#       ),
#       arg = "fixed.predictors"
#     )
#     if (missing(given.values))
#       given.values <- fixed.predictors$given.values
#     if (!is.null(given.values)) {
#       if (given.values[1] == "default")
#         given.values <- NULL
#       if (given.values[1] == "equal")
#         given.values <- .set.given.equal(mod)
#     }
#     if (missing(typical))
#       typical <- fixed.predictors$typical
#     if (missing(offset))
#       offset <- fixed.predictors$offset
#     apply.typical.to.factors <-
#       fixed.predictors$apply.typical.to.factors
#     if (!missing(confint))
#       se <- confint
#     confint <- applyDefaults(
#       se,
#       list(
#         compute = TRUE,
#         level = 0.95,
#         type = "pointwise"
#       ),
#       onFALSE = list(
#         compute = FALSE,
#         level = 0.95,
#         type = "pointwise"
#       ),
#       arg = "se"
#     )
#     se <- confint$compute
#     if (missing(confidence.level))
#       confidence.level <- confint$level
#     confidence.type <- match.arg(confint$type, c("pointwise",
#                                                  "Scheffe", "scheffe"))
#     default.levels <- NULL
#     data <- if (partial.residuals) {
#       all.vars <- all.vars(formula(mod))
#       expand.model.frame(mod, all.vars)[, all.vars]
#     }
#     else
#       NULL
#     if (!is.null(given.values) &&
#         !all(which <- names(given.values) %in%
#              names(coef(mod))))
#       stop("given.values (", names(given.values[!which]),
#            ") not in the model")
#     off <- if (is.numeric(offset) && length(offset) == 1)
#       offset
#     else if (is.function(offset)) {
#       mod.off <- model.offset(model.frame(mod))
#       if (is.null(mod.off))
#         0
#       else
#         offset(mod.off)
#     }
#     else
#       stop("offset must be a function or a number")
#     formula.rhs <- formula(mod)[[3]]
#     if (!missing(x.var)) {
#       if (!is.numeric(x.var)) {
#         x.var.name <- x.var
#         x.var <- which(x.var == focal.predictors)
#       }
#       if (length(x.var) == 0)
#         stop("'", x.var.name, "' is not among the focal predictors")
#       if (length(x.var) > 1)
#         stop("x.var argument must be of length 1")
#     }
#     model.components <- Analyze.model(
#       focal.predictors,
#       mod,
#       xlevels,
#       default.levels,
#       formula.rhs,
#       partial.residuals = partial.residuals,
#       quantiles = quantiles,
#       x.var = x.var,
#       data = data,
#       typical = typical
#     )
#     excluded.predictors <- model.components$excluded.predictors
#     predict.data <- model.components$predict.data
#     predict.data.all.rounded <-
#       predict.data.all <- if (partial.residuals)
#         na.omit(data[, all.vars(formula(mod))])
#     else
#       NULL
#     factor.levels <- model.components$factor.levels
#     factor.cols <- model.components$factor.cols
#     n.focal <- model.components$n.focal
#     x <- model.components$x
#     X.mod <- model.components$X.mod
#     cnames <- model.components$cnames
#     X <- model.components$X
#     x.var <- model.components$x.var
#     formula.rhs <- formula(mod)[c(1, 3)]
#     Terms <- delete.response(terms(mod))
#     mf <- model.frame(Terms, predict.data, xlev = factor.levels,
#                       na.action = NULL)
#     mod.matrix <-
#       model.matrix(formula.rhs,
#                    data = mf,
#                    contrasts.arg = mod$contrasts)
#     if (is.null(x.var))
#       partial.residuals <- FALSE
#     factors <- sapply(predict.data, is.factor)
#     if (partial.residuals) {
#       for (predictor in focal.predictors[-x.var]) {
#         if (!factors[predictor]) {
#           values <- unique(predict.data[, predictor])
#           predict.data.all.rounded[, predictor] <-
#             values[apply(outer(predict.data.all[,
#                                                 predictor], values, function(x, y)
#                                                   (x - y) ^ 2),
#                          1,
#                          which.min)]
#         }
#       }
#     }
#     mod.matrix.all <- model.matrix(mod)
#     wts <- weights(mod)
#     if (is.null(wts))
#       wts <- rep(1, length(residuals(mod)))
#     mod.matrix <- Fixup.model.matrix(
#       mod,
#       mod.matrix,
#       mod.matrix.all,
#       X.mod,
#       factor.cols,
#       cnames,
#       focal.predictors,
#       excluded.predictors,
#       typical,
#       given.values,
#       apply.typical.to.factors
#     )
#     null.basis <- estimability::nonest.basis(mod)
#     is.estimable <- estimability::is.estble(mod.matrix, null.basis)
#     scoef <- ifelse(is.na(mod$coefficients), 0L, mod$coefficients)
#     effect <- off + mod.matrix %*% scoef
#     effect[!is.estimable] <- NA
#     if (partial.residuals) {
#       res <- na.omit(residuals(mod, type = "working"))
#       fitted <- na.omit(if (inherits(mod, "glm"))
#         predict(mod, type = "link")
#         else
#           predict(mod))
#       partial.residuals.range <- range(fitted + res)
#     }
#     else {
#       res <- partial.residuals.range <- NULL
#     }
#     result <- list(
#       term = paste(focal.predictors, collapse = "*"),
#       formula = formula(mod),
#       response = response.name(mod),
#       variables = x,
#       fit = effect,
#       x = predict.data[, 1:n.focal,
#                        drop = FALSE],
#       x.all = predict.data.all.rounded[,
#                                        focal.predictors, drop = FALSE],
#       model.matrix = mod.matrix,
#       data = X,
#       discrepancy = 0,
#       offset = off,
#       residuals = res,
#       partial.residuals.range = partial.residuals.range,
#       x.var = x.var
#     )
#     if (se) {
#       if (any(family(mod)$family == c("binomial", "poisson"))) {
#         z <- if (confidence.type == "pointwise") {
#           qnorm(1 - (1 - confidence.level) / 2)
#         }
#         else {
#           p <- length(na.omit(coef(mod)))
#           scheffe(confidence.level, p)
#         }
#       }
#       else {
#         z <- if (confidence.type == "pointwise") {
#           qt(1 - (1 - confidence.level) / 2, df = mod$df.residual)
#         }
#         else {
#           p <- length(na.omit(coef(mod)))
#           scheffe(confidence.level, p, mod$df.residual)
#         }
#       }
#       V <- vcov.(mod, complete = FALSE)
#       mmat <- mod.matrix[,!is.na(mod$coefficients)]
#       eff.vcov <- mmat %*% V %*% t(mmat)
#       rownames(eff.vcov) <- colnames(eff.vcov) <- NULL
#       var <- diag(eff.vcov)
#       result$vcov <- eff.vcov
#       result$se <- sqrt(var)
#       result$se[!is.estimable] <- NA
#       result$lower <- effect - z * result$se
#       result$upper <- effect + z * result$se
#       result$confidence.level <- confidence.level
#     }
#     if (is.null(transformation$link) &&
#         is.null(transformation$inverse)) {
#       transformation$link <- I
#       transformation$inverse <- I
#     }
#     result$transformation <- transformation
#     result$family <- family(mod)$family
#     result$link <- family(mod)
#     class(result) <- "eff"
#     result
#   }
# 
# #effects:::applyDefaults
# applyDefaults <-
# function (args, defaults, onFALSE, arg = "") 
# {
#   if (is.null(args)) 
#     return(defaults)
#   if (isFALSE(args)) {
#     if (missing(onFALSE)) 
#       return(FALSE)
#     else return(onFALSE)
#   }
#   names <- names(args)
#   names <- names[names != ""]
#   if (!isTRUE(args) && length(names) != length(args)) 
#     warning("unnamed ", arg, " arguments, will be ignored")
#   if (isTRUE(args) || is.null(names)) 
#     defaults
#   else defaults[names] <- args[names]
#   as.list(defaults)
# }
# 
# 
# 
# 
# #effects:::Analyze.model
# Analyze.model<-
# function (focal.predictors, mod, xlevels, default.levels = NULL, 
#           formula.rhs, partial.residuals = FALSE, quantiles, x.var = NULL, 
#           data = NULL, typical = mean) 
# {
#   if ((!is.null(mod$nan.action)) && inherits(mod$na.action, 
#                                              "exclude")) 
#     class(mod$na.action) <- "omit"
#   all.predictors <- all.vars(formula.rhs)
#   check.vars <- !(focal.predictors %in% all.predictors)
#   excluded.predictors <- setdiff(all.predictors, focal.predictors)
#   number.bad <- sum(check.vars)
#   if (any(check.vars)) {
#     message <- if (number.bad == 1) 
#       paste("the following predictor is not in the model:", 
#             focal.predictors[check.vars])
#     else paste("the following predictors are not in the model:", 
#                paste(focal.predictors[check.vars], collapse = ", "))
#     stop(message)
#   }
#   X.mod <- model.matrix(mod)
#   cnames <- colnames(X.mod)
#   factor.cols <- rep(FALSE, length(cnames))
#   names(factor.cols) <- cnames
#   for (name in all.predictors) {
#     if (is.factor.predictor(name, mod)) {
#       factor.cols[grep(paste("^", name, sep = ""), 
#                        cnames)] <- TRUE
#     }
#   }
#   factor.cols[grep(":", cnames)] <- FALSE
#   X <- na.omit(expand.model.frame(mod, all.predictors))
#   which.matrices <- sapply(X, function(x) is.matrix(x) && ncol(x) == 
#                              1)
#   if (any(which.matrices)) {
#     nms <- names(which.matrices[which.matrices])
#     msg <- if (length(nms) > 1) {
#       paste("the predictors", paste(nms, collapse = ", "), 
#             "are one-column matrices that were converted to vectors")
#     }
#     else {
#       paste("the predictor", nms, "is a one-column matrix that was converted to a vector")
#     }
#     warning(msg)
#     for (nm in nms) {
#       X[, nm] <- as.vector(X[, nm])
#     }
#   }
#   for (name in all.predictors) {
#     if (is.factor.predictor(name, mod) && is.null(xlevels[[name]])) {
#       xlevels[[name]] <- levels(X[, name])
#     }
#   }
#   bad <- sapply(X[, all.predictors, drop = FALSE], function(x) !(is.factor(x) || 
#                                                                    is.numeric(x)))
#   if (any(bad)) {
#     message <- if (sum(bad) == 1) 
#       paste("the following predictor isn't a factor, logical, character, or numeric:", 
#             all.predictors[bad])
#     else paste("the following predictors aren't factors, logical, character, or numeric:", 
#                paste(all.predictors[bad], collapse = ", "))
#     stop(message)
#   }
#   x <- list()
#   factor.levels <- list()
#   if (length(xlevels) == 0 & length(default.levels) == 1L) 
#     xlevels <- default.levels
#   if (is.numeric(xlevels) & length(xlevels) == 1L) {
#     levs <- xlevels
#     for (name in focal.predictors) xlevels[[name]] <- levs
#   }
#   for (name in focal.predictors) {
#     levels <- mod$xlevels[[name]]
#     if (is.null(levels)) 
#       levels <- mod$xlevels[[paste("factor(", name, 
#                                    ")", sep = "")]]
#     fac <- !is.null(levels)
#     if (!fac) {
#       levels <- if (is.null(xlevels[[name]])) {
#         if (partial.residuals) {
#           quantile(X[, name], quantiles)
#         }
#         else {
#           nice(seq(min(X[, name]), max(X[, name]), length.out = 5))
#         }
#       }
#       else {
#         if (length(xlevels[[name]]) == 1L) {
#           nice(seq(min(X[, name]), max(X[, name]), length = xlevels[[name]]))
#         }
#         else xlevels[[name]]
#       }
#     }
#     else factor.levels[[name]] <- levels
#     x[[name]] <- list(name = name, is.factor = is.factor(X[, 
#                                                            name]), levels = levels)
#   }
#   if (partial.residuals) {
#     numeric.predictors <- sapply(focal.predictors, function(predictor) is.numeric.predictor(predictor, 
#                                                                                             mod))
#     if (is.null(x.var)) {
#       x.var <- if (any(numeric.predictors)) 
#         which(numeric.predictors)[1]
#       else 1
#     }
#     x.var.name <- focal.predictors[x.var]
#     if (is.numeric(X[, x.var.name]) && is.null(xlevels[[x.var.name]])) {
#       x.var.range <- range(X[, focal.predictors[x.var]])
#       x[[x.var]][["levels"]] <- seq(from = x.var.range[1], 
#                                     to = x.var.range[2], length = 100)
#     }
#   }
#   x.excluded <- list()
#   for (name in excluded.predictors) {
#     levels <- mod$xlevels[[name]]
#     if (is.logical(X[, name])) 
#       levels <- c("FALSE", "TRUE")
#     fac <- !is.null(levels)
#     level <- if (fac) 
#       levels[1]
#     else typical(X[, name])
#     if (fac) 
#       factor.levels[[name]] <- levels
#     x.excluded[[name]] <- list(name = name, is.factor = fac, 
#                                level = level)
#   }
#   dims <- sapply(x, function(x) length(x$levels))
#   len <- prod(dims)
#   n.focal <- length(focal.predictors)
#   n.excluded <- length(excluded.predictors)
#   n.vars <- n.focal + n.excluded
#   predict.data <- matrix("", len, n.vars)
#   excluded <- sapply(x.excluded, function(x) x$level)
#   for (i in 1:len) {
#     subs <- subscripts(i, dims)
#     for (j in 1:n.focal) {
#       predict.data[i, j] <- x[[j]]$levels[subs[j]]
#     }
#     if (n.excluded > 0) 
#       predict.data[i, (n.focal + 1):n.vars] <- excluded
#   }
#   colnames(predict.data) <- c(sapply(x, function(x) x$name), 
#                               sapply(x.excluded, function(x) x$name))
#   colclasses <- lapply(X, class)
#   colclasses[colclasses == "matrix"] <- "numeric"
#   colclasses[colclasses == "array"] <- "numeric"
#   predict.data <- matrix.to.df(predict.data, colclasses = colclasses)
#   list(predict.data = predict.data, factor.levels = factor.levels, 
#        factor.cols = factor.cols, focal.predictors = focal.predictors, 
#        n.focal = n.focal, excluded.predictors = excluded.predictors, 
#        n.excluded = n.excluded, x = x, X.mod = X.mod, cnames = cnames, 
#        X = X, x.var = x.var)
# }



#tbll_extract.eff
# caption = "",
# include.se = FALSE,
# include.ci = TRUE,
# include.n = FALSE,
# digits = 2,   
# fit <- x$transformation$inverse(x$fit)
# 
# 
# if (!include.se & !include.ci) {
#   fit <- stp25rndr::Format2(fit, digits = digits)
#   note <- "mean"
# }
# else if (include.ci) {
#   ci_low <- x$transformation$inverse(x$lower)
#   ci_hig <- x$transformation$inverse(x$upper)
#   fit <-
#     stp25rndr::rndr_mean_CI(fit, cbind(ci_low, ci_hig), digits = digits)
#   note <- "mean [95%-CI]"
# } else{
#   se <- x$transformation$inverse(x$se)
#   fit <- stp25rndr::rndr_mean(fit,  se, digits)
#   note <- "mean (SE)"
# }
# var_source <- lapply(x$variables, function(z)
#   z$levels)
# var_length <- sapply(x$variables, function(z)
#   length(z$levels))
# 
# if (length(var_length) > 1)
#   fit <- cbind(var_source[1], as.data.frame(array(
#     fit,
#     dim = var_length,
#     dimnames = var_source
#   )))
# else
#   fit <- data.frame(var_source, fit)
# 
# if (include.n) {
#   n <- ectract_n(x)
#   fit <-  stp25tools::combine_data_frame(n, fit, by = 1)
# 
# }
# 
# prepare_output(fit, caption = caption, note = note)


# extract_n
# 
# orginal geht nicht  "Tue Apr 20 11:53:58 2021"
# 
# function (x, row.names = NULL, optional = TRUE, type = c("response", 
#                                                          "link"), ...) 
# {
#   type <- match.arg(type)
#   linkinv <- if (is.null(x$link$linkinv)) 
#     I
#   else x$link$linkinv
#   linkmu.eta <- if (is.null(x$link$mu.eta)) 
#     function(x) NA
#   else x$link$mu.eta
#   xx <- x$x
#   for (var in names(xx)) {
#     if (is.factor(xx[[var]])) {
#       xx[[var]] <- addNA(xx[[var]])
#     }
#   }
#   x$x <- xx
#   result <- switch(type, response = {
#     if (is.null(x$se)) data.frame(x$x, fit = transform(x$fit)) else data.frame(x$x, 
#                                                                                fit = linkinv(x$fit), se = linkmu.eta(x$fit) * x$se, 
#                                                                                lower = linkinv(x$lower), upper = linkinv(x$upper))
#   }, link = {
#     if (is.null(x$se)) data.frame(x$x, fit = x$fit) else data.frame(x$x, 
#                                                                     fit = x$fit, se = x$se, lower = x$lower, upper = x$upper)
#   })
#   attr(result, "type") <- type
#   result
# }
# 

#tbll_extract.efflist
# rslt <- list()
# for (i in names(x)) {
#   rslt[[i]] <- tbll_extract.eff(
#     x[[i]],
#     caption = paste(caption, i),
#     include.se = include.se,
#     include.ci = include.ci,
#     include.n = include.n,
#     digits = digits
#   )
# }
# rslt