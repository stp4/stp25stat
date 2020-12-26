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
#' mod <- lm(prestige ~ type * (education + income) + women, Prestige)
#' Tbll_effect(mod, ~ type * education + women)
#'
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
#'  require(effects)
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
#'
#'
#'
#' Tbll(x1)
#' Tbll(x3)
#' Tbll(x2)
#' Tbll(x4)
tbll_extract.eff <-
  function(x,
           caption = "",
           include.se = FALSE,
           include.ci = TRUE,
           include.n = FALSE,
           digits = 2,
           ...) {
    fit <- x$transformation$inverse(x$fit)
    if (!include.se & !include.ci) {
      fit <- stp25rndr::Format2(fit, digits = digits)
      note <- "mean"
    }
    else if (include.ci) {
      ci_low <- x$transformation$inverse(x$lower)
      ci_hig <- x$transformation$inverse(x$upper)
      fit <-
        stp25rndr::rndr_mean_CI(fit, cbind(ci_low, ci_hig), digits = digits)
      note <- "mean [95%-CI]"
    } else{
      se <- x$transformation$inverse(x$se)
      fit <- stp25rndr::rndr_mean(fit,  se, digits)
      note <- "mean (SE)"
    }
    var_source <- lapply(x$variables, function(z)
      z$levels)
    var_length <- sapply(x$variables, function(z)
      length(z$levels))
    
    if (length(var_length) > 1)
      fit <- cbind(var_source[1], as.data.frame(array(
        fit,
        dim = var_length,
        dimnames = var_source
      )))
    else
      fit <- data.frame(var_source, fit)
    
    if (include.n) {
      n <- ectract_n(x)
      fit <-  stp25tools::combine_data_frame(n, fit, by = 1)
 
    }
    
    prepare_output(fit, caption = caption, note = note)
  }




ectract_n <- function(x) {
  y <- names(x$data)[1L]
  fm <- formula(paste0(y, "~", paste0(names(x$variables), collapse = "+")))
  
  var_is_factor <- lapply(x$variables, function(z)
    z$is.factor)
  var_source <- lapply(x$variables, function(z)
    z$levels)
  var_length <- sapply(x$variables, function(z)
    length(z$levels))
  
  for (i in names(var_source)) {
    if (!var_is_factor[[i]])
      x$data[[i]] <- cut(x$data[[i]] , length(var_source[[i]]))
  }
  res <- aggregate(
    fm,
    x$data,
    FUN = function(x)
      length(x),
    drop = FALSE
  )
  
  if (length(var_length) > 1)
    cbind(var_source[1], as.data.frame(array(
      res[[ncol(res)]],
      dim = var_length,
      dimnames = var_source
    )))
  else
    data.frame(var_source,  res[[ncol(res)]])
}








#' @rdname Tbll
#' @export
#'
tbll_extract.efflist <-
  function(x,
           caption = "",
           include.se = FALSE,
           include.ci = TRUE,
           include.n = FALSE,
           digits = 2,
           ...) {
    rslt <- list()
    for (i in names(x)) {
      rslt[[i]] <- tbll_extract.eff(
        x[[i]],
        caption = paste(caption, i),
        include.se = include.se,
        include.ci = include.ci,
        include.n = include.n,
        digits = digits
      )
    }
    rslt
  }


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