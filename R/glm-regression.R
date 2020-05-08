# glm-regression
# Fitting Generalized Linear Models

#' @rdname APA
#' @export
APA.glm <- function(x, ...) {
  lrtst <-  lmtest::lrtest(x)
  paste0("LogLik=",
         Format2(lrtst[2, 2], 2),
         ", ",
         rndr_X(lrtst[2, 4],
                lrtst[1, 1],
                lrtst[2, 1],
                lrtst[2, 5]))
}

#' @rdname APA2
#' @export
#'
APA2.glm <- function(x,
                     caption = NULL, note = NULL,
                     output = stp25output::which_output(),
                     col_names = NULL,
                     include.b = TRUE, include.se = TRUE,
                     include.ci = FALSE,
                     include.odds = TRUE, include.odds.ci =  include.ci,
                     include.statistic = TRUE,
                     include.p = TRUE, include.stars = FALSE,
                     include.r = TRUE, include.pseudo = include.r,
                     include.test = FALSE,
                     include.rr = FALSE, include.rr.ci = include.ci,
                     ci.level = .95, conf.method = "Wald",
                     conf.style.1 =TRUE,
                     digits = 2,
                     ...) {
 
  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
  
  if (is.null(note)) {
    note <-  paste0("Model: ", info$family[1])
    if (include.test)
      note <- paste(note, APA(x, FALSE))
    if (include.r) {
 
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
    include.stars =include.stars,
    ci.level = ci.level,
    conf.method = conf.method,
    fix_format = TRUE, conf.style.1 = conf.style.1
  )
  
  if (conf.method == "Wald") {
    note <- paste(note, "Wald-Test")
  } else {
    note <- paste(note, "LR-Test")
  }
  
  # Relaltv-Risk   include.rr
  if (include.rr) {
    rr <-  sjstats::odds_to_rr(x)
    rr <- data.frame(RR = rndr_ods(rr[, 1]),
                     RR.CI = rndr_ods_CI(rr[, 2:3]))
    
    if (rownames(coefs)[1] == "(Intercept)")
      rr[1, 1:2] <- NA
    
    if (include.rr.ci)
      coefs <- cbind(coefs, rr)
    else
      coefs <- cbind(coefs, rr[, 1])
  }
  
  res <- prepare_output(coefs,
                        caption, note, info$N, info$labels)
  
  if (!is.logical(output))
    Output(res, output = output, col_names = col_names)
  
  invisible(res)
}