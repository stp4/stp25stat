# lm-regression
# Fitting Linear Models

#' @rdname APA
#' @description APA.lm F-Test aus lm und Anova
#' @param include.r APA.lm: R-Squar
#' @export
APA.lm <- function(x, include.r = TRUE) {
  if (any(class(x) == "aov"))
    x <- lm(x)
  fitSummary <- summary(x)
  fstats <- fitSummary$fstatistic
  pValue <-  stats::pf(fstats[['value']],
                       fstats[['numdf']],
                       fstats[['dendf']], lower.tail = FALSE)
  if (include.r)
    rndr_lm(fstats[['value']] ,
            fstats[['numdf']],
            fstats[['dendf']],
            pValue,
            fitSummary$r.squared,
            fitSummary$adj.r.squared)
  else
    rndr_F(fstats[['value']] ,
           fstats[['numdf']],
           fstats[['dendf']],
           pValue)
  
}


#' @rdname APA2
#' @export
#'
APA2.lm <- function(x,
                    caption = NULL,
                    note = NULL,
                    output = stp25output::which_output(),
                    col_names = NULL,
                    include.b = TRUE,
                    include.se = TRUE,
                    include.beta = FALSE,
                    include.ci = FALSE,
                    include.r = TRUE,
                    include.test = FALSE,
                    include.eta = TRUE,
                    include.sumsq = TRUE,
                    include.meansq = FALSE,
                    digits.test = 2,
                    ci.level = .95,
                    conf.style.1 = TRUE,
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
      note <- paste(note, "\nr-squared:", rndr_r2(r2))
    }
  }
  if (is.null(caption))
    caption <-  paste0("AV: ", AV, " Obs: ", info$N)
  
  if (inherits(x, "aov")) {
    
    res<-  prepare_output( extract_param_aov(
      x,
      include.eta,
      include.sumsq ,
      include.meansq ,
   
      fix_format=TRUE,
      digits.test,
      format = "f"
    ),
    caption,
    note,
    info$N,
    info$labels
    )
    
  } else{
    res <- prepare_output(
      extract_param(
        x,
        include.b,
        include.se,
        include.beta,
        include.ci,
        ci.level = ci.level,
        fix_format = TRUE,
        conf.style.1 = conf.style.1
      )
      ,
      caption,
      note,
      info$N,
      info$labels
    )
  }
  
  if (!is.logical(output))
    Output(res, output = output, col_names = col_names)
  
  invisible(res)
}