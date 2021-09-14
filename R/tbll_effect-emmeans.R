#' @description  Effect Displays
#' 
#' @param include.ci an broom::tidy mit conf.int = include.ci, conf.level = 0.95,
#' @param ... an  broom::tidy zB  adjust = "tukey"
#'
#' @rdname Tbll
#' @export
#'
#' @examples
#'
#' #require(stpvers)
#' require(emmeans)
#'
#'  warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
#' x1<-emmeans (warp.lm,  ~ wool | tension)
#' APA2(emmeans (warp.lm,  ~ wool | tension))
#'
#' x2<-emmeans (warp.lm, poly ~ tension | wool)
#' APA2(emmeans (warp.lm, poly ~ tension | wool))
#'
#' x3<-emmeans (warp.lm, pairwise ~ tension | wool)
#' APA2(emmeans (warp.lm, pairwise ~ tension | wool))
#'
#' APA2(x3, adjust = "tukey")
#' APA2(x3, adjust = "scheffe")
#'
#' ?summary.emmGrid
tbll_extract.emm_list <- function(x,
                                  caption = "",
                                  note = "",
                                  include.ci = TRUE,
                                  ...) {
  means <-
    broom::tidy(x$emmeans,
                conf.int = include.ci,
                conf.level = 0.95,
                ...)
  contrasts <-
    broom::tidy(x$contrasts,
                conf.int = include.ci,
                conf.level = 0.95,
                ...)
  
  list(
    means = prepare_output(
      fix_format(means),
      caption = paste("Means", caption),
      note = note
    ),
    contrasts = prepare_output(
      fix_format(contrasts),
      caption = paste("Contrasts", caption),
      note = note
    )
  )
  
}


#' @rdname Tbll
#' @export
tbll_extract.emmGrid <-
  function(x,
           caption = "",
           note = "",
           include.ci = TRUE,
           ...) {
    contrasts <-  means <- NULL
    if (is.null(names(x))) {
      means <-
        broom::tidy(x,
                    conf.int = include.ci,
                    conf.level = 0.95,
                    ...)
      
      prepare_output(fix_format(means),
                     caption = paste("Means", caption),
                     note = note)
      
    }
    else{
      means <-
        broom::tidy(x$emmeans,
                    conf.int = include.ci,
                    conf.level = 0.95,
                    ...)
      
      contrasts <-
        broom::tidy(x$contrasts,
                    conf.int = include.ci,
                    conf.level = 0.95,
                    ...)
      
      list(
        means = prepare_output(
          fix_format(means),
          caption = paste("Means", caption),
          note = note
        ),
        contrasts = prepare_output(
          fix_format(contrasts),
          caption = paste("Contrasts", caption),
          note = note
        )
      )
    }
  }


#' @rdname Tbll
#' @export
tbll_extract.visreg <- function(x,
                                caption = x$meta$y,
                                note = "",
                                include.ci = TRUE,
                                digits = 2,
                                ...) {
  res <- x$fit
  nc <- ncol(res)
  ci <- res[, (nc - 1):nc]
  
  res <- res[,-c((nc - 1):nc)]
  res[, nc - 2] <-
    stp25rndr::rndr2(res[, nc - 2],  digits = digits)
  names(res)[nc - 2] <- "fit"
  
  if (include.ci)
    res$ci <- stp25rndr::rndr_CI(ci,  digits = digits)
  
  prepare_output(res[-which(names(res) == x$meta$y)],
                 caption = caption,
                 note = note,
                 N = nrow(x$res))
}

#' @rdname APA2
#' @export
APA2.emm_list <- function(x,
                          caption = "",
                          note = "",
                          include.ci = TRUE,
                          output = which_output(),
                          ...) {
  rslt <- tbll_extract.emm_list(x, caption,
                                note,
                                include.ci = include.ci,
                                ...)
  Output(rslt, output = output)
  invisible(rslt)
}

#' @rdname APA2
#' @export
APA2.emmGrid <- function(x,
                         caption = "",
                         note = "",
                         include.ci = TRUE,
                         output = which_output(),
                         ...) {
  rslt <- tbll_extract.emmGrid(x, caption,
                               note,
                               include.ci = include.ci,
                               ...)
  Output(rslt, output = output)
  invisible(rslt)
}

#' @rdname APA2
#' @export
APA2.visreg <- function(x,
                        caption = x$meta$y,
                        note = "",
                        include.ci = TRUE,
                        digits = 2,
                        output = which_output(),
                        ...) {
  rslt <- tbll_extract.visreg(x,
                              caption,
                              note,
                              include.ci = include.ci,
                              digits = digits,
                              ...)
  Output(rslt, output = output)
  invisible(rslt)
}


