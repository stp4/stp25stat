#  # not-used-functions


# library(psycho) ---------------------------------------------------------


#  #  APA2
#  #'
#  #  @param x lm object.
#  #  @param include.ci Confidence interval
#  #  @param include.effect Text zu Effect_Size
#  #  @export
#  #'
#  #  @examples
#  #'
#  #  library(psycho)
#  #'
#  #'
#  #   df <- psycho::affective  # Load a dataset from the psycho package
#  #   #df <- standardize(df)  # Standardize all numeric variables
#  #'
#  #   fit <- lm(Age ~ Salary, data=df)  # Fit a Bayesian linear model
#  #   # results <- analyze(fit)  # Format the output
#  #   #APA2(results )
#  #'
#  #'
#  #'
#  #   library(lmerTest)
#  #   fit <- lmerTest::lmer(Sepal.Length ~ Sepal.Width + (1|Species), data=iris)
#  #'
#  #   #results <- analyze(fit)
#  #   #APA2(results)
#  APA2.psychobject <- function(x,
#                               caption = "",
#                               note = NULL,
#                               # paste("contrasts: ", paste(options()$contrasts, collapse=", ")),
#                               include.ci = FALSE,
#                               include.effect = FALSE,
#                               output = stp25output::which_output(),
#                               ...) {
#    # class(x)
#    
#    res <-
#      fix_format(summary(x),
#                 pattern_pval = "p",
#                 pattern_est = c("SE", "SE.std"))
#    
#    if (!include.ci) {
#      ci <- which(names(res) %in% c("CI_lower", "CI_higher"))
#      res <- res[-ci]
#      
#    }
#    
#    if (!include.effect) {
#      eff <- which(names(res) == "Effect_Size")
#      res <- res[-eff]
#      
#    }
#    if (is.null(note)) {
#      r2s <- x$values$model
#      note <- ""
#      for (i in names(r2s)) {
#        note <- paste(note, i, "=", rndr_r(r2s[[i]], FALSE))
#        if (names(r2s)[1] == i)
#          note <- paste0(note, ",")
#      }
#      note
#    }
#    res <-  prepare_output(res, caption, note)
#    
#    Output(res, output = output)
#    invisible(res)
#  }
#  
#  
#  #  @rdname APA2
#  #  @export
#  #'
#  APA2.psychobject <- function(...) {
#    Output(...)
#  }
#  
#  
#  
#  #  @rdname APA
#  #  @export
#  APA2.psychobject <- function(x, ...) {
#    x$text
#  }
