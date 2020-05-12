#  # not-used-functions


# berecne -----------------------------------------------------------------

# roxygen   16-10-2018

# berechneMean <- function(data = NULL,
#                          measurevar,
#                          by = NULL,
#                          na.rm = TRUE,
#                          conf.interval = .95,
#                          .drop = TRUE) {
#   Text("berechneMean: Achtung die Funktion wird bals geloescht!")
#   # This does the summary. For each group's data frame, return a vector with
#   # N, mean, and sd
#   if (length(measurevar) != 1)
#     return(measurevar)
#   
#   datac <- plyr::ddply(
#     data,
#     by,
#     .fun = function(xx, col) {
#       c(
#         variable = NA,
#         N    = length2(xx[[col]], na.rm = na.rm),
#         mean = mean   (xx[[col]], na.rm = na.rm),
#         sd   = sd     (xx[[col]], na.rm = na.rm),
#         min  = min    (xx[[col]], na.rm = na.rm),
#         max  = max    (xx[[col]], na.rm = na.rm)
#       )
#     },
#     measurevar,
#     .drop = .drop
#   )
#   
#   # Rename the "mean" column
#   #  datac <- plyr::rename(datac, c("mean" = measurevar))
#   datac$se <-
#     datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
#   
#   # Confidence interval multiplier for standard error
#   # Calculate t-statistic for confidence interval:
#   # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
#   ciMult <- qt(conf.interval / 2 + .5, datac$N - 1)
#   datac$ci <- datac$se * ciMult
#   datac$ci.low <-    datac$mean - datac$ci
#   datac$ci.hig <-    datac$mean + datac$ci
#   datac$variable <- GetLabelOrName(data[measurevar])
#   return(datac)
# }

#  #   berechne: berechne Mittelwerte
#  #'
#  #   Die Lagemasse werden ueber die Standard-Funktionen berechnet unterschied ist nur dass
#  #   Faktoren zu Zahlen transformiert werden und das \code{na.rm=TRUE} gesetzt wird.
#  #   CI = Hmisc::smean.cl.normal
#  #'
#  #'
#  #   @return  ein dataframe Objekt oder ein Character-String
#  #   @param ... alles weitere
#  #   @export
#   
#   berechne <- function(...) {
#     UseMethod("berechne")
#   }
#   
#  #   @rdname berechne
#  #   @param na.rm NAs
#  #   @param conf.interval,ci Grenzen der Konfidenzintervalle CIs
#  #   @param .drop anplyr::ddply
#  #   @export
#  #'
#  #   @examples
#  #   # erlaubt:  varana %>% berechne(4, 5, by= ~geschl )
#  #   #  berechne(hyper, "chol0" )
#  #   #  names(hyper)
#  #   #  hyper %>% berechne(chol0,chol1,chol6,chol12, by=~med+g)
#   berechne.data.frame <- function(data,
#                                   ...,
#                                   by = "1",
#                                   type = 1,
#                                   na.rm = TRUE,
#                                   conf.interval = .95,
#                                   .drop = TRUE) {
#     measure <-
#       sapply(lazyeval::lazy_dots(...), function(x) {
#         as.character(x[1])
#       })
#     
#     meAsNum <- grep("^[[:digit:]]", measure)
#     if (length(meAsNum) != 0) {
#       measure[meAsNum] <- names(data[as.numeric(measure[meAsNum])])
#     }
#     
#     if (is_formula2(by))
#       by <- all.vars(by)
#     
#     
#     res <- NULL
#     for (i in measure) {
#       res <- rbind(
#         res,
#         berechneMean(
#           data,
#           i,
#           by,
#           na.rm = na.rm,
#           conf.interval = conf.interval,
#           .drop = .drop
#         )
#       )
#     }
#     res$variable <- factor(res$variable, unique(res$variable))
#     
#     res
#   }
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
