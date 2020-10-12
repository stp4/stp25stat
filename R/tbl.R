#' Tabelle (Tbll)
#'
#'
#' Tabelle ohne Output an html
#'
#' @param ... alles an APA2 odere Tabelle
#'
#' @return data.frame oder liste mit data.frames
#' @export
#'
#' @examples
#'
#' #require(stpvers)
#'
#' lm1 <- lm(breaks ~ wool + tension, data = warpbreaks)
#' fm1 <- aov(breaks ~ wool + tension, data = warpbreaks)
#' tk1 <- TukeyHSD(fm1, "tension", ordered = TRUE)
#'
#'
#' Tbll(breaks ~ wool + tension, data = warpbreaks, caption = "Tabelle() mit formula")
#'
#' Tbll(warpbreaks,
#'      breaks,
#'      wool,
#'      tension,
#'      include.ci = TRUE,
#'      caption = "Tabelle() mit Namen")
#' Tbll(warpbreaks,
#'      breaks ~ wool + tension,
#'      caption = "Tabelle() aber Fehler in Formel abfangen",
#'      output = TRUE)
#'
#' Tbll(
#'   lm1,
#'   fm1,
#'   include.p = FALSE,
#'   include.ci = TRUE,
#'   ci = 95,
#'   caption = "Liste mit lm und aov"
#' )
#'
Tbll <- function(...)  {
  dots <- lazyeval::lazy_dots(...)
  n <- length(dots)
  rslt <- NULL
  
  isa_formula <- FALSE
  frst <-
    lazyeval::lazy_eval(lazyeval::make_call(quote(class), dots[[1]]))
  
  if (n > 1 & "data.frame" %in%  frst)
    isa_formula <-
    any(grepl("~", as.character(dots[[2]][1])))
  
  if (frst[1] == "formula" |
      ("data.frame" %in% frst & !isa_formula)) {
    rslt <- Tbll_desc(...)
  }
  else if ("data.frame" %in% frst & isa_formula) {
    if (n <= 2)
      rslt <-
        lazyeval::lazy_eval(lazyeval::make_call(
          quote(Tbll_desc), dots[c(2, 1)]))
    else
      rslt <-
        lazyeval::lazy_eval(lazyeval::make_call(
          quote(Tbll_desc), dots[c(2, 1, (3:n))]))
  }
  else{
    if (n > 1) {
      is_arg <- sapply(dots,
                       function(x) {
                         lazyeval::lazy_eval(lazyeval::make_call(
                           quote(is.vector), x))
                       })
      
      if (all(is_arg[-1])) {
        rslt <- tbll_extract(...)
      }
      else{
        is_obj <- which(!is_arg)
        is_arg <- which(is_arg)
        names_obj <- sapply(dots[is_obj],
                            function(x) {
                              as.character(x[1])
                            })
        for (i in seq_along(is_obj)) {
          rslt[[names_obj[i]]] <-
            lazyeval::lazy_eval(lazyeval::make_call(
              quote(tbll_extract), dots[c(i, is_arg)]))
        }
      }
    }
    else
      rslt <- tbll_extract(...)
  }
  
  rslt
}


#' @rdname Tbll
#' @description tbll_extract(): APA2 ohne Output
#' @export
#' 
#' 
tbll_extract<- function(...){
  UseMethod("tbll_extract")
}

#' @rdname Tbll
#' @export
#'
tbll_extract.default <- function(...) {
 # print(class(list(...)[[1]]))
  
  rslt <-  APA2(..., output = FALSE)
  cat( "\n in APA2()\n")
 # if (tibble::is_tibble(rslt))
 #   rslt
 # else if (is.data.frame(rslt))
 #   (tibble::as_tibble(rslt))
 # else
    rslt
}


#' @rdname Tbll
#' @export
#'
tbll_extract.survfit <- function(x,
                         caption = "Median", 
                         include.survival=FALSE,
                         digits = 2) {
  rslt <- extract_survfit(x, digits)
  rslt$median <- prepare_output(rslt$median, caption=caption, note="")
  
  if(include.survival) rslt
  else  rslt$median
}


tbll_extract.summary.survfit <- function(x,
                                 caption="Summary of a Survival Curve",
                                 digits = NULL,
                                 include.se=FALSE,
                                 include.ci=TRUE) {
  include <- c(time = "time", n.risk = "n.risk", n.event = "n.event", surv = "survival")
  if(include.se) include<- append(include, c(std.err = "std.err"))
  if(include.ci) include<- append(include, c(lower = "lower 95% CI", upper = "upper 95% CI"))
  rslt <- extract_summary_survfit(x, digits=digits, percent=FALSE, include=include)
  prepare_output(rslt, caption=caption, note="")
}

tbll_extract.survdiff <- function(x,
                          caption = "Test Survival Curve Differences") {
  prepare_output( extract_survdiff(x),
                  caption=caption,
                  note=APA(x)
                  )
}

tbll_extract.coxph <- function(x,
                               caption = "Cox proportional hazards regression model",
                               include.param = FALSE,
                               include.test = TRUE,
                               ...) {
  rslt <- NULL
  prm <- NULL
 # cat( "\n in tbll_extract.coxph()\n")
  if (include.test) {
    rslt <- extract_coxph_test(x)

  }
  if (include.param) {
    prm <- prepare_output(extract_coxph_param(x), caption=caption)
    
    if(include.test) rslt<- list(test = rslt, param = prm)
    else rslt <- prm
  }
  
  rslt
}
