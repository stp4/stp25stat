#' Tabelle (Tbll)
#'
#'
#' Tabelle ohne Output an html
#'
#' @param ... alles an APA2 odere Tabelle
#' @param output an Output
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
Tbll <- function(..., output = FALSE)  {
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
    rslt <- Tabelle(...)
  }
  else if ("data.frame" %in% frst & isa_formula) {
    if (n <= 2)
      rslt <-
        lazyeval::lazy_eval(lazyeval::make_call(quote(Tabelle), dots[c(2, 1)]))
    else
      rslt <-
        lazyeval::lazy_eval(lazyeval::make_call(quote(Tabelle), dots[c(2, 1, (3:n))]))
  }
  else{
    if (n > 1) {
      is_arg <- sapply(dots,
                       function(x) {
                         lazyeval::lazy_eval(lazyeval::make_call(quote(is.vector), x))
                       })
      
      if (all(is_arg[-1])) {
        rslt <- apa2(...)
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
            lazyeval::lazy_eval(lazyeval::make_call(quote(apa2), dots[c(i, is_arg)]))
        }
      }
    }
    else
      rslt <- apa2(...)
  }
  
  rslt
}


#' @rdname Tbll
#' @description apa2(): APA2 ohne Output
#' @export
apa2 <- function(..., output = FALSE) {
  rslt <-  APA2(..., output = FALSE)
  
  
  
  if (tibble::is_tibble(rslt))
    rslt
  else if (is.data.frame(rslt))
    (tibble::as_tibble(rslt))
  else
    rslt
}


#' @rdname Tbll
#' @export
Tbll_desc <-
  function(...,
           output = FALSE,
           APA = TRUE) {
    rslt <- Tabelle(..., APA = APA)
    if (length(rslt) == 1 & is.list(rslt))
      rslt <- rslt[[1]]
    
    rslt
  }

#' @rdname Tbll
#' @export
Tbll_corr <-
  function(...,
           output = FALSE) {
    APA_Correlation(..., output = FALSE)
  }


#' @rdname Tbll
#' @export
Tbll_reg  <- function(...,
                      output = FALSE) {
  APA_Table(..., output = FALSE)
  
}


#' @rdname Tbll
#' @export
Tbll_xtabs <- function(...,
                       output = FALSE) {
  APA_Xtabs(..., output = FALSE)
  
}
