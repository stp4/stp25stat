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
  
  if (n > 1 & "data.frame" %in%  frst )
    isa_formula <-
    any(grepl("~", as.character(dots[[2]][1])))
  
  if (frst[1] == "formula" |
      ("data.frame" %in% frst & !isa_formula & frst[1] != "anova")) {
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
tbll_extract.matchit <- function (x, caption = "", note)
{
  note <-
    paste0("method = ", x[["info"]]$method, ", ratio = ", x[["info"]]$ratio)
    prepare_output(stp25tools::fix_to_df(x$nn[c(2, 4, 5, 6), ]),
                 caption = caption,
                 note = note)
  
}

#' @rdname Tbll
#' @export
#'
tbll_extract.summary.matchit <-
  function (x,
            caption = "Summary of balance for matched data",
            note = "",
            digits = 3)
  {
    prepare_output(stp25tools::fix_to_df(round(x$sum.matched[, c(3, 4)], digits)),
                   caption = caption,
                   note = "")
    
  }



#' @rdname Tbll
#' @export
#'
tbll_extract.principal <- function (x, ...)
{
  Tbll_pca(x, ...)
}

#'  Tabelle PCA
#'  
#'
#' @param x psych::principal
#' @param cut display loadings 
#' @param sort by value
#' @param digits  digits
#' @param ... not used
#'
#' @return data.frame ore list with data.frame
#' @export
#'
#' @examples
#' 
#' rslt <- psych::principal(fkv, 5)
#' #  rslt %>% stp25stat:::extract_principal() iste eine Kopie von print(rslt)
#' 
#' rslt  %>%  Tbll_pca_loadings()
#' rslt  %>%  Tbll_pca_eigen()
#' rslt  %>%  Tbll_pca_test()
Tbll_pca<- function(x,  cut = .30,
                    sort = TRUE,
                    digits = 2,
                    ...){
  
  rslt <- extract_principal(x,
                            cut = .30,
                            sort = TRUE,
                            digits = 2)
  
  
  rslt$eigen <- Tbll_pca_eigen(extprn=rslt)
  rslt
}




#' @rdname Tbll_pca
#' @export
Tbll_pca_loadings <-
  function(x,
           caption = "Principal components analysis (PCA)",
           cut = .30,
           sort = TRUE,
           digits = 2,
           ...
  ) {
    
    if(!is.null(x))
      extract_principal(
        x,
        caption = caption,
        digits = digits,
        cut = cut,
        sort = sort
      )$loadings
    else extprn$loadings
    
  }


#' @rdname Tbll_pca
#' @export
Tbll_pca_eigen <- function(x,
                           digits = 2,    
                           extprn =NULL,
                           ...) {
  if(is.null(extprn))
    rslt <- extract_principal(x,  digits = digits)$eigen
  else   rslt <- extprn$eigen
  
  caption <- (attr(rslt, "caption"))
  
  prepare_output(cbind(
    data.frame(Source = rownames(rslt)),
    stp25rndr::Format2(rslt, digits = digits)
  ),
  caption = caption)
}


#' @rdname Tbll_pca
#' @export
Tbll_pca_test <- function(x,
                          ...) {
  rslt <- extract_principal(x)$test
  rslt
}
