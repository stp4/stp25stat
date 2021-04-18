#' @rdname APA
#' @description APA.htest: T-Test
#' @export
#' @examples
#'  # T-Test
#' require(coin)
#' APA(coin::wilcox_test(mpg ~ factor(vs), mtcars))
#' APA(wilcox.test(mpg ~ vs, mtcars))
#' APA(t.test(mpg ~ vs, mtcars))
#'
#'  TeaTasting <-
#' matrix(c(3, 1, 1, 3),
#'        nrow = 2,
#'        dimnames = list(Guess = c("Milk", "Tea"),
#'                        Truth = c("Milk", "Tea")))
#' APA.htest(fisher.test(TeaTasting, alternative = "greater"))
APA.htest <- function(x,  ...) {
  if (any(names(x) == "statistic")) {
    if (names(x$statistic) == "t") {
      rndr_T(x$statistic,
             x$parameter,
             x$p.value)
    }
    else if (names(x$statistic) == "BP") {
      rndr_BP(x$statistic,
              x$parameter,
              x$p.value)
    }
    else if (names(x$statistic) == "DW") {
      rndr_DW(x$statistic,
              x$parameter,
              x$p.value)
    }
    else if (names(x$statistic) == "X-squared") {
      rndr_Chisq(x$statistic, x$parameter, x$p.value)
    }
    else{
      rndr_W(x$statistic,
             x$p.value)
    }
  }
  else{
    rndr_fischer(x$estimate, x$p.value)
  }
  
}


#' @rdname APA2
#' @export
APA2.htest <- function(x, caption = "", ...) {
  # t.test
  
  if (any(names(x) == "statistic")){
  if (names(x$statistic) == "t")
    Output(
      fix_data_frame2(
        Source = x$data.name,
        T = x$statistic,
        df = x$parameter,
        p.value = x$p.value
      ), caption = paste(x$method, caption), ...)
  else
    Output(
      fix_data_frame2(
        Source = x$data.name,
        W = x$statistic,
        p.value = x$p.value
      ), caption = paste(x$method, caption) , ...)
    }
  else {"Eventuel Fisher-Test"}
}



#' @rdname APA2
#' @export
APA2.pairwise.htest <-
  function(x, caption = "", ...) {
    #pairwise.t.test
    #-- ?pairwise.wilcox.test
    Output(
      data.frame(
        Source = row.names(x$p.value),
        Format2(
          as.data.frame(x$p.value),
          digits = 3 ,
          lead.zero = FALSE
        )
      ),
      caption = paste(x$data.name, x$method, "(p-Value)", caption),
      ...
    )
  }


