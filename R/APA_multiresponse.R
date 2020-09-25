

#' @rdname APA_
#' @description Wird in APA2 verwendet Tabelle Arbeitet mit Multi2default()
#' und hat anderen Rueckgabewert als Tabelle (Mittelverte vs String)
#' @export
#' @examples 
#' 
#' #' APA_multiresponse
#' 
#' DF <- data.frame(
#'   Sex = gl(2, 10, labels = c("male", "femal")),
#'   Magazines = c(0, 0, 0, 0, 0, 1, 0, 1, 1, 1,
#'                 1, 0, 0, 1, 1, 1, 0, 0, 1, 0),
#'   Comic.books = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#'   Fiction = c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1,
#'               1, 0, 0, 1, 1, 1, 0, 0, 1, 0),
#'   Newspapers = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1,
#'                  1, 0, 0, 1, 1, 1, 0, 0, 0, 1)
#' )
#' 
#' 
#' 
#' APA_multiresponse( ~ Magazines + Comic.books + Fiction + Newspapers,
#'                    DF,
#'                    fun = Prozent)
#' APA_multiresponse(
#'   Magazines + Comic.books + Fiction + Newspapers ~ Sex,
#'   DF,
#'   fun = Prozent,
#'   include.test = TRUE,
#'   order = TRUE, include.total=TRUE
#' )
#' 
#'  # das funktioniert mit X$measure = "multi"
#'  DF %>% Tabelle(Magazines, Comic.books, Fiction, Newspapers, type="multiresponse" )
#'  DF %>% Tabelle(Magazines, Comic.books, Fiction, Newspapers, by=~Sex, type="multiresponse" )
#' 
APA_multiresponse <- function(x,
                              data,
                              caption = "",
                              note = "",
                              output = which_output(),
                              test = FALSE,
                              na.action = na.pass,
                              include.test = test,
                              include.n = FALSE,
                              include.nr = FALSE,
                              include.total = FALSE,
                              order = FALSE,
                              decreasing = TRUE) {
  rhs <- all.vars(x[-2])
  lhs <- all.vars(x[-3])
  
  if (length(x) == 2) {
    formula <- NULL
    test <- FALSE
    include.total <- FALSE
  }
  else {
    formula <- formula(paste("variable~", rhs[1]))
    if (is.logical(include.test)) {
      if (include.test) {
        include.test <- "chisq.test"
        test <- TRUE
      }
    } else
      test <- TRUE
  }
  
  rslt <- Summarise(
                    x,
                    data = data,
                    fun = Prozent_multi,
                    na.action = na.action,
                    formula = formula,
                    margins = include.total)
  
  means <- Summarise(x,
                     data = data,
                     fun = mean2,
                     na.action = na.action)
  
  if (test) {
    ans <- NULL
    for (i in lhs) {
      fm <- formula (paste("~", i, "+", rhs))
      ans <- c(ans,  catTest(fm, data))
      }
    rslt$statistics <- ans
    }
  
  if (order) {
    rslt <-
      rslt[order(sapply(data[lhs], mean2), decreasing = decreasing),]
  }
  
  rslt <- prepare_output(rslt, caption, note)
  Output(rslt, output = output)
  
  invisible(list(tab = rslt, mean = means))
}


#' r1<-APA_multiresponse( ~ Magazines + Comic.books + Fiction + Newspapers,
#'                    DF,
#'                    fun = Prozent, output=FALSE)
#' r2APA_multiresponse(
#'   Magazines + Comic.books + Fiction + Newspapers ~ Sex,
#'   DF,
#'   fun = Prozent,
#'   include.test = TRUE,
#'   order = TRUE, include.total=TRUE,   output=FALSE
#' )

