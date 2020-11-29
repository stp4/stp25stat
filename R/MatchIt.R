#' @rdname APA2
#' @description  APA2.matchit  print a short summary
#'
#' @param x MatchIt -Objekt
#' @param ...  an Output
#'
#' @export
#' @examples 
#'  
#' require(MatchIt)
#' require(wakefield)
#' require(stpvers)
#' #Projekt("html")
#' 
#' 
#' set.seed(0815)
#' 
#' nonMetra <- 702
#' Metra <- 59
#' 
#' DF3 <- r_data_frame(
#'   n = nonMetra + Metra,
#'   d.age = age,
#'   d.sex = sex,
#'   r.hcv = rnorm,
#'   et.dri  = rnorm,
#'   d.dcd = rnorm,
#'   d.steatose = answer
#' )
#' 
#' 
#' #+ results='asis'
#' DF3$metra <- c(rep(0, nonMetra),  rep(1, Metra))
#' DF3$Metra<- factor(DF3$metra, 0:1,  c("non-Metra", "Metra"))
#' fm <-  metra ~  d.age + d.sex + r.hcv + et.dri + d.dcd + d.steatose
#' 
#' 
#' # 
#' # m.out <- matchit(fm, data = DF3,   method = "nearest")
#' # APA2(m.out,caption = "Exact Matching ")
#' # df.match <- match.data(m.out)
#' # summary(df.match)
#' 
#' 
#' 
#' 
#' 
#' m.out0 <- matchit(fm, data = DF3,   method = "full")
#' APA2(m.out0,caption = "Matching  method = full")
#' 
#' # m.out1 <- matchit(fm, data = DF3, method = "genetic")
#' # APA2(m.out1,caption = "Matching method = genetic")
#' 
#' m.out2 <- matchit(fm, data = DF3, method = "optimal", ratio = 1)
#' APA2(m.out2, caption = "Matching  method = optimal, ratio = 1")
#' 
#' #APA2(summary(m.out0, standardize = TRUE))
#' # plot(m.out0, type = 'jitter', interactive = FALSE)
#' 
#' 
#' df.match0 <- match.data(m.out0)
#' summary(df.match0)
#' df.match0 %>% Tabelle2(
#'   d.age, d.sex, r.hcv, et.dri, d.dcd, d.steatos,
#'   distance,
#'   weights,
#'   subclass,
#'   by =  ~ Metra,
#'   APA = TRUE
#' )
#' 
#' # df.match1 <- match.data(m.out1)
#' # summary(df.match1)
#' # df.match1 %>% Tabelle2(
#' #   d.age, d.sex, r.hcv, et.dri, d.dcd, d.steatos,
#' #   distance,
#' #   weights,
#' #   by =  ~ Metra,
#' #   APA = TRUE
#' # )
#' 
#' df.match2 <- match.data(m.out2)
#' summary(df.match2)
#' df.match2 %>% Tabelle2(
#'   d.age, d.sex, r.hcv, et.dri, d.dcd, d.steatos,
#'   distance,
#'   weights,
#'   subclass,
#'   by =  ~ Metra,
#'   APA = TRUE
#' )
#' 
#' #df.match0$subclass 
#' # End()
APA2.matchit <- function (x, ...)
{
  # cat("\nCall: ", deparse(x$call), sep = "\n")
  # cat("\nSample sizes:\n")
  # print.table(x$nn, ...)
  # invisible(x)
  # cat("\n")
 #stp25tools::fix_to_df()
  Output( stp25tools::fix_to_df(x$nn), ...)
  invisible(x$nn)
  
}
#' @rdname APA2
#' @export
#' 

APA2.summary.matchit <- function (x,  ..., digits = 2)
{
  # "call"        "nn"          "sum.all"     "sum.matched" "reduction"
  # APA2(cbind(Source=row.names(x$sum.all), round(x$sum.all[1:3], digits)),
  #       caption="Summary of balance for all data")
  
  Output( 
    cbind(Source = row.names(x$sum.matched),
                         round(x$sum.matched[c(1, 2, 4)],
                               digits))
  ,
  caption = "Summary of balance for matched data")
  
  #   Output(cbind(Source = row.names(x$sum.matched),
  #                round(x$sum.matched[c(1, 2, 4)],
  #                      digits)),
  # APA2(cbind(Source=row.names(x$reduction), round(x$reduction[1:3], digits)),
  #        caption="Percent Balance Improvement")
  
  
  invisible(cbind(Source = row.names(x$sum.matched), x$sum.matched))
  
}


