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
#' # Projekt("html")
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
#' DF3$Metra <- factor(DF3$metra, 0:1,  c("non-Metra", "Metra"))
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
#' Tbll(m.out0) %>% Output(caption = "Matching  method = full")
#' summary(m.out0) %>%  Tbll() %>% Output()
#' 
#' # m.out1 <- matchit(fm, data = DF3, method = "genetic")
#' # APA2(m.out1,caption = "Matching method = genetic")
#' 
#' m.out2 <- matchit(fm,
#'                   data = DF3,
#'                   method = "optimal",
#'                   ratio = 1)
#' APA2(m.out2, caption = "Matching  method = optimal, ratio = 1")
#' 
#' #APA2(summary(m.out0, standardize = TRUE))
#' # plot(m.out0, type = 'jitter', interactive = FALSE)
#' 
#' 
#' df.match0 <- match.data(m.out0)
#' 
#' 
#' 
#' df.match0 %>% Tbll_desc(d.age,
#'                         d.sex,
#'                         r.hcv,
#'                         et.dri,
#'                         d.dcd,
#'                         d.steatos,
#'                         distance,
#'                         weights,
#'                         subclass,
#'                         by =  ~ Metra)
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
#' #summary(df.match2)
#' df.match2 %>% Tbll_desc(d.age,
#'                         d.sex,
#'                         r.hcv,
#'                         et.dri,
#'                         d.dcd,
#'                         d.steatos,
#'                         distance,
#'                         weights,
#'                         subclass,
#'                         by =  ~ Metra)
#' 
#' #df.match0$subclass
#' #End()

APA2.matchit <- function (x,
                          caption = "",
                          note = "",
                          output = which_output())
{
 rslt <- tbll_extract.matchit(x, caption, note)
 Output(rslt, output = output)
 invisible(rslt)  
}



#' @rdname APA2
#' @export
#' 
APA2.summary.matchit <- function (x,
                                  caption = "Summary of balance for matched data",
                                  note = "",
                                  digits = 2,
                                  output = which_output())
{
  rslt <-
    tbll_extract.summary.matchit(x,
                                 caption = caption,
                                 note = note,
                                 digits = digits)
  Output(rslt, output = output)
  invisible(rslt)
}



