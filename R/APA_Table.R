#' Regressions Tabelle
#'
#' APA_Table: Erstellt APA-Style Tabellen aus R-Objekten (lm, glm,lmer, usw).
#' 
#' 
#' @rdname APA_Table
#' @param ... Modelle
#' @param caption,note,names,custom.model.names,rgroup rgroup:ype="long" long die Zwischenueberschrift  Beschriftung
#' @param type type = c("default", "broom","long", "texreg", "stargazer", "sjPlot","anova")
#' @param digits Kommastellen auch als Matrix bei type =long
#' @param include.b,include.se,include.beta,include.odds Parameter, b=estimate, OR exp(b)
#' @param include.eta Eta bei Anova
#' @param include.ci,ci.level 95-CI
#' @param include.p,include.stars Sternchen und P-Werte
#' @param include.variance Variance
#' @param include.r,include.pseudo R-Quadrat
#' @param include.ftest,include.loglik noch nicht fertig
#' @param include.custom Liste mit eigenen Einträgen
#' @param include.aic,include.bic AIC BIC
#' @param include.anova    Explizite ANOVA Tabelle  include.anova=TRUE
#' @param single.row Agrument texreg = TRUE
#'
#' @return invisible data.frame und Output mit html/knit oder Text.
#' @export
#'
#' @examples
#' 
#' 
#' 
#' ##library(stp25data)
#' 
#' summary(schools)
#' ##Head("Lineare-Regression lm")
#' fit1 <-
#'   lm(sr ~ pop15 + pop75 + dpi + cut(ddpi, 3), data = LifeCycleSavings)
#' 
#' #' extract(fit1) = 
#' extract_param(fit1)
#' extract_gof(fit1) 
#' #' Aufruf von extract_param()
#' #' 
#' APA2(fit1) 
#' 
#' #'Das gleiche wie APA2 nur als Ausgabe als Liste
#' #'
#' APA_Table(fit1,  type = "default")
#' 
#' #'Geht an APA_list und danach an  extract_param hier ist die Formatierung anderst
#' #'include.stars kombiniet B + Sternchen. Die GOFs werden auch mit ausgegeben.
#' #'
#' APA_Table(fit1,  type = "long")
#' 
#' #' Das ist wie type = "long" nur ohne GOF
#' #'
#' APA_Table(fit1,  type = "wide")
#' 
#' 
#' fit1<-lm(score ~ grade + treatment, schools)
#' fit2<-lm(score ~ grade + treatment + stdTest, schools)
#' 
#' APA_Table(fit1, fit2,
#'           
#'           include.custom = list(Wald = c(
#'             APA(fit1, include.r = FALSE),
#'             APA(fit2, include.r = FALSE)
#'           ))
#'           
#' )
#' 
#' #' Lineare-Mixed-Effect-Regression
#' #' 
#' #' lme::lme
#' #'      lme(y ~ 1, random = ~ 1 | subjects, data=data)
#' #' 
#' lme1<-nlme::lme(score ~ grade +treatment + stdTest, schools, random =~ 1|classroom)
#' class(lme1) 
#' extract_param(lme1, fix_format = TRUE, include.stars=TRUE )
#' broom::glance(lme1)
#' RMSE(lme1)
#' extract_gof(lme1, fix_format=FALSE) 
#' 
#' #APA2_list(list(lme1),  include.stars=TRUE, include.gof=FALSE )
#' 
#' 
#' APA2(lme1)
#' APA_Table(lme1)
#' 
#' 
#' #' lme4
#' #'      lmer(y ~ 1 + (1 | subjects), data=data)
#' #'      
#' lmer1 <-lmerTest::lmer(score ~ grade +treatment + stdTest + (1|classroom), schools)
#' APA2(lmer1)
#' APA_Table(lmer1)
#' 
#' 
#' #' lme4::lmer 
#' lmer12<-lme4::lmer(score ~  grade +treatment  + stdTest + (1|classroom), schools)
#' APA_Table(lmer12)
#' 
#' 
#' APA_Table(lm1, lme1, lmer1, lmer12 , names= c("lm", "lme", "lmerTest", "lme4"),
#'           include.se=FALSE,
#'           include.stars=FALSE)
#'           
#'           
#'           
APA_Table <- function(...,
                      caption = NULL,
                      note =  NULL,
                      output = stp25output::which_output(),
                      type = c("long", "wide", "default", "anova"),
                      names = NULL,
                      col_names=NULL,   #c("b", "SE", "p"),
                      digits = NULL,

                      include.b = TRUE,
                      include.se = TRUE,
                      #noch nicht fertig
                      include.t= FALSE,
                      include.beta = FALSE,

                      include.odds = FALSE,
                      include.ci = FALSE,
                      include.odds.ci=FALSE,

                      #Fehler abfangeb
                      include.p = FALSE,
                      include.stars = if (include.p) FALSE  else TRUE,

                      include.variance = TRUE,
                      include.r = TRUE,
                      include.pseudo = FALSE,
                      # noch nicht fertig
                      include.ftest = FALSE,
                      include.loglik = FALSE,

                      include.custom = NULL,

                      include.aic = TRUE,
                      include.bic = include.aic,

                      include.anova = FALSE,
                      include.sumsq = TRUE ,
                      include.meansq = FALSE,
                      include.eta = FALSE,

                      include.gof=TRUE,
                      include.param=TRUE,

                      ci.level = .95,
                      single.row = TRUE,
     
                      rgroup = c("Parameter", "Goodness of fit"),

                      include.effects = c("ran_pars", "fixed"),
                      include.statistic=FALSE,
                      include.test=FALSE,
                      conf.int = TRUE,
                      conf.level = 0.95,
                      conf.method = "Wald",
                      fix_format = FALSE,
                      
                      digits.param = 3,
                      digits.odds = 2,
                      digits.test = 2,
                      digits.beta = 2,
                      format="fg"
                      )
{

    type <-  match.arg(type, several.ok = TRUE)
  type<- type[1]
  
  
  if (type == "anova") {
    #car::ANOVA Type II 
      return(
        APA_Table_Anova(
          ...,
          caption = caption,
          note = note,
          output = output,
          names = names,
          include.eta = include.eta,
          include.sumsq = include.sumsq,
          include.meansq = include.meansq
        )
      )
  }
  
  
  if(include.ftest) Text("Achtung include.ftest noch nicht fertig!")
  result <- NULL

  myfits <- list(...)

   if(length(myfits) == 0) 
     return(
     Info_Statistic(
     c("lm",  "lm-beta",    "lm-gof",
       "glm",  "glm-odds",    "glm-gof", "glm-minus.LL", "glm-LogLik", "glm-R2",
 
       "lmerModLmerTest", "lmerModLmerTest-RMSE", "lmerModLmerTest-Wald-CI", "",
       "anova", "Breusch-Pagan", "Durbin-Watson", "Levene", "Bartlett", 
       "Shapiro-Wilk" ,"VIV"
 
     ),  
      c("broom", "rockchalk",  "broom", 
        "broom", "base",       "broom", "pscl","lmtest", "pscl",
        "lme4",  "sjstats", "base","",
        "car","lmtest" ,"lmtest", "car", "stats",
        "stats","car"
        ),
     
     c(  "tidy", "standardize", "glance", 
         "tidy", "exp" ,        "glance","pR2","lrtest","pR2",
         "summary, ranef","rmse", "mult = qnorm((1 + conf.level) / 2)", "ci = estimate +- mult * std.error ",
         "Anova","bptest","dwtest", "leveneTest", "bartlett.test",  
         "shapiro.test(stats(fit))", "vif"
         )
   
      ))
  
  

  if (is(myfits[[1]], "list")) {
    myfits <- myfits[[1]]  # hier kommt ein fit_with-Objekt
    if (is.null(names))
      names <- names(myfits)
  }

 
  
  if (type == "long") {
    result <- APA2_list(
      myfits,
      caption = caption, note = note, output=output,
      custom.model.names = names,
      include.param=include.param, include.gof=include.gof, include.custom = include.custom,
      include.b = include.b, include.se = include.se, include.beta = include.beta,
      include.ci = include.ci,
      include.odds = include.odds, include.odds.ci=include.odds.ci,
      include.statistic = include.statistic,
      include.p = include.p,
      include.stars = include.stars,
      include.df = FALSE,
      include.r = include.r, include.pseudo = include.pseudo,
      include.test = include.test ,
      include.loglik = include.loglik ,
      include.aic = include.aic,include.bic = include.bic,
      ci.level = ci.level,
      rgroup = rgroup,
      digits=digits, digits.param = digits.param,
      digits.odds = digits.odds,
      digits.test = digits.test,
      digits.beta = digits.beta,
      format = format, 
      col_names=col_names
    )
  }
  else if (type == "wide") {
  names <-  
      if (length(myfits) == 1) "" else if (is.null(names)) paste0("m", 1:length(myfits), "") else names
    
    for (i in  seq_len(length(myfits)) ) {
      x <-  
        APA2_list(
          list(myfits[[i]]),
          caption = paste(names[i], caption), note = note , output=output,
        #  custom.model.names = "",
          include.param = TRUE,  include.gof = FALSE, #include.custom = include.custom,
          include.b = include.b, 
          include.se = include.se, 
          include.beta = include.beta,
          include.ci =  include.ci,
          include.odds = include.odds, include.odds.ci=include.odds.ci,
          include.statistic = include.statistic,
          include.p =  include.p,
          include.stars =  include.stars,
          include.df = FALSE,
          include.r = include.r, include.pseudo = include.pseudo,
          include.test = include.test ,
          include.loglik = include.loglik ,
          include.aic =  include.aic,include.bic = include.bic,
          ci.level =  ci.level,
          rgroup = rgroup,
   
          digits=digits, digits.param = digits.param,
          digits.odds = digits.odds,
          digits.test = digits.test,
          digits.beta = digits.beta,
          format=format
        )
      result[[i]] <- x
    }
  }
  # else if (type == "long") {
  #   result <- APA2_list(
  #     myfits,
  #     caption = caption ,
  #     note = note , output=output,
  #     digits =  digits,
  #     custom.model.names = names ,
  #     include.custom = include.custom ,
  #     include.b = include.b,
  #     include.ci =  include.ci,
  #     #Fehler abfangen mit alter schreibweise
  #     include.odds = include.odds,
  #     include.se = include.se,
  #     include.t = include.t,
  #     include.p =  include.p,
  #     include.stars =  include.stars,
  #     
  #     include.pseudo = include.pseudo,
  #     include.r = include.r,
  #     include.ftest = include.ftest ,
  #     
  #     include.loglik = include.loglik ,
  #     include.aic =  include.aic,
  #     include.bic = include.bic ,
  #     
  #     include.gof=include.gof,
  #     include.param=include.param,
  #     
  #     ci.level =  ci.level,
  #     rgroup = rgroup,
 
  #   )
  # }
  # else if (type == "broom") {
  #   APA_Table(
  #     ...,
  #     caption = caption,
  #     note = note, output=output,
  #     type = "default",
  #     names = names,
  #     digits = digits,
  #     single.row = single.row,
  #     include.stars = include.stars,
  #     include.p  = include.p ,
  #     include.variance = include.variance
  #   )
  # }
  else if (type == "default") {
    names <-  
      if (length(myfits) == 1) "" else if (is.null(names)) paste0("m", 1:length(myfits), "") else names
    
    for (i in  seq_len(length(myfits)) ) {
      x <- APA2(
        myfits[[i]],
        caption = paste(names[i], caption), note = note , output=output,
       # custom.model.names = custom.model.names[i],
        include.b = include.b,
        include.se = include.se,
        include.t = include.t,
        include.beta = include.beta,
        include.eta = include.eta,
        include.odds = include.odds,
        include.ci = include.ci,
        #Fehler abfangeb
        include.p = include.p,
        include.stars = include.stars,
        
        include.variance = include.variance,
        include.r = include.r,
        include.pseudo = include.pseudo,
        # noch nicht fertig
        include.ftest = include.ftest,
        include.loglik = include.loglik,
        # noch nicht fertig
        include.custom = include.custom,
        include.aic = include.aic,
        include.bic = include.bic,
        ci.level = .95,

        
        digits=digits, digits.param = digits.param,
        digits.odds = digits.odds,
        digits.test = digits.test,
        digits.beta = digits.beta,
        format=format
        
      )
      result[[i]] <- x
    }
  }
  # else if (type == "texreg") {
  #   result <-  type_texreg(
  #     myfits,
  #     caption = caption,
  #     note = note, output=output,
  #     digits = digits,
  #     single.row = single.row,
  #     include.stars = include.stars,
  #     include.p = include.p ,
  #     names = names,
  #     include.variance = include.variance
  #   )
  # }
  # else if (type == "stargazer") {
  #   type_stargazer(myfits,
  #                  caption = caption,
  #                  digits = digits)
  # }
  # else if (type == "sjPlot") {
  #   Text("sjPlot ist noch nicht implementiert")
  # }
  else{
    
    stop(type, " not includet" )
  #  if(type=="anova")  include.anova=TRUE ## Altlast
  }

# 
#   if (include.anova) {
#     #car::ANOVA Type II  print(str(result))

#     result[["anova"]] <- APA_Table_Anova(
#       myfits,
#       caption = caption,
#       note = note,
#       output = output,
#       names = names,
#       include.eta = include.eta,
#       include.sumsq = include.sumsq ,
#       include.meansq = include.meansq
#     )
#   }

 

  invisible(result)
}



 

 
# type_texreg <- function(list,
#                         caption = "",
#                         note = "", output=NA,
#                         names = NULL,
#                         digits = 2,
#                         single.row = TRUE,
#                         include.stars = TRUE,
#                         include.p  = FALSE,
#                         ci.force = FALSE,
#                         include.variance = TRUE,
#                         custom.model.names = if (is.null(names))
#                           paste0("(", 1:length(list), ")")
#                         else
#                           names,
#                         center = options()$stp25$apa.style$center,
#                         stars = options()$stp25$apa.style$p$stars.value,
#                         stars.symbols = options()$stp25$apa.style$p$stars.symbols) {
#   # - p-value -------------------------------
#   stars.string <- function (pval,
#                             stars,
#                             star.char,
#                             star.prefix,
#                             star.suffix,
#                             symbol) {
#     if (output != "text") {
#       paste0('<br>',  "p=", ffpvalue(pval))
#     }
#     else{
#       paste0(" p=", ffpvalue(pval))
#     }
#   }
# 
# 
#   if (include.p) {
#     old_stars.string <- texreg:::stars.string  # texreg::stars.string
#     assignInNamespace("stars.string", stars.string, "texreg")
#   }
# 
#   # eigendliche Funktion ----------------------------------------------------
# 
#   if (is.null(caption))
#     caption <- ""
#   if (is.null(note))
#     note <- ""
#   res <- NULL
#   cptn <- paste(Tab(), caption)
# 
#   if (output != "text") {
#     # "html" oder "markdown"
#     reg_result <- texreg::htmlreg(
#       list,
#       return.string = TRUE,
#       single.row = single.row,
#       caption.above = TRUE,
#       caption = cptn,
#       inline.css = TRUE,
#       doctype = FALSE,
#       html.tag = FALSE,
#       head.tag = FALSE,
#       body.tag = FALSE,
# 
#       stars = stars,
#       custom.model.names = custom.model.names,
#       ci.force = ci.force,
#       center = center,
#       digits = digits,
#       include.variance = include.variance
#     )
# 
#     # Text("In texregTable")
#     # class(reg_result) <- "texregTable"
# 
#     Output.htmlTable(reg_result, output=output)
# 
#   }
#   else {
#     res <-
#       texreg::screenreg(
#         list,
#         single.row = single.row,
#         caption.above = caption,
#         digits = digits,
#         stars = stars,
#         title = paste(Tab(), "Regression", caption),
#         include.variance = include.variance
#       )
#     # print(res)
#   }
#   # reset p-value -----------------------------
#   if (include.p) {
#     assignInNamespace("stars.string", old_stars.string, "texreg")
#   }
#   texreg:::get.data(list) # ruckgabe der Ergebnisse als liste
# }
# 
# 
#  
# type_stargazer <- function(list, caption, digits) {
#   # output <- options()$prompt[1] == "HTML> "
#   # res <- NULL
#   if (any(sapply(list, class) %in% "merModLmerTest")) {
#     Text("Stargazer kan nicht mit LmerTest arbeiten. Alternative ist lme4::lmer(...)")
#     return(NULL)
#   }
#   # default <-default_caption_note(myfits[[i]])
#   if (is.null(caption))
#     caption <- "" #default$caption
#   caption <- Tab(caption)
# #  Output_info$table <<-  c(Output_info$table, caption)
#   # if(is.null(note)) note <- "" #default$note
#   Output.htmlTable(
#     stargazer::stargazer(
#       list,
#       title = caption,
#       decimal.mark = options()$stp4$apa.style$OutDec,
#       digits = digits,
#       digit.separator = "",
#       type = "html"
#     ), output=output
#   )
# 
#   NULL
# }

 
# type_long <- function(...)
#   APA2_list(...)
