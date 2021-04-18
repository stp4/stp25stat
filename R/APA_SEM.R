#' @rdname APA_SEM
#' @title APA_SEM
#' @name APA_SEM
#' @description Struckturgleichungs Modelle mit lavaan. Die Funktionen sind Kopieen von lavaan
#' und helfen den Output zu erstellen.
#' APA_SEM ist davei einfach lavaan::sem(x, ...)
#' mehr unter http://www.understandingdata.net/2017/03/22/cfa-in-lavaan/
#'
#' \subsection{APA2.lavaan}{Ueber APA2 wird die Ausgabe formatiert. Ausgegeben werden die Guetemasse und der ModelFit.
#' Loading ist dabei der standartisierte Estimate und Communality ist die quadrierte Ladung
#'
#' }
#'
#' Chi-Quadrat-Wert
#'
#' ML:  Validitaet des Models H0: empirische Kovarianz entspricht modelltheoretischer Kovarianz
#' Chi-Quadrat/df moeglichst klein (Chi-Quadrat/df<2.5 oder p<0.100)
#' Ist nur zuverlaessig wenn Normalverteilung und ausreichend grosse Stichprobe gegeben ist.
#'
#' Model test Baseline model Chi-Quadrat Null-Modell wenn signifikant dann besteht die Gefahr einer Fehl-Spezifikation
#'
#' Goodness-of-Fit-Index (GFI)
#'
#' Ist vergleichbar mit dem Bestimmtheitsmass in der Regressionsanalyse, also ein Mass fuer die erklaerende Varianz
#' GFI>0.90
#' Adjusted-Goodness-of-Fit-Index (AGFI)
#' Analog wie GFI nur korrigiert durch df und Anzahl an Variablen
#' AGFI>0.90
#'
#' Normed-Fit-Index NFI
#'
#' Vergleicht das Modell mit einem Model bei dem alle Manifesten Variablen un-korreliert angenommen werden
#' NFI>0.90
#' Comparative-Fit-Index
#'
#' Wie NFI nur korrigiert durch df und Anzahl an Variablen
#' CFI>0.90
#'
#' Root-Mean-Square-Error of Approximation (RMSEA)
#' RMSEA<0.05
#'
#' Backhaus Multivariate Analysemethoden 11 AuflageSeite 383
#' Moosbrugger, Kelava 2012 Testtheorie 2. Auflage Seite 339
#'
#' @param x Objekt
#' @param ... weitere Objekte nicht benutzt
#' @return Ein lavaan -Objekt oder html.
#'
#' @examples
#' head(fkv)
#' APA2( ~., fkv, test=T)
#' library(arm)
#' # windows(5,5)
#' # corrplot(fkv, abs=TRUE, n.col.legend=7)#  corrplot {arm}
#' #
#' Principal2(fkv, 5, cut=.35)
#'
#' library(lavaan)
#' library(semPlot)
#'
#'
#' Model<-'
#' Verarbeitung =~ F5+F16+F22+F9+F26+F6+F35+F33+F12+F34+F4
#' Coping =~ F7+F8+F17+F14+F15+F18+F19+F1+F13+F20
#' Vertrauen =~ F28+F27+F31+F29
#' Religion =~F21+F25+F30+F23+F24
#' Distanz =~F3+F2+F10+F11
#'
#' '
#' fit.Lavaan <- sem( Model, data=fkv)
#' APA2(fit.Lavaan)
#' # parameterEstimates(fit.Lavaan)
#' # Est <- parameterEstimates(fit.Lavaan, ci = FALSE, standardized = TRUE)
#' # #fitMeasures(fit.Lavaan, c("chisq", "df", "pvalue", "cfi", "rmsea"))
#' # #round( inspect(fit.Lavaan,"r2")  ,2)
#' # #parTable(fit.Lavaan)
#' # #show(fit.Lavaan)
#' # anova(fit.Lavaan)
#'
#' #semPaths(fit.Lavaan, "std", rotation=2, title = FALSE)
#' #title("Std", line = 3)
#' @export
APA_SEM <- function(x, ...) {
  UseMethod("APA_SEM")
}

#' @rdname APA_SEM
#' @export
APA_SEM.default <- function(x, ...) {
  lavaan::sem(x, ...)
}

#' @export
 SEM <- function(...) {
  APA_SEM(...)
}



#' @rdname APA_SEM
#' @param include.ci,include.varianz,include.latent,include.model,include.loading was soll ausgegeben werden
#' @export
APA2.lavaan <- function(x,
                        baseline.model = NULL,
                        caption = "" ,
                        note = "",
                        output = which_output(),
                        type = "all",
                        include.ci = FALSE,
                        include.model = if (type == "all") TRUE else  FALSE, 
                        include.varianz = if (type == "all") TRUE else FALSE,
                        include.latent = if (type == "all") TRUE else FALSE,
                        ...) {
  res_lavaan <- as.list(lavaan::fitMeasures(
    x,
    fit.measures = c(
      "chisq",
      "df",
      "pvalue",
      "baseline.chisq" ,
      "baseline.df",
      "baseline.pvalue" ,
      "rmsea" ,
      "srmr",
      "gfi",
      "agfi",
      "cfi" ,
      "nfi"
    ),
    baseline.model =  baseline.model
  ))
  
  
  res_lavaan$df <- res_lavaan$df + 1  # unbekannter Fehler
  
  res_lavaan <- with(
    res_lavaan,
    rbind(
      "ML"  = c(rndr_Chisq(chisq, df, pvalue), ""),
      "Model test baseline model" = c(
        rndr_Chisq(baseline.chisq, baseline.df, baseline.pvalue),
        ""
      ),
      "Number of observations" = c(x@Data@nobs[[1]], ""),
      "chisq/df" = c(Format2(chisq / df, 2), rndr_Chisq_cfa(chisq, df)),
      "GFI" = c(Format2(gfi, 2), rndr_gfi_cfa(gfi)),
      "AGFI" = c(Format2(agfi, 2), rndr_agfi_cfa(agfi)),
      "NFI"  = c(Format2(nfi, 2), rndr_nfi_cfa(nfi)),
      "CFI" =  c(Format2(cfi, 2), rndr_cfi_cfa(cfi)),
      "RMSEA" = c(Format2(rmsea, 2), rndr_rmsea_cfa(rmsea)),
      "SRMR" =  c(Format2(srmr, 2), rndr_rmsea_cfa(srmr))
    )
  )
  
  colnames(res_lavaan) <- c("Anpassungsmass", "Anforderung")
  res_lavaan <-
    data.frame(Wert = rownames(res_lavaan),
               res_lavaan)
  
  
  if (include.model) {
    Output(res_lavaan,
           caption = caption,
           note = note,
           output = output)
  }
  
  
  # label    est    se      z    pvalue ci.lower ci.upper std.lv std.all std.nox
  #Lhr      0.230 0.051  4.483  0.000   0.129    0.330  0.230   0.230   0.230
  #Standardized solution of a latent variable model.
   ldng <- lavaan::standardizedSolution(
    x,
    type = "std.all",
    se = TRUE,
    zstat = TRUE,
    pvalue = TRUE,
    ci = TRUE
  )
  
   
    var_type = ifelse(ldng$lhs == ldng$rhs, "Variances", "Latent")
  res_ldng <-
    data.frame(
      model = ifelse(
        ldng$lhs ==  ldng$rhs,
        ldng$rhs,
        paste(ldng$lhs, ldng$op, ldng$rhs)
      ),
      
      loading = Format2(ldng$est.std, digits = 2),
      h2 = ifelse(ldng$op == "=~",
                  Format2(ldng$est.std ^ 2) , NA),
      
      SE = Format2(ldng$se, digits = 2),
      z.value = Format2(ldng$z, digits = 2),
      p.value = rndr_P(ldng$pvalue, include.symbol = FALSE),
 
      stringsAsFactors = FALSE
    )
  
  if(include.ci){
    res_ldng$CI<- rndr_CI(ldng[c("ci.lower", "ci.upper")]  )
 
    }
    latent <- res_ldng[which(var_type=="Latent"),]
  varianz <- res_ldng[which(var_type=="Variances"), 1:2]
  
  if (include.latent) {
    Output(latent,
           caption = caption ,
           note = note,
           output = output)
    }
    
   if(include.varianz){
     Output(varianz,
            caption = caption ,
            note = note,
            output = output)
   }
    
  invisible(list(model=res_lavaan,
              latent=latent,
              varianz=varianz))
  
  
}
