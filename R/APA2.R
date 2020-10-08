#' APA Style HTML-Tabellen-Ausgabe
#'
#' APA2 erstellt fertigen HTML-Tabellen Output.
#'
#' @name APA2
#' @param x Ein R Objekt oder eine Formel oder ein data.frame
#' @param ... weitere Argumente
#' @return html-String ueber cat sowi einen data.frame
#' @export
#'
APA2 <- function(x,
                 ...) {
  UseMethod("APA2")
}

#' @rdname APA2
#' @export
APA2.NULL <- function(x,
                      ...) {
 Info_Statistic(
    c("catTest", "conTest", "Wilkox", "Kruskal",
      "ANOVA",
      "T Test"),
    c("stats", "Hmisc", "stats", "stats",
      "car",
      "stats"),
    c(
      "chisq.test",
      "spearman2",
      "wilcox.test",
      "kruskal.test",
      "Anova, type = 3",
      "t.test"
    ), paste(methods("APA2"), collapse=", ")
  )
}


#' @rdname APA2
#' @export
APA2.formula <- function(...) {
  Tabelle2(...)
}

#' @rdname APA2
#' @export
APA2.default <- function(x,
                         caption = NULL,  note = NULL, output=which_output(),
                         custom.model.names = NULL,
                         ...) {
 
    rslt <-  extract(x, ...)
    
    if (is.null(caption))
      caption <- paste(attr(rslt, "caption"),
                       "Obs: ", attr(rslt, "N"))
    if (is.null(note))
      note <- attr(rslt, "note")
    
    rslt <- fix_format(rslt)
    
    if( !is.logical(output) )
      Output(
        rslt,
        caption = paste(custom.model.names, caption),
        note = note,
        output=output)
    
    invisible(rslt)
}


#' @rdname APA2
#'
#' @description
#' Die Funktion \code{APA2.formula} estellt die Standard-Tabellen (analog wie die Hmisc:summary).
#'  Links stehen die Zielvariablen rechts die Gruppen.
#'
#'  Fie Formel  \code{a1 + a2[4] +a3 ~ group1 + group2} ergibt zwei Auswertungen. Die Zahle in eckiger Klammer
#'  sind die Nachkommastellen. Achtung die Formeln sind auf 500 zeichen begrenzt (Limitation von der Funktion \code{deparse()})
#'  Einstellungen werden global erstellt:
#'
#'      \code{set_my_options(prozent=list(digits=c(1,0), style=2))}
#'
#'      \code{get_my_options()$apa.style$prozent}
#'
#' @param data data.frame wenn x eine Formel ist
#' @param caption,note Ueberschrift an Output
#' @param fun,na.action,direction  eigene Funktion na.action=na.pass
#' @param type formula: \code{c("auto", "freq", "mean", "median", "ci", "freq.ci")}
#' xtabs: type = c("fischer", "odds","sensitivity", "chisquare","correlation", "r")
#' @param cor_diagonale_up bei Correlation art der Formatierung
#' @param order,decreasing Sortieren   Reihenfolge der Sortierung
#' @param use.level Benutzter level in Multi zB ja/nein
#' @param include.n,include.all.n,include.header.n,include.total N mit ausgeben
#' @param test,include.test,corr_test,include.p,include.stars    Sig test bei  \code{type = auto} moegliche Parameter sind  test=TRUE, test="conTest" oder "sapiro.test" fuer den Test auf Normalverteilung, fuer SPSS-like \code{test=="wilcox.test"}  oder \code{test=="kruskal.test"}
#'  corr_test-ddefault ist  "pearson" c("pearson","spearman")
#' @param include.names,include.labels Beschriftung der zeilen
#' @param digits.mean,digits.percent Nachkommastellen
#' @param output Ausgabe von Ergebiss ueber Output
#' @param digits  digits
#' @param max_factor_length,useconTest,normality.test,test_name,exclude,stars,Formula interne Parameter in erate_statistik
#' @return liste mit data.frames
#' @export
#' @examples
#'
#' #-- APA2.formula --
#' require(stp25data)
#' APA2(chol0+chol1 ~ g, hyper, print.n=FALSE)
#' APA2(~ g, hyper, caption="Einfache Tabelle")
#' APA2(chol0+chol1 ~ g, hyper, caption="Spalte mit Characteristik loeschen", print_col=-2)
#' APA2(gew + rrs0 ~ g, hyper, print.n=FALSE, test=TRUE)
#' APA2(~chol0+chol1~chol6+chol12, hyper, caption="Korrelation", test=TRUE)
#' APA2(~chol0+chol1+chol6+chol12, hyper, caption="Korrelation", test=TRUE, stars=FALSE)
#'
#'
#' #varana <- varana %>% Label(m1="Mesung1", m2="BMI")
#' x<-APA2( ~m1,varana)
#' x<-APA2( ~m1+m2,varana)
#'
#' x<-APA2( m1~geschl,varana)
#' x<-APA2( m1+m2~alter,varana)
#' x<-APA2( m1+m2+geschl~alter,varana, include.test = TRUE)
#' x<-APA2( ~m1+m2+m3+m4,varana, test=TRUE)
#' 
APA2.formula<- function(..., APA=TRUE) Tabelle2(..., APA=APA)

