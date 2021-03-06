% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kano.R
\name{Kano}
\alias{Kano}
\alias{Kano_Auswertung}
\title{Analyze Kano Type Items.}
\usage{
Kano(
  ...,
  type = 5,
  umcodieren = FALSE,
  rm_Q = 10000,
  rm_I = 10000,
  methode = 1,
  vars_func = NULL,
  vars_dysfunc = NULL
)

Kano_Auswertung(
  x,
  caption = "",
  note = "",
  formula = x$formula,
  data = x$molten[-1],
  digits = options()$stp4$apa.style$prozent$digits[1],
  include.n = TRUE,
  include.percent = TRUE,
  include.total = TRUE,
  include.test = TRUE,
  include.fong = TRUE,
  rnd_output = TRUE
)
}
\arguments{
\item{...}{x, data, by, subset, na.action}

\item{type}{type Fragetype entweder  vollstaendig (5) oder gekuerzt (3)}

\item{umcodieren}{umcodieren logical False}

\item{rm_Q}{Remove Q Kategorien Q entfernen  Anzahl an erlaubten Qs}

\item{rm_I}{Remove I Kategorien I entfernen  Anzahl an erlaubten Is}

\item{methode}{wie sind die Items geordnet default = 1  (func dfunk func dfunc func)}

\item{vars_func, vars_dysfunc}{weche Variablen sind die zwei Dimensionen}

\item{caption, note}{Ueberschrift}

\item{note}{}

\item{formula}{intern daten aus Kano-Objekt}

\item{include.n, include.percent}{Anzahl, Prozent}

\item{include.total}{Anzahl Total}

\item{include.test, include.fong}{Fong und Chie-Test}

\item{rnd_output}{Intern fuer Plot bei FALSE ausgabe als Zahl}
}
\value{
Liste mit:
data: data mit der Kano-Kodierung.
molten: Daten-Lang
scors:  Scors sind eine Alternative Codierung zum Zweck der Transformierung zu einer metrischen Skala.
formula, removed=Errorrs, N, attributes, answers
}
\description{
Die Funktion \code{Kano()} transformiert Kano-Fragebogen zur Kano-Kodierung
}
\details{
http://www.eric-klopp.de/texte/angewandte-psychologie/18-die-kano-methode
https://de.wikipedia.org/wiki/Kano-Modell

 \subsection{M Basis-Faktoren (Mussfaktoren)}{
 Basis-Merkmale (\strong{M}ustbe) werden vom Kunden Vorausgesetzt schaffen
 Unzufriedenheit wenn sie nicht vorhanden sind.
 }
  \subsection{O Leistungs- Faktoren}{
 Leistungs-Merkmale (\strong{O}ne-dimensional) werden vom Kunden verlangt
 }
 \subsection{A Begeisterung-Faktoren}{
 Begeisterungs-Merkmale (\strong{A}ttractive) Kunde rechnet nicht damit hebt das
 Produkt vom Konkurrenten  ab.
 }
 \subsection{I Unerhebliche- Faktoren }{
 Unerhebliche-Merkmale (\strong{I}ndifferent) werden vom Kunden ignoriert.
 }
 \subsection{R Rueckweisende- Faktoren }{
 Ablehnende-Merkmale (\strong{R}) werden vom Kunden abgelehnt. Fuehren bei Vorhandensein zu Unzufriedenheit, bei Fehlen jedoch nicht zu Zufriedenheit.
 }



 \tabular{lrrrrr}{
 \strong{Func/Dyfunc} \tab like (1) \tab must-be (2) \tab neutral (3) \tab live with (4)  \tab dislike (5) \cr
   like (1) \tab O \tab A \tab A \tab A \tab O \cr
   must-be (2) \tab R \tab I \tab I \tab I \tab M \cr
   neutral (3) \tab R \tab I \tab I \tab I \tab M \cr
   live with (4) \tab R \tab I \tab I \tab I \tab M \cr
   dislike (5) \tab R \tab R \tab R \tab R \tab Q

     }


\strong{Kodierung}

Das würde mich sehr freuen (1)
Das setze ich voraus (2)
Das ist mir egal (3)
Das könnte ich in Kauf nehmen (4)
Das würde mich sehr stören (5)




M O A I R Q Heufigkeit

max Category

M>O>A>I  max Category mit Hierarchie  M Wichtiger als O usw.
also wen der Unterschied zwischen den zwei am hoechsten gelisteten Attributen
zwei Kategorien gleich ist,  5% Schwelle, dann gilt die Regel M>O>A>I

Total Strength als zweite Masszahl gibt an wie hoch der Anteil an bedeutenden
Produktmerkmalen ist.

Category Strength ist eine Masszahl die die angibt ob eine Anforderung nur in
eine Kategorie gehoert

CS plus	Index Positiv  CS.plus=  (A+O)/(A+O+M+I)

CS minus	Index Negativ CS.minus= (O+M)/(A+O+M+I)

Chi-Test	ist eigentlich Unsinn, Testet ob die Verteilung von M, A, O und I gleich ist.
Wird aber in wissenschaftlichen Arebitengerne angegeben.

Fong-Test Vergleich der zwei Haeufigsten-Kategorien gegenueber der Gesamtzahl
Ergebnis ist entweder ein signifikante oder ein nicht signifikante Verteilung.
Ich verwende zur Berechnung die Kategorien A,O,M,I und R. Q verwende ich nur für die Gesamtsumme
}
\examples{
 
kano_labels <- c( "like",
                  "must be",
                  "neutral",
                  "live with",
                  "dislike")

DF<-stp25aggregate::GetData( 
  "sex Edu f1 d1 f2 d2 f3 d3 f4 d4 f5  d5  f6  d6  f7  d7  f8  d8  f9  d9  f10 d10
  w  med 1  1  1  2  1  3  1  5  1   5   5   1   3   3   5   2   5   1   5   2
  w  med 1  2  2  5  2  3  1  5  1   5   2   5   3   3   2   5   2   5   5   2
  m  med 1  3  3  5  1  5  3  4  1   5   5   1   3   3   5   2   5   1   5   2
  m  med 1  4  4  2  1  5  4  4  1   5   5   1   3   3   5   2   5   1   5   2
  w  med 1  5  5  5  5  3  1  5  1   5   5   1   3   3   5   2   5   1   5   2
  w  med NA NA NA NA NA NA NA NA NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
  m  med 2  1  1  5  2  5  1  5  1   5   2   5   3   3   1   5   2   5   5   2
  w  med 2  2  2  5  1  3  1  5  1   5   3   3   3   3   1   4   1   3   5   2
  m  med 2  3  2  5  2  3  1  3  1   5   1   3   3   3   2   4   3   3   5   2
  m  med 2  4  1  5  1  5  1  5  1   5   1   4   3   3   2   5   1   3   5   2
  w  med 2  5  2  5  1  4  1  5  1   5   1   4   3   3   2   5   1   4   5   2
  m  med NA NA NA NA NA NA NA NA NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
  w  med 3  1  2  5  3  3  1  5  2   5   1   5   3   3   3   3   3   3   5   2
  m  med 3  2  1  5  1  5  2  5  2  NA   1   5   3   3   2   5   1   5   5   2
  w  med 3  3  2  5  1  3  1  5  1   5   1   3   3   3   2   5   1   3   5   2
  w  low 3  4  2  5  2  5  2  5  1   5   1   4   3   3   2   5   1   3   5   2
  w  low 3  5  2  5  1  5  1  5  2   5   1   4   3   3   2   5   1   4   5   2
  w  low NA NA NA NA NA NA NA NA NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
  m  low 4  1  2  5  1  5  2  5  2   5   1   4   2   3   2   5   1   3   5   2
  w  low 4  2  2  5  2  5  2  5  2   5   1   3   3   3   2   5   1   3   5   2
  w  low 4  3  2  5  1  5  2  5  2   5   1   5   1   3   2   5   1   3   5   2
  m  low 4  4  2  5  1  5  2  5  2   5   1   3   3   3   1   3   1   3   5   2
  w  low 4  5  2  5  3  3  2  5  2   5   1   4   1   3   2   5   1   4   5   2
  w  low NA NA NA NA NA NA NA NA NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
  m  hig 5  1  1  5  1  5  2  4  1   5   1   3   3   5   2   4   1   3   5   2
  w  hig 5  2  1  5  1  3  1  5  1   5   1   3   1   5   1   5   3   3   5   2
  w  hig 5  3  2  5  3  3  1  4  2   4   1   3   3   5   3   3   5   1   5   2
  w  hig 5  4  2  5  1  4  2  5  1   5   1   3   3   5   2   5   4   1   5   2
  w  hig 5  5  2  5  2  4  2  4  2   5   1   4   1   5   1   5   1   4   5   2
  m  hig NA NA 2  5  1  5  1  3  1   4   1   3   1   5   1   3   1   3   5   2
  m  hig NA NA 2  1  1  5  1  4  3   3   5   2   3   5  NA  NA   1   3   5   2")



DF<- stp25aggregate::upData2(DF,  labels=c(f1="Fahreigenschaften"
                                           ,f2="Sicherheit"
                                           ,f3="Beschleunigung"
                                           ,f4="Verbrauch"
                                           ,f5="Lebensdauer"
                                           ,f6="Sonderausstattung"
                                           ,f7="Schiebedach"
                                           ,f8="Rostschutz"
                                           ,f9="Design"
                                           , f10= "Rostflecken"
))



kano_res1 <-  Kano( ~ . , DF[-c(1,2)])
DF[-c(1,2)] <- stp25aggregate::dapply2(DF[-c(1,2)], 
                                       function(x) factor( x, 1:5, kano_labels))
kano_res2 <-  Kano( ~ . , DF[-c(1,2)])
kano_res1$scors
kano_res2$scors

#stp25stat:::Kano_Auswertung(kano_res1, rnd_output=FALSE)
# kano_plot(kano_res1)
# library(lattice)
# x<-data.frame (xtabs(~ value+variable, kano_res1$molten ))
#   barchart(Freq ~value|variable, x, origin=0)


APA2(kano_res1, caption = "Einzeln")


kano_res <- Kano( .~ sex, DF[-2])
APA2(kano_res, caption = "Gruppe")
APA2(kano_res, caption = "Gruppe", include.percent=FALSE)
# kano_plot(kano_res,
#           legend.position = list(x = .75, y = 1),
#           #legend.title= "HAllo",
#           cex.legend=1)

kano_res <- Kano( .~ sex + Edu, DF )
#stp25stat:::Kano_Auswertung( kano_res, rnd_output=FALSE)
# Kontrolle der Logik
#kano_res1 <-  Kano( ~ . , DF[-c(1,2)], na.action = na.pass)
#dat<- na.omit(cbind(DF[c("f1", "d1")], kano_res1$data[2]))
#tidyr::spread(dat , d1, Fahreigenschaften)

}
