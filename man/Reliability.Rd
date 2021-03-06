% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dimred-reliability.R
\name{Reliability}
\alias{Reliability}
\alias{Reliability2}
\alias{print.stp25_reliability}
\alias{Reliability.data.frame}
\alias{Reliability.formula}
\alias{Alpha}
\alias{Alpha2}
\alias{Reliability.default}
\alias{Index}
\title{Reliability und Cronbach-Alpha}
\usage{
Reliability(...)

Reliability2(...)

\method{print}{stp25_reliability}(x, ...)

\method{Reliability}{data.frame}(data, name = NULL, ...)

\method{Reliability}{formula}(x, data, name = "Skale", ...)

Alpha(..., type = 1, names = NULL)

Alpha2(
  ...,
  caption = "",
  note = "",
  output = which_output(),
  type = 1,
  names = NULL
)

\method{Reliability}{default}(
  x,
  revcoded = FALSE,
  check.keys = FALSE,
  max.level = NA,
  min.level = NA,
  type = "mean",
  na.rm = TRUE,
  ...
)

Index(
  x,
  revcoded = FALSE,
  fun = "mean",
  na.rm = TRUE,
  digits = 4,
  max.level = NA,
  min.level = NA,
  return.index = TRUE,
  ...
)
}
\arguments{
\item{...}{an psych::alpha()}

\item{data}{data.frame mit den Daten}

\item{name}{Slalen namen}

\item{type}{Aggregatfunktion fuer die Skala (mean, median und trimmed)}

\item{revcoded}{position zum umcodieren. Kann entweder nummer oder name oder TRUE sein.}

\item{max.level, max.level}{an aggregate}

\item{na.rm}{Fehlende Werte}

\item{return.index}{TRUE/FALSE index oder Daten}

\item{check.keye}{aus psych wenn \code{check.keye=TRUE} 
gesetzt wird werden die Daten automatisch umkodiert}
}
\value{
\code{Reliability()} gibt eine Liste mit den Resultaten und den transformierten Daten
"data", "range", "labels", "keys", "psych",
 "item_statistik", "Alpha", "index", "Items","n",
 "M", "SD","Skew", "Kurtosi", "shapiro"

 psych: ist psych::alpha

 index: ist dabei der Mittelwert-Index

 data: Daten Umcodiert.

 keys:  Umcodiert.

Vektor
}
\description{
\code{Reliability()} Reliabilitaets Analyse mit Cronbach-Alpha + Mittelwerte

\code{Appha} Cronbach-Alpha Werte extrahieren

 Cronbachs Alpha oder einfach nur α ist ein Maß für die interne Konsistenz einer Skala.

 > .9	Exzellent

 > .8	Gut / Hoch

 > .7	Akzeptabel

 > .6	Fragwürdig

 > .5	Schlecht / Niedrig

 < .5	Inakzeptabel

Quelle http://statistikguru.de/spss/reliabilitaetsanalyse/auswerten-und-berichten-2.html

\code{Index} Summen Index eine Summenfunktion mit der Erweiterung zum Umcodieren
}
\details{
Deutsch
Um die interne Konsistenz zu bestimmen, wurde Cronbachs Alpha für die Subskala positiver Affekt (insgesamt zehn Fragen) berechnet. Die interne Konsistenz war hoch, mit Cronbachs Alpha = .89 für positiven Affekt.

English
For reliability analysis, Cronbach’s alpha was calculated to assess the internal consistency of the subscale for positive affect, which consists of ten questions. The internal consistency of the questionnaire is satisfying, with Cronbach’s alpha for positive affect = .89.
}
\examples{


require(stp25data)
n<-200
x<- runif(n, min = 1, max = 5)
set.seed(0815)
data<- data.frame( x1 = x+rnorm(n),
                   x2 = x+rnorm(n),
                   x3 = x+rnorm(n),
                   x4 = x+rnorm(n),
                   x5 = 6-x+rnorm(n),
                   x6 = x+rnorm(n),
                   x7 = x+rnorm(n))

Reliability(data)
Reliability(data, revcoded=TRUE)
Reliability(data, check.keys=TRUE)
Reliability(data, revcoded=5)
Reliability(data, revcoded="x5")

library(lavaan)
population.model <- '
Fachinteresse =~ F1+F2+F3+F4+F5
Soziale.Einbindung =~ S1+S2+S3+S4
Relevanz.Inhalte=~ R1+R2+R3+R4
Kompetenzerleben =~ K1+K2+K3+K4
Autonomieerleben=~ A1+A2+A3+A4+A5+A6
Motivierungsqualitaet=~ M1+M2+M3+M4
'

# generate data
set.seed(1234)
DF <- simulateData(population.model, sample.nobs=60 )
DF[1,10]<- NA
DF<-stp25aggregate::dapply2(DF, function(x) cut(x, 5, 1:5))

DF<-stp25aggregate::dapply2(DF)
DF[,1]<- 6-DF[,1]
DF[,3]<- 6-DF[,3]


Fachinteresse <- Reliability( DF[ , Cs(F1,F2,F3,F4,F5)], check.keys=TRUE)
APA_Reliability( DF[ , Cs(F1,F2,F3,F4,F5)], check.keys=TRUE)


APA(Reliability(~F3+F2+F10+F11, fkv, check.keys =TRUE))


# ALPHA
Alpha( Fachinteresse)
Distanz <-  Reliability(~F3+F2+F10+F11, fkv, check.keys =TRUE)

Distanz \%>\% Alpha()
Alpha(Distanz)

Verarbeitung <- Reliability(~ F5+F16+F22+F9+F26+F6+F35+F33+F12+F34+F4, fkv, check.keys =TRUE)
Coping <- Reliability(~ F7+F8+F17+F14+F15+F18+F19+F1+F13+F20, fkv, check.keys =TRUE)
Vertrauen <- Reliability(~ F28+F27+F31+F29, fkv, check.keys =TRUE)
Religion <- Reliability(~F21+F25+F30+F23+F24, fkv, check.keys =TRUE)
Distanz <- Reliability(~F3+F2+F10+F11, fkv, check.keys =TRUE)


Alpha(Verarbeitung, Coping, Vertrauen, Religion, Distanz)
}
