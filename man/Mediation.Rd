% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regression-mediation.R
\name{Mediation}
\alias{Mediation}
\alias{Sobel_Test}
\title{Causal Mediation Analysis}
\usage{
Mediation(
  y_x,
  y_xm,
  m_x,
  y_inter_xm = NULL,
  data = NULL,
  caption = "",
  note = "",
  output = which_output(),
  include.parameter = TRUE,
  include.p = TRUE,
  include.sobl = TRUE,
  z.transform = TRUE,
  treat = model_info(m_x)$x[1],
  mediator = model_info(m_x)$y,
  type = "long2",
  ...
)

Sobel_Test(y_x, y_xm, m_x, treat, mediator, digits = 2)
}
\arguments{
\item{y_x}{y~x formula oder lm-Objekt}

\item{y_xm}{y~x+m formula oder lm-Objekt}

\item{m_x}{m~x formula oder lm-Objekt}

\item{y_inter_xm}{m~x*m formula oder lm-Objekt}

\item{data}{Daten bei der Verwendendung von Formeln}

\item{caption, note, output}{an Output}

\item{include.parameter, include.sobl}{was soll Ausgegeben werden}

\item{z.transform}{Transformation bei der Verwendendung von Formeln}

\item{treat, mediator}{Einfluss und  Mediatorals string}

\item{type, include.p, ...}{fuer Regression APA_Tabelle()}

\item{digits}{Nachkommastellen}

\item{y_x, y_xm, m_x}{in Sobel-Test lm-Objekte}
}
\value{
list(param, sobel, methode)

data.frame
}
\description{
Einfache Mediation und Moderationsanalyse mit Sobel-Test
Mediation ist gegeben wenn alle Modelle signifikant (1) und (4)
}
\details{
(1) Y~X
  (2) Y~X+M  (wenn hier M signifikant ist => partielle Mediation)
  (4) M~X

  Moderation ist gegeben wenn die Interaktion (X:M) signifikant ist

  (3) Y~ X + M + X:M
  
  Ein Beispiel mit Laavan findet sich unter
  https://paolotoffanin.wordpress.com/2017/05/06/multiple-mediator-analysis-with-lavaan/
}
\examples{
#' 
## https://paolotoffanin.wordpress.com/2017/05/06/multiple-mediator-analysis-with-lavaan/
## simpleMediation
#Projekt("html", "Test")
set.seed(1234)
Motivation <- rnorm(100)
Lerndauer <- 0.5 * Motivation + rnorm(100)
Note <- 0.7 * Lerndauer + rnorm(100)
dat <- data.frame(Note = Note,
                  Lerndauer = Lerndauer,
                  Motivation = Motivation)

dat <- stp25aggregate::Label(dat,
                             Note = "Note (Ziel-Variable)" ,
                             Lerndauer = "Lerndauer (Mediator)",
                             Motivation = "Motivation (Einfluss-Variable)")
Tabelle2(dat, Note, Lerndauer, Motivation)
Mediation(
  Note ~ Motivation,
  Note ~ Motivation + Lerndauer,
  Lerndauer ~ Motivation,
  Note ~ Motivation * Lerndauer,
  dat,
  digits = 2,
  caption = "Einfache Mediation und Moderationsanalyse mit Sobel-Test"
)

model <- ' # direct effect
Note ~ c*Motivation
# mediator
Lerndauer ~ a*Motivation
Note ~ b*Lerndauer
# indirect effect (a*b)
indir := a*b
# total effect
total := c + (a*b)
'

#xyplot(Note ~ value |
#         variable,
#       melt2(dat, Motivation, Lerndauer, by =  ~ Note))


fit <-  lavaan::sem(model, data = dat)
APA2(fit)
#End()
#' 

# Daten -------------------------------------------------------------------


set.seed(1234)
n <- 2 * 10

Motivation <- rnorm(n)
Lerndauer <- 0.5 * Motivation + rnorm(n)
Note <- 0.7 * Lerndauer + rnorm(n)

t0 <- data.frame(
  id = 1:n,
  sex = gl(2, n / 2, labels = c("m", "f")),
  time = 0,
  Note = Note,
  Lerndauer = Lerndauer,
  Motivation = Motivation
)

t1 <- rnorm(n, 0.1, 0.1)
t1 <- data.frame(
  id = 1:n,
  sex = gl(2, n / 2, labels = c("m", "f")),
  time = 1,
  Note = Note * .1 + t1,
  Lerndauer = Lerndauer * .1 + t1,
  Motivation = Motivation * .1 + t1
)

t2 <- rnorm(n, 0.2, 0.5)
t2 <- data.frame(
  id = 1:n,
  sex = gl(2, n / 2, labels = c("m", "f")),
  time = 2,
  Note = Note * .15 + t2  ,
  Lerndauer = Lerndauer * .15 + t2,
  Motivation = Motivation * .15 + t2
)
dat <- rbind(t0, t1, t2)





#model1 <- lm(Note ~ Motivation +time, dat)
#model2 <- lm(Note ~ Motivation + Lerndauer+time, dat)
#model3 <- lm(Lerndauer ~ Motivation+time, dat)


model1 <- lmerTest::lmer(Note ~ Motivation +time+ (1 | id), dat)
model2 <- lmerTest::lmer(Note ~ Motivation + Lerndauer +time+ (1 | id), dat)
model3 <- lmerTest::lmer(Lerndauer ~ Motivation +time+ (1 | id), dat)


Sobel_Test(model1, model2, model3,
           treat="Motivation",
           mediator="Lerndauer" )
}
