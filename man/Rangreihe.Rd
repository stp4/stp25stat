% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/describe-rank.R
\name{Rangreihe}
\alias{Rangreihe}
\alias{Rangreihe_default}
\title{Rangreihe}
\usage{
Rangreihe(
  ...,
  caption = "Rangreihe",
  note = "Law of Categorical Judgement",
  output = stp25output::which_output(),
  na.action = na.pass,
  include.percent = TRUE,
  include.freq = TRUE,
  include.mean = TRUE,
  include.z = TRUE,
  include.na = TRUE,
  groups = NULL,
  order = TRUE,
  decreasing = TRUE,
  digits.mean = 2
)

Rangreihe_default(
  items,
  caption = "",
  note = "",
  output = stp25output::which_output(),
  include.percent = TRUE,
  include.freq = TRUE,
  include.mean = TRUE,
  include.z = TRUE,
  include.na = TRUE,
  groups = NULL,
  order = TRUE,
  decreasing = TRUE,
  digits.mean = 2,
  input = NULL,
  pattern = "____"
)
}
\arguments{
\item{...}{Weitere Argumente}

\item{caption, note, output}{an stp25output}

\item{include.percent, include.freq, include.mean, include.z, include.na}{was soll ausgewertet werden}

\item{groups}{gruppen}

\item{digits.mean, order, decreasing}{sortierung}

\item{items}{data.frame}

\item{input}{Format der Items c("ranking", "ordering"),}

\item{pattern}{intern gruppen}
}
\value{
Vector
}
\description{
Rangordnungen von Objekten koennen durch eine Transformation der Rangreihen in
Intervallskalierte Merkmale ueberfuehrt werden. Die Grundidee dieser Methode geht
auf Thurstone (1927) nach dem "Law of Categorical Judgement" zurueck.
}
\details{
Dabei werden
die kumulierten Haeufigkeiten in Normalverteilte z-Werte uebergefuehrt und aus diesen
die Intervallskalierten Markmalsauspraegungen gebildet.

Literatur: Bortz, J. & Doering, N. (2006). Forschungsmethoden und
 Evaluation fuer Human-und Sozialwissenschaftler (4. Auflage). Berlin: Springer. Seite 155
}
\examples{


require(stpvers)
library(PlackettLuce)

nlv <- 5
n <- 2 * 3 * nlv * 1
set.seed(n)

DF <-
  data.frame(
    Geschlecht = gl(2, n / 2, labels = c("Maennlich", "Weiblich")),
    Alter = gl(4, n / 4,   labels = c("20-29", "30-39", "40-49", "50-59")),
    Landwirtschaft = gl(2, n / 2, labels = c("konventionell", "biologisch"))
  )

Attribute <-
  as.data.frame(t(apply(matrix(NA, ncol = n, nrow = 5), 2,
                        function(x)
                          sample.int(5))))

Attribute[1, ] <- c(5, 1, 4, 2, 3)
Attribute[2, ] <- c(5, 1, 4, 2, 3)
Attribute[3, ] <- c(5, 2, 4, 3, 1)
Attribute[4, ] <- c(5, 1, 4, 3, 2)
Attribute[5, ] <- c(5, 1, 4, 3, 2)

Attribute[21, ] <- c(1, 2, 5, 4, 3)
Attribute[22, ] <- c(1, 4, 5, 3, 2)
Attribute[23, ] <- c(2, 5, 1, 4, 3)
Attribute[24, ] <- c(1, 4, 2, 5, 3)
Attribute[25, ] <- c(1, 4, 3, 5, 2)

attribute  <- c("Verfuegbarkeit",
                "Vielfalt",
                "Qualitaet",
                "Geschmack",
                "Preis")

Attribute<- dapply2(Attribute, function(x) factor(x, 1:5, attribute))

DF <- cbind(DF, Attribute)

head(DF)

res <-
  Rangreihe( ~ V1+V2+V3+V4+V5,
             DF, include.percent=FALSE, order=FALSE, include.na=FALSE,
             caption="Produkte aus konventioneller und biologischer  Landwirtschaft")

res$input
names(res)
x<- res$res





R <- as.rankings(res$items, res$input)

mod <- PlackettLuce( R )
coef(mod)


summary(mod)
x$pc <-  round(coef(mod, log = FALSE) ,2)
x$log.pc <- round(coef(mod, log = TRUE) ,2)
x[order(x$pc,decreasing=TRUE),] 



DF1 <-  data.frame(
  A = c(1, 1, 1, 2, 3, 1),
  B = c(2, 2, 2, 3, 2, 3),
  C = c(3, 3, 3, 1, 1, NA),
  D = c(NA, NA, NA, NA, NA, 2)
)
DF2 <-   data.frame(
  R1 = factor(c("A", "A", "A", "C", "C", "A"),   c("A", "B", "C", "D")),
  R2 = factor(c("B", "B", "B", "A", "B", "D"),   c("A", "B", "C", "D")),
  R3 = factor(c("C", "C", "C", "B", "A", "B"),   c("A", "B", "C", "D"))
)


Rangreihe(DF1)$mean
Rangreihe(DF2)$mean

dat_bortz<-
  as.table(matrix(c(
    2,8,10,13,17,
    5,10,15,18,2,
    10,12,20,5,3,
    15,20,10,3,2,
    22,18,7,2,1)
    , nrow = 5, ncol=5, byrow=TRUE,
    dimnames = list(c("A", "B", "C", "D", "E"),1:5)))  

Rangreihe(dat_bortz)



# dat_table <-
#   as.table(matrix(c(
#     50,0,0,0,0,
#     0,50,0,0,0,
#     0,0,50,0,0,
#     0,0,0,50,0,
#     0,0,0,0,50
#      )
#     , nrow = 5, ncol=5, byrow=TRUE,
#     dimnames = list(c("A", "B", "C", "D", "E"),1:5)))
#  # Calc_Rank(dat_table)



n <- 2 * 3 * 4 * 1

set.seed(n)
kaffee <- c("Guatemala", "Vietnam", "Honduras", "Ã„thiopien")
sex<- c("male", "female")
age<- c("20-29", "30-39", "40-49", "50-59")
kaffe<- c("Espresso", "Filterkaffee", "Milchkaffee")

DF <-
  data.frame(
    sex = factor("male",sex),
    Alter = factor("20-29",age ),
    Kaffeeform = factor("Espresso", kaffe),
    R1 = factor(kaffee[1], kaffee),
    R2 = factor(kaffee[2], kaffee),
    R3 = factor(kaffee[3], kaffee),
    R4 = factor(kaffee[4], kaffee)
  )

DF<- rbind(DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF)

for(i in 1:n){
  DF<- rbind(DF,
             c(sample(sex)[1],
               sample(age)[1],
               sample(kaffe)[1],
               sample(kaffee)
             ))
}

x <- DF[4:7]
Rangreihe(x, include.percent=FALSE, groups=DF$sex)
x<-Rangreihe(R1 + R2 +R3 ~sex, DF, include.percent=FALSE, output=FALSE)


names( x)
x$mean

#' 
#lattice::dotplot( reorder(Items, mean)~ mean|"Kaffee", 
                  x$mean, groups=group , xlab="",
#                  xlim=range(x$mean$mean)*1.10 , auto.key=list(), cex=1)
 
}
