## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


 require(stpvers)
require(flextable)
Projekt("text",  "Exampel")
#set_my_options(output="html")
 

 
set_my_options(prozent = list(
 digits = c(0, 0),
 style = 2,
 null_percent_sign = "."
))

vars <- c(
  "o1",  "o2",  "o3",  "o4",  "o5",  "c1",  "c2",  "c3",
  "c4",  "c5",  "e1",  "e2",  "e3",  "e4",  "e5",  "n1",
  "n2",  "n3",  "n4",  "n5",  "a1",  "a2",  "a3",  "a4",  "a5"
)


## ----DiagrammeR, echo=FALSE----------------------------------------------
library(DiagrammeR)

 DiagrammeR("graph RL;

Openness-->personality
Conscientiousness-->personality
Extraversion-->personality
Neuroticism-->personality
Agreeableness-->personality
personality-->Einkommen
Alter-->Einkommen
Geschlecht-->Einkommen
 
 ")

## ---- include = FALSE----------------------------------------------------

n<-100

population.model <- '

Sex =~ sex
Age =~ age
Bildungszertifikat =~ edu
Beruf =~ beruf
Education  ~ 1.04*Bildungszertifikat + 2.76* Beruf
 
single~ 1*age + 2*edu

Openness =~ o1+o2+o3+o4+o5
Conscientiousness =~ c1+c2+c3+c4+c5
Extraversion =~ e1+e2+e3+e4+e5
Neuroticism =~  n1+n2+n3+n4+n5
Agreeableness =~ a1+a2+a3+a4+a5

Einkommen ~ 1.25*Sex +  (-0.31)*Age  + .12*Openness + .23*Conscientiousness + (-0.91)*Extraversion + 2.3*Neuroticism + (-0.541)*Agreeableness

'
 

set.seed(1234)
DF <- lavaan::simulateData(population.model, sample.nobs = n)
scale2<- function(x, mn=1, mx=5){
 
x<-  x-min(x, na.rm=TRUE)
x<-  x/max(x, na.rm=TRUE)
x*(mx -1 )+mn
}
DF<- transform(DF, sex=cut(sex, 2, c("m", "f")),
               age=scale2(age, 18, 55),
               edu=cut(edu,3, c("low", "med", "hig")),
               beruf= cut(edu,2, c("blue", "withe")),
               single =cut(single, 2, c("yes", "no"))
               
               )
DF$Einkommen <- (4-log( DF$Einkommen  - min(DF$Einkommen ) +1 ) )*1000


DF[, -c(1:4, ncol(DF))] <- stp25aggregate::dapply2(DF[, -c(1:4)], 
function(x) {
  if(is.numeric(x)) cut(x, 5, 1:5) else x})



## ----default-table, results='asis'---------------------------------------
 names(DF)
DF %>% Tabelle2(sex, age, edu, beruf,single, APA = TRUE, caption= "Characteristics")

## ----pca-table, results='asis'-------------------------------------------

 APA_PCA(DF[vars], 5, cut=.35,include.plot = FALSE)
 


## ----rel-table, results='asis'-------------------------------------------
 
Openness <- Reliability(~o1+o2+o3+o4+o5, DF, check.keys=TRUE)
Conscientiousness <- Reliability(~ c1+c2+c3+c4+c5, DF, check.keys=TRUE)
Extraversion <- Reliability(~ e1+e2+e3+e4+e5, DF, check.keys=TRUE)
Neuroticism <- Reliability(~  n1+n2+n3+n4+n5, DF, check.keys=TRUE)
Agreeableness <- Reliability(~ a1+a2+a3+a4+a5, DF, check.keys=TRUE)

DF$O<- Openness$index
DF$C<- Conscientiousness$index
DF$E<- Extraversion$index
DF$N<- Neuroticism$index
DF$A<- Agreeableness$index

Alpha2(Openness,Conscientiousness,Extraversion,Neuroticism,Agreeableness)


## ------------------------------------------------------------------------


DF %>% Tabelle2(Einkommen , sex, age , O , C , E , N , A)
fit<- lm(Einkommen ~ sex + age + O + C + E + N + A, DF)

APA_Table(fit)


## ------------------------------------------------------------------------
 

hist(DF$Einkommen)

## ---- fig.cap = "Your figure caption.", fig.width=8, fig.height=8--------
 
# MySet()
require(effects)
 plot(allEffects(fit), main="")


## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

