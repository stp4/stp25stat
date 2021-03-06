## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup2, include=FALSE----------------------------------------------------
 #getwd()
library(stpvers)
require(broom)
#require(nlme)

set.seed(1)
Projekt("", "Introduction" )
#set_my_options(output="html")

hkarz$Lai<- factor(hkarz$lai, 0:1, c("negativ", "positiv"))
hkarz<- upData2(hkarz, c(gruppe="Gruppe"
                      ,tzell="T-Zelltypisierung"
                      ,Lai="LAI-Test"))

## -----------------------------------------------------------------------------
Tabelle()

## -----------------------------------------------------------------------------
APA2()

## ---- include=FALSE-----------------------------------------------------------
smokers  <- c( 83, 90, 129, 70 )
patients <- c( 86, 93, 136, 82 )

n<-999
g =gl(3, n/3, labels = c("Control", "Treat A", "Treat B"))
g2<- g[sample.int(n)]
levels(g2)<- c("male", "female", "female")
data<- data.frame(g=g, g2=g2,
                  x=rnorm(n) )[sample.int(n)[1:78],]


## ----simpel-ferq, results='asis', warning=FALSE-------------------------------
 APA_CI( data, g )  
#  library(BayesianFirstAid)
#  x<-as.data.frame(table(data$g))$Freq
#  bayes.prop.test(x, rep(nrow(data), nlevels(data$g)), p=1/3)
 

## ----tzel-median, results='asis', warning=FALSE-------------------------------
hkarz %>% Tabelle2(tzell[median], Lai, gruppe) 

set_my_options(median=list(style="IQR"))

APA2(tzell[1]+Lai~gruppe, hkarz, 
     type=c("auto", "median"),
     caption="Einfache Auswertung",
     test=TRUE, include.n = TRUE)

## ----ttest-1, results='asis', warning=FALSE-----------------------------------
# T-Test
 APA( t.test(m1 ~ geschl, varana, var.equal=TRUE))


## ----ttest-2, results='asis', warning=FALSE-----------------------------------
 APA( t.test(m1 ~ geschl, varana))

## ----ttest-3-aov, results='asis', warning=FALSE-------------------------------
 APA(aov( m1 ~ geschl, varana ))

## ----ttest-4, results='asis', warning=FALSE-----------------------------------
APA2(m1 + m2 ~ geschl, varana, test = "t.test")


## ----ttest-5, results='asis', warning=FALSE-----------------------------------
APA_Ttest(m1 + m2 ~ geschl, varana)

## ----ttest-6, results='asis', warning=FALSE-----------------------------------
#varanax<-Melt2(m1+m2~nr,varana , key="time", value="m")
# broom::tidy(with(varana, t.test(m1,m2 ) ))
# broom::tidy(  t.test(m~time, varanax, var.equal=FALSE)) )

#APA_Ttest(m~time, varanax, paired = TRUE, include.mean=FALSE)
#APA_Ttest(m~time, varanax, paired = TRUE, type="U-Test", include.mean=FALSE)
#APA_Ttest(m~time, varanax, var.equal=TRUE, include.mean=FALSE)



#https://www.r-bloggers.com/is-it-time-to-ditch-the-comparison-of-means-t-test/

#exakt das selbe wie t.test(m1 ~ geschl, varana, var.equal=TRUE))
#t.test(m1 ~ geschl, varana, var.equal=TRUE))

res <-
summary(nlme::gls(m1 ~ geschl, varana,  weights = nlme::varIdent(form = ~ 1 |
geschl)))
res$tTable %>% fix_format() %>% Output(caption = "Generalized Least Squares")



## -----------------------------------------------------------------------------
APA_Xtabs()

## ----xtab-prepare, include=FALSE----------------------------------------------
hkarz$LAI<- factor(hkarz$lai, 0:1, c("pos", "neg"))
hkarz$Tzell<- cut(hkarz$tzell, 3, c("low", "med", "hig"))

## ----xtab-2x2, results='asis'-------------------------------------------------
xtab <- xtabs(~ LAI+ gruppe, hkarz)
APA2(xtab, caption="Harnblasenkarzinom")
APA_Xtabs(~ LAI+ gruppe, hkarz, include.percent=FALSE, include.test=TRUE)


## ----xtab-NxMxO---------------------------------------------------------------
data(infert, package = "datasets")
infert$case  <- factor(infert$case ,1:0, c("case", "control") )
 
infert$spontaneous <- factor(infert$spontaneous)
infert$induced2    <- factor(infert$induced==0)

tab_1<- xtabs(~  case, infert)
tab_2x2<- xtabs(~ induced2 + case, infert)
tab_3x2<- xtabs(~ induced + case, infert)
tab_3x3<- xtabs(~ induced + education, infert)
tab_3x3x2<- xtabs(~ induced + education+case, infert)

#APA2(summary(tab_3x3x2))

(APA2(tab_1, include.test=TRUE, output=FALSE))
(APA2(tab_2x2, include.test=TRUE, output=FALSE))
(APA2(tab_3x2, include.test=TRUE, output=FALSE))
(APA2(tab_3x3, include.test=TRUE, output=FALSE))
(APA2(tab_3x3x2, include.test=TRUE, output=FALSE))
 

## ----epi-tests, results='asis'------------------------------------------------
 
require(epiR)
DF<- GetData("
Diagnose RT.qPCR Anzahl
krank positiv 111
krank negativ 12
gesund positiv 2
gesund negativ 62 ", 
             Tabel_Expand =TRUE, id.vars=1:2, output=FALSE)
DF$Diagnose<- factor( DF$Diagnose, rev(levels( DF$Diagnose)))
DF$RT.qPCR<- factor( DF$RT.qPCR, rev(levels( DF$RT.qPCR)))

 
APA2(epi.tests(xtabs(~ RT.qPCR + Diagnose, DF)))
 
APA_Xtabs(~ RT.qPCR + Diagnose, DF, include.diagnostic=TRUE, 
          caption="caret confusionMatrix")
 


## ----xtab-klass---------------------------------------------------------------
 
fit1<- glm(Diagnose~RT.qPCR, DF, family = binomial)

 
  Klassifikation(fit1)$statistic[c(1,7,8),]

## ----fig-roc-1, fig.cap = "ROC-Kurve zu den Blutzuckerwerten", fig.width=4, fig.height=4----

require(pROC)

blz.diab <- round(rnorm(100, mean = 115, sd = 20), 1)
blz.cntr <- round(rnorm(100, mean = 85, sd = 20), 1)
data <- data.frame(Gruppe = factor(c(
  rep("Diabetiker", length(blz.diab)),
  rep("Kontrollen", length(blz.cntr))
)),
Blutzucker = c(blz.diab, blz.cntr))


roc_curve <-  roc(data$Gruppe, data$Blutzucker)
plot(roc_curve, print.thres = "best", print.auc = TRUE)


## ----fig-roc-2, fig.cap = "ROC-Kurve zu den Blutzuckerwerten", fig.width=4, fig.height=4----

fit1  <- glm(Gruppe ~ Blutzucker, data, family = binomial)
x1 <- Klassifikation(fit1)
roc_curve   <- roc(x1$response, x1$predictor)

plot(roc_curve, print.thres = "best", print.auc = TRUE)
abline(v = 1, lty = 2)
abline(h = 1, lty = 2)

## ----fig-roc-3, fig.cap = "ROC-Kurve zu den Blutzuckerwerten", fig.width=4, fig.height=4----

fit1 <- glm(gruppe ~ lai, hkarz, family = binomial)
fit2 <- glm(gruppe ~ lai + tzell, hkarz, family = binomial)
#thkarz <- as.data.frame(xtabs(~gruppe+lai, hkarz))
#fit2<- glm(Freq ~ gruppe*lai, thkarz, family = poisson())
x1 <- Klassifikation(fit1)
x2 <- Klassifikation(fit2)
#require(pROC)
roc1   <- roc(x1$response, x1$predictor)
roc2   <- roc(x2$response, x2$predictor)

plot(roc1, print.auc = TRUE, print.auc.y = 0.6)
plot(
  roc2,  lty = 2,  col = "blue",
  print.auc.y = 0.7,  print.auc = TRUE,
  add = TRUE
)
legend(
  "bottomright",
  legend = c("Lai", "Lai + T-Zell"),
  col = c(par("fg"), "blue"),
  lty = 1:2,  lwd = 2
)
roc.test(roc1, roc2)


## ----effsize-cohen, results='asis', warning=FALSE-----------------------------
# APA2(tzell~gruppe,hkarz, type="cohen.d")  # APA_Effsize ist das gleiche

1+1


## ----corr, include=FALSE------------------------------------------------------
 n<- 2*8
 e<- rnorm(n)*10
 DF<- data.frame(a=rnorm(n) + e,
                    b=rnorm(n), c=rnorm(n), d=rnorm(n) + e,
                    g=gl(2, 8, labels = c("Control", "Treat")))
  

## ----cor-apa-formula, results='asis', warning=FALSE---------------------------
   APA_Correlation(~a+b+c, DF, caption="Formula a+b+c", output= "text")
#   in der Tabelle gibt es seltsame sonerzeichen ist nicht in caption
 


## ----likert-data, include=FALSE-----------------------------------------------
 set.seed(1)
n<-100
lvs<-c("--","-","o","+","++")
DF2<- data.frame(
  Magazines=gl(length(lvs),1,n,lvs),
  Comic.books=gl(length(lvs),2,n,lvs),
  Fiction=gl(length(lvs),3,n,lvs),
  Newspapers=gl(length(lvs),5,n,lvs))
DF2$Comic.books[sample.int(n/2)]<- lvs[length(lvs)]
DF2$Newspapers[sample.int(n/2)]<- lvs[1]
DF2$Magazines[sample.int(n/2)]<- lvs[2]

DF2<- transform(DF2, Geschlecht= cut( rnorm(n), 2, Cs(m, f)))
  

## ---- results='asis', warning=FALSE-------------------------------------------
Res1 <- Likert(~., DF2 )
APA2(Res1)

## ----likert-apa, results='asis', warning=FALSE--------------------------------
APA2(Res2 <- Likert(.~ Geschlecht, DF2 ))
APA2(Res2, ReferenceZero=3, na.exclude=TRUE, type="freq")

  

## ----likert-plot, results='asis', warning=FALSE-------------------------------
#require(HH)
# ?likertplot
#
#windows(7,3)
#likertplot( Item   ~ .| Geschlecht , data=Res2$results,
#            main='',ylab="", sub="" ,
#            # col=brewer_pal_likert(5, "RdBu", "Geschlechtay80") ,
#            positive.order=TRUE,
#            as.percent = TRUE,
#            auto.key=list(space="right", columns=1)
#)
#data<- Res2$names
#data$mean<- Res2$m
#  barchart( Item~ mean |Geschlecht, Mymean2, xlim=c(1,5))
#windows(3,6)
#dotplot(Item ~ mean, data,
#        groups=Geschlecht, xlim=c(.085, 5.15),
#        type=c("p", "l"))  

## ----rank-data, include=FALSE-------------------------------------------------

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


## ----rank-apa, echo=TRUE, results='asis', warning=FALSE-----------------------
 res <-
  Rangreihe( ~ V1+V2+V3+V4+V5,
             DF, 
             include.percent=FALSE, 
             order=FALSE, 
             include.na=FALSE,
             caption="Produkte aus konventioneller und biologischer  Landwirtschaft")



## ----rank-pcl-----------------------------------------------------------------
 
names(res)
 

R <- as.rankings(res$items)

mod <- PlackettLuce( R )
coef(mod)

summary(mod)


summary(mod, ref = "Verfuegbarkeit")
round(coef(mod, log = TRUE, ref = "Verfuegbarkeit") ,2)
round(coef(mod, log = TRUE ) ,2)


res$res$pc <-  round(coef(mod, log = FALSE) ,2)
res$res$log.pc <- round(coef(mod, log = TRUE) ,2)
res$res[order(res$res$pc,decreasing=TRUE),] 








## ----met-comp-data, Giavarina-data, include=FALSE-----------------------------
set.seed(0815)
Giavarina <- data.frame(
A=c(1,5,10,20,50,
    40,50,60,70,80,
    90,100,150,200,250,
    300,350,400,450,500,
    550,600,650,700,750,
    800,850,900,950,1000),
B=c(8,16,30,14,39,
    54,40,68,72,62,
    122,80,181,259,275,
    380,320,434,479,587,
    626,648,738,766,793,
    851,871,957,1001,980),
group= sample(gl(2, 15, labels = c("Control", "Treat")))
)


## ----tab-giavarina, results='asis'--------------------------------------------

MetComp(~A+B, Giavarina, caption = "Giavarina")


## ----sachs-627-data, include=FALSE--------------------------------------------
Botulinum <- data.frame(
  A= factor(c(rep(1, 14), rep(1, 3),
              rep(0, 5),rep(0, 18)),
            1:0, c("+", "-")),
  B= factor(c(rep(1, 14), rep(0, 3),
              rep(1, 5),rep(0, 18)),
            1:0, c("+", "-")))

## ----tab-sachs, results='asis'------------------------------------------------
# APA2(xtabs(~A+B, Botulinum), test=TRUE)

MetComp(~A+B, Botulinum)
 

## ----tab-sachs2, results='asis'-----------------------------------------------
 xt <-xtabs(~A+B, Botulinum)
 Klassifikation(xt)$statistic[c(1,3,4), ]

## ----tab-icc2, results='asis'-------------------------------------------------
Giavarina <- transform(Giavarina, C = round( A + rnorm(30,0,20)),
                D = round( A + rnorm(30,0,10) + A/10 ),
                E = round( A + rnorm(30,5,10) + (100-A/10) ))


#ICC2(~A+E, Giavarina, caption="ICC (Korrelationen)")
 

## ----met-comp-data2, include=FALSE--------------------------------------------


set.seed(0815)

n<-100
DF<- data.frame(
  A=rnorm(n, 100,50),
  B=rnorm(n, 100,50),
  C=NA,  D=NA,  E=NA,
  group= sample(gl(2, n/2, labels = c("Control", "Treat")))
)

cutA<-mean(DF$A)
DF <- transform(DF, C = round( A + rnorm(n, -5, 20)),
                D = round( A + rnorm(n,0,10) + A/10 ),
                E = A + ifelse(A<cutA, A/5, -A/5 )+ rnorm(n, 0, 10)
)



## ----tab-anova-1, results='asis', warning=FALSE-------------------------------
#' breaks ~ wool + tension 
#' warpbreaks %>% Tabelle2(breaks, by= ~ wool + tension)
fm1 <- aov(breaks ~ wool + tension, data = warpbreaks)
APA2(fm1, caption="ANOVA")

## ----tab-anova-2, results='asis', warning=FALSE-------------------------------
#'  R Squared = .43 (Adjusted R Squared = .43)
#' Levene's Test of Equality of Error Variances F=1.8, df1=3,df2=486, p=.146
#' 
schools2 <-  transform(
  schools,
  classroom = factor(classroom),
  grade = factor(grade),
  treatment = factor(treatment),
  score10 = score > 10,
  score2 =  round((log((
  score + 20
  )) - 1.78) / .023)
  
  )
  
  fit <- aov(score ~ grade + treatment + stdTest, schools2)
  
  APA2(fit, include.eta = TRUE)

#APA_Validation(fit, include.levene = TRUE,
#               include.aic = FALSE, include.ftest = FALSE,include.deviance = FALSE,
#               include.heteroskedasticity = FALSE,
#               include.durbin = FALSE, include.normality = FALSE)


## ----tab-anova-tukey, results='asis', warning=FALSE---------------------------
TukeyHSD(fm1, "tension", ordered = TRUE) %>%
  APA2(caption="TukeyHSD" )

#' plot(TukeyHSD(fm1, "tension"))
#' levels(warpbreaks$tension)
#' Lm Split

fm1_split <-  summary(fm1,
                      split=list(tension=list( M=1,  H=3, L=2)),
                      expand.split=FALSE)
APA2(fm1_split)


## ----tab-anova-tukey2, results='asis', warning=FALSE--------------------------

require(multcomp)

fit_Tukey <- glht(fm1,
linfct = mcp(tension = "Tukey"),
alternative = "less")

APA2(fit_Tukey, caption = "APA2: multcomp mcp Tukey")


## ----tab-anova-contrast, results='asis', warning=FALSE------------------------

### contrasts for `tension'
K <- rbind(
  "L - M" = c(1,-1,  0),
  "M - L" = c(-1,  1,  0),
  "L - H" = c(1,  0,-1),
  "M - H" = c(0,  1,-1)
  )
  
  warpbreaks.mc <- glht(fm1,
  linfct = mcp(tension = K),
  alternative = "less")
  APA2(warpbreaks.mc, caption = "APA2: multcomp mcp mit Contrasten")
### correlation of first two tests is -1
#cov2cor(vcov(fm1))

### use smallest of the two one-sided
### p-value as two-sided p-value -> 0.0232
#summary(fm1)


## ----tab-anova-tukey3, results='asis', warning=FALSE--------------------------

fm2 <- aov(breaks ~ wool * tension, data = warpbreaks)
APA2(fm2)
x <- TukeyHSD(fm2, "tension",
              ordered = TRUE)
APA2(x, caption="Interaction: TukeyHSD" )

warpbreaks$WW<-interaction(warpbreaks$wool,warpbreaks$tension )
mod2<-aov(breaks~WW, warpbreaks)
APA2(mod2, caption="ANOVA interaction haendich zu den Daten hinzugefuehgt")


## ----tab-lm-1, results='asis', warning=FALSE----------------------------------

fit1 <- lm(tzell ~ gruppe, hkarz)
fit2 <- lm(tzell ~ gruppe + Lai, hkarz)
fit3 <- lm(tzell ~ gruppe * Lai, hkarz)

APA_Table(fit1, fit2, fit3, type = "long", caption = "Regression")


## ----reg-glm-fit1, results='asis', warning=FALSE------------------------------

fit1 <- glm(gruppe~tzell, hkarz, family = binomial)
APA_Table(fit1, type="long")


## ----tab-broom, results='asis', warning=FALSE---------------------------------

res <- broom::tidy(fit1) 
cbind(res[1:2], 
      confint(fit1),
      res[5]) %>% 
  fix_format %>% Output("broom::tidy(fit1): Odds Ratios")


## ----tab-lm-lrtest, results='asis', warning=FALSE-----------------------------
#--Omnibus Test  Fuer F-Test :  lmtest::waldtest(fit1)
 lmtest::lrtest(fit1) %>% fix_format() %>% Output("lmtest::lrtest(fit1): LR-Test")


## ----tab-lm-gof, results='asis', warning=FALSE--------------------------------
Goodness <- function(x) {
  cbind(round(broom::glance(x)[, c(6,  3, 4, 5)], 1),
  round(R2(x), 2),
  round(RMSE(x)[2], 2))
}
fit1 %>% Goodness %>% Output()


## ----tab-lm-class, results='asis', warning=FALSE------------------------------

Klassifikation(fit1)




## ----tab-glm-1, results='hide', warning=FALSE---------------------------------


fit1 <- glm(gruppe~tzell+factor(lai), hkarz, family = binomial)
x<- APA2(fit1, include.odds=TRUE )
Nagelkerke<- R2(fit1)[3]

# Interpretation

txt_log_reg <-  paste("Eine logistische Regressionsanalyse zeigt, dass sowohl das Modell als Ganzes (",
 APA(fit1), 
 ") als auch die einzelnen Koeffizienten der Variablen signifikant sind.",
 "Steigen die T-Zelltypisierung  um jeweils eine Einheit, 
 so nimmt die relative Wahrscheinlichkeit eines Krank/Gesund um OR =",  x$odds[2], 
 "zu. Ist die  T-Zelltypisierung positiv so nimmt die  relative Wahrscheinlichkeit um OR= ", x$odds[2],
 "Das R-Quadrat nach Nagelkerke beträgt",round(Nagelkerke,2), 
" was nach Cohen (1992) einem starken Effekt entspricht."  )
# 

#  Quelle Text: http://www.methodenberatung.uzh.ch/de/datenanalyse/zusammenhaenge/lreg.html


## ----tab-glm-exp, warning=FALSE-----------------------------------------------

# Wahrscheinlichkeiten T-Zell
fit1 <- glm(gruppe ~ tzell , hkarz, family = binomial)
t.zell<-    c(50,55,60,65,70,75,80) #seq(50,80, by=1)  

i<- coef(fit1)["(Intercept)"]
b<-coef(fit1)["tzell"]

z <- i + b*t.zell
p<- 1/(1+exp(z)) 

cbind(t.zell, p=round(p,2))
 

## ---- results='asis', warning=FALSE-------------------------------------------
#-- SPSS kodiert die Gruppe 3 als Referenz
poisson_sim$prog <-
  factor(poisson_sim$prog, c("vocation", "general",  "academic"))
  fit5 <- glm(num_awards ~ prog + math,
  poisson_sim, family =  poisson())
  
  poisson_sim %>% Tabelle2(num_awards, prog, math)
  
  APA_Table(fit5)


## ----tab-glm-2, results='asis', warning=FALSE---------------------------------
APA2(xtabs(~ gruppe + lai, hkarz), test = TRUE, type = "fischer")
fit1 <- glm(gruppe ~ lai, hkarz, family = binomial)
thkarz <- as.data.frame(xtabs(~ gruppe + lai, hkarz))
fit2 <- glm(Freq ~ gruppe * lai, thkarz, family = poisson())

APA_Table(fit1, include.odds = TRUE)
APA_Table(
fit1,
fit2,
include.odds = TRUE,
include.b = FALSE,
include.se = FALSE,
type = "long"
)


## ----tab-lmer-1, results='asis', warning=FALSE--------------------------------
lmer_fit1<-lmerTest::lmer(score ~ agegrp+trial + (1|id), MMvideo)

lmer_fit2<-lmerTest::lmer(score ~ agegrp*trial + (1|id), MMvideo)
APA_Table(lmer_fit1, lmer_fit2, type="long")


# MySet()
# library(gridExtra)
#   windows(8,4)
#   p1 <- plot(effect("trial",lmer_fit1), multiline=TRUE, main="")
#   p2 <- plot(effect("agegrp*trial",lmer_fit2), multiline=TRUE, main="")
# 
#   grid.arrange(p1,p2,ncol=2)
# 
#  library(coefplot)
#  windows(4,3)
#   coefplot(lmer_fit2, intercept=F, xlab="b (SE)")
# 
#    windows(4,3)
#   multiplot(lmer_fit1, lmer_fit2, intercept=F, xlab="b (SE)")

## ----manova-data, include=FALSE-----------------------------------------------
m_data<-GetData("C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25data/extdata/manova.sav")
m_data$GROUP<- factor(m_data$GROUP, 1:3, c("website", "nurse ", "video tape" ))

z<- as.matrix(m_data[,-1])


## ----tab-manova-apa, results='asis', warning=FALSE----------------------------
 
fit1<- manova(z ~ m_data$GROUP)
APA2(fit1)
#summary(fit1)$Eigenvalues
#library(MASS)
#fit2 <- MASS::lda(GROUP ~ ., data=m_data)
#APA2(fit2)
#plot(fit2)

## -----------------------------------------------------------------------------
Diagnose_Plot <- function(data,
                          x,
                          y,
                          fit,
                          main = "",
                          ylab = "",
                          xlab = "",
                          level = 0.95) {
  q <- data[, x]
  predicted.intervals <-
    predict(fit,
            data[x],
            interval = 'confidence',
            level = level)
  
  plot(data[, x],
       data[, y],
       # col = 'deepskyblue4',
       xlab = xlab,
       ylab = ylab,
       main = '(a)')
  
  title("Observed data", line = .5)
  
  lines(q, predicted.intervals[, 1], col = 'deepskyblue4', lwd = 2)
  lines(q, predicted.intervals[, 2], col = 'black', lwd = 1)
  lines(q, predicted.intervals[, 3], col = 'black', lwd = 1)
  
  
  plot(fit, 1, main = "(b)",  cex.caption = .8)
  plot(fit, 2, main = "(c)",  cex.caption = .8)
  plot(fit, 3, main = "(d)",  cex.caption = .8)
  plot(fit, 5, main = "(e)",  cex.caption = .8)
  influence_year <- car::influencePlot(fit, main = '(f)')
  title("influence Plot", line = .5)
  
  invisible(influence_year)
  
}

#fit <- lm(Age ~ Concealing, data=df)
#par(mfrow = c(2, 3))
#infFall<- Diagnose_Plot(
#  df, "Concealing","Age",
#  fit,
#  xlab="",  ylab = 'Age' 
#)





## ----tab-alpha, results='asis', warning=FALSE---------------------------------
Verarbeitung <- Reliability(~ F5+F16+F22+F9+F26+F6+F35+F33+F12+F34+F4, fkv, check.keys =TRUE)
Coping <- Reliability(~ F7+F8+F17+F14+F15+F18+F19+F1+F13+F20, fkv, check.keys =TRUE)
Vertrauen <- Reliability(~ F28+F27+F31+F29, fkv, check.keys =TRUE)
Religion <- Reliability(~F21+F25+F30+F23+F24, fkv, check.keys =TRUE)
Distanz <- Reliability(~F3+F2+F10+F11, fkv, check.keys =TRUE)
#Verarbeitung %>% APA2()

Alpha(Verarbeitung, Coping, Vertrauen, Religion, Distanz) %>% Output()

## ----tab-icc, include=FALSE---------------------------------------------------
 sf <- GetData("
               J1 J2 J3 J4 J5 J6
               1  1  6  2  3  6
               2  2  7  4  1  2
               3  3  8  6  5 10
               4  4  9  8  2  4
               5  5 10 10  6 12
               6  6 11 12  4  8")

## ----tab-icc-2, results='asis', warning=FALSE---------------------------------
# #Quelle  http://www.personality-project.org/r/book/Chapter7.pdf
ICC(sf)


## ----data-multi-split, warning=FALSE------------------------------------------
x <- c(123, 23, 456,3)
separate_multiple_choice(x,
                          label = c("Allgemein", "Paro", 
                          "Endo", "Prothetik",
                           "Oral chirurgie",
                          "Kieferorthopedie"))



## ----data-weibull, include=FALSE, warning=FALSE-------------------------------

#Beispiel: Sachs Seite 337
#Scheuerfestigkeit eines Garns
garn  <- c( 550,  760,  830,  890, 1100, 
           1150, 1200, 1350, 1400, 1600, 
           1700, 1750, 1800, 1850, 1850, 
           2200, 2400, 2850, 3200)

Weibull<- function(x){# empirische Verteilungsfunktion
x <- sort(x); n <- length(x); i <- rank(x)
 if(n < 50) F_t <- (i - 0.3) / (n+0.4)  
 else       F_t <- i/(n+1)

 data.frame(data=x, x=log(x),
      y =log(log(1/(1-F_t))))
}



## ----res-weibull, warning=FALSE-----------------------------------------------

data<-Weibull(garn) 
z <- lm(y ~ x, data)
  res<-round(cbind(shape=coef(z)[2],             
                   scale=exp(-(coef(z)[1]/coef(z)[2]))), 2) 
res

 
m<-(-(coef(z)[1]/coef(z)[2]))


## ----fig-weibull, fig.width=8, fig.height=4-----------------------------------
#library(MASS)
fit <- MASS::fitdistr(garn, densfun="weibull", lower = 0)
 
library(car)

par(mfrow=c(1,2), lwd=2, font.axis=2, bty="n", ps=11,
    bg="white", las=1, cex=1.1)

plot(data$x, data$y, main="Weibull-Diagram",
     xlab="x=log(Garn)", ylab="y=log(log(1/(1-F)))", 
     xlim=c(5.9,8.5), ylim=c(-4, 2), axes = FALSE, pch=16, cex=1.0)
abline(z)
my.at<- signif(c(500, 1000, exp(m), 5000),4)
axis(1, at = log(my.at), labels = my.at)
axis(2)
i <- rank(data$x)
n <- length(data$x)
v.unten <- 1 / (((n-i+1)/i)*(qf(0.025, 2*(n-i+1), 2*i))+1)
v.oben  <- 1 - 1 / (1 + (i / (n-i+1)*qf(0.025, 2*i, 2*(n-i+1))))
lines(data$x, log(log(1/(1-v.oben))), lty=2)
lines(data$x, log(log(1/(1-v.unten))), lty=2)
abline(h=0, lty=3)
abline(v=m, lty=3)
points(m, 0, cex=5, col="red")

 

qqPlot(garn, distribution="weibull", main="qqPlot",
       scale=coef(fit)[1], shape=coef(fit)[2], las=1, pch=19)



