## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")


## ---- include = FALSE---------------------------------------------------------


require(tidyverse)
require(lmerTest)
require(modelr)
require(stpvers)
library(effects)
library(gridExtra)
 library(broom)
 library(car)
Tab_Index <- 0
Abb_Index <- 0
#require(semPlot)
#require(ggm)


## ----simpel-apa, results='asis', warning=FALSE--------------------------------
APA2(alter ~ geschl, varana)
Tabelle2(alter ~ geschl, varana)
Tabelle2(alter ~ geschl, varana, APA = TRUE)
#varana %>% berechne(m1, m2, m3, m4, by =  ~ geschl) %>%
#  fix_format() %>% (function(x) x[, c(1:7,9)])


## ----gather, results='asis', warning=FALSE------------------------------------
require(tidyr)
varana2 <- varana %>%
  gather( Zeit, Merkfgk, m1:m4 ) %>%
  mutate(Zeit=factor( Zeit, Cs(m1, m2, m3 ,m4), Cs(t0, t1, t2, t3))) %>%
  Label(Merkfgk="Merkfaehigkeit")

Tabelle( Merkfgk[median] ~ Zeit, varana2, APA=TRUE, include.n=FALSE)

## ----modelr, results='asis', warning=FALSE------------------------------------

disp_fits <- varana2 %>%
  fit_with(lmer,
           formulas(~Merkfgk,
                    basline = ~ Zeit  +(1 | nr),
                    additive = ~ geschl + alter  + (1 | nr),
                    interaction = ~ geschl *  alter * Zeit  + (1 | nr) ))

#class(disp_fits)

#+ (1|Zeit)
 APA_Table(disp_fits, type="long")




## ----effecte,  results='asis', warning=FALSE----------------------------------
library(effects)
fit<- lmer(Merkfgk~ geschl * alter * Zeit  + (1 | nr) , varana2 )
APA2(allEffects(fit) )
# library(car)
# leveneTest( Merkfgk~   Zeit   , DF, center= mean)

## ----corr-hyper,  results='asis', warning=FALSE-------------------------------

#head(hyper)
##-- 
APA_Correlation(~chol0+chol1+chol6+chol12, hyper,
caption="Korrelation nach Pearson"#,
#p.value=FALSE, sig.star=TRUE
)

APA_Correlation(~chol0+chol1+chol6+chol12, hyper,
         caption="Rangkorrelation nach Spearman",
         type="spearman",cor_diagonale_up=FALSE)



## ----manova-cbind-------------------------------------------------------------
#fit0<-lm(rrs0 ~ ak, hyper)
fit1<-lm(cbind(rrs0,rrd0,chol0,bz0) ~ ak, hyper)
fit2<-aov(cbind(rrs0,rrd0,chol0,bz0) ~ ak, hyper)
#  library(effects)
# plot(allEffects(fit1), main="")
summary(fit1)
car::Anova(fit1)
#APA_Table(fit1)  ## Regressionsanalyse
summary(fit2)

R2(fit1)

## ----kirche-korr,  results='asis', warning=FALSE------------------------------
#head(kirche)
##-- Partial-Korrelation Bühl Seite 327
APA_Correlation(~alter+kirche+gast, kirche)


## ----patial-cor---------------------------------------------------------------
#kirche<- dapply2(kirche, scale)
fit<-summary(lm(kirche~alter+gast, kirche))
p<- coefficients( fit)[3,4]

ki_al<- residuals(lm(kirche~alter, kirche))
ga_al<- residuals(lm(gast~alter, kirche))
round(c(r=cor(ki_al, ga_al),  df=fit$df[2],  p.value= p), 3)


## ----hkarz, results='asis'----------------------------------------------------
library(broom)
fit2<- glm(gruppe~tzell, hkarz, family = binomial)
APA_Table(fit2)

## -----------------------------------------------------------------------------
lmtest::lrtest(fit2) %>%
  tidy %>% fix_format()
r2<- R2(fit2)
#-- R2 wie SPSS
round(c( '-2 Log-Likeliehood' = anova(fit2)[2, "Resid. Dev" ],
         'Cox & Snell R2'= r2$r2ML,
         'Nagelkerke R2' =r2$r2CU),2)

fit2 %>% tidy %>% transform(exp= round(exp(estimate),2)) %>% fix_format()

Klassifikation(fit2)

## ---- results='asis'----------------------------------------------------------
fit3<- glm(gruppe~tzell+lai, hkarz, family = binomial)
APA_Table(fit3)

## -----------------------------------------------------------------------------
lmtest::lrtest(fit3) %>%
  tidy %>% fix_format()
r2<- R2(fit3)
#-- R2 wie SPSS
round(c( '-2 Log-Likeliehood' = min(anova(fit3)[ , "Resid. Dev" ]),
         'Cox & Snell R2'= r2$r2ML,
         'Nagelkerke R2' =r2$r2CU),2)

fit3 %>% tidy %>% transform(exp= round(exp(estimate),2)) %>% fix_format()

Klassifikation(fit3)

## -----------------------------------------------------------------------------
require(survival)
mkarz <-
  GetData("C:/Users/wpete/Dropbox/3_Forschung/1 Statistik/BspDaten/SPSS/_Buehl/MKARZ.SAV")

Text(
  "Buehl Seite 553: Die Datei mkarz.sav ist ein Datensatz mit 106 Patientan
    mit Magenkarzinom über einen Zeitraum von 5 Jahren"
)
head(mkarz)

mkarz %>% Tabelle2(survive[median], status, lkb)
mkarz$status <- ifelse(mkarz$status == "tot", 1, 0)

#Head("Kaplan-Meier estimator without grouping", style=3)
#Text("
#     m0 <- Surv(survive, status) ~ 1
#     res0<- survfit(m0, mkarz)
#
#     ")
m0 <- Surv(survive, status) ~ 1
res0 <- survfit(m0, mkarz)
APA2(res0)
#windows(8,4)
#par(mfrow=c(1,2))
#plot( res0 , ylab="Hazard", mark.time = T)
#plot( res0, fun="cumhaz",  ylab="Cumulative Hazard" )
#SaveData(caption="plot: mkarz")




m1 <- Surv(survive, status) ~ lkb
res1 <- survfit(m1, mkarz)
fit1 <- coxph(m1, mkarz)
logrank1 <- survdiff(m1, mkarz)

model_info(logrank1)
APA2(res1, caption = "Kaplan-Meier")
APA2(logrank1)
APA2(coxph(m1, mkarz))
 

## -----------------------------------------------------------------------------
head(MMvideo)
fit1<-lm(score ~ agegrp+trial, MMvideo)
fit2<-lmerTest::lmer(score ~ agegrp+trial + (1|id), MMvideo)
fit3<-lm(score ~ agegrp*trial, MMvideo)
fit4<-lmerTest::lmer(score ~ agegrp*trial + (1|id), MMvideo)



## ----mix-mod, results='asis'--------------------------------------------------
APA_Table(fit1, fit2, fit3, fit4, type="long")


#  windows(8,6)
#  p1 <- plot(effect("trial",fit2), multiline=TRUE)
#  p2 <- plot(effect("agegrp*trial",fit4), multiline=TRUE)

  #grid.arrange(p1,p2,ncol=2)

# library(coefplot)
# windows(4,3)
#  coefplot(fit3, intercept=F, xlab="b (SE)")

#  windows(4,3)
#  multiplot(fit1, fit2, intercept=F, xlab="b (SE)")


## -----------------------------------------------------------------------------
#schools<- read.table("file:///C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25/extdata/schools.txt",
#                    header=TRUE)
summary(schools)
fit<-lmerTest::lmer(score ~  grade +treatment  + stdTest + (1|classroom), schools)

## ----schools, results='asis'--------------------------------------------------

APA_Table(fit)



## -----------------------------------------------------------------------------
 
# SPSS kodiert die Gruppe 3 als Referenz
poisson_sim$prog <-
  factor(poisson_sim$prog, c("vocation", "general",  "academic"))
  fit1 <- glm(num_awards ~ prog + math, poisson_sim, family = poisson())


Goodness <- function(x, ..) {
  glance(x)[, c(6, 7, 3, 4, 5)]
}
fit1 %>% Goodness
#--Omnibus Test
 lmtest::lrtest(fit1)
 Anova(fit1)

## ----poisson, results='asis'--------------------------------------------------
#-- Wald Chi-Square

 APA_Table(fit1)



## -----------------------------------------------------------------------------
cbind (tidy(fit1), confint(fit1)) %>% fix_format()
x <- cbind(tidy(fit1)[1:2], confint(fit1))
x[2:3] <- exp(x[2:3])
x %>% fix_format()


R2(fit1)
RMSE(fit1)

