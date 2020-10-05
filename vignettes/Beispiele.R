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


## ----fig-mean-berechne, fig.cap = "Mittelwerte",  fig.width=4, fig.height=3, cache=TRUE----


renameLevels<- function(data,
                        labels=c("Begin", "2 Monate", "6 Monate", "12 Monate"),
                        var= "variable"){
  levels( data[,var])<- labels
  data
}


pd <- position_dodge(0.15)

varana %>% Summarise(m1, m2, m3, m4, by =  ~ geschl, 
                     fun=function(x) c(mean=mean(x), sd=sd(x))) %>%
  renameLevels %>%
  ggplot(aes(variable, mean, group = geschl,
             colour = geschl,
             shape= geschl)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = .1,
                position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 3) +
  xlab("") +
  ylab("Merkfaehigkeitstest")
#+  coord_cartesian(ylim=c(1, 25)) + scale_y_reverse(breaks=1:6)



## ----fig-mean-melt, fig.cap = "Mittelwerte",  fig.width=4, fig.height=3, cache=TRUE----
pd<-position_dodge(.5)
varana %>%  melt2(m1, m2, m3, m4, by =  ~ geschl) %>%
  renameLevels %>%
  ggplot(aes(variable,
             value,
             fill=geschl)) +
  stat_boxplot(geom ='errorbar', width=0.4, position=pd) +
  geom_boxplot(width=0.4, position=pd) +
  xlab("") +
  ylab("Merkfaehigkeitstest")

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


## ----fkv-korr,  results='asis', warning=FALSE---------------------------------
#head(fkv)
APA2( ~., fkv, test=TRUE)


## ----fkv-kor-plot, fig.cap = "Mittelwerte",  fig.width=4, fig.height=4, cache=TRUE----
require(arm)
corrplot(fkv, abs=TRUE, n.col.legend=7)#  corrplot {arm}

## ----pca,   results='asis', warning=FALSE-------------------------------------
Principal2(fkv, 5, cut=.35)

## ----pca-lavan,  results='asis', warning=FALSE--------------------------------
require(lavaan)

Model<-'
Verarbeitung =~ F5+F16+F22+F9+F26+F6+F35+F33+F12+F34+F4
Coping =~ F7+F8+F17+F14+F15+F18+F19+F1+F13+F20
Vertrauen =~ F28+F27+F31+F29
Religion =~F21+F25+F30+F23+F24
Distanz =~F3+F2+F10+F11

'
fit.Lavaan <- sem( Model, data=fkv)
APA2(fit.Lavaan)

# parameterEstimates(fit.Lavaan)
# Est <- parameterEstimates(fit.Lavaan, ci = FALSE, standardized = TRUE)
# #fitMeasures(fit.Lavaan, c("chisq", "df", "pvalue", "cfi", "rmsea"))
# #round( inspect(fit.Lavaan,"r2")  ,2)
# #parTable(fit.Lavaan)
# #show(fit.Lavaan)
# anova(fit.Lavaan)


## ----pca-lavan-plot, fig.cap = "Mittelwerte",  fig.width=4, fig.height=4, cache=TRUE----
# require(semPlot)
# 
# semPaths(fit.Lavaan, "std", rotation=2, title = FALSE)
# title("Std", line = 3)

plot(1)
 

## ----reliability,  results='asis', warning=FALSE------------------------------

Reliability2(~F3+F2+F10+F11, fkv, check.keys =TRUE)
res<- Reliability(~F3+F2+F10+F11, fkv, check.keys =TRUE)
#res$index
#names(res)
#res %>% Output
res %>% Alpha() %>% Output()

Verarbeitung <- Reliability(~ F5+F16+F22+F9+F26+F6+F35+F33+F12+F34+F4, fkv, check.keys =TRUE)
Coping <- Reliability(~ F7+F8+F17+F14+F15+F18+F19+F1+F13+F20, fkv, check.keys =TRUE)
Vertrauen <- Reliability(~ F28+F27+F31+F29, fkv, check.keys =TRUE)
Religion <- Reliability(~F21+F25+F30+F23+F24, fkv, check.keys =TRUE)
Distanz <- Reliability(~F3+F2+F10+F11, fkv, check.keys =TRUE)


Alpha(Verarbeitung, Coping, Vertrauen, Religion, Distanz) %>% Output()


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

