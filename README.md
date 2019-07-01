stp25stat
================

# stp25stat

Functions for Statistical Computations

stp25stat ersaetzt die Funkrion stp25APA2

``` r
which_output()
```

    ## [1] "text"

Artificial data

``` r
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

    DF <- lavaan::simulateData(population.model, sample.nobs = 100)

    DF<- transform(DF, 
                   sex=cut(sex, 2, c("m", "f")),
                   age=scale2(age, 18, 55),
                   edu=cut(edu,3, c("low", "med", "hig")),
                   beruf= cut(edu,2, c("blue", "withe")),
                   single =cut(single, 2, c("yes", "no")),
                   Einkommen = (4-log(Einkommen-min(Einkommen)+1))*1000
                   )
```

## Mittelwerte und Prozent

``` r
    DF %>% Tabelle2(sex, age, edu, beruf, single,
                    APA = TRUE, caption = "Characteristics")
```

    ## 
    ##  Tab 1: Characteristics 
    ##              Item   n             m
    ## sex          sex  100              
    ## 1               m          50 (50%)
    ## 2               f          50 (50%)
    ## age    age (mean) 100 43.46 (11.24)
    ## edu          edu  100              
    ## 11            low          11 (11%)
    ## 21            med          69 (69%)
    ## 3             hig          20 (20%)
    ## beruf      beruf  100              
    ## 12           blue          44 (44%)
    ## 22          withe          56 (56%)
    ## single    single  100              
    ## 13            yes          40 (40%)
    ## 23             no          60 (60%)
    ## 
    ## 

``` r
APA_Correlation(~o1+o2+o3+o4+o5+c1+c2+c3, DF)
```

    ## 
    ##  Tab 2: Korrelation 
    ##    Quelle (1)    (2)    (3)    (4)    (5)  (6)    (7)    (8)
    ## o1 (1) o1   1 .50*** .53*** .50*** .53*** -.03   -.15    .06
    ## o2 (2) o2          1 .52*** .62*** .54*** -.01   -.18   -.03
    ## o3 (3) o3                 1 .50*** .48*** -.05   -.09   -.13
    ## o4 (4) o4                        1 .56***  .12   -.13    .10
    ## o5 (5) o5                               1  .07   -.06    .05
    ## c1 (6) c1                                    1 .45*** .52***
    ## c2 (7) c2                                           1 .36***
    ## c3 (8) c3                                                  1
    ## 
    ##   pearson

## PCA

``` r
    vars <- c(
      "o1",  "o2",  "o3",  "o4",  "o5",  "c1",  "c2",  "c3",
      "c4",  "c5",  "e1",  "e2",  "e3",  "e4",  "e5",  "n1",
      "n2",  "n3",  "n4",  "n5",  "a1",  "a2",  "a3",  "a4",  "a5"
    )
     APA_PCA(DF[vars], 5, cut=.35, include.plot = FALSE)
```

    ## R was not square, finding R from data

    ## 
    ##  Tab 3: Standardized loadings (pattern matrix) based upon correlation matrix 
    ##    Item Nr   PC1   PC2   PC3   PC4   PC5   h2
    ## a5   a5 25  0.85                         0.73
    ## a3   a3 23  0.81                         0.67
    ## a4   a4 24  0.81                         0.65
    ## a2   a2 22  0.80                         0.70
    ## a1   a1 21  0.77                         0.61
    ## n5   n5 20        0.82                   0.70
    ## n3   n3 18        0.82                   0.69
    ## n1   n1 16        0.80                   0.65
    ## n2   n2 17        0.79                   0.62
    ## n4   n4 19        0.79                   0.67
    ## o4   o4  4              0.82             0.69
    ## o5   o5  5              0.79             0.64
    ## o2   o2  2              0.79             0.65
    ## o1   o1  1              0.78             0.61
    ## o3   o3  3              0.75             0.61
    ## c1   c1  6                    0.81       0.67
    ## c5   c5 10                    0.77       0.60
    ## c4   c4  9                    0.74       0.58
    ## c3   c3  8                    0.73       0.56
    ## c2   c2  7                    0.71       0.54
    ## e5   e5 15                          0.78 0.65
    ## e1   e1 11                          0.77 0.69
    ## e3   e3 13                          0.74 0.58
    ## e2   e2 12                          0.72 0.55
    ## e4   e4 14                          0.68 0.47
    ## 
    ##   
    ## 
    ## 
    ##  Tab 4: Standardized loadings (pattern matrix) based upon correlation matrix 
    ##               Measures    Test Statistik
    ## 1                n.obs               100
    ## 2 Mean item complexity               1.1
    ## 3                 RMSR              0.05
    ## 4 empirical chi square X2=163.15, p=.875
    ## 
    ##   
    ## 
    ## 
    ##  Tab 5: Standardized loadings (pattern matrix) based upon correlation matrix 
    ##                        Measures               Statistic
    ## 1    Kaiser-Meyer-Olkin Measure                    0.72
    ## 2 Bartlett's test of sphericity X2(300)=1086.03, p<.001
    ## 
    ## 

## Reliability

``` r
    Openness <- Reliability(~o1+o2+o3+o4+o5, DF, check.keys=TRUE)
    Conscientiousness <- Reliability(~ c1+c2+c3+c4+c5, DF, check.keys=TRUE)
    Extraversion <-Reliability(~ e1+e2+e3+e4+e5, DF, check.keys=TRUE)
    Neuroticism <- Reliability(~  n1+n2+n3+n4+n5, DF, check.keys=TRUE)
    Agreeableness <- Reliability(~ a1+a2+a3+a4+a5, DF, check.keys=TRUE)

    DF$O<- Openness$index
    DF$C<- Conscientiousness$index
    DF$E<- Extraversion$index
    DF$N<- Neuroticism$index
    DF$A<- Agreeableness$index

    Alpha2(Openness,Conscientiousness,Extraversion,Neuroticism,Agreeableness)
```

    ## 
    ##  Tab 6:  
    ##              Quelle Item   n     M   SD Alpha       Range  Skew Kurtosi
    ## 1          Openness    5 100  0.00 1.15  0.85 -4.86; 5.29  0.23    0.48
    ## 2 Conscientiousness    5 100  0.13 1.00  0.81 -3.45; 4.32  0.04   -0.16
    ## 3      Extraversion    5 100 -0.12 1.01  0.80 -4.27; 3.00 -0.42    0.58
    ## 4       Neuroticism    5 100 -0.04 1.21  0.87 -5.21; 4.59 -0.22    0.62
    ## 5     Agreeableness    5 100 -0.04 1.26  0.87 -4.71; 3.64 -0.22   -0.60
    ##     Shapiro.Test
    ## 1 W=0.99, p=.667
    ## 2 W=1.00, p=.992
    ## 3 W=0.98, p=.112
    ## 4 W=0.97, p=.048
    ## 5 W=0.99, p=.337
    ## 
    ## 

## Test des Regressionsmodels

### Mittelwerte und Effecte

``` r
fit<- lm(Einkommen ~ sex + age + O + C + E + N + A, DF)
#Tabelle2  gibt die Tabellen formatiert aus.
mean_sd<- Tabelle(fit )
mean_sd$sex %>% Output( "Mittelwerte")
```

    ## 
    ##  Tab 7: Mittelwerte 
    ##   sex Einkommen_n Einkommen_M Einkommen_SD
    ## 1   m          50     2102.40       485.98
    ## 2   f          50     1947.15       395.59
    ## 
    ## 

``` r
#mean_sd$O %>% Output(output="md")
```

``` r
APA2(effects::Effect("sex", fit))
```

    ## 
    ##  Tab 8: Effekte:  Effect 
    ##   sex     fit   lower   upper
    ## 1   m 2080.63 2000.45 2160.81
    ## 2   f 1968.92 1888.75 2049.10
    ## 
    ## 

``` r
APA2(visreg::visreg(fit, "sex", plot=FALSE))
```

    ## 
    ##  Tab 9: Einkommen 
    ##   sex      age          O        C         E            N          A
    ## 1   m 43.45497 -0.1551748 0.111488 -0.086209 -0.001044988 0.03638651
    ## 2   f 43.45497 -0.1551748 0.111488 -0.086209 -0.001044988 0.03638651
    ##       fit                 ci
    ## 1 2087.12 [2005.86, 2168.39]
    ## 2 1975.42 [1895.35, 2055.48]
    ## 
    ## 

### Regressionsmodel

``` r
APA_Table(fit, include.se=FALSE, include.ci=TRUE, output="md")
```

–\> Output md\[1\] “tbl\_df” “tbl” “data.frame”

| Quelle        | b           | conf            |
| :------------ | :---------- | :-------------- |
| (Intercept)   | 1799\*\*\*  | \[2045, 1552\]  |
| sexf          | \-112       | \[2.51, -226\]  |
| age           | 6.67\*      | \[11.9, 1.41\]  |
| O             | \-41.7      | \[9.75, -93.2\] |
| C             | \-24        | \[ 33, -81\]    |
| E             | 90.3\*\*    | \[ 147, 33.1\]  |
| N             | \-254\*\*\* | \[-206, -302\]  |
| A             | 64.3\*\*    | \[ 110, 18.4\]  |
| r.squared     | 0.63        |                 |
| adj.r.squared | 0.60        |                 |
| AIC           | 1422.78     |                 |
| BIC           | 1446.22     |                 |
| RMSE          | 271.77      |                 |
| Obs           | 100         |                 |

Tab 10:

### GOF

``` r
 APA_Validation(fit) 
```

    ## 
    ##  ende Schleife
    ##                                  Test
    ## 2                         F-Statistic
    ## 3                  Deviance Residuals
    ## 4                           R-Squared
    ## 5  Heteroskedasticity (Breusch-Pagan)
    ## 6     Autocorrelation (Durbin-Watson)
    ## 7         Shapiro-Wilk normality test
    ## 8                                 AIC
    ## 9                                 BIC
    ## 10                      Var: Residual
    ## 1                                Obs.
    ##                                   statistic
    ## 2                    F(7, 92)=22.17, p<.001
    ## 3                                 7385824.9
    ## 4  R<sup>2</sup>=.63, adj.R<sup>2</sup>=.60
    ## 5                       BP(7)=20.87, p=.004
    ## 6                           DW=1.94, p=.383
    ## 7                            W=0.95, p=.002
    ## 8                                    1422.8
    ## 9                                    1446.2
    ## 10                                 80280.71
    ## 1                                       100
    ## 
    ##  Tab 11: Testing Regression Models 
    ##                                  Test
    ## 2                         F-Statistic
    ## 3                  Deviance Residuals
    ## 4                           R-Squared
    ## 5  Heteroskedasticity (Breusch-Pagan)
    ## 6     Autocorrelation (Durbin-Watson)
    ## 7         Shapiro-Wilk normality test
    ## 8                                 AIC
    ## 9                                 BIC
    ## 10                      Var: Residual
    ## 1                                Obs.
    ##                                   statistic
    ## 2                    F(7, 92)=22.17, p<.001
    ## 3                                 7385824.9
    ## 4  R<sup>2</sup>=.63, adj.R<sup>2</sup>=.60
    ## 5                       BP(7)=20.87, p=.004
    ## 6                           DW=1.94, p=.383
    ## 7                            W=0.95, p=.002
    ## 8                                    1422.8
    ## 9                                    1446.2
    ## 10                                 80280.71
    ## 1                                       100
    ## 
    ## 

## Test

    class::lvqtest                      Classify Test Set from LVQ Codebook
    Hmisc::biVar                        Bivariate Summaries Computed Separately by a Series of Predictors
    Hmisc::bpower                       Power and Sample Size for Two-Sample Binomial Test
    Hmisc::ciapower                     Power of Interaction Test for Exponential Survival
    Hmisc::cpower                       Power of Cox/log-rank Two-Sample Test
    Hmisc::spower                       Simulate Power of 2-Sample Test for Survival under Complex Conditions
    Hmisc::t.test.cluster               t-test for Clustered Data
    MASS::anova.negbin                  Likelihood Ratio Tests for Negative Binomial GLMs
    mgcv::anova.gam                     Hypothesis tests related to GAM fits
    multilevel::sobel                   Estimate Sobel's (1982) Test for Mediation
    multilevel::sobel.lme               Estimate Sobel's (1982) Test for Mediation in Two-Level lme Model
    stats::ansari.test                  Ansari-Bradley Test
    stats::bartlett.test                Bartlett Test of Homogeneity of Variances
    stats::binom.test                   Exact Binomial Test
    stats::Box.test                     Box-Pierce and Ljung-Box Tests
    stats::chisq.test                   Pearson's Chi-squared Test for Count Data
    stats::cor.test                     Test for Association/Correlation Between Paired Samples
    stats::fisher.test                  Fisher's Exact Test for Count Data
    stats::fligner.test                 Fligner-Killeen Test of Homogeneity of Variances
    stats::friedman.test                Friedman Rank Sum Test
    stats::kruskal.test                 Kruskal-Wallis Rank Sum Test
    stats::ks.test                      Kolmogorov-Smirnov Tests
    stats::mantelhaen.test              Cochran-Mantel-Haenszel Chi-Squared Test for Count Data
    stats::mauchly.test                 Mauchly's Test of Sphericity
    stats::mcnemar.test                 McNemar's Chi-squared Test for Count Data
    stats::mood.test                    Mood Two-Sample Test of Scale
    stats::oneway.test                  Test for Equal Means in a One-Way Layout
    stats::pairwise.prop.test           Pairwise comparisons for proportions
    stats::pairwise.t.test              Pairwise t tests
    stats::pairwise.wilcox.test         Pairwise Wilcoxon rank sum tests
    stats::poisson.test                 Exact Poisson tests
    stats::power.anova.test             Power calculations for balanced one-way analysis of variance tests
    stats::power.prop.test              Power calculations two sample test for proportions
    stats::power.t.test                 Power calculations for one and two sample t tests
    stats::PP.test                      Phillips-Perron Test for Unit Roots
    stats::print.power.htest            Print method for power calculation object
    stats::prop.test                    Test of Equal or Given Proportions
    stats::prop.trend.test              Test for trend in proportions
    stats::quade.test                   Quade Test
    stats::shapiro.test                 Shapiro-Wilk Normality Test
    stats::stats-defunct                Defunct Functions in Package stats
    stats::t.test                       Student's t-Test
    stats::var.test                     F Test to Compare Two Variances
    stats::wilcox.test                  Wilcoxon Rank Sum and Signed Rank Tests
    survival::cox.zph                   Test the Proportional Hazards Assumption of a Cox Regression
    survival::plot.cox.zph              Graphical Test of Proportional Hazards
    survival::survdiff                  Test Survival Curve Differences
    survival::survival-internal         Internal survival functions
    survival::survobrien                O'Brien's Test for Association of a Single Variable with Survival
    survival::survregDtest              Verify a survreg distribution
    vcd::coindep_test                   Test for (Conditional) Independence
    vcd::goodfit                        Goodness-of-fit Tests for Discrete Data
    vcd::woolf_test                     Woolf Test
