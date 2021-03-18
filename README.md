stp25stat
================
Wolfgang Peter
2021-02-23

Functions for Statistical Computations

``` r
which_output()
```

    ## [1] "markdown_html"

``` r
#'  #  set_my_options(style_mean = list(line_break = '<br>'))
#'  #  set_my_options(prozent = list(null_percent_sign = '.'))
#'  #  set_my_options(mittelwert = list(mean.style = 2, median.style = "IQR"), 
#'  #              prozent=list(style=2))
#'  #  set_my_options(caption= "include.n") 
```

## Deskriptive Tabellen mit sig-Test

``` r
#' effect_size
#'
#' Cohen's d  d=(x1-x2)/s psych::cohen.d
#' Hedges' g  g= (x1-x2)/s1s2 ( pooled standard deviation)
#' g enspricht  x<-scale(x)  (mean(x1) - mean(x2)) 
#' 
#' Generalized Log Odds Ratios for Frequency Tables vcd::oddsratio
#' 
#' @param x vector
#' @param by factor
#' @param measure  intern c("mean", "median", "numeric", "factor", "logical")
#' @param measure.test, test  intern c("cattest", "contest", "notest", "ttest", ...)
#' @export
#'
#' @examples
#' 
#'  effect_size(c(2, 3, 4, 2, 3, 4, 3, 6,
#' 7, 6, 8, 9, 4, 5, 6, 7) ,
#' gl(2, 8, labels = c("Control", "Treat")))
#' 
#' effect_size(factor(c(2, 2, 2, 2, 1, 1, 1, 1,
#'                      2, 2, 2, 2, 1, 2, 1, 2)) ,
#'             gl(2, 8, labels = c("Control", "Treat")))
#'             
#'             
effect_size2 <- function(x,
                        by,
                        measure,
                        ...
                        ) {
  dat <-  na.omit(data.frame(x = x, by = by))
  n <- nrow(dat)
  es <- rslt <- "n.a."
  if (n > 10) {
    if (measure %in% c("mean", "median", "numeric")) {
      es <- psych::cohen.d(dat$x, dat$by)
      es <- stp25rndr::Format2(es$cohen.d)
      es <- paste0(es[2], " [", es[1], ", ", es[3], "] ES")
    }
    else  if (measure %in% c("factor", "logical")) {
      # Generalized Log Odds Ratios for Frequency Tables
      if (measure == "factor" & nlevels(dat$x) != 2) {
        es <- "n.a."
      }
      else{
        es <- vcd::oddsratio(table(dat$x, dat$by), log = FALSE)
        if (coef(es) < 0.01)
          es <- "n.a."
        else if (coef(es) > 100)
          es <- "n.a."
        else{
          es <- stp25rndr::rndr_ods(c(coef(es) ,  confint(es)))
          es <- paste0(es[1], " [", es[2], ", ", es[3], "] OR")
        }
      }
    }
  }
  cbind('Odds Ratio/Effect Size' = es)
}
```

``` r
# Zu Auto-Test existiiert eine eigene Funktion unter stp25stat::auto_test

auto_test2 <- function(x,
                      by,
                      measure,
                      measure.test,
                      type = 2) {
  dat <-  na.omit(data.frame(x = x, by = by))
  rslt <- NULL
 
  contest <- stp25stat:::contest
  cattest <- stp25stat:::cattest
  
  if (measure.test == "notest") {
    rslt <-  ""
  }
  else if (measure.test == "contest") {
    if (inherits(x, "factor")) {
      dat$x <- as.numeric(dat$x)
    }
    rslt <- stp25stat:::conTest(x ~ by, dat)
  }
  else if (measure.test == "cattest") {
    rslt <- stp25stat:::catTest( ~ x + by, dat)
  }
  else if (measure.test %in% contest) {
    if (inherits(x, "factor")) {
      dat$x <-
        as.numeric(dat$x)
    }
    rslt <- stp25stat:::conTest(x ~ by, dat, measure.test)
  }
  else if (measure.test %in% cattest) {
    rslt <- stp25stat:::catTest( ~ x + by, dat, measure.test)
  }
  
  
  
  if (type == 1) {
    cbind('Statistics' = rslt)
  }
  else if (type == 2) {
    rslt <-   strsplit(rslt, ', p')[[1]]
    cbind("Test Statistics" = rslt[1],
          "p-value" = gsub("=", "", rslt[2]))
  }   else if (type == 3) {
    cbind('Statistics' = gsub(", p", ",<BR>p", rslt))
  }
  
  
}
```

``` r
DF %>% Tbll_desc(
  age[median, 1],
  height[1, mean, ttest],
  sex,
  smoker[fisher],
  iq[0],
  likert[2, mean],
  by =  ~ died,
  include.custom = effect_size2,
  include.test=TRUE,
  include.n=TRUE,
  include.total=TRUE
) %>% Output()
```

    ## Warning in `[[<-.factor`(`*tmp*`, i, value = 4): invalid factor level, NA
    ## generated

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="6" style="text-align: left;">
Tab 1: died
</td>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Item
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Total
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
FALSE
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
TRUE
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Odds Ratio/Effect Size
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Test Statistik
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(N) 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
30
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
15
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
15
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Age (median)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
10.0 (IQR 2.0)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
10.0 (IQR 2.0)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
10.0 (IQR 2.5)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-0.15 \[-0.86, 0.57\] ES
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
F(1, 28)=0.11, p=.739
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Height (mean)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
52.2 (8.7)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
52.8 (9.4)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
51.7 (8.3)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-0.13 \[-0.85, 0.59\] ES
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
T(28)=0.35, p=.729
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Sex 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.76 \[0.18, 3.24\] OR
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
X2(1)=0.14, p=.713
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 Male
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
13 (43%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
6 (40%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
7 (47%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 Female
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
17 (57%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
9 (60%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
8 (53%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Smoker true
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
20 (67%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
10 (67%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
10 (67%)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1.00 \[0.22, 4.56\] OR
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
OR=1.00, p=1.000
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
IQ (mean)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
99 (11)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
99 (13)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
100 (9)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.13 \[-0.59, 0.84\] ES
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
F(1, 28)=0.65, p=.427
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
Likert (mean)
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
3.07 (1.41)
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
3.13 (1.41)
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
3.00 (1.46)
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
-0.05 \[-0.78, 0.68\] ES
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
F(1, 28)=0.06, p=.804
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="6">
Wilcoxon-Test, T-Test, Pearson Chi-squared, Fisher Exact Test
</td>
</tr>
</tfoot>
</table>

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
#APA_Correlation(~o1+o2+o3+o4+o5+c1+c2+c3, DF)
Tbll_corr(~o1+o2+o3+o4+o5+c1+c2+c3, DF) %>% Output()
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="9" style="text-align: left;">
Tab 2:
</td>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Quelle
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
\(1\)
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
\(2\)
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
\(3\)
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
\(4\)
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
\(5\)
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
\(6\)
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
\(7\)
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
\(8\)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(1) o1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.62\*\*\*
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.53\*\*\*
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.58\*\*\*
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.55\*\*\*
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.09
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-.01
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-.02
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(2) o2
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.48\*\*\*
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.63\*\*\*
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.45\*\*\*
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-.03
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-.00
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.05
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(3) o3
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.56\*\*\*
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.46\*\*\*
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.13
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.11
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.10
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(4) o4
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.42\*\*\*
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.12
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.06
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.08
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(5) o5
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.06
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.09
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.17
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(6) c1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.37\*\*\*
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.35\*\*\*
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(7) c2
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
.50\*\*\*
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
(8) c3
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
1
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="9">
pearson
</td>
</tr>
</tfoot>
</table>

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

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="8" style="text-align: left;">
Tab 3: Standardized loadings (pattern matrix) based upon correlation
matrix
</td>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Item
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Nr
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
PC1
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
PC2
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
PC3
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
PC4
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
PC5
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
h2
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
o1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.83
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.71
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
o4
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 4
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.81
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.67
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
o2
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 2
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.81
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.67
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
o3
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 3
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.76
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.59
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
o5
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 5
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.72
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.55
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
a2
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
22
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.84
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.74
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
a4
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
24
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.79
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.65
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
a3
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
23
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.77
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.64
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
a5
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
25
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.75
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.60
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
a1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
21
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.74
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.59
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
n2
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
17
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.81
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.70
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
n5
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
20
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.78
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.62
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
n4
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
19
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.78
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.62
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
n1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
16
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.77
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.62
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
n3
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
18
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.76
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.62
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
e1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
11
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.79
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.63
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
e4
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
14
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.79
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.62
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
e3
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
13
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.76
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.65
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
e5
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
15
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.75
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.58
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
e2
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
12
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.74
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.56
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
c4
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 9
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.79
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.63
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
c3
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 8
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.77
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.62
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
c2
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 7
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.74
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.56
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
c5
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
10
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 0.68
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.53
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
c1
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
 6
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
 0.64
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
0.45
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="8">
</td>
</tr>
</tfoot>
</table>
<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="2" style="text-align: left;">
Tab 4: Standardized loadings (pattern matrix) based upon correlation
matrix
</td>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Measures
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Test Statistik
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
n.obs
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
100
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Mean item complexity
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1.1
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
RMSR
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.06
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
empirical chi square
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
X2=184.22, p=.502
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="2">
</td>
</tr>
</tfoot>
</table>
<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="2" style="text-align: left;">
Tab 5: Standardized loadings (pattern matrix) based upon correlation
matrix
</td>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Measures
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Statistic
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Kaiser-Meyer-Olkin Measure
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.71
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
Bartlett’s test of sphericity
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
X2(300)=1038.49, p&lt;.001
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="2">
</td>
</tr>
</tfoot>
</table>

## Reliability

``` r
    Openness <- Reliability(~o1+o2+o3+o4+o5, DF, check.keys=TRUE)
```

    ## Number of categories should be increased  in order to count frequencies.

``` r
    Conscientiousness <- Reliability(~ c1+c2+c3+c4+c5, DF, check.keys=TRUE)
```

    ## Number of categories should be increased  in order to count frequencies.

``` r
    Extraversion <-Reliability(~ e1+e2+e3+e4+e5, DF, check.keys=TRUE)
```

    ## Number of categories should be increased  in order to count frequencies.

``` r
    Neuroticism <- Reliability(~  n1+n2+n3+n4+n5, DF, check.keys=TRUE)
```

    ## Number of categories should be increased  in order to count frequencies.

``` r
    Agreeableness <- Reliability(~ a1+a2+a3+a4+a5, DF, check.keys=TRUE)
```

    ## Number of categories should be increased  in order to count frequencies.

``` r
    DF$O<- Openness$index
    DF$C<- Conscientiousness$index
    DF$E<- Extraversion$index
    DF$N<- Neuroticism$index
    DF$A<- Agreeableness$index

    Alpha2(Openness,Conscientiousness,Extraversion,Neuroticism,Agreeableness)
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="10" style="text-align: left;">
Tab 6:
</td>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Quelle
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Item
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
n
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
M
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
SD
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Alpha
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Range
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Skew
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Kurtosi
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Shapiro.Test
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Openness
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
5
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
100
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.11
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1.24
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.85
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-3.96; 4.02
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.17
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-0.17
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
W=0.99, p=.543
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Conscientiousness
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
5
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
100
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.02
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.94
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.78
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-4.47; 3.50
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-0.07
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-0.12
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
W=0.99, p=.938
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Extraversion
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
5
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
100
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-0.11
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1.06
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.83
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-4.51; 4.12
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-0.12
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.15
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
W=0.99, p=.703
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Neuroticism
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
5
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
100
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-0.09
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1.10
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.84
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-4.11; 4.85
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.30
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-0.43
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
W=0.99, p=.356
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
Agreeableness
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
5
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
100
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
0.00
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
1.15
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
0.84
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
-4.54; 4.23
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
0.13
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
-0.29
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
W=0.99, p=.769
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="10">
</td>
</tr>
</tfoot>
</table>

## Test des Regressionsmodels

### Mittelwerte und Effecte

``` r
fit<- lm(Einkommen ~ sex + age + O + C + E + N + A, DF)
#Tabelle2  gibt die Tabellen formatiert aus.
mean_sd<- Tabelle(fit )
```

    ## Registered S3 methods overwritten by 'lme4':
    ##   method                          from
    ##   cooks.distance.influence.merMod car 
    ##   influence.merMod                car 
    ##   dfbeta.influence.merMod         car 
    ##   dfbetas.influence.merMod        car

``` r
mean_sd$sex %>% Output( "Mittelwerte")
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="5" style="text-align: left;">
Tab 7: Mittelwerte
</td>
</tr>
<tr>
<th colspan="1" style="font-weight: 900; border-top: 2px solid black; text-align: center;">
</th>
<th style="border-bottom: none; border-top: 2px solid black;" colspan="1">
 
</th>
<th colspan="3" style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Einkommen
</th>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid black; text-align: center;">
sex
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; text-align: center;" colspan="1">
 
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; text-align: center;">
n
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; text-align: center;">
M
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; text-align: center;">
SD
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
m
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;" colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
31
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2303.33
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
566.81
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
f
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;" colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
69
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
2172.44
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
462.23
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="5">
</td>
</tr>
</tfoot>
</table>

``` r
#mean_sd$O %>% Output(output="md")
```

``` r
APA2(effects::Effect("sex", fit))
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="4" style="text-align: left;">
Tab 8: Effekte: Effect
</td>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
sex
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
fit
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
lower
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
upper
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
m
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2379.71
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2253.87
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2505.56
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
f
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
2138.12
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
2055.28
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
2220.96
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="4">
</td>
</tr>
</tfoot>
</table>

``` r
APA2(visreg::visreg(fit, "sex", plot=FALSE), digits=1)
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="9" style="text-align: left;">
Tab 9: Einkommen
</td>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
sex
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
age
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
O
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
C
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
E
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
N
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
A
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
fit
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
ci
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
m
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
44.6420180327509
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-0.0399716297517768
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.0278381187668443
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.0288540142655275
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-0.177365131342206
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-0.0727364994640318
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2419.9
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
\[2292.4, 2547.5\]
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
f
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
44.6420180327509
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
-0.0399716297517768
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
0.0278381187668443
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
0.0288540142655275
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
-0.177365131342206
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
-0.0727364994640318
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
2178.3
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
\[2095.0, 2261.7\]
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="9">
</td>
</tr>
</tfoot>
</table>

### Regressionsmodel

``` r
APA_Table(fit, include.se=FALSE, include.ci=TRUE)
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="3" style="text-align: left;">
Tab 10:
</td>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Quelle
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
b
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
conf
</th>
</tr>
</thead>
<tbody>
<tr>
<td colspan="3" style="padding-left: .5em; padding-right: .2em; font-weight: 900;">
Parameter
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(Intercept)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2326\*\*\*
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
\[2659, 1992\]
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
sexf
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-242\*\*
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
\[-87.9, -395\]
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
age
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.866
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
\[7.92, -6.19\]
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
O
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-16.5
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
\[41.4, -74.3\]
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
C
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-4.98
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
\[69.5, -79.5\]
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
E
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 106\*\*
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
\[ 172, 40.2\]
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
N
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-321\*\*\*
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
\[-254, -387\]
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
A
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
69.5\*
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
\[ 131, 8.41\]
</td>
</tr>
<tr>
<td colspan="3" style="padding-left: .5em; padding-right: .2em; font-weight: 900;">
Goodness of fit
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
R2
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.56
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
adj. R2
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.53
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
AIC
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1460.0
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
BIC
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1483.4
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
RMSE
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
327.30
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
Obs
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
100
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="3">
</td>
</tr>
</tfoot>
</table>

### GOF

``` r
 APA_Validation(fit) 
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="2" style="text-align: left;">
Tab 11: Testing Regression Models
</td>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Test
</th>
<th style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
statistic
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
F-Statistic
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
F(7, 92)=16.96, p&lt;.001
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Deviance Residuals
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
10712573.9
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
R2
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.56
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
adj. R2
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.53
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Heteroskedasticity (Breusch-Pagan)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
BP(7)=10.57, p=.158
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Autocorrelation (Durbin-Watson)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
DW=2.02, p=.536
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Shapiro-Wilk normality test
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
W=0.93, p&lt;.001
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
AIC
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1460.0
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
BIC
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1483.4
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
Var: Residual
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
116441.02
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
Obs
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
100
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="2">
</td>
</tr>
</tfoot>
</table>

``` r
library(units)
```

    ## udunits system database from C:/Users/wpete/OneDrive/Dokumente/R/win-library/4.0/units/share/udunits

``` r
dat<- data.frame(
  spd1 = set_units(1:5, m/s),
  spd2 = set_units(1:5, km/h))

dat<- Label(dat, spd1="Ultra", spd2="Half")

dat
```

    ##      spd1     spd2
    ## 1 1 [m/s] 1 [km/h]
    ## 2 2 [m/s] 2 [km/h]
    ## 3 3 [m/s] 3 [km/h]
    ## 4 4 [m/s] 4 [km/h]
    ## 5 5 [m/s] 5 [km/h]

``` r
dat %>% Tabelle(spd1, spd2)
```

    ##          Item                              value
    ## 1 Ultra [m/s] 3.00 (SD 1.58, range 1.00 to 5.00)
    ## 2 Half [km/h] 3.00 (SD 1.58, range 1.00 to 5.00)

## Test

Count Data

    stats::chisq.test                   Pearson's Chi-squared Test for Count Data
    stats::fisher.test                  Fisher's Exact Test for Count Data
    stats::mcnemar.test                 McNemar's Chi-squared Test for Count Data
    stats::pairwise.prop.test           Pairwise comparisons for proportions
    stats::prop.test                    Test of Equal or Given Proportions
    stats::prop.trend.test              Test for trend in proportions 
    stats::binom.test                   Exact Binomial Test
     
    stats::cor.test                     Test for Association/Correlation Between Paired Samples
    stats::ansari.test                  Ansari-Bradley Test

T-Test

    stats::t.test                       Student's t-Test    
    stats::pairwise.t.test              Pairwise t tests
    Hmisc::t.test.cluster               t-test for Clustered Data
    stats::oneway.test                  Test for Equal Means in a One-Way Layout

Nicht-parametrische Tests

    stats::kruskal.test                 Kruskal-Wallis Rank Sum Test
    stats::wilcox.test                  Wilcoxon Rank Sum and Signed Rank Tests    
    stats::pairwise.wilcox.test         Pairwise Wilcoxon rank sum tests
    Hmisc::biVar                        Bivariate Summaries Computed Separately by a Series of Predictors
    stats::mantelhaen.test              Cochran-Mantel-Haenszel Chi-Squared Test for Count Data
    stats::friedman.test                Friedman Rank Sum Test
    stats::ks.test                      Kolmogorov-Smirnov Tests
    stats::shapiro.test                 Shapiro-Wilk Normality Test

survival

    survival::cox.zph                   Test the Proportional Hazards Assumption of a Cox Regression
    survival::plot.cox.zph              Graphical Test of Proportional Hazards
    survival::survdiff                  Test Survival Curve Differences 
    survival::survival-internal         Internal survival functions
    survival::survobrien                O'Brien's Test for Association of a Single Variable with Survival
    survival::survregDtest              Verify a survreg distribution

Power calculations

    stats::power.anova.test             Power calculations for balanced one-way analysis of variance tests
    stats::power.prop.test              Power calculations two sample test for proportions
    stats::power.t.test                 Power calculations for one and two sample t tests    
    Hmisc::bpower                       Power and Sample Size for Two-Sample Binomial Test
    Hmisc::ciapower                     Power of Interaction Test for Exponential Survival
    Hmisc::cpower                       Power of Cox/log-rank Two-Sample Test
    Hmisc::spower                       Simulate Power of 2-Sample Test for Survival under Complex Conditions

Sobel

    multilevel::sobel                   Estimate Sobel's (1982) Test for Mediation
    multilevel::sobel.lme               Estimate Sobel's (1982) Test for Mediation in Two-Level lme Model

GOF

    stats::bartlett.test                Bartlett Test of Homogeneity of Variances    
    stats::mauchly.test                 Mauchly's Test of Sphericity    
    stats::var.test                     F Test to Compare Two Variances    
    vcd::coindep_test                   Test for (Conditional) Independence
    vcd::goodfit                        Goodness-of-fit Tests for Discrete Data    
    stats::poisson.test                 Exact Poisson tests

Andere

    stats::Box.test                     Box-Pierce and Ljung-Box Tests
    stats::fligner.test                 Fligner-Killeen Test of Homogeneity of Variances
    stats::mood.test                    Mood Two-Sample Test of Scale
    stats::PP.test                      Phillips-Perron Test for Unit Roots
    stats::quade.test                   Quade Test
