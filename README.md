stp25stat
================

# stp25stat

Functions for Statistical Computations

stp25stat ersaetzt die Funkrion stp25APA2

``` r
which_output()
```

    ## [1] "markdown_html"

## Artificial data

``` r
require(wakefield)
```

    ## Loading required package: wakefield

``` r
set.seed(0815)
DF <- r_data_frame(
  n = 30,
  id,
  race,
  age(x = 8:14),
  sex,
  hour,
  iq,
  height(mean = 50, sd = 10),
  died,
  Smoker = valid,
  likert,
  date_stamp(prob = probs(12))
) %>% clean_names()



DF %>% Tabelle2(age, sex, height,
                race, died, smoker,  
                iq, likert,
                by =  ~ sex,
                APA = TRUE)
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">

<thead>

<tr>

<td colspan="7" style="text-align: left;">

Tab 1: Sex Charakteristik

</td>

</tr>

<tr>

<th colspan="1" style="font-weight: 900; border-top: 2px solid grey; text-align: center;">

</th>

<th style="border-top: 2px solid grey;; border-bottom: hidden;">

 

</th>

<th colspan="2" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

 Male

</th>

<th style="border-top: 2px solid grey;; border-bottom: hidden;">

 

</th>

<th colspan="2" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

 Female

</th>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; text-align: center;">

Item

</th>

<th style="border-bottom: 1px solid grey;" colspan="1">

 

</th>

<th style="border-bottom: 1px solid grey; text-align: center;">

n

</th>

<th style="border-bottom: 1px solid grey; text-align: center;">

m

</th>

<th style="border-bottom: 1px solid grey;" colspan="1">

 

</th>

<th style="border-bottom: 1px solid grey; text-align: center;">

n

</th>

<th style="border-bottom: 1px solid grey; text-align: center;">

m

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Age (mean)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

13

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

10.00 (1.83)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

17

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

11.06 (1.78)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Sex 

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

13

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

17

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 Male

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

13 (100%)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 Female

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

17 (100%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Height (mean)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

13

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

48.46 (8.32)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

17

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

51.76 (7.60)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Race 

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

13

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

17

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 White

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

7 (54%)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

12 (71%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 Hispanic

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

2 (15%)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

3 (18%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 Black

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

3 (23%)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

2 (12%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 Asian

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1 (8%)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 Bi-Racial

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 Native

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 Other

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 Hawaiian

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Died true

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

13

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

6 (46%)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

17

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

12 (71%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Smoker true

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

13

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

9 (69%)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

17

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

5 (29%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

IQ (mean)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

13

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

97.85 (8.98)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

17

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

101.06 (14.77)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Likert 

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

13

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

17

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 Strongly Disagree

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1 (8%)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

3 (18%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 Disagree

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

3 (23%)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

5 (29%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 Neutral

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

4 (31%)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

4 (24%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 Agree

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

4 (31%)

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

4 (24%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

 Strongly Agree

</td>

<td style="border-bottom: 2px solid grey;" colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

1 (8%)

</td>

<td style="border-bottom: 2px solid grey;" colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

1 (6%)

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td colspan="7">

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
    DF %>% Tabelle2(sex, age, edu, 
                    "Zwischen Überschrift",
                    beruf, single,
                    APA = TRUE, caption = "Characteristics")
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">

<thead>

<tr>

<td colspan="3" style="text-align: left;">

Tab 2: Characteristics

</td>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

Item

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

n

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

m

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

sex 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

100

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 m

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

29 (29%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 f

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

71 (71%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

age (mean)

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

100

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

49.38 (10.65)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

edu 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

100

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 low

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

10 (10%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 med

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

51 (51%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 hig

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

39 (39%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Zwischen Überschrift 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

beruf 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

100

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 blue

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

34 (34%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 withe

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

66 (66%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

single 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

100

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 yes

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

29 (29%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

 no

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

71 (71%)

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

``` r
APA_Correlation(~o1+o2+o3+o4+o5+c1+c2+c3, DF)
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">

<thead>

<tr>

<td colspan="9" style="text-align: left;">

Tab 3: Korrelation

</td>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

Quelle

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

(1)

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

(2)

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

(3)

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

(4)

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

(5)

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

(6)

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

(7)

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

(8)

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

.57\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.55\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.60\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.52\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.19

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.04

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-.03

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

.45\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.60\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.45\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-.04

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-.06

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-.03

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

.61\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.51\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.20\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.06

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.01

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

.55\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.11

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.02

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.01

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

.10

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.04

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.08

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

.49\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.34\*\*\*

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

.46\*\*\*

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

(8) c3

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

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

Tab 4: Standardized loadings (pattern matrix) based upon correlation
matrix

</td>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

Item

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

Nr

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

PC1

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

PC2

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

PC3

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

PC4

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

PC5

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

h2

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

o4

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 4

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 0.85

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

0.73

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

o1

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 1

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 0.82

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

0.72

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

 0.78

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

0.62

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

 0.74

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

0.61

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

 0.83

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

a4

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

24

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

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.68

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

 0.78

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.63

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

0.55

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

 0.68

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.52

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

 0.80

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

 0.79

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.66

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

 0.78

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.61

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

 0.76

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

 0.75

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

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 0.81

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.71

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

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 0.77

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

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 0.74

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.60

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

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 0.73

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.57

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

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 0.73

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

 0.77

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.62

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

 0.76

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.60

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

 0.75

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.59

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

 0.74

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.59

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

c1

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

 6

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

 0.68

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

0.54

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

Tab 5: Standardized loadings (pattern matrix) based upon correlation
matrix

</td>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

Measures

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

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

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

empirical chi square

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

X2=182.28, p=.543

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

Tab 6: Standardized loadings (pattern matrix) based upon correlation
matrix

</td>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

Measures

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

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

0.72

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

Bartlett’s test of sphericity

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

X2(300)=1037.47, p\<.001

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

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">

<thead>

<tr>

<td colspan="10" style="text-align: left;">

Tab 7:

</td>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

Quelle

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

Item

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

n

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

M

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

SD

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

Alpha

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

Range

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

Skew

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

Kurtosi

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

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

0.04

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1.22

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.85

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-3.96; 3.78

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.06

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-0.18

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

W=0.99, p=.502

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

0.08

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.97

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.80

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-4.47; 3.55

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-0.13

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-0.21

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

W=0.99, p=.671

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

0.10

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1.09

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.84

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-4.51; 4.12

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-0.16

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.20

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

W=0.99, p=.499

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

\-0.15

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1.06

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.81

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-4.11; 4.85

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.27

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-0.56

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

W=0.98, p=.257

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

Agreeableness

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

5

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

100

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

0.03

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

1.12

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

0.84

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

\-4.54; 4.23

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

0.19

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

\-0.30

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

W=0.99, p=.527

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

Tab 8: Mittelwerte

</td>

</tr>

<tr>

<th colspan="1" style="font-weight: 900; border-top: 2px solid grey; text-align: center;">

</th>

<th style="border-top: 2px solid grey;; border-bottom: hidden;">

 

</th>

<th colspan="3" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

Einkommen

</th>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; text-align: center;">

sex

</th>

<th style="border-bottom: 1px solid grey;" colspan="1">

 

</th>

<th style="border-bottom: 1px solid grey; text-align: center;">

n

</th>

<th style="border-bottom: 1px solid grey; text-align: center;">

M

</th>

<th style="border-bottom: 1px solid grey; text-align: center;">

SD

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

m

</td>

<td style colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

29

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

2323.36

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

594.87

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

f

</td>

<td style="border-bottom: 2px solid grey;" colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

71

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

2124.77

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

401.93

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

Tab 9: Effekte: Effect

</td>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

sex

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

fit

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

lower

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

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

2398.31

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

2268.58

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

2528.04

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

f

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

2094.16

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

2012.93

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

2175.39

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
APA2(visreg::visreg(fit, "sex", plot=FALSE))
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">

<thead>

<tr>

<td colspan="9" style="text-align: left;">

Tab 10: Einkommen

</td>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

sex

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

age

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

O

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

C

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

E

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

N

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

A

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

fit

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

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

49.5034145628059

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.0257084929372925

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.131716153092842

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.196639846431887

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-0.203510181429749

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-0.0629899907928555

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

2420.51

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[2288.67, 2552.35\]

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

f

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

49.5034145628059

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

0.0257084929372925

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

0.131716153092842

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

0.196639846431887

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

\-0.203510181429749

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

\-0.0629899907928555

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

2116.36

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

\[2035.63, 2197.09\]

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

    ## Warning: The `x` argument of `as_tibble.matrix()` must have column names if `.name_repair` is omitted as of tibble 2.0.0.
    ## Using compatibility `.name_repair`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">

<thead>

<tr>

<td colspan="3" style="text-align: left;">

Tab 11:

</td>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

Quelle

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

b

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

conf

</th>

</tr>

</thead>

<tbody>

<tr>

<td colspan="3" style="font-weight: 900;">

Parameter

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

(Intercept)

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

2181\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[2529, 1833\]

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

sexf

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-304\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[-148, -460\]

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

age

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

3.22

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[ 10, -3.55\]

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

O

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

25.1

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[82.9, -32.7\]

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

C

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-17.7

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[53.5, -88.9\]

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

E

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 122\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[ 185, 58\]

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

N

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-299\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[-232, -366\]

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

A

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

49.4

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[ 111, -12.2\]

</td>

</tr>

<tr>

<td colspan="3" style="font-weight: 900;">

Goodness of fit

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

r.squared

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.52

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

adj.r.squared

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.48

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

AIC

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1459.01

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

BIC

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1482.46

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

RMSE

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

325.75

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

Obs

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

100

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

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

Tab 12: Testing Regression Models

</td>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

Test

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

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

F(7, 92)=14.14, p\<.001

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Deviance Residuals

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

10611275.3

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

R-Squared

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

R<sup>2</sup>=.52, adj.R<sup>2</sup>=.48

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Heteroskedasticity (Breusch-Pagan)

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

BP(7)=10.05, p=.186

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Autocorrelation (Durbin-Watson)

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

DW=2.11, p=.702

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Shapiro-Wilk normality test

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

W=0.90, p\<.001

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

AIC

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1459.0

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

BIC

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1482.5

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Var: Residual

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

115339.95

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

Obs.

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

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
