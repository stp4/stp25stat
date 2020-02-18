stp25stat
================

# stp25stat

Functions for Statistical Computations

stp25stat ersaetzt die Funkrion stp25APA2

``` r
which_output()
```

    ## [1] "markdown_html"

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
    DF %>% Tabelle2(sex, age, edu, 
                    "Zwischen Überschrift",
                    beruf, single,
                    APA = TRUE, caption = "Characteristics")
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">

<thead>

<tr>

<td colspan="3" style="text-align: left;">

Tab 1: Characteristics

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

50 (50%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 f

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

50 (50%)

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

43.46 (11.24)

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

11 (11%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 med

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

69 (69%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 hig

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

20 (20%)

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

44 (44%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 withe

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

56 (56%)

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

40 (40%)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

 no

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

60 (60%)

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

Tab 2: Korrelation

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

.50\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.53\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.50\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.53\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-.03

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-.15

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.06

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

.52\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.62\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.54\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-.01

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-.18

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

.50\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.48\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-.05

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-.09

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-.13

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

.56\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.12

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-.13

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.10

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

.07

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-.06

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.05

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

.45\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

.52\*\*\*

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

.36\*\*\*

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

Tab 3: Standardized loadings (pattern matrix) based upon correlation
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

a5

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

25

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

a3

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

23

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

a4

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

24

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

0.65

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

 0.80

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

0.70

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

 0.77

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

n5

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

20

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

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

0.70

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

 0.82

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.69

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

 0.80

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

n2

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

17

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

 0.79

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

o4

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 4

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 0.82

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.69

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

0.64

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

0.65

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

o3

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 3

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

0.61

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

c1

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 6

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

0.67

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

 0.77

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.60

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

 0.74

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.58

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

 0.71

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.54

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

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 0.78

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.65

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

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 0.77

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.69

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

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 0.74

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

 

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 0.72

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.55

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

e4

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

14

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

0.47

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

0.05

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

empirical chi square

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

X2=163.15, p=.875

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

X2(300)=1086.03, p\<.001

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

Tab 6:

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

0.00

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1.15

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.85

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-4.86; 5.29

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.23

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.48

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

W=0.99, p=.667

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

0.13

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1.00

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.81

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-3.45; 4.32

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.04

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-0.16

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

W=1.00, p=.992

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

\-0.12

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1.01

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.80

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-4.27; 3.00

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-0.42

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.58

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

W=0.98, p=.112

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

\-0.04

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1.21

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.87

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-5.21; 4.59

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-0.22

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.62

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

W=0.97, p=.048

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

\-0.04

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

1.26

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

0.87

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

\-4.71; 3.64

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

\-0.22

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

\-0.60

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

W=0.99, p=.337

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

50

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

2102.4

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

485.98

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

f

</td>

<td style="border-bottom: 2px solid grey;" colspan="1">

 

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

50

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

1947.15

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

395.59

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

2080.63

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

2000.45

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

2160.81

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

f

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

1968.92

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

1888.75

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

2049.10

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

Tab 9: Einkommen

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

43.4549708930786

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-0.155174813756757

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.111487965611448

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-0.0862089974845101

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-0.00104498769617332

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.0363865109370558

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

2087.12

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[2005.86, 2168.39\]

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

f

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

43.4549708930786

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

\-0.155174813756757

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

0.111487965611448

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

\-0.0862089974845101

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

\-0.00104498769617332

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

0.0363865109370558

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

1975.42

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

\[1895.35, 2055.48\]

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
APA_Table(fit, include.se=FALSE, include.ci=TRUE, output="html")
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">

<thead>

<tr>

<td colspan="3" style="text-align: left;">

Tab 10:

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

1799\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[2045, 1552\]

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

sexf

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-112

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[2.51, -226\]

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

age

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

6.67\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[11.9, 1.41\]

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

O

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-41.7

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[9.75, -93.2\]

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

C

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

 -24

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[ 33, -81\]

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

E

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

90.3\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[ 147, 33.1\]

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

N

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\-254\*\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[-206, -302\]

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

A

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

64.3\*\*

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

\[ 110, 18.4\]

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

0.63

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

adj.r.squared

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

0.60

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

AIC

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1422.78

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

BIC

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1446.22

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

RMSE

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

271.77

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
 APA_Validation(fit, output="html") 
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">

<thead>

<tr>

<td colspan="2" style="text-align: left;">

Tab 11: Testing Regression Models

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

F(7, 92)=22.17, p\<.001

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Deviance Residuals

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

7385824.9

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

R-Squared

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

R<sup>2</sup>=.63, adj.R<sup>2</sup>=.60

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Heteroskedasticity (Breusch-Pagan)

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

BP(7)=20.87, p=.004

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Autocorrelation (Durbin-Watson)

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

DW=1.94, p=.383

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Shapiro-Wilk normality test

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

W=0.95, p=.002

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

AIC

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1422.8

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

BIC

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

1446.2

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

Var: Residual

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

80280.71

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

    ## udunits system database from C:/Users/wpete/OneDrive/Dokumente/R/win-library/3.6/units/share/udunits

``` r
dat<- data.frame(
  spd1 = set_units(1:5, m/s),
  spd2 = set_units(1:5, km/h))

dat<- Label(dat, spd1="Ultra", spd2="Half")

dat %>% Tabelle(spd1, spd2)
```

    ##          Item                              value
    ## 1 Ultra [m/s] 3.00 (SD 1.58, range 1.00 to 5.00)
    ## 2 Half [km/h] 3.00 (SD 1.58, range 1.00 to 5.00)

``` r
dat
```

    ##      spd1     spd2
    ## 1 1 [m/s] 1 [km/h]
    ## 2 2 [m/s] 2 [km/h]
    ## 3 3 [m/s] 3 [km/h]
    ## 4 4 [m/s] 4 [km/h]
    ## 5 5 [m/s] 5 [km/h]

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
