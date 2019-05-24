# stp25stat
Functions for Statistical Computations

stp25stat ersaetzt die Funkrion stp25APA2


Artificial data
---------------

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

Mittelwerte und Prozent
-----------------------

    DF %>% Tabelle2(sex, age, edu, beruf, single,
                    APA = TRUE, caption = "Characteristics")

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

PCA
---

    vars <- c(
      "o1",  "o2",  "o3",  "o4",  "o5",  "c1",  "c2",  "c3",
      "c4",  "c5",  "e1",  "e2",  "e3",  "e4",  "e5",  "n1",
      "n2",  "n3",  "n4",  "n5",  "a1",  "a2",  "a3",  "a4",  "a5"
    )
     APA_PCA(DF[vars], 5, cut=.35, include.plot = FALSE)

    ## R was not square, finding R from data

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="8" style="text-align: left;">
Tab 2: Standardized loadings (pattern matrix) based upon correlation
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
Tab 3: Standardized loadings (pattern matrix) based upon correlation
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
Tab 4: Standardized loadings (pattern matrix) based upon correlation
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
X2(300)=1086.03, p&lt;.001
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

Reliability
-----------

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

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="10" style="text-align: left;">
Tab 5:
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
-4.86; 5.29
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
-3.45; 4.32
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.04
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-0.16
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
-0.12
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1.01
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.80
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-4.27; 3.00
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-0.42
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
-0.04
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1.21
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.87
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-5.21; 4.59
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
-0.22
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
-0.04
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
1.26
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
0.87
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
-4.71; 3.64
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
-0.22
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
-0.60
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

Test des Regressionsmodels
--------------------------

    fit<- lm(Einkommen ~ sex + age + O + C + E + N + A, DF)

    Tabelle2(fit)

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="5" style="text-align: left;">
Tab 6: AV: Einkommen
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
<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="5" style="text-align: left;">
Tab 7: AV: Einkommen
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
age
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
(17.9,28.8\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
10
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2041.4
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
322.34
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(28.8,39.6\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
26
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2032.08
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
479.58
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(39.6,50.4\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
37
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1947.1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
332.8
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(50.4,61.2\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
22
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2113.32
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
625.82
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
(61.2,72.1\]
</td>
<td style="border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
5
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
2138.79
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
346.93
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
<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="5" style="text-align: left;">
Tab 8: AV: Einkommen
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
O
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
(-3.24,-1.85\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
5
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2264.7
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
584.19
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(-1.85,-0.465\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
26
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2006.77
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
420.32
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(-0.465,0.921\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
50
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2056.21
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
443.54
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(0.921,2.31\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
17
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1819.91
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
284.03
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
(2.31,3.7\]
</td>
<td style="border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
2
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
2614.58
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
1173.07
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
<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="5" style="text-align: left;">
Tab 9: AV: Einkommen
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
C
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
(-2.63,-1.56\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
5
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2144.19
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
289.54
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(-1.56,-0.488\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
20
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2075.92
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
541
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(-0.488,0.579\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
40
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2063.53
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
471.34
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(0.579,1.65\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
25
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1926.85
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
395.02
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
(1.65,2.72\]
</td>
<td style="border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
10
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
1952.57
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
343.56
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
<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="5" style="text-align: left;">
Tab 10: AV: Einkommen
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
E
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
(-3.16,-2.07\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
4
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2044.59
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
518.04
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(-2.07,-0.988\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
12
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1797.16
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
359.28
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(-0.988,0.0957\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
41
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1947.83
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
455.21
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(0.0957,1.18\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
36
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2145.46
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
448.11
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
(1.18,2.27\]
</td>
<td style="border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
7
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
2233.67
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
317.7
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
<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="5" style="text-align: left;">
Tab 11: AV: Einkommen
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
N
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
(-3.79,-2.49\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
4
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2707.06
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
871.9
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(-2.49,-1.2\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
9
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2763.63
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
434.18
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(-1.2,0.096\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
45
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2050.58
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
302.25
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(0.096,1.39\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
31
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1816.97
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
235.24
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
(1.39,2.69\]
</td>
<td style="border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
11
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
1652.2
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
265.36
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
<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="5" style="text-align: left;">
Tab 12: AV: Einkommen
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
A
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
(-2.82,-1.71\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
12
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1727.95
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
332.23
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(-1.71,-0.602\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
21
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2084.56
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
599.53
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(-0.602,0.504\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
33
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1954.7
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
390.59
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(0.504,1.61\]
</td>
<td style colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
25
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2176.2
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
405.08
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
(1.61,2.72\]
</td>
<td style="border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
9
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
2117.37
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">
290.58
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

    APA_Table(fit, include.se=FALSE, include.ci=TRUE)

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="3" style="text-align: left;">
Tab 13:
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
-112
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
-41.7
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
-254\*\*\*
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

