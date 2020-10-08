



# stp25stat 0.2.2

## Changes to functions


* `Tbll_desc()`ist jetzt eine eigenstaendige Funktion. Das Resultat ist etwas andest formatiert, arbeitet aber ansonsten gleich wie Tabelle()
* Neue Statistik Methode in erate_statistik  Beispiel: `hkarz %>% Tbll_desc(gruppe[binomial], by =  ~ Lai, include.test = TRUE)`
* 
* `Tbll_desc_long()`ist jetzt eine eigenstaendige Funktion abgeleitet von Tabelle()



# stp25stat 0.2.1

## New functions



## APA2 Methoden

### Effectsize

*emmeans: APA2.emm_list, APA2.emmGrid


*  `Tbll()` Kombiniert APA2 und Tabelle. Ziel ist, einheitliche Namen fuer die Zukunft zu schaffen. `Tbll_desc()`,
`Tbll_corr()`, `Tbll_xtabs` usw


## Changes to functions

* In die Funktion `APA_Xtabs()` sind die margins und die add.margins neu.



# stp25stat 0.2.0

## New functions

* `APA2.matchit` print a short summary.
* `APA_CI`  Konfidenzintervalle
* `APA_Text` Funktion für offene Texte diese werden Roh als Tabelle ausgeben.

 
## Changes to functions
* In die Funktion `Likert()` wurde neu geschrieben.
* In die Funktion `Tabelle()` wurde Describe2 integirert.
* Rename `Ordnen()` to `extract()`
* Loeschen der Funktionen  `berechne()`.
* In die Funktion `prepare_output()`   set_my_options(caption= "include.n") 



# stp25stat 0.0.1

## General

* Collection of convenient functions for common statistical computations.

## New functions

* `APA()` Extrahiert aus einem R-Ststistik-Objekte einen Character-String mit p-Werten
* `APA2()`Extrahiert aus einem R-Ststistik-Objekte Statistik und p-Werten und erstellt eine Tabelle.
* `Tabelle()` Deskriptive Analyse.


