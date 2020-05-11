require(stpvers)
require(stp25data)
require(lmerTest)
require(modelr)
require(tidyverse)
varana2 <- varana %>%
  gather( Zeit, Merkfgk, m1:m4 ) %>%
  mutate(Zeit=factor( Zeit, Cs(m1, m2, m3 ,m4), Cs(t0, t1, t2, t3))) %>%
  Label(Merkfgk="Merkfaehigkeit")

tabl <- function(...) {
  res <- APA2(..., output = FALSE)
  res
}
desc_tabl <-
  function(..., APA = TRUE) {
    res <- Tabelle(..., APA = APA)
   if(length(res)==1) res[[1]]
    else res
    
  }
reg_tabl <- function(...) {
  res <- APA_Table(..., output = FALSE)
  res
}
 
cor_tabl <-
  function(...) {
    res <- APA_Correlation(..., output = FALSE)
    res
  }
x_tabl <-  function(...) {
  res <- APA_Xtabs(..., output = FALSE)
  res
}


#+ simpel-apa, results='asis', warning=FALSE
tabl(alter ~ geschl, varana)
desc_tabl(alter ~ geschl, varana)
varana %>% desc_tabl(alter, by= ~ geschl)


#+ modelr, results='asis', warning=FALSE
disp_fits <- varana2 %>%
  fit_with(lmer,
           formulas(~Merkfgk,
                    basline = ~ Zeit  +(1 | nr),
                    additive = ~ geschl + alter  + (1 | nr),
                    interaction = ~ geschl *  alter * Zeit  + (1 | nr) ))

 

#+  
reg_tabl(disp_fits )