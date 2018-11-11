context("test-dimred-pca")

test_that("APA_PCA works", {
 
population.model <- '
  Fachinteresse =~ F1+F2+F3+F4+F5
  Soziale.Einbindung =~ S1+S2+S3+S4
  Relevanz.Inhalte=~ R1+R2+R3+R4
  Kompetenzerleben =~ K1+K2+K3+K4
  Autonomieerleben=~ A1+A2+A3+A4+A5+A6
  Motivierungsqualitaet=~ M1+M2+M3+M4
  '
  
  set.seed(1234)
  DF <- lavaan::simulateData(population.model, sample.nobs = 60)
  
  x <- APA_PCA(
    DF,
    6,
    cut = .35,
    include.test = FALSE,
    include.plot = FALSE,
    include.kmo = FALSE,
    output = FALSE
  )
  expect_equal(x$Loadings$h2[1:5],
               c("0.80", "0.66", "0.65", "0.59", "0.61"))
  
  
})
