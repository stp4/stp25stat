context("test-dimred-sem-lavaan")

test_that("APA2.sem works", {
  population.model <- '
  Fachinteresse =~ F1+F2+F3+F4+F5
  Soziale.Einbindung =~ S1+S2+S3+S4
  Relevanz.Inhalte=~ R1+R2+R3+R4
  Kompetenzerleben =~ K1+K2+K3+K4
  Autonomieerleben=~ A1+A2+A3+A4+A5+A6
  Motivierungsqualitaet=~ M1+M2+M3+M4
  '
  
  # generate data
  set.seed(1234)
  DF <- lavaan::simulateData(population.model, sample.nobs = 60)
  
  
  fit.Lavaan <- lavaan::sem(population.model, data = DF)
  #  lavaan::standardizedSolution(fit.Lavaan)
  x <- APA2(fit.Lavaan, output = FALSE)
  expect_equal(x$varianz$loading[1:5],
               c("0.73" , "0.39" , "0.59", "0.66" , "0.26"))
})
