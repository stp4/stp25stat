context("test-dimred-reliability")

test_that("APA_Reliability and Alpha works", {
  
  population.model <- '
Fachinteresse =~ F1+F2+F3+F4+F5
  '
  
  # generate data
  set.seed(1234)
  DF <- lavaan::simulateData(population.model, sample.nobs = 60)
  
  
  
  Fachinteresse1 <-
    Reliability(DF[, c("F1", "F2", "F3", "F4", "F5")], 
                check.keys = TRUE)
  Fachinteresse2 <-
    APA_Reliability(DF[, c("F1", "F2", "F3", "F4", "F5")], 
                    check.keys = TRUE, output = FALSE)
  
  Fachinteresse3 <-
    APA_Reliability(~F1+F2+F3+F4+F5, DF,
                    check.keys = TRUE, output = FALSE)
  
  
  expect_equal(Fachinteresse1$Alpha, Fachinteresse2$Alpha)
  expect_equal(Fachinteresse1$Alpha, Fachinteresse3$Alpha)
  
  expect_equal(
  Alpha(Fachinteresse1,Fachinteresse2,Fachinteresse3)$Alpha,
  c("0.81", "0.81", "0.81")
  )
})
