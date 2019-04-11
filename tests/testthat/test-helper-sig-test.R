context("test-helper-sig-test.R")

test_that("catTest chi-test", {
  expect_equal(
    stp25stat:::catTest(~ tension + wool, warpbreaks),
    "X2(2)=0.00, p=1.000"
    
  )
  
  expect_equal(
    stp25stat:::conTest(breaks ~ wool,
                        warpbreaks,
                        "h.test") ,
    "H(1)=1.33, p=.250"
  )
  
  
})
