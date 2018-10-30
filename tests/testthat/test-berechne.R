context("test-berechne")

test_that("simpel mean works", {


  x <- c(1.14, 2.47, 3.89, 4.12, 5.17, 6.01, 7.71, 8.08, 9.47, NA)

  expect_equal(mean2(x),   mean(x, na.rm = TRUE))
  expect_equal(sd2(x),   sd2(x, na.rm = TRUE))
  expect_equal(round(CI(x),6),   c(
    upper = 7.447311,
    mean = 5.340000 ,
    lower = 3.232689
  ))

})
