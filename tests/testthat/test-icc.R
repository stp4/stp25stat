context("test-icc")

test_that("ICC works", {
  
  expect_output(
  data <- stp25aggregate::GetData("
                                  Item J1 J2 J3 J4 J5 J6 J7 J8 J9 J10
                                  1  5  4  5  4  5  4  5  4  5  4
                                  2  4  5  4  5  4  5  4  5  4  5
                                  3  5  5  5  5  5  4  4  4  4  4
                                  4  5  4  4  5  5  5  4  5  4  5")
  
  )
  ad <-
    AD_Index2( ~ J1 + J2 + J3 + J4 + J5 + J6 + J7 + J8 + J9 + J10,
               data,
               A = 5,
               output = FALSE)
  expect_equal(names(ad),
               c("AD.Index", "expected.error", "mean"))
  
  ic <- ICC2( ~ J1 + J2 + J3 + J4 + J5 + J6 + J7 + J8 + J9 + J10, data, output =
                FALSE)
  expect_is(ic, "data.frame" )
  #psych::ICC( as.matrix( data[,-1]))$results
  
  
})
