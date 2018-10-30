context("test-percent")

test_that("simpel Percent works", {
  expect_equal(Prozent(gl(2, 8, labels = c(
    "Control", "Treat"
  ))),
  c(Control = "50% (8)", Treat =  "50% (8)"))



  expect_equal (Prozent2default(factor(c(1, 2, 3, 3, 3), 1:4)),
                data.frame(
                  lev = c("1", "2", "3" , "4"),
                  n = c("5", "", "" , ""),
                  m = c("20% (1)", "20% (1)", "60% (3)", "<1% (0)"),
                  stringsAsFactors = FALSE
                ))




  expect_equal(
    Prozent2APA(gl(2, 8, labels = c(
      "Control", "Treat"
    ))),
    data.frame(
      Characteristics = c("Control", "Treat"),
      n  = c("16", ""),
      Statistics  = c("50.0% (8)", "50.0% (8)"),
      stringsAsFactors = FALSE
    )
  )

})
