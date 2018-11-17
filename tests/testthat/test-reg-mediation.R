context("test-reg-mediation")

test_that("mediation works", {
  set.seed(1234)
  Motivation <- rnorm(100)
  Lerndauer <- 0.5 * Motivation + rnorm(100)
  Note <- 0.7 * Lerndauer + rnorm(100)
  dat <- data.frame(Note = Note,
                    Lerndauer = Lerndauer,
                    Motivation = Motivation)
  
  
  
  res <- Mediation(
    Note ~ Motivation,
    Note ~ Motivation + Lerndauer,
    Lerndauer ~ Motivation,
    Note ~ Motivation * Lerndauer,
    dat,
    digits = 2,
    output = FALSE
  )
  
  expect_equal( names(res) , c("parameter", "sobel", "model") )
  
  
  expect_equal(res$sobel$B,
               "0.28")
  
})
