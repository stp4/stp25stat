context("test-anova-lda")

test_that("APA2.lda works", {
  n <- 3 * 2 * 3 *10
  set.seed(n)
  x <- rnorm(n)
    DF <-
    data.frame(
      JOB = gl(3, n, labels = c(
        "customer service", "mechanic", "dispatcher"
      )),
      OUTDOOR = rep(seq(1, 50, length.out = 3),n/3)  + x,
      SOCIAL =   rep(seq(1, 20, length.out = 6),n/6) + x,
      CONSERVATIVE = rep(seq(1, 30, length.out = 9), n/9) + x
    )
  
  
  fit <- MASS::lda(JOB ~ OUTDOOR + SOCIAL + CONSERVATIVE, data = DF)
  
  res <- APA2(fit, output = FALSE)
  expect_equal(names(res),
               c("mean",   "scal",   "svd" ,   "cTab" ,  "cTotal"))
  
  expect_equal(round(res$cTotal[, 2],2),
               32.22)
  
  
})
