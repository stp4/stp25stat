context("test-anova-manova")

test_that("manova works", {
  set.seed(1)
  npk2 <- within(npk, foo <- rnorm(24))
  npk2 <- within(npk2, foo2 <- rnorm(24))
  
  
  npk2.aov <-
    manova(cbind(yield, foo, foo2) ~ block + N * P * K, npk2)
  
  
  # Fehler mit , include.eta=TRUE !!!!!!!!
  x1 <- APA2(npk2.aov, output = FALSE, include.eta=FALSE)
  x2 <- APA2(npk2.aov, test = "Pillai", output = FALSE, include.eta=FALSE)
  
  
  expect_equal(x1$manova$p.value[2:4],
               c(".016", ".004" , ".475"))
  
  expect_equal(x1$test$Wilks[1:3],
               c("0.19", "0.32", "0.81"))
  
  expect_equal(x2$test$Pillai[1:3],
               c("1.14" , "0.68" , "0.19"))
  
  
  
  
})
