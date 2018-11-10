context("test-anova-multcomp")

test_that("TukeyHSD works", {
  fm1 <- aov(breaks ~ wool + tension, data = warpbreaks)
  tk1 <- TukeyHSD(fm1, "tension", ordered = TRUE)
  res <-  APA_Table(tk1, caption = "TukeyHSD" , output = FALSE)
  
  expect_true(is.list(res))
  expect_equal(round(res[[1]]$estimate, 2),
               c(4.72, 14.72, 10.00))
})



test_that("summary-split works", {
  fm1 <- aov(breaks ~ wool + tension, data = warpbreaks)
  
  fm1_split <-  summary(fm1,
                        split = list(tension = list(
                          M = 1,  H = 3, L = 2
                        )),
                        expand.split = FALSE)
  res <- APA2(fm1_split, output = FALSE)
  
  
  expect_equal(res$statistic,
               c("3.34",  "7.54",  "0.62",  "",      "14.45", ""))
})
