context("test-Tbll_effect")

require(effects)
 
test_that("Tbll_effect", {
 
   
   mod <- lm(prestige ~ type * (education + income) + women, Prestige)
   ans<- Tbll_effect(mod, ~ type * education + women, include.ci=FALSE)
   
   expect_equivalent(
   ans$women,
   data.frame(women=c(0, 20,50,70,100), value=c( "46.94",  "48.20", "50.08", "51.33", "53.21"))
)

})


test_that("allEffects + tbll_extract", {
   
   
 

fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
fit1 <- update(fit0, . ~ . + pop15)



expect_equivalent(tbll_extract(allEffects(fit1), include.ci=FALSE),
data.frame(pop15 =  c(21, 28, 35, 41, 48), value = c("12.81", "11.25", "9.69", "8.35", "6.79")))

})



test_that("effect transformation", {
   DF <- data.frame(
      
      y= c(12, 15, 23, 44, 11, 44, 49, 27, 25,  8, 
           11, 10, 28, 15, 34, 20, 31, 55, 15, 34, 
           47, 11, 27,  9, 12, 15,  7, 15, 42, 14,
           19, 24, 20, 10, 38, 28, 36,  9, 31, 30),
      iq =c(91,  95, 103, 116,  88, 116, 118, 106, 105,  82, 
            88,  87, 107,  95, 111, 100, 109, 120,  95, 111, 
            117,  88, 106,  85,  91,  95,  81,  95, 115,  94,  
            99, 104, 100,  87, 113, 107, 112,  84, 109,  110 ) 
   )
   
   DF$log.y =  log(DF$y)
   
   fit.linear <- lm(y ~ iq, DF)
   fit.log.y <- lm(log.y ~ iq, DF)
   fit.model.log <- lm(log(y) ~ iq, DF)
   
   ans <- cbind(
      Tbll(effect("iq", fit.linear), include.ci = FALSE),
      log.y = Tbll(effect("iq", fit.log.y), include.ci = FALSE)[[2]],
      log.y.trans = Tbll(effect(
         "iq", fit.log.y,
         transformation = list(link = log, inverse = exp)
      ), include.ci = FALSE)[[2]],
      model.log = Tbll(effect("iq", fit.model.log), include.ci = FALSE)[[2]],
      model.log.trans = Tbll(
         effect("iq", fit.model.log,
                transformation =  list(link = log, inverse = exp)),
         include.ci = FALSE
      )[[2]]
   )
   
   
   expect_equivalent(
      ans,
      
      data.frame(
         iq = c(81, 91, 100, 110, 120),
         value  = c("1.63", "12.82", "22.90", "34.09", "45.28"),
         log.y  = c("2.01", "2.51" , "2.97", "3.48", "3.99"),
         log.y.trans  = c("7.43", "12.36", "19.54" , "32.52", "54.11"),
         model.log  = c("2.01", "2.51", "2.97", "3.48", "3.99"),
         model.log.trans = c("7.43", "12.36", "19.54", "32.52", "54.11")
      )
   )
   
})


test_that("effect glm", {

DF<-LifeCycleSavings
DF$sr10<-DF$sr < 10

fit <-
   glm(sr10 ~ pop15 + pop75 + dpi,
       data = DF,
       family = binomial)


expect_equivalent(
   tbll_extract(allEffects(fit), include.ci=FALSE),
   list(
      pop15 = data.frame(
         pop15 = c(21, 28, 35, 41, 48),
         value = c("0.02", "0.11", "0.43", "0.78", "0.96")
      ),
      
      pop75 = data.frame(
         pop75 = c(0.6, 2, 3, 4, 5),
         value = c("0.26", "0.40", "0.52", "0.63",  "0.73")
      ),
      
      dpi = data.frame(
         dpi = c(90, 1000, 2000, 3000, 4000),
         value = c("0.24", "0.41", "0.62", "0.80", "0.90")
      )
   )
)
   
   })

 