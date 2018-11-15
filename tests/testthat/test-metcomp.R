context("test-metcomp")

test_that("bland-altman works", {
  
  
  DF<- data.frame(
    A=c(1, 5,10,20,50,40,50,60,70,80, 90,100,150,200,250,
        300,350,400,450,500,550,600,650,700,750,800,850,900, 950,1000),
    B=c(8,16,30,14,39,54,40,68,72,62,122, 80,181,259,275,
        380,320,434,479,587,626,648,738,766,793,851,871,957,1001, 980),
    group= sample(gl(2, 15, labels = c("Control", "Treat")))
  )
  
  x1<- MetComp(~A+B, DF, caption = "Giavarina", output=FALSE)
  expect_equal(
  names(x1),
  c("lines", "CI.lines",        
   "lines.percent","CI.lines.percent",
   "stat", "data",            
   "name", "name.diff",       
  "met_A", "met_B")
  )
  
  expect_equal(x1$stat$Unit,
               c("29", "-27.50", "33.94", "66.52", "-94.02", "39.02"))
  
  
  x2 <-
    MetComp(
      A + B ~ group,
      DF,
      caption = "Giavarina",
      output = FALSE,
      include.ci = FALSE
    )
  
  expect_equal(names(x2$stat),
               c("Parameter", "Unit", "SE", "Percent"))
  
  
})
