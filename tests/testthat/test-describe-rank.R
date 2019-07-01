context("test-describe-rank.R")

test_that("table-rank works", {
  
  dat_table <-
    as.table(matrix(c(
      50,0,0,0,0,
      0,50,0,0,0,
      0,0,50,0,0,
      0,0,0,50,0,
      0,0,0,0,50
    )
    , nrow = 5, ncol=5, byrow=TRUE,
    dimnames = list(c("A", "B", "C", "D", "E"),1:5)))
  
  
  res<-Rangreihe(dat_table, output=FALSE) 
  #Rangreihe(dat_table , include.percent = FALSE)
  
  
  expect_equivalent(round(res$z.value[, 6],5),
              round(c(
                 A = 3.090232,
                 B = 1.545116,
                 C = 0.000000,
                 D =  -1.545116,
                 E = -3.090232
               ),5))
   
})


test_that("data.frame-formula-ranking", {
 
nlv <- 5
n <- 2 * 3 * nlv * 1
set.seed(n)

Attribute <-
  as.data.frame(t(apply(matrix(NA, ncol = n, nrow = 5), 2,
                        function(x)
                          sample.int(5))))

attribute  <- c("Verfuegbarkeit",
                "Vielfalt",
                "Qualitaet",
                "Geschmack",
                "Preis")

Attribute1 <-
  dapply2(Attribute, function(x)
    factor(x, 1:5, attribute))


# Attribute1
# V1             V2             V3             V4
# 1  Verfuegbarkeit       Vielfalt      Geschmack          Preis
# 2  Verfuegbarkeit      Geschmack          Preis       Vielfalt
# 3  Verfuegbarkeit       Vielfalt      Geschmack      Qualitaet

x1 <-
  
  Rangreihe(
    ~ V1 + V2 + V3 + V4 + V5,
    Attribute1,
    include.percent = FALSE,
    order = TRUE,
    include.na = FALSE,
    output = FALSE
  )
expect_equal(x1$input, "ordering")



DF2 <-   data.frame(R1 = factor(c("A", "A", "A", "C", "C", "A"),   c("A", "B", "C", "D")),
                    R2 = factor(c("B", "B", "B", "A", "B", "D"),   c("A", "B", "C", "D")),
                    R3 = factor(c("C", "C", "C", "B", "A", "B"),   c("A", "B", "C", "D")))

x1 <- Rangreihe( ~ R1 + R2 + R3, DF2, output = FALSE)

x2 <- DF2 %>% Rangreihe(R1, R2, R3, output = FALSE)

x3 <- Rangreihe(DF2, output = FALSE)

# expect_equal(x1 , x2)
# expect_equal(x1 , x3)
# 

expect_equal(x1$res , x2$res)
expect_equal(x1$res , x3$res)

})

test_that("transpose3 workes", {
  
  DF2 <-   data.frame(
    R1 = factor(c("A", "A", "A", "C", "C", "A"),   c("A", "B", "C", "D")),
    R2 = factor(c("B", "B", "B", "A", "B", "D"),   c("A", "B", "C", "D")),
    R3 = factor(c("C", "C", "C", "B", "A", "B"),   c("A", "B", "C", "D"))
  )
  DF1 <-  stp25stat:::transpose3(DF2)
  # 
  # # Attribute2
  # # 
  # #   R1 R2 R3  A B C D
  # # 1  A  B  C  1 2 3 0
  # # 2  A  B  C  1 2 3 0
  # # 3  A  B  C  1 2 3 0
  # # 4  C  A  B  2 3 1 0
  # # 5  C  B  A  3 2 1 0
  # # 6  A  D  B  1 3 0 2
  # 
  
  x11<- Rangreihe(DF1, A, B, C, D, include.percent = FALSE,
                  order = TRUE,
                  include.na = FALSE,
                  output = FALSE)
  
  
  x22<- Rangreihe(DF2, R1, R2, R3,  include.percent = FALSE,
                  order = TRUE,
                  include.na = FALSE,
                  output = FALSE)
  expect_equal(x11$input, "ranking")
  expect_equal(x22$input, "ordering")
  expect_equal(x11$res, x22$res)
  # 
  expect_equivalent(x11$items, x22$items)
  # 
  expect_equal(
    stp25stat:::guess_input(DF1)$rankings,
    stp25stat:::guess_input(DF2)$rankings
  )
  
  
  
  
})
