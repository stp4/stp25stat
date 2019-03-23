context("test-describe-rank.R")

test_that(" input table rank works", {
  
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


test_that(" input ranking and ordering works", {
 
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



x1 <-
  Rangreihe(
    ~ V1 + V2 + V3 + V4 + V5,
    Attribute1,
    include.percent = FALSE,
    order = TRUE,
    include.na = FALSE,
    output = FALSE
  )

Attribute2 <- as.data.frame(stp25stat:::transpose(Attribute))
colnames(Attribute2)<- attribute

x2<- Rangreihe(Attribute2, include.percent = FALSE,
               order = TRUE,
               include.na = FALSE,
               output = FALSE)


expect_equal(x1$input, "ordering")
expect_equal(x2$input, "ranking")
expect_equal(x1$res,x2$res)

 

})



# Alt
# 
# 
# 
# DF <-structure(list(
#   Geschlecht = structure(c(1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 
#                            2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 
#                            2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 
#                            2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 
#                            2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 
#                            2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L),
#                          .Label = c("Maennlich", "Weiblich"), 
#                          class = "factor"),
#   Alter = structure(c(2L, 4L, 2L, 4L, 2L, 2L, 2L, 3L, 3L, 2L, 1L, 
#                       1L, 3L, 4L, 4L, 4L, 2L, 1L, 2L, 1L, 4L, 4L, 
#                       3L, 4L, 2L, 2L, 1L, 4L, 4L, 3L, 3L, 3L, 3L, 
#                       2L, 3L, 4L, 3L, 3L, 1L, 3L, 1L, 1L, 2L, 1L, 
#                       1L, 4L, 3L, 1L, 4L, 2L, 2L, 1L, 3L, 3L, 2L, 
#                       3L, 4L, 4L, 1L, 2L, 3L, 2L, 1L, 2L, 1L, 2L, 3L),
#                     .Label = c("20 - 29", "30 - 39", "40 - 49", "50 - 59"), 
#                     class = "factor"),
#   Konsum = structure(c(1L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 
#                        1L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 3L, 1L, 1L, 
#                        1L, 2L, 3L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 
#                        2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 
#                        2L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 2L,
#                        1L, 1L, 2L, 1L, 1L, 2L, 3L, 1L, 2L, 2L, 3L, 2L),
#                      .Label = c("weniger als 3 T.", "3 bis 6 T.", "mehr als 6 T."), 
#                      class = "factor"),
#   Kaffeeform = structure(c(3L, 1L, 3L, 2L, 3L, 3L, 3L, 1L, 3L, 1L, 
#                            3L, 3L, 1L, 2L, 3L, 3L, 3L, 3L, 1L, 3L, 
#                            3L, 2L, 2L, 3L, 3L, 3L, 3L, 2L, 3L, 1L, 
#                            2L, 2L, 2L, 1L, 3L, 3L, 2L, 2L, 3L, 3L, 
#                            3L, 3L, 3L, 3L, 2L, 3L, 3L, 3L, 2L, 3L, 
#                            3L, 2L, 3L, 3L, 3L, 3L, 3L, 2L, 3L, 3L, 
#                            2L, 1L, 3L, 3L, 3L, 2L, 2L),
#                          .Label = c("Espresso", "Filterkaffee", "Milchkaffee"), 
#                          class = "factor"),
#   FavA = structure(c(3L, 1L, 2L, 1L, 3L, 3L, 4L, 1L, 2L, 2L, 1L, 
#                      1L, 1L, 4L, 3L, 4L, 3L, 1L, 2L, 2L, 2L, 2L, 
#                      1L, 1L, 3L, 4L, 1L, 1L, 4L, 4L, 1L, 1L, 1L, 
#                      2L, 1L, 2L, 4L, 3L, 2L, 4L, 1L, 1L, 2L, 2L, 
#                      2L, 4L, 2L, 2L, 2L, 1L, 3L, 2L, 4L, 2L, 4L, 
#                      1L, 4L, 4L, 2L, 1L, 1L, 4L, 2L, 1L, 3L, 2L, 3L),
#                    .Label = c("Cubanischer Arabica Filter", "Cubanischer Arabica Kaltextrakt", 
#                               "Dallmayr Prodomo Kaltextrakt", "Dallmayr Prodomo Filter"), 
#                    class = "factor"),
#   FavB = structure(c(4L, 2L, 1L, 3L, 2L, 1L, 3L, 2L, 1L, 1L, 4L, 
#                      4L, 2L, 2L, 2L, 2L, 4L, 3L, 1L, 1L, 1L, 1L, 
#                      2L, 2L, 2L, 2L, 2L, 4L, 2L, 2L, 3L, 4L, 4L, 
#                      1L, 3L, 1L, 2L, 4L, 4L, 1L, 3L, 3L, 1L, 3L, 
#                      1L, 1L, 1L, 3L, 1L, 2L, 2L, 1L, 3L, 3L, 3L, 
#                      2L, 2L, 3L, 3L, 2L, 4L, 1L, 1L, 2L, 2L, 1L, 2L),
#                    .Label = c("Cubanischer Arabica Filter", "Cubanischer Arabica Kaltextrakt", 
#                               "Dallmayr Prodomo Kaltextrakt", "Dallmayr Prodomo Filter"), 
#                    class = "factor"),
#   FavC = structure(c(2L, 3L, 3L, 4L, 1L, 2L, 1L, 4L, 4L, 3L, 2L, 
#                      3L, 3L, 3L, 4L, 3L, 2L, 2L, 3L, 3L, 3L, 3L, 
#                      3L, 3L, 4L, 3L, 3L, 3L, 3L, 1L, 4L, 3L, 2L, 
#                      3L, 2L, 3L, 1L, 2L, 3L, 2L, 4L, 4L, 4L, 4L, 
#                      3L, 2L, 3L, 1L, 3L, 4L, 4L, 3L, 2L, 1L, 1L, 
#                      3L, 3L, 2L, 1L, 4L, 2L, 3L, 3L, 4L, 1L, 3L, 1L),
#                    .Label = c("Cubanischer Arabica Filter", "Cubanischer Arabica Kaltextrakt", 
#                               "Dallmayr Prodomo Kaltextrakt", "Dallmayr Prodomo Filter"), 
#                    class = "factor"),
#   FavD = structure(c(1L, 4L, 4L, 2L, 4L, 4L, 2L, 3L, 3L, 4L, 3L, 
#                      2L, 4L, 1L, 1L, 1L, 1L, 4L, 4L, 4L, 4L, 4L, 
#                      4L, 4L, 1L, 1L, 4L, 2L, 1L, 3L, 2L, 2L, 3L, 
#                      4L, 4L, 4L, 3L, 1L, 1L, 3L, 2L, 2L, 3L, 1L, 
#                      4L, 3L, 4L, 4L, 4L, 3L, 1L, 4L, 1L, 4L, 2L, 
#                      4L, 1L, 1L, 4L, 3L, 3L, 2L, 4L, 3L, 4L, 4L, 4L),
#                    .Label = c("Cubanischer Arabica Filter", "Cubanischer Arabica Kaltextrakt", 
#                               "Dallmayr Prodomo Kaltextrakt", "Dallmayr Prodomo Filter"), 
#                    class = "factor")),
#   .Names = c("Geschlecht", "Alter", "Konsum", "Kaffeeform", "FavA", "FavB", "FavC", "FavD"), 
#   row.names = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 12L, 13L, 14L, 15L, 
#                 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 
#                 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 
#                 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 
#                 52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 
#                 64L, 65L, 66L, 67L, 68L), class = "data.frame")
# 
# 
# 
# expect_warning( expect_equal( 
#   as.character(Rangreihe(~FavA+FavB+FavC+FavD, DF )$results$FavA),
#   c("31% (21)", "33% (22)", "15% (10)", "21% (14)"  ) )
#)