context("test-dimred-kano")

test_that("aggregate works", {
  
  
  kano_labels <- c( "like",
                    "must be",
                    "neutral",
                    "live with",
                    "dislike")
  
  DF<-stp25aggregate::GetData( 
    "sex Edu f1 d1 f2 d2 f3 d3 f4 d4 f5  d5  f6  d6  f7  d7  f8  d8  f9  d9  f10 d10
    w  med 1  1  1  2  1  3  1  5  1   5   5   1   3   3   5   2   5   1   5   2
    w  med 1  2  2  5  2  3  1  5  1   5   2   5   3   3   2   5   2   5   5   2
    m  med 1  3  3  5  1  5  3  4  1   5   5   1   3   3   5   2   5   1   5   2
    m  med 1  4  4  2  1  5  4  4  1   5   5   1   3   3   5   2   5   1   5   2
    w  med 1  5  5  5  5  3  1  5  1   5   5   1   3   3   5   2   5   1   5   2
    w  med NA NA NA NA NA NA NA NA NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
    m  med 2  1  1  5  2  5  1  5  1   5   2   5   3   3   1   5   2   5   5   2
    w  med 2  2  2  5  1  3  1  5  1   5   3   3   3   3   1   4   1   3   5   2
    m  med 2  3  2  5  2  3  1  3  1   5   1   3   3   3   2   4   3   3   5   2
    m  med 2  4  1  5  1  5  1  5  1   5   1   4   3   3   2   5   1   3   5   2
    w  med 2  5  2  5  1  4  1  5  1   5   1   4   3   3   2   5   1   4   5   2
    m  med NA NA NA NA NA NA NA NA NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
    w  med 3  1  2  5  3  3  1  5  2   5   1   5   3   3   3   3   3   3   5   2
    m  med 3  2  1  5  1  5  2  5  2  NA   1   5   3   3   2   5   1   5   5   2
    w  med 3  3  2  5  1  3  1  5  1   5   1   3   3   3   2   5   1   3   5   2
    w  low 3  4  2  5  2  5  2  5  1   5   1   4   3   3   2   5   1   3   5   2
    w  low 3  5  2  5  1  5  1  5  2   5   1   4   3   3   2   5   1   4   5   2
    w  low NA NA NA NA NA NA NA NA NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
    m  low 4  1  2  5  1  5  2  5  2   5   1   4   2   3   2   5   1   3   5   2
    w  low 4  2  2  5  2  5  2  5  2   5   1   3   3   3   2   5   1   3   5   2
    w  low 4  3  2  5  1  5  2  5  2   5   1   5   1   3   2   5   1   3   5   2
    m  low 4  4  2  5  1  5  2  5  2   5   1   3   3   3   1   3   1   3   5   2
    w  low 4  5  2  5  3  3  2  5  2   5   1   4   1   3   2   5   1   4   5   2
    w  low NA NA NA NA NA NA NA NA NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
    m  hig 5  1  1  5  1  5  2  4  1   5   1   3   3   5   2   4   1   3   5   2
    w  hig 5  2  1  5  1  3  1  5  1   5   1   3   1   5   1   5   3   3   5   2
    w  hig 5  3  2  5  3  3  1  4  2   4   1   3   3   5   3   3   5   1   5   2
    w  hig 5  4  2  5  1  4  2  5  1   5   1   3   3   5   2   5   4   1   5   2
    w  hig 5  5  2  5  2  4  2  4  2   5   1   4   1   5   1   5   1   4   5   2
    m  hig NA NA 2  5  1  5  1  3  1   4   1   3   1   5   1   3   1   3   5   2
    m  hig NA NA 2  1  1  5  1  4  3   3   5   2   3   5  NA  NA   1   3   5   2",
    output=FALSE)



DF<- stp25aggregate::upData2(DF,  labels=c(f1="Fahreigenschaften"
                           ,f2="Sicherheit"
                           ,f3="Beschleunigung"
                           ,f4="Verbrauch"
                           ,f5="Lebensdauer"
                           ,f6="Sonderausstattung"
                           ,f7="Schiebedach"
                           ,f8="Rostschutz"
                           ,f9="Design"
                           , f10= "Rostflecken"
))

 

kano_res1 <-  Kano( ~ . , DF[-c(1,2)])
DF[-c(1,2)] <- stp25aggregate::dapply2(DF[-c(1,2)], 
                                       function(x) factor( x, 1:5, kano_labels))
kano_res2 <-  Kano(~ . , DF[-c(1, 2)])

expect_equal(kano_res1$scors,
             kano_res2$scors)
  
})
