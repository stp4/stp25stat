context("test-Tbll_desc")


 




require(stp25stat)
require(stp25data)
test_that("Tabelle vs Tbll_desc", {

a<-Tabelle(
m1[median] + m2[median] + m3[median] + m4[median] ~ geschl,
varana,
APA = TRUE,
include.n = FALSE,
test = TRUE
)

b<-Tbll_desc(
  m1[median] + m2[median] + m3[median] + m4[median] ~ geschl,
  varana,
  include.n = FALSE,
  include.test = TRUE
)

expect_equivalent(a$geschl, b)


  a<- c(
"wilcox"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="wilcox")[[1]]$statistics[1],
"h.test"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="htest")[[1]]$statistics[1],
"anova"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="anova")[[1]]$statistics[1],
"t.test"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="ttest")[[1]]$statistics[1],
"hmisc"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="contest")[[1]]$statistics[1]
)


b<- c(
   "wilcox"=Tbll_desc(alter ~ geschl, varana, include.n=FALSE, include.test="wilcox")$statistics[1],
   "h.test"=Tbll_desc(alter ~ geschl, varana, include.n=FALSE, include.test="htest")$statistics[1],
   "anova"=Tbll_desc(alter ~ geschl, varana, include.n=FALSE, include.test="anova")$statistics[1],
   "t.test"=Tbll_desc(alter ~ geschl, varana, include.n=FALSE, include.test="ttest")$statistics[1],
   "hmisc"=  Tbll_desc(alter ~ geschl, varana, include.n=FALSE, include.test="contest")$statistics[1]
 )

expect_equal(a, b)

 a<-Tabelle(alter ~ geschl, varana, include.n=FALSE, APA=TRUE)
 b<-Tbll_desc(alter ~ geschl, varana, include.n=FALSE )
 expect_equivalent(a$geschl[[3]],b[[3]])



})


# #Tabelle(alter ~ geschl, varana, APA=TRUE)
# Tbll_desc(alter ~ geschl, varana )

# Tbll_desc(alter ~ geschl, varana, include.nr=TRUE )
# Tbll_desc(alter ~ geschl, varana, include.total=TRUE )
# Tbll_desc(alter ~ geschl, varana, include.test=TRUE )

 