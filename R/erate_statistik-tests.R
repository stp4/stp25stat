contest <-
  c("contest",
    "wilcox",
    "utest",
    "htest",
    "kruskal",
    "ttest",
    "aov",
    "anova")
cattest <-  c("cattest", "chisq", "fisher", "ordtest", "binomial")
notest <-  c("notest")
ordtest <-  c("ordtest")
disttest <-  c("shapiro", "kstest")
cortest <-   c("pearson", "kendall", "spearman")
stattest <-   c(contest, cattest, notest, disttest)



#' Sig-Test fur Tabelle
#'
#' @param x formula x~gruppe
#' @param data data.frame
#' @param test_name c("contest", "wilcox","utest", "htest", "kruskal", "ttest","aov", "anova")
#'
#' @return formatierter String
#' @noRd
#' @examples 
#' 
#' dat <- data.frame(
#' m1 = c(1, 2, 1, 3, 1, 4, 1, 1,
#'        3, 5, 3, 1, 3, 6, 3, 1),
#' geschl = gl(2, 8, labels = c("m", "f"))
#' )
#' dat$m2 <- cut(dat$m1, 2)
#' 
#' conTest(m1  ~ geschl, dat)
#' catTest(m1  ~ geschl, dat)
#' ordTest(m1  ~ geschl, dat)
#' spearmanTest2(m1  ~ geschl, dat)
#' TTest2(m1  ~ geschl, dat)
#' Aov2(m1  ~ geschl, dat)
#' KruskalTest2(m1  ~ geschl, dat)
#' WilkoxTest2(m1  ~ geschl, dat)
#' chisqTest2(m1  ~ geschl, dat)
#' chisqTest2( ~ m1 + geschl, dat)
#' fisherTest2( ~ m1 + geschl, dat)
#' 
#' fisherTest2( ~ m2 + geschl, dat)
#' 
#' # Wilcoxon.Test       "F(1, 14)=3.20, p=.095"    
#' # V2                  "Error wrong formula!"     
#' # Logistic.Regression "LR(1)=3.14, p=.077"       
#' # Wilcoxon.Test.1     "F(1, 14)=3.20, p=.095"    
#' # T.Tests             "T(12)=-1.87, p=.086"      
#' # ANOVA               "F(1, 14)=3.49, p=.083"    
#' # Kruskal.Wallis.Test "H(1)=2.79, p=.095"        
#' # Wilcoxon.Test.2     "U=17.00, p=.106"          
#' # V9                  "Error wrong formula!"     
#' # Pearson.Chi.squared "X2(5)=7.09, p=.214"       
#' # V11                 "wrong dim for fisher-test"
#' # Fisher.Exact.Test   "OR=2.21, p=1.000" 
conTest = function(x,
                   data,
                   test_name = TRUE) {
  g <- size_data_tabel(x, data)
  
  if (all(g > 4)) {
    if (is.logical(test_name)) {
      spearmanTest2(x, data)
    } else{
      if (test_name == "wilcox")
        WilkoxTest2(x, data)
      else if (test_name == "utest")
        WilkoxTest2(x, data)
      else if (test_name == "htest")
        KruskalTest2(x, data)
      else if (test_name == "kruskal")
        KruskalTest2(x, data)
      else if (test_name == "ttest")
        TTest2(x, data)
      else if (test_name == "aov")
        Aov2(x, data)
      else if (test_name == "anova")
        Aov2(x, data)
      else
        test_name
    }
  }
  else
    paste0("sample to small (", paste(g, collapse = "/"), ")")
}

catTest = function(x,
                   data,
                   include.test = "chisq") {
  g <- size_data_tabel(x, data)
  cat("\n include.test: ", include.test,"\n")
  if (all(g > 4)) {
    if (include.test == "chisq")
      chisqTest2(x, data)
    else if (include.test == "fisher")
      fisherTest2(x, data)
    else if (include.test == "binomial")
      gml_binomial(x, data)
    else{
      include.test
    }
  }
  else
    paste0("sample to small (", paste(g, collapse = "/"), ")")
}


#' Logistic Regression Model
ordTest = function(x, data) {
  g <- size_data_tabel(x, data)
  if (all(g > 4)) {
    f <- rms::lrm(x, data)$stats
    # list(P = f["P"], stat = f["Model L.R."], df = f["d.f."],
    #      testname = "Proportional odds likelihood ratio",
    #      statname = "Chi-square", plotmathstat = "chi[df]^2")
    res<-gsub("X2", "LR", stp25rndr::rndr_X(f["Model L.R."], f["d.f."], NULL, f["P"]))
    names(res)<- "Logistic Regression"
    res
  }
  else
    paste0("sample to small (", paste(g, collapse = "/"), ")")
}

size_data_tabel <- function(x, data) {
  g <- all.vars(x)
  data <- na.omit(data[g])
  g <- g[length(g)]
  table(data[[g]])
}

#' Uses midranks in case of ties, as described by Hollander and Wolfe.
#' P-values for Spearman, Wilcoxon, or Kruskal-Wallis tests are
#' approximated by using the t or F distributions.
spearmanTest2 <- function(x, data) {
  st <- Hmisc::spearman2(x, data)
  if (is.na(st[3]))
    return("Error")
  
  res <- stp25rndr::rndr_F(st[2], st[3], st[4], st[5])
  if (st[3] == 1)
    names(res) <- "Wilcoxon-Test"
  else
    names(res) <- "Kruskal-Wallis-Test"
  
  res
}

WilkoxTest2 <- function(x, data) {
  suppressWarnings(res <-
                     stats::wilcox.test(x, data, alternative =  "two.sided"))
  
  res <- stp25rndr::rndr_U(res$statistic, res$p.value)
  names(res) <- "Wilcoxon-Test"
  res
}

KruskalTest2 <- function(x, data) {
  res <- stats::kruskal.test(x, data)
  res <-
    stp25rndr::rndr_H(res$statistic, res$parameter, res$p.value)
  names(res) <- "Kruskal-Wallis-Test"
  res
}

Aov2 <- function(x, data) {
  res <- stats::aov(x, data)
  res <- car::Anova(res, type = 3)
  res <-
    stp25rndr::rndr_F(res[2, 3], res[2, 2], res[3, 2], res[2, 4])
  names(res) <- "ANOVA"
  res
}

TTest2 <- function(x, data) {
  res <- stats::t.test(x, data, alternative =  "two.sided")
  res <-
    stp25rndr::rndr_T(res$statistic, res$parameter, res$p.value)
  names(res) <- "T-Test"
  res
}

chisqTest2 <- function(x, data) {
  # print(stats::xtabs(x, data, drop.unused.levels = TRUE))
  res <-
    suppressWarnings(stats::chisq.test(stats::xtabs(x, data, drop.unused.levels = TRUE),
                                       correct = FALSE))
  if (!grepl("Pearson", res$method))
    return("Error wrong formula!")
  res <-
    stp25rndr::rndr_X(res$statistic, res$parameter, NULL, res$p.value)
  names(res) <- "Pearson Chi-squared"
  res
}

# das geht nicht wegen lmertest
# gml_binomial <- function(x, data) {
#   xt <- as.data.frame(stats::xtabs(x, data))
#   fm <- formula(paste("Freq ~ ", paste(all.vars(x), collapse = "*")))
#   APA(glm(fm, xt, family = poisson()))
#   
# }

gml_binomial <- function(x, data) {
  fm <- as.formula(paste(all.vars(x), collapse = "~"))
 # fm0 <- as.formula(paste(all.vars(x)[1],  "~ 1"))
  fit_1 <- glm(fm, data, family = binomial())
 # fit0 <- glm(fm0, data, family = binomial())
  
  # # https: /  / api.rpubs.com / tomanderson_34 / lrt
  # 
  # A <- logLik(fit1)
  # M0 <- logLik(fit0)
  # teststat <- -2 * (as.numeric(M0) - as.numeric(A))
  # 
  # df <- attr(A, "df") - 1
  # p <- pchisq(teststat, df = df, lower.tail = FALSE)
  # rslt <- paste(
  #   "log Lik ",
  #   paste(format(as.numeric(A), digits = 2), collapse = ", "),
  #   " (df=",
  #   format(df),
  #   "), ",
  #   stp25rndr::rndr_P(p),
  #   sep = ""
  # )
  
  rslt <- APA(fit_1)
  
  names(rslt) <- "LRT-Test"
  rslt
}


# counts <- c(18,17,15,20,10,20,25,13,12)
# outcome <- gl(3,1,9)
# treatment <- gl(3,3)
# dat<-data.frame(treatment, outcome, counts) # showing data
# dat$counts <- as.numeric(dat$counts <15)
# dat
# 
# glm.D93 <- glm(counts ~ 1, dat, family = binomial())
# APA(glm.D93)
# lmtest::lrtest(glm.D93)
# 
# 
# glm.D93 <- glm(counts ~ treatment, dat, family = binomial())
# APA(glm.D93)
# lmtest::lrtest(glm.D93)
# gml_binomial(~ counts + treatment, dat)
# 
# pchisq(-2 * ( (-5.7286) - (-3.8191)), 2, lower.tail = FALSE)


fisherTest2 <- function(x, data) {
  xt <- stats::xtabs(x, data)
  if (all(dim(xt) == c(2, 2))) {
    res <- stats::fisher.test(xt)
    res <- stp25rndr::rndr_fischer(res$estimate, res$p.value)
    names(res) <- "Fisher Exact Test"
    res
    
  } else
    "wrong dim for fisher-test"
}




