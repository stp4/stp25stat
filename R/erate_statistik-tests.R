#' @rdname Tabelle
#' @description Hilfsfunctionen
#' Correlation in Dokument APA2_Correlation.R
#'
#' conTest Hilfsfunktion fuer Tabellen
#'
conTest = function(fml,
                   data,
                   test_name = TRUE) {
   #  cat("\nin conTest\n\n")
    g <- all.vars(fml)
    data<- na.omit(data[g])
    g <- g[length(g)]
    g <- table(data[[g]])
    
    if (all(g > 4)) {
      if (is.logical(test_name)) {
        spearmanTest2(fml, data)
      } else{
        if (test_name == "SPSS") {
          if (res[1] == "Wilcoxon")
            WilkoxTest2(fml, data)
          else
            KruskalTest2(fml, data)
        }
        else if (test_name == "wilcox.test")
          WilkoxTest2(fml, data)
        else if (test_name == "u.test")
          WilkoxTest2(fml, data)
        else if (test_name == "h.test")
          KruskalTest2(fml, data)
        else if (test_name == "kruskal.test")
          KruskalTest2(fml, data)
        else if (test_name == "t.test")
          TTest2(fml, data)
        else if (test_name == "aov")
          Aov2(fml, data)
        else if (test_name == "anova")
          Aov2(fml, data)
        else if (test_name == "Hmisc")
          spearmanTest2(fml, data)
        else
          test_name
        
      }
    }
    else
      paste0("sample to small (", paste(g, collapse = "/"), ")")
    
  }
  
 
  spearmanTest2 <- function(fml, data) {
    st <- Hmisc::spearman2(fml, data)
    if (is.na(st[3]))
      return("Error")
    stp25rndr::rndr_F(st[2], st[3], st[4], st[5])
  }
  # Wilkox
  WilkoxTest2 <- function(fml, data) {
    wlx <- stats::wilcox.test(fml, data, alternative =  "two.sided")
    stp25rndr::rndr_U(wlx$statistic, wlx$p.value)
  }
  KruskalTest2 <- function(fml, data) {
    wlx <- stats::kruskal.test(fml, data)
    stp25rndr::rndr_H(wlx$statistic, wlx$parameter, wlx$p.value)
    
  }
  # ANOVA
  Aov2 <- function(fml, data) {
    res <- stats::aov(fml, data)
    res <- car::Anova(res, type = 3)
    stp25rndr::rndr_F(res[2, 3], res[2, 2], res[3, 2], res[2, 4])
    
  }
  # t-Test
  TTest2 <- function(fml, data) {
    res <- stats::t.test(fml, data, alternative =  "two.sided")
    stp25rndr::rndr_T(res$statistic, res$parameter, res$p.value)
  }


  # Chi-Test
  chisqTest2 <- function(fml, data) {
    res <- suppressWarnings(
      stats::chisq.test(
        stats::xtabs(fml, data, drop.unused.levels = TRUE),
        correct = FALSE))
    stp25rndr::rndr_X(res$statistic, res$parameter, NULL, res$p.value)
  }
  # Chi-Test
  chisqTest2 <- function(fml, data) {
    res <- suppressWarnings(
      stats::chisq.test(
        stats::xtabs(fml, data, drop.unused.levels = TRUE),
        correct = FALSE))
    stp25rndr::rndr_X(res$statistic, res$parameter, NULL, res$p.value)
  }
  
  
 # fisher.test()
  fisherTest2 <- function(fml, data) {
    xt <- stats::xtabs(fml, data)
    if (all(dim(xt) == c(2, 2))) {
      res <- stats::fisher.test(xt)
      stp25rndr::rndr_fischer(res$estimate, res$p.value)
    } else "wrong dim for fisher-test"
  }
  
  
#' @rdname Tabelle
#' @description conTest Hilfsfunktion fuer Tabellen
  catTest = function(fml,
                     data,
                     include.test = "chisq.test") {
    #Fehlende Factoren eliminieren , drop.unused.levels = TRUE
    #   res <- stats::chisq.test(
    #     stats::xtabs(fml, data, drop.unused.levels = TRUE),
    #     correct = FALSE)
    # stp25rndr::rndr_X(res$statistic,res$parameter, NULL,res$p.value)
  #  cat("\nin catTest\n\n")
   # print(fml)
    g <- all.vars(fml)
    data <- na.omit(data[g])
    g <- g[length(g)]
    g <- table(data[[g]])
    if (all(g > 4)) {
      if (include.test == "chisq.test")
        chisqTest2(fml, data)
      else if (include.test == "fisher.test")
        fisherTest2(fml, data)
      else{
        #' Hier kann auch ein conTest aufgerufen werden
        if (include.test %in% c(
          "Wilcoxon", "wilcox.test", "u.test", "h.test",
          "kruskal.test", "t.test", "aov", "anova", "Hmisc"
        )) {
          g <-  all.vars(fml)
          fml <- formula(paste(g[1], "~", g[2]))
          data[[g[1]]] <- as.numeric(data[[g[1]]])
          conTest(fml, data, include.test)
        } else{
          include.test
        }
      }
    }
    else
      paste0("sample to small (", paste(g, collapse = "/"), ")")
  }




ordTest = function(group, x) {

  f <- rms::lrm(x ~ group)$stats
  list(P = f["P"], stat = f["Model L.R."], df = f["d.f."],
       testname = "Proportional odds likelihood ratio",
       statname = "Chi-square", plotmathstat = "chi[df]^2")
}



# #  cat("\nin conTest\n\n")
# 
 
#   spearmanTest2 <- function(fml, data) {
#     st <- Hmisc::spearman2(fml, data)
#     if (is.na(st[3]))
#       return("Error")
#     stp25rndr::rndr_F(st[2], st[3], st[4], st[5])
#   }
#   # Wilkox
#   WilkoxTest2 <- function(fml, data) {
#     wlx <- stats::wilcox.test(fml, data, alternative =  "two.sided")
#     stp25rndr::rndr_U(wlx$statistic, wlx$p.value)
#   }
#   KruskalTest2 <- function(fml, data) {
#     wlx <- stats::kruskal.test(fml, data)
#     stp25rndr::rndr_H(wlx$statistic, wlx$parameter, wlx$p.value)
# 
#   }
#   # ANOVA
#   Aov2 <- function(fml, data) {
#     res <- stats::aov(fml, data)
#     res <- car::Anova(res, type = 3)
#     stp25rndr::rndr_F(res[2, 3], res[2, 2], res[3, 2], res[2, 4])
# 
#   }
#   # t-Test
#   TTest2 <- function(fml, data) {
#     res <- stats::t.test(fml, data, alternative =  "two.sided")
# 
#     stp25rndr::rndr_T(res$statistic, res$parameter, res$p.value)
#   }
# 
 
# 
#   if (is.logical(test_name)) {
#     spearmanTest2(fml, data)
#   } else{
#     if (test_name == "SPSS") {
#       if (res[1] == "Wilcoxon")
#         WilkoxTest2(fml, data)
#       else
#         KruskalTest2(fml, data)
#     }
#     else if (test_name == "wilcox.test")
#       WilkoxTest2(fml, data)
#     else if (test_name == "u.test")
#       WilkoxTest2(fml, data)
#     else if (test_name == "h.test")
#       KruskalTest2(fml, data)
#     else if (test_name == "kruskal.test")
#       KruskalTest2(fml, data)
#     else if (test_name == "t.test")
#       TTest2(fml, data)
#     else if (test_name == "aov")
#       Aov2(fml, data)
#     else if (test_name == "anova")
#       Aov2(fml, data)
#     else if (test_name == "Hmisc")
#       spearmanTest2(fml, data)
#     else
#       test_name
# 
#   }