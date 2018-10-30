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

  # Default-Tests ------------------------------------
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

# Begin Funktion ---------------------------

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


#' @rdname Tabelle
#' @description conTest Hilfsfunktion fuer Tabellen
catTest = function(fml, data, include.test="chisq.test") {
  #Fehlende Factoren eliminieren , drop.unused.levels = TRUE
  res <- stats::chisq.test(
    stats::xtabs(fml, data, drop.unused.levels = TRUE),
    correct = FALSE)
stp25rndr::rndr_X(res$statistic,res$parameter, NULL,res$p.value)
}




ordTest = function(group, x) {

  f <- rms::lrm(x ~ group)$stats
  list(P = f["P"], stat = f["Model L.R."], df = f["d.f."],
       testname = "Proportional odds likelihood ratio",
       statname = "Chi-square", plotmathstat = "chi[df]^2")
}
