#' MetComp:  Uebereinstimmung und Praezision von Messwerten
#'
#' Tukey Mean Difference oder auch Bland Altman Methode. Oft interessiert die Zuverlaessigkeit und Reproduzierbarkeit ein einer Diagnose. Die Beurteilung kann dabei durch einen Bewerter (Messverfahren) in wiederholter Form erfolgen und wird dann als Intra-Rater bezeichnet oder die Beurteilung eines Merkmals erfolgt durch mehrere Bewerter (Messverfahren). und hier spricht man von Inter-Rater.
#' Die Methode der Beurteilung der uebereinstimmung haengt von den jeweiligen Datentype ab.
#' Bei Nominalen wird abgezaehlt und die Rate der uebereinstimmung bewertet (Cohen-Koeffizient) Bei Ordinalen-Daten werden die gewichteten uebereinstimmungen ausgezaehlt (gewichteter Cohen-Koeffizient). Bei metrischen(stetigen) Daten werden die Differenzen beurteilt (Bland-Altman-Methode).
#'
#' Bland-Altman-Methode Bias (d) systematische Abweichung Messfehler (s) Standardabweichung der Differenz Limits of agreement (LOA) Intervall von 95 (entspricht d+-1.96 -> es wird eine Normalverteilung unterstellt).
#' Methoden Die generische Funktion MetComp() kann sowohl Kappa als auch Tukey-means berechnen. Kappa kann aber auch ueber die xtab() und APA2 berechnet werden. Wobei hier nur 2x2-Tabellen untersucht werden und bei Kappa() sind hingegen auch mehrere ordinale Kategorien erlaubt sind.
#' aehnliche Methode ist ICC die aber eher zur Reliabilitaetsanalyse gehoert.
#' @param data Daten
#' @param x Formula Objekt
#' @return Ein bland_altman-Objekt mit den Daten (data) und der Statistik (stat).
#' @export
#' @examples
#'
#' #require(stp25stat)
#' #require(stp25plot)
#' #require(stp25output)
#' ### Verschiedene Situationen Im folgenden habe ich eine fiktive Messung mit simulierten Daten
#'
#' set.seed(0815)
#'
#' n <- 100
#' DF <- data.frame(
#'   A = rnorm(n, 100, 50),
#'   B = rnorm(n, 100, 50),
#'   C = NA,
#'   D = NA,
#'   E = NA,
#'   F = NA,
#'   group = sample(gl(2, n / 2, labels = c("Control", "Treat")))
#' )
#'
#' cutA <- mean(DF$A)
#' DF <- transform(
#'   DF,
#'   C = round(A + rnorm(n,-5, 20)),
#'   D = round(A + rnorm(n, 0, 10) + A / 10),
#'   E = A + ifelse(A < cutA, A / 5,-A / 5) + rnorm(n, 0, 10),
#'   F = A +  rnorm(n, 50, 10)
#' )
#'
#'
#'
#' #### Methoden messen das Selbe
#'
#' x<- MetComp(~A+C, DF)
#' #plot(x)
#' tab<- x$stat[,1:2]
#' names(tab)   <- c("Parameter", "Methoden messen das Selbe_M=0" )
#'
#' x<- MetComp(~A+F, DF)
#' tab<- cbind(tab, "Methoden messen das Selbe_Fehler M=50"= x$stat$Unit)
#' #plot(x)
#'
#'
#'
#' #### Methoden messen unterschiedlich Werte
#' x<- MetComp(~A+B, DF)
#' tab<- cbind(tab, "Methoden unterschiedliche_Fehler M=0"= x$stat$Unit)
#' #plot(x)
#'
#'
#'
#'
#' Output(tab, caption="BA" )
#'
#' t1 <-
#'   APA2(with(DF, t.test( A, C,  paired = TRUE)), output=FALSE)
#' t2 <-
#'   APA2(with(DF, t.test( A, F,  paired = TRUE)), output=FALSE)
#' t3 <-
#'   APA2(with(DF, t.test( A, B,  paired = TRUE)), output=FALSE)
#'
#'
#' #Output(rbind( t1, t2, t3), caption="T-Test")
#'
#'
#' #### Methoden haben systematische Abweichungen
#'
#' x<- MetComp(~A+D, DF)
#' #plot(x)
#'
#'
#'
#' x<- MetComp(~A+E, DF)
#' #plot(x)
#'



MetComp <- function(...,
                    include.ci = TRUE,
                    ci.level = .95,
                    caption = NULL,
                    note = "",
                    digits = 2,
                    output = stp25output::which_output()) {
  X <- stp25formula::prepare_data2(...)
  res <- NULL
  
  
  
  if (all(X$measure.class == "numeric") |
      all(X$measure.class == "integer")) {
    res <-
      MetComp_BAP(
        X = X,
        include.ci = include.ci,
        ci.level = ci.level,
        digits = digits
      )
    stp25output::Output(res$stat,
                        caption = caption,
                        note = note,
                        output = output)
  }
  else if (all(X$measure.class == "factor")) {
    if (is.null(caption))
      caption = "Cohen's Kappa-Koeffizient"
    
    
    xtb <- xtabs(X$formula, X$data[X$measure.vars])
    res <-
      MetComp_Kappa(xtb, include.ci = include.ci, ci.level = ci.level)
    
    stp25output::Output(res,
                        caption = caption,
                        note = note,
                        output = output)
  }
  else{
    print(X$measure.class)
    
    stop("Unbekannte measure.variablen!")
  }
  
  
  invisible(res)
}


#' @rdname MetComp
#' @export
#' @examples
#'
#'   ## ----sachs-627-data ---------------------------------------
#' Botulinum <- data.frame(
#'   A= factor(c(rep(1, 14), rep(1, 3),
#'               rep(0, 5),rep(0, 18)),
#'             1:0, c("+", "-")),
#'   B= factor(c(rep(1, 14), rep(0, 3),
#'               rep(1, 5),rep(0, 18)),
#'             1:0, c("+", "-")))
#'
#'   MetComp(~A+B, Botulinum)
#'
#' #require(vcd)
#'
#' vcd::Kappa(xtabs(~A+B, Botulinum))
#' #data("SexualFun")
#' #MetComp_Kappa(SexualFun)
#'
#'
#'
#'
MetComp_Kappa <- function(x,
                          include.ci = TRUE,
                          ci.level = .95) {
  x_kapa <- vcd::Kappa(x)
  
  tab <-
    rbind(Unweighted = x_kapa$Unweighted,
          Weighted = x_kapa$Weighted)
  
  z <- tab[, 1] / tab[, 2]
  p <-  2 * pnorm(-abs(z))
  if (include.ci) {
    q <- qnorm((1 + ci.level) / 2)
    lower <- tab[, 1] - q * tab[, 2]
    upper <- tab[, 1] + q * tab[, 2]
    ci <- cbind(lower, upper)
  }
  
  res <- data.frame(
    Source = c("Unweighted", "Weighted"),
    Kapa = stp25rndr::Format2(tab[, 1]),
    CI = stp25rndr::rndr_CI(ci),
    ASE = stp25rndr::Format2(tab[, 2]),
    
    "z-Test" = stp25rndr::rndr_Test_Statistic(z),
    p.value = stp25rndr::rndr_P(p, FALSE),
    stringsAsFactors = FALSE
  )
  if (all(dim(x) == c(2, 2)))
    res[1, ]
  else
    res
}







#' @rdname MetComp
#' @param ... an Formula_Data
#' @param X  Aufbereitete Daten aus prepare_data2
#'
#' @return list(stats, name, name.dif. met_A, met_B, groups)
#' @export
#'
#' @examples
#'
#'
#' #- Understanding Bland Altman analysis
#' #Davide Giavarina
#' #Biochemia medica 2015;25(2) 141-51
#' #http://dx.doi.org/10.11613/BM.2015.015
#'
#' set.seed(0815)
#' DF<- data.frame(
#'   A=c(1, 5,10,20,50,40,50,60,70,80, 90,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900, 950,1000),
#'   B=c(8,16,30,14,39,54,40,68,72,62,122, 80,181,259,275,380,320,434,479,587,626,648,738,766,793,851,871,957,1001, 980),
#'   group= sample(gl(2, 15, labels = c("Control", "Treat")))
#' )
#'
#' MetComp(~A+B, DF, caption = "Giavarina")
#'
#'
MetComp_BAP <-
  function(...,
           X = NULL,
           include.ci = TRUE,
           ci.level = .95,
           digits = 2) {
    if (is.null(X))
      X <- prepare_data2(...)
    
    ba.stats <- bland.altman.stats(
                    X$data[X$measure.vars],
                    include.ci = include.ci,
                    ci.level = ci.level,
                    digits = digits
                  )
    ba.stats$name <-  paste(X$yname, collapse = ", ")
    ba.stats$name.diff <-  paste(X$yname[1:2], collapse = " - ")
    ba.stats$met_A <- X$measure.vars[1]
    ba.stats$met_B <- X$measure.vars[2]
    ba.stats$groups <-  X$X_data
    
    
    ba.stats
  }


#' @rdname APA
#' @export
APA.bland_altman <-
  function(x, ...) {
    paste0("m = ",
           x$stat$Unit[2],
           ", d = [",
           x$stat$Unit[5],
           ", ",
           x$stat$Unit[5],
           "]")
  }


#' @rdname APA2
#' @export
APA2.bland_altman <- function(x,
                              caption = paste0("Difference (", x$name.diff,
                                               "), Mean (",  x$name, ")"),
                              note = "",
                              ...) {
  res <-  prepare_output(x$stat, caption = caption)
  Output(res)
  invisible(res)
}


#' @rdname APA2
#' @export
print.bland_altman <- function(x) {
  print(x$stat)
}



#-- Helper Bland Altman
bland.altman.stats <- function (dfr,
                                two = 1.96,
                                include.ci = TRUE,
                                ci.level = .95,
                                digits = 2) {
  # called.with <- nrow(dfr)
  dfr <- na.omit(dfr)
  based.on <- nrow(dfr)
  if (based.on < 2)
    warning("Warning in bland.altman.stats:less than 2 data pairs after deleting NAs.",
            call. = FALSE)
  if (ncol(dfr) > 2)
    warning("Warning in bland.altman.stats:Mehr als 2 Methoden.",
            call. = FALSE)
  diffs <- dfr[[1]] - dfr[[2]]
  means <-  rowMeans(dfr)
  diffs.percent <- diffs / means * 100
  diffs.percent[is.infinite(diffs.percent)] <- 0
  
  critical.diff <- two * sd(diffs)
  mean.diffs <- mean(diffs)
  sd.diffs <- sd(diffs)
  lower.limit <- mean.diffs - critical.diff
  upper.limit <- mean.diffs + critical.diff
  lines <- c(
    lower.limit = lower.limit,
    mean.diffs = mean.diffs,
    upper.limit = upper.limit
  )
  t1 <- qt((1 - ci.level) / 2, df = based.on - 1)
  t2 <- qt((ci.level + 1) / 2, df = based.on - 1)
  
  se.ci <- sqrt(sd(diffs) ^ 2 * 3 / based.on)
  se.mean <- sd(diffs) / sqrt(based.on)
  CI.lines <- c(
    lower.limit.ci.lower = lower.limit + t1 * se.ci,
    lower.limit.ci.upper = lower.limit + t2 * se.ci,
    mean.diff.ci.lower = mean.diffs + t1 * se.mean,
    mean.diff.ci.upper = mean.diffs + t2 * se.mean,
    upper.limit.ci.lower = upper.limit +  t1 * se.ci,
    upper.limit.ci.upper = upper.limit +  t2 * se.ci
  )
  #--- Prozent
  
  
  mean.percent <- mean(diffs.percent)
  ssd.percent <- sd(diffs.percent)
  critical.diff.percent <- two * ssd.percent
  se.ci.percent <- sqrt(ssd.percent ^ 2 * 3 / based.on)
  se.mean.percent <- ssd.percent / sqrt(based.on)
  lower.limit.percent = mean.percent - critical.diff.percent
  upper.limit.percent = ssd.percent + critical.diff.percent
  
  CI.lines.percent <-
    c(
      lower.limit.ci.lower = lower.limit.percent + t1 * se.ci.percent,
      lower.limit.ci.upper = lower.limit.percent + t2 * se.ci.percent,
      mean.diff.ci.lower = mean.percent + t1 * se.mean.percent,
      mean.diff.ci.upper = mean.percent + t2 * se.mean.percent,
      upper.limit.ci.lower = upper.limit.percent +  t1 * se.ci.percent,
      upper.limit.ci.upper = upper.limit.percent +  t2 * se.ci.percent
    )
  
  ci_format <-   stp25rndr::rndr_CI(cbind(
    low = c(CI.lines[3], CI.lines[1], CI.lines[5]),
    up = c(CI.lines[4], CI.lines[2], CI.lines[6])
  ) ,
  digits = digits)
  
  
  
  stat <- data.frame(
    Parameter = c(
      "df (n-1)",
      "difference mean (d)",
      "standard deviation (s)",
      "critical.diff (1.96s)",
      "d-1.96s",
      "d+1.96s"
    ),
    Unit = c(
      stp25rndr::Format2(based.on - 1, 0),
      stp25rndr::Format2(
        c(
          mean.diffs,
          sd.diffs,
          critical.diff,
          lower.limit,
          upper.limit
        ),
        digits
      )
    ),
    CI = c(NA, ci_format[1], NA, NA, ci_format[2], ci_format[3]) ,
    
    
    SE = stp25rndr::Format2(c(NA, se.mean, NA, NA, se.ci, se.ci), digits),
    Percent = c("",
                stp25rndr::rndr_percent(
                  c(
                    mean.percent,
                    ssd.percent,
                    critical.diff.percent,
                    lower.limit.percent,
                    upper.limit.percent
                  )
                  ,
                  digits = 1
                ))
    
    ,
    stringsAsFactors = FALSE
  )
  if (!include.ci) {
    stat <- stat[,-3]
  }
  
  res <- list(
    lines = lines,
    # wie oben ll mean ul
    CI.lines = CI.lines,
    lines.percent = c(
      mean.percent - critical.diff.percent,
      mean.percent,
      mean.percent + critical.diff.percent
    ),
    CI.lines.percent = CI.lines.percent,
    
    stat = stat,
    data = cbind(dfr,
                 means,
                 diffs,
                 diffs.percent = diffs.percent)
    
  )
  class(res) <- c("bland_altman")
  
  res
}
