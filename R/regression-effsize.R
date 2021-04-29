#' Effsize
#'
#'
#' Measures of association
#' Pearson's r correlation Small 0.2,  Medium 0.5, Large 0.8
#' r2 coefficient of determination Small 0.04, Medium 0.25, Large 0.64
#' Quelle: http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3444174/pdf/i1949-8357-4-3-279.pdf
#'
#' Gestolen von https://cran.r-project.org/web/packages/effsize/effsize.pdf
#' @name Effsize
#' @param x Objekt oder Formel
#' @param ... weitere Optionen
#'
APA_Effsize<- function(x, ...){
  if(inherits(x, "formula") | is.numeric(x)) cohens.d(x, ...)
  else etaSquared2(x, ...)
}

 
#' @rdname Effsize
#' @description Eta-Quadrat Kopie von lsr::etaSquared.
#' Die Ergebnisse entsprechen denen von SPSS (Univariat-Partial Eta Squared)
#' @param type etaSquared2: Anova Type default ist 2
#' @param anova  etaSquared2: Ausgabe der ANOVA Tabelle
#' @return Vector
#' @export
#' @examples
#'
#' # etaSquared
#'
#' fit1<-lm(y1~x1, anscombe)
#' #etaSquared2(aov (y1~x1, anscombe), anova=TRUE)
#' #etaSquared2(fit1, anova=TRUE)
#' etaSquared2(fit1 )
#'
etaSquared2 <-
  function (x, type = 2, anova = FALSE, ...)
    ##   <environment: namespace:lsr
  {
    if (!is(anova, "logical") | length(anova) != 1) {
      stop("\"anova\" must be a single logical value")
    }
    if (!is(x, "lm")) {
      if (is(x, "anova")) {
        #etaSquared2(fit1)
        eta <- effectsize::eta_squared(x)
        eta_p <-  effectsize::eta_squared(x, partial = TRUE)

      #  if (nrow(eta) == 1) {
          res <- rbind(cbind(eta[, 2],
                             eta_p[, 2]), NA)
          rownames(res)  <- c(unlist(eta[, 1]), "Residuals")
       # } else{
        #  res <- rbind(cbind(eta[-nrow(eta), 2],
       #                      eta_p[-nrow(eta), 2]), NA)
        #  rownames(res)  <- c(unlist(eta[-1, 1]), "Residuals")
        #}
        colnames(res) <-  c("eta.sq", "eta.sq.part")


        return(res)

      } else {
        stop("\"x\" must be a linear model object")
      }
    }
    if (!is(type, "numeric") | length(type) != 1) {
      stop("type must be equal to 1,2 or 3")
    }
    if (type == 1) {
      ss <- anova(x)[, "Sum Sq", drop = FALSE]
      ss.res <- ss[dim(ss)[1],]
      ss.tot <- sum(ss)
      ss <- ss[-dim(ss)[1], , drop = FALSE]
      ss <- as.matrix(ss)
    }
    else {
      if (type == 2) {
        ss.tot <- sum((x$model[, 1] - mean(x$model[, 1])) ^ 2)
        ss.res <- sum((x$residuals) ^ 2)
        terms <- attr(x$terms, "factors")[-1, , drop = FALSE]
        l <- attr(x$terms, "term.labels")
        ss <- matrix(NA, length(l), 1)
        rownames(ss) <- l
        for (i in seq_along(ss)) {
          vars.this.term <- which(terms[, i] != 0)
          dependent.terms <- which(apply(terms[vars.this.term,
                                               , drop = FALSE], 2, prod) > 0)
          m0 <- lm(x$terms[-dependent.terms], x$model)
          if (length(dependent.terms) > 1) {
            m1 <- lm(x$terms[-setdiff(dependent.terms,
                                      i)], x$model)
            ss[i] <- anova(m0, m1)$`Sum of Sq`[2]
          }
          else {
            ss[i] <- anova(m0, x)$`Sum of Sq`[2]
          }
        }
      }
      else {
        if (type == 3) {
          mod <- drop1(x, scope = x$terms)
          ss <- mod[-1, "Sum of Sq", drop = FALSE]
          ss.res <- mod[1, "RSS"]
          ss.tot <- sum((x$model[, 1] - mean(x$model[,
                                                     1])) ^ 2)
          ss <- as.matrix(ss)
        }
        else {
          stop("type must be equal to 1, 2 or 3")
        }
      }
    }
    if (anova == FALSE) {
      ss <- rbind(ss, ss.res)
      eta2 <- ss / ss.tot
      eta2p <- ss / (ss + ss.res)
      k <- length(ss)
      E <- cbind(eta2, eta2p)
      E[k, 2] <- NA
      colnames(E) <- c("eta.sq", "eta.sq.part")
      rownames(E) <- rownames(ss)
      rownames(E)[k] <- "Residuals"

      # eta2 <- ss/ss.tot
      # eta2p <- ss/(ss + ss.res)
      # E <- cbind(eta2, eta2p)
      # rownames(E) <- rownames(ss)
      # colnames(E) <- c("eta.sq", "eta.sq.part")
    }
    else {
      ss <- rbind(ss, ss.res)
      eta2 <- ss / ss.tot
      eta2p <- ss / (ss + ss.res)
      k <- length(ss)
      # eta2p[k] <- NA
      df <- anova(x)[, "Df"]
      ms <- ss / df
      Fval <- ms / ms[k]
      p <- 1 - pf(Fval, df, rep.int(df[k], k))
      E <- cbind(ss, df, ms, Fval, eta2, eta2p, p)
      E[k, c(4, 6, 7)] <- NA
      colnames(E) <- c("SS", "df",
                       "MS", "F", "eta.sq", "eta.sq.part", "p")

      rownames(E) <- rownames(ss)
      rownames(E)[k] <- "Residuals"
    }
    return(E)
  }


#' @rdname Effsize
#' @description   Cohen's d and Hedges g effect size
#' Between groups
#' Cohen's d Small 0.2, Medium 0.5, Large 0.8, Very large 1.3
#' Odds ratio (OR) Small 1.5, Medium 2, Large 3
#' Relative risk or risk ratio (RR) R Small 2, Medium 3, Large 4
#' Cohen, J. (1988).  Statistical power analysis for the behavioral sciences (2nd ed.).  New York:Academic Press.
#' @param x,y formel oder x, y
#' @param ...
#'
#' @return vector
#' @export
#'
#' @examples
#'
#' set.seed(45)                        ## be reproducible
#'  x <- rnorm(10, 10, 1)
#'  y <- rnorm(10, 5, 5)
#'
#' cohens.d(x, y)
#' varanax<-Melt2(m1+m2~nr,varana , key="time", value="m")
#' cohens.d(m~time, varanax )
#'
cohens.d <- function(x, ...) {
  UseMethod("cohens.d")
}

#' @rdname Effsize
cohens.d.default <- function(x, y, ...) {
  lx <- length(x) - 1
  ly <- length(y) - 1
  md  <-
    abs(mean(x) - mean(y))        ## mean difference (numerator)
  csd <- lx * var(x) + ly * var(y)
  csd <- csd / (lx + ly)
  csd <- sqrt(csd)                     ## common sd computation

  c(cohens.d = md / csd)                        ## cohen's d
}

#' @rdname Effsize
cohens.d.formula = function(x, data = list(), ...) {
  #mf <- model.frame(formula=x, data=data)
  mf <- aggregate(x, data, function(x) {
    x <- na.omit(x)
    c(l = length(x) - 1,
      m = mean(x),
      var = var(x))
  })[[2]]

  #return(mf[,"l"])
  md  <-
    abs(mf[1, "m"] - mf[2, "m"])        ## mean difference (numerator)
  csd <- mf[1, "l"] * mf[1, "var"] + mf[2, "l"] * mf[2, "var"]
  csd <- csd / (mf[1, "l"] + mf[2, "l"])
  csd <- sqrt(csd)                     ## common sd computation

  c(cohens.d = md / csd)                        ## cohen's d

}



