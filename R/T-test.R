#' @rdname APA
#' @description APA.htest: T-Test
#' @export
#' @examples
#'  # T-Test
#' require(coin)
#' APA(coin::wilcox_test(mpg ~ factor(vs), mtcars))
#' APA(wilcox.test(mpg ~ vs, mtcars))
#' APA(t.test(mpg ~ vs, mtcars))
APA.htest <- function(x,  ...) {
  if (names(x$statistic) == "t") {
    rndr_T(x$statistic,
           x$parameter,
           x$p.value)
  }
  else if (names(x$statistic) == "BP"){
    rndr_BP(x$statistic,
            x$parameter,
            x$p.value)}
  else if (names(x$statistic) == "DW"){
    rndr_DW(x$statistic,
            x$parameter,
            x$p.value)}
  else{
    rndr_W(x$statistic,
           x$p.value)}
}


#' @rdname APA
#' @export
APA.ScalarIndependenceTest <- function(x, ...) {
  ## coin::wilcox_test
  capture.output(x)[5]
}




#' @rdname APA_
#' @description \strong{APA_Ttest}:
#'  Berechnung von Mittelwerten und Mittelwertdifferenzen und des T-Test.
#'
#' @param var.equal,paired,alternative an t.Test var.equal = FALSE  "two.sided"
#' @param random  formula bei paired muss zwingend eine eindeutige id oder Fallnummer vorhanden sein
#' @param include.mean Mittelwerte mit SD
#' @param include.d Cohens d
#' @param include.mean.diff  Mittlere Differenzen
#' @param include.se Standardfehler noch nicht Fertig
#' @export
#'
#' @examples
#'
#'  
#'
#'  
#'
#' APA_Ttest(m1+m2 ~ geschl, varana)
#' varanax<-Melt2(m1+m2~nr,varana , key="time", value="m")
#'
#' # broom::tidy(with(varana, t.test(m1,m2 ) ))
#' # broom::tidy(  t.test(m~time, varanax, var.equal=FALSE)) )
#'
#' APA_Ttest(m~time, varanax, paired = TRUE)
#'
#'

#'
#' APA_Ttest(m~time, varanax, var.equal=TRUE)
#' APA_Ttest(m~time, varanax, var.equal=FALSE)
#' #broom::tidy(t.test(m~time, varanax, var.equal=TRUE))
#' #broom::tidy(t.test(m~time, varanax, var.equal=FALSE))
#'
#'  varanax$m[1] <- NA
#' APA_Ttest(m ~ time, varanax, paired = TRUE, random=~nr, include.n=TRUE) 
#' 
#' 
APA_Ttest  <- function(x,
                       data,
                       caption = "T Test",
                       note = alternative,
                       output = stp25output::which_output(),
                       var.equal = FALSE,
                       paired = FALSE,
                       alternative =  "two.sided",
                       include.mean = TRUE,
                       include.d = TRUE,
                       include.mean.diff = TRUE,
                       include.se = FALSE,
                       include.n = FALSE,
                       type = "t.test",
                       digits = 2,
                       random = NULL,
                       ...) {
  lhs <- all.vars(x[-3])
  rhs <- all.vars(x[-2])
  id <- all.vars(random)
  result <- list()
  method <- alternative # Fehler abfangen
  for (r in rhs) {
    ANS <- NULL
    if (nlevels(data[[r]]) == 2)
      # T.test nur 2-Faktorstufen
    {
      for (l in lhs) {
        fm <- formula(paste(l, "~", r))
        
        if (paired) {
          if (is.null(random))
            stop(
              "Bei paired =TRUE muss zwingend eine eindeutige id mit random=~id uebergeben werden!"
            )
          
          dat <- data[c(id, l, r)]
          if (!all(complete.cases(dat))) {
            dat <-
              na.omit(Wide(dat, key = r, value = l))
            dat <-  Long(formula(paste(
              "~", paste(names(dat)[-1], collapse = "+")
            )),
            dat,
            key = r,
            value = l)
          }
          # print(dat)
          #  return(NULL)
        }
        else {
          dat <- data[c(l, r)]
        }
        
        res <- t.test(
          fm,
          dat,
          var.equal = var.equal,
          paired = paired,
          alternative = alternative
        )
        
        # print(aggregate(fm, data, function(x) x))
        res_t <- broom::tidy(res)
        method <- as.character(res_t$method)
        res_sig  <- APA(res)
        res_mean <-
          Tabelle(fm, dat, APA = TRUE, include.n = include.n)[[1]]
        
        ans <- res_mean[1]
        
        if (include.mean) {
          if (!include.n)
            vr <- 2:3
          else if (paired)
            vr <- c(2:3, 5)
          else
            vr <- 2:5
          ans <- cbind(ans, res_mean[vr])
        }
        if (include.mean.diff) {
          res_diff <- paste(
            Format2(res_t$estimate, digits = digits),
            rndr_CI(as.matrix(res_t[c("conf.low", "conf.high")]), digits = digits)
          )
          
          
          ans <- cbind(ans, Differenz = res_diff)
        }
        if (include.d) {
          ans <- cbind(ans, cohens.d = Format2(cohens.d(fm, dat), 2))
        }
        if (include.se) {
          ans <- cbind(ans, SE = Format2(calculate_se(fm, dat), 2))
        }
        
        
        if (type == "t.test")
          ans <- cbind(ans, T.test = res_sig)
        else {
          res <- wilcox.test(fm,
                             dat,
                             paired = paired,
                             alternative = alternative)
          ans <- cbind(ans, wilcox.test = APA(res))
          method <- as.character(res$method)
        }
        if (is.null(ANS))
          ANS <- ans
        else
          ANS <- rbind(ANS, ans)
      }
      
      
    }
    else{
      Text(r, "hat ", nlevels(data[[r]])," levels und kann daher nicht mittels T-Test berechnet werden!")
    }
    
    result[[l]] <- prepare_output(ANS,
                                  caption = caption,
                                  note = note)
  }
  
  Output(result,  
         output = output)
  
  invisible(result)
}
# Standard error
### Calculate standard error manually
calculate_se <- function(x, data) {
  x <- as.numeric(data[[all.vars(x[[2L]])]])
  sd(x, na.rm = TRUE) /
    sqrt(length(x[!is.na(x)]))
}




#' @rdname APA2
#' @export
APA2.htest <- function(x, caption = "", ...) {
  # t.test
  if (names(x$statistic) == "t")
    Output(
      fix_data_frame2(
        Source = x$data.name,
        T = x$statistic,
        df = x$parameter,
        p.value = x$p.value
      ),
      caption = paste(x$method, caption),
      ...
    )
  else
    Output(
      fix_data_frame2(
        Source = x$data.name,
        W = x$statistic,
        p.value = x$p.value
      ),
      caption = paste(x$method, caption)
      ,
      ...
    )
}


#' @rdname APA2
#' @export
APA2.pairwise.htest <-
  function(x, caption = "", ...) {
    #pairwise.t.test
    #-- ?pairwise.wilcox.test
    Output(
      data.frame(
        Source = row.names(x$p.value),
        Format2(
          as.data.frame(x$p.value),
          digits = 3 ,
          lead.zero = FALSE
        )
      ),
      caption = paste(x$data.name, x$method, "(p-Value)", caption),
      ...
    )
  }



