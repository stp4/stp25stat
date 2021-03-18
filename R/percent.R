#' Prozent
#'
#' Berechnung der Prozent
#'
#' @param x vector
#' @param digits nachkommastellen digits = 0,
#' @param n Anzahl
#' @param exclude,useNA an table
#' @param max_factor_length lange Eintraege kuerzen
#' @param return_data_frame,is_true_false  intern
#'
#' @return
#' @export
#'
#' @examples
#'
#'  x<- gl(2, 8, labels = c("Control", "Treat"))
#' x[1]<- NA
#' stp25stat:::Prozent2default(x)
#' Prozent(x )
#' Prozent(x, useNA ="always")
#'
#' stp25stat:::Multi2default(x)
#' stp25stat:::Prozent_multi(x)
#'
#'
#' x<- as.numeric(x)
#' stp25stat:::Prozent2default(x)
#' Prozent(x)
#'
#' x<- ifelse(x==1, TRUE, FALSE)
#'
#' stp25stat:::Prozent2default(x)
#' Prozent(x)
#' Prozent(x, useNA ="always")
#'
#'
#' # in Tabelle() verwendet
#'
#' set_my_options(prozent = list(digits = 1, style = 2, null_percent_sign= "."))
#' Prozent2default(factor(c(1,2,3,3,3,5), 1:5))
#' x <- c(1, 1, 1, 0, 0, 0, 1, 1)
#'  set_my_options(prozent=list(include_name=FALSE))
#'  stp25stat:::Multi2default(x)
#'  stp25stat:::Prozent2default(x)
#'
#' Prozent2default(treatment, style=1)
#' Prozent2default(treatment, style = "ci")
#' 
#' Prozent2default(health, style=1)
#' Prozent2default(health, style = "ci")
#' Prozent2default(health, style = "%ci")
#' Prozent2default(health, style = "n%ci")
#' Prozent2default(health, style = "%nci")
#' 
#' #' Test ob die Confidence Intervals vertretbare Ergebnisse liefern.
#' #' #'  
#' #' require(effects)
#' #' stp25stat::Tbll(allEffects(glm( out=="1" ~ treatment , family = binomial )
#' #'   ) )
#' #' rbind(Prozent2default(out[treatment=="Control"], style = "%ci")[1,],
#' #' Prozent2default(out[treatment=="Treat"], style = "%ci")[1,])
#' 
#' 
Prozent <- function (x,
                     digits = 0,
                     exclude = if (useNA == "no")
                       c(NA, NaN),
                     useNA = "no",
                     max_factor_length = 25,...) {
  Prozent2default(
    x,
    digits = digits,
    exclude = exclude,
    useNA = useNA,
    max_factor_length = max_factor_length,
    return_data_frame = FALSE
  )
  
}

#' @rdname Prozent
#' @param n intern
#' @param exclude geh noch nicht
#' @param max_factor_length lange Einträge kuerzen
Prozent2default <-
  function(x,
           digits = 0,
           n = length(x),
           exclude = if (useNA == "no") c(NA, NaN),
           max_factor_length = 25,
           useNA = "no",
           return_data_frame = TRUE,
           is_true_false = FALSE,
           style = get_my_options()$apa.style$prozent$style,
           ...
           ) 
{
    if (is.factor(x)) {
      tbl <- table(x, exclude = exclude, useNA = useNA)
      if (length(tbl) > max_factor_length) {
        naLev <- levels(x)[-(1:max_factor_length)]
        Text("NA = ", paste(naLev, collapse = ", "))
        x <-  factor(x, levels(x)[1:max_factor_length], exclude = NULL)
        x <- addNA(x)  #- addNA modifies a factor by turning NA into an extra level
        tbl <- table(x)
        
      }
    } else if (is.logical(x)) {
      x <- factor(x, c(TRUE, FALSE), c("true", "false"))
      is_true_false <- TRUE
      tbl <- table(x, exclude = exclude, useNA = useNA)
    } else {
      xt <- factor(x)
      
      if (nlevels(xt) > max_factor_length)
        stop("class = ", class(xt), " nlevels = ", nlevels(xt))
      else
        tbl <- table(xt, exclude = exclude, useNA = useNA)
    }
    
    
    if (n == 0) {
      rslt <- ""
      tbl <- rep(NA, nlevels(x))
      names(tbl) <- levels(x)
    } else {
     
      
      if (!is.null(style)) {
        #print(style)
         if ( any(grepl("ci", tolower(style)))) {
          if (nlevels(x) > 2)
            rslt <-  rndr2_percent(as.vector(prop.table(tbl)) * 100, 
                        as.vector(tbl), 
                        ci=ci_factor(tbl = tbl),
                        digits=digits, style=style)
          else
            rslt <- rndr2_percent(as.vector(prop.table(tbl)) * 100, 
                        as.vector(tbl), 
                        ci=ci_binom(tbl = tbl),
                        digits=digits, style=style)
        } else{
          rslt <-
            rndr2_percent(as.vector(prop.table(tbl)) * 100, 
                          as.vector(tbl),  
                          digits=digits, style=style)
          
        }
      }else{
        
        rslt <-
          rndr2_percent(as.vector(prop.table(tbl)) * 100, 
                        as.vector(tbl),  
                        digits=digits, style=style)
      }
      
      
    }

    if (return_data_frame) {
      rslt <-
        data.frame(
          lev = names(tbl),
          n = c(n, rep("", length(tbl) - 1)),
          m = as.vector(rslt),
          stringsAsFactors = FALSE
        )
      rslt <- if (!is_true_false)  rslt else rslt[1, ]
    } else{
      names(rslt) <- names(tbl)
      rslt <- if (!is_true_false) rslt else rslt[1]
    }
    
    rslt
  }

 



#' @rdname Prozent
#' @export
Prozent_multi <- function(x,
                          digits = 0,
                          use.level = 1) {
  if (is.logical(x)) {
    res <- Prozent(x, digits)
  } else if (is.factor(x)) {
    res <-
      Prozent(ifelse(x == levels(x)[use.level], TRUE, FALSE), digits)
    
  } else if (is.numeric(x)) {
    res <- Prozent(ifelse(x ==  use.level, TRUE, FALSE), digits)
  }
  else
    (stop(class(x)))
  
  res
}

#' @rdname Prozent
#' @param use.level welcher level wir gezaelt
#' @param include.level mit yes in labels?
Multi2default <- function(x,
                          digits =0,
                          n = length(x),
                          use.level = 1,
                          include.level = get_my_options()$apa.style$prozent$include_name) {
  if (is.null(include.level))
    include.level <- TRUE
  
  
  
  if (is.logical(x)) {
    res <- Prozent2default(x, digits=digits, n=n)
  } else if (is.factor(x)) {
    res <-
      Prozent2default(ifelse(x == levels(x)[use.level], TRUE, FALSE), digits=digits, n=n)
    res$lev <- levels(x)[use.level]
  } else if (is.numeric(x)) {
    res <-
      Prozent2default(ifelse(x ==  use.level, TRUE, FALSE), digits=digits, n=n)
    res$lev <- use.level
  }
  else
    (stop(class(x)))
  
  if (!include.level)
    res$lev <- ""
  
  
  res
}


#' Workaraund für  die Confidence Intervals 
rndr2_percent <- function(x = n / sum(n, na.rm = TRUE) * 100,
                          n = NULL,
                          ci = "NULL",
                          digits = 0 ,
                          style = "1") {
  if (style %in% c("1", "2", "3", "5"))
  {
    rslt <-
            stp25rndr::rndr_percent(x, n, digits = digits, style = style)
  }
  else {
    style <- tolower(style)
    if (nchar(style) == 2) {
      if (any(grepl("%n", style)))
        rslt <-
          stp25rndr::rndr_percent(x, n, digits = digits, style = 1)
      else if (any(grepl("n%", style)))
        rslt <-
          stp25rndr::rndr_percent(x, n, digits = digits, style = 2)
      else if (any(grepl("ci", style)))
        rslt <-   ci
    }
    else if (nchar(style) == 1) {
      if (any(grepl("%", style)))
        rslt <-
          stp25rndr::rndr_percent(x, n, digits = digits, style = 3)
      else if (any(grepl("n", style)))
        rslt <-
          stp25rndr::rndr_percent(x, n, digits = digits, style = 4)
    }
    else{
      if (any(grepl("n%ci", style)))
        rslt <-  paste(stp25rndr::rndr_percent(x, n, digits = digits, style = 2),
                       ci)
      else if (any(grepl("%nci", style)))
        rslt <-  paste(stp25rndr::rndr_percent(x, n, digits = digits, style = 1),
                       ci)
      else if (any(grepl("%ci", style)))
        rslt <-  paste(stp25rndr::rndr_percent(x, n, digits = digits, style = 3),
                       ci)
      else if (any(grepl("nci", style)))
        rslt <-  paste(stp25rndr::rndr_percent(x, n, digits = digits, style = 4),
                       ci)
      else
        relt <- paste("error in style" , style)
    }
  }
  
  rslt
}


#' für Tabelle
rndr_prct_ci <-
  function( 
    low,
    upr,
    digits = 0,
    prc = "%",
    sep = ", ",
    sep_1 = "[",
    sep_2 = "]") {
  
   if (which_output() == "latex")
   prc <- "prc"
    
    paste0(
      sep_1,
      stp25rndr::Format2(low, digits=digits),
      prc,
      sep,
      stp25rndr::Format2(upr, digits=digits),
      prc,
      sep_2
    )
  }

#' muss nach stp25rndr verschoben werden
#' wird in Surv verwendet
#'
rndr_percent_CI <-
  function(x,
           low,
           upr,
           digits = default_stp25("digits", "prozent"),
           prc = "% ",
           sep = ", ",
           sep_1 = "[",
           sep_2 = "]") {
    
    if(which_output()=="latex") prc<- "prc"
    
    paste0(
      stp25rndr::Format2(x, digits),
      prc,
      sep_1,
      stp25rndr::Format2(low, digits),prc,
      sep,
      stp25rndr::Format2(upr, digits),prc,
      sep_2
    )
  }



#' Confidence Intervals for Binomial Proportions
#'
#' The Wilson interval, which is the default, was introduced by Wilson (1927)
#' and is the inversion of the CLT approximation to the family of equal tail tests of p = p0. The Wilson interval is
#' recommended by Agresti and Coull (1998) as well as by Brown et al (2001).
#'
ci_binom <- function(x,
                     tbl = table(x),
                     conf.level = .95,
                     sides = "two.sided",
                     method = "wilson") {
 
   rslt <- DescTools::BinomCI(
      tbl,
      n = sum(tbl),
      conf.level = conf.level,
      sides = sides,
      method = method
    )
 
 r<- rndr_prct_ci(rslt[,2]*100, rslt[,3]*100)
 
 ifelse(rslt[,1] <= 0, ".", r )
}

#' Confidence Intervals for Multinomial Proportions
#'
#' Sison, C.P and Glaz, J. (1995) Simultaneous confidence intervals
#' and sample size determination for multinomial proportions.
#' Journal of the American Statistical Association, 90:366-369.
#'
ci_factor <- function(x,
                      tbl = table(x),
                      conf.level = .95,
                      sides = "two.sided",
                      method = "sisonglaz") {
 
 rslt <-   DescTools::MultinomCI(tbl,
                          conf.level = conf.level,
                          sides = sides,
                          method = method)
 
 r<- rndr_prct_ci(rslt[,2]*100, rslt[,3]*100)
  
  ifelse(rslt[,1] <= 0, ".", r )
}







grade_level <-
  function (n,
            x = c("K", "1", "2", "3"),
            prob = NULL)
  {
    out <- sample(
      x = x,
      size = n,
      replace = TRUE,
      prob = prob
    )
    factor(out, levels = x)
    
  }
set.seed(1)
n <- 550
out = grade_level(n,  c("1", "0"), prob = c(3 / 7, 
                                            4 / 7))
treatment = grade_level(n,  c("Control", "Treat"), prob = c(1 / 3, 2 / 3))
health = grade_level(n, x = c("poor", "fair", "good"), prob = c(2 / 3, 1 / 6, 1 / 6))




