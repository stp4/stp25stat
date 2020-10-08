#' @description  Tbll_desc_long
#' 
#' @param include.range lang oder kurz
#' @param measure.name "m"
#' @rdname Tbll_desc
#' @export
#'
#' @examples
#' 
#' # require(stpvers)
#' #mtcars %>% Tbll_desc(mpg, cyl,  disp,  hp, drat,
#' #                    wt,  qsec, vs, am, gear, carb)
#' mtcars %>% Tbll_desc_long(
#'   mpg[mean, 1],
#'   cyl[median],
#'   "Hallo",
#'   disp[0],
#'   hp[0],
#'   drat,
#'   wt,
#'   qsec[1],
#'   vs[freq],
#'   am[freq],
#'   gear,
#'   carb,
#'   include.range = TRUE,
#'   include.n = TRUE
#' )
#' 
#' mtcars$G <- factor(mtcars$vs, 0:1, c("A", "B"))
#' 
#' mtcars <- Label(
#'   mtcars,
#'   mpg	= "Miles/(US) gallon",
#'   cyl	= "Number of cylinders",
#'   disp =	"Displacement (cu.in.)",
#'   hp	= "Gross horsepower",
#'   drat =	"Rear axle ratio",
#'   wt	= "Weight (1000 lbs)",
#'   qsec =	"1/4 mile time",
#'   vs	= "Engine (0 = V-shaped, 1 = straight)",
#'   am	= "Transmission (0 = automatic, 1 = manual)",
#'   gear =	"Number of forward gears",
#'   carb =	"Number of carburetors"
#' )
#' 
#' mtcars %>% Tbll_desc_long(
#'   mpg[mean, 1],
#'   cyl[median],
#'   "Hallo",
#'   disp[0],
#'   hp[0],
#'   drat,
#'   wt,
#'   qsec[1],
#'   vs[freq],
#'   am[freq],
#'   gear,
#'   carb,
#'   by =  ~ G,
#'   include.range = TRUE,
#'   include.n = TRUE,
#'   include.label=FALSE
#' )
Tbll_desc_long <- function(...,
                           caption="",
                           include.range = TRUE,
                           include.n = TRUE,
                           include.test = FALSE,
                           include.label = TRUE,
                           digits = NULL,
                           measure.name = "m"
                          ) {
  X <- stp25formula::prepare_data2(...)
  type <- if (include.range) "auto_long" else "auto_kurz"
  
  rslt <-
    .desc_long(X,
               type = type,
               measure.name = measure.name,
               digits = digits, include.label=include.label)
  
  if (include.n) {
    X$measure <- ifelse(X$measure == "header", "header", "custom_fun")
    rslt_n <- .desc_long(X,
                         type = "custom_fun",
                         fun = length2,
                         measure.name = "n")
    rslt <-  if (ncol(rslt) == 2)
      cbind(rslt[1], rslt_n[2], rslt[-1])
    else
      cbind(rslt[1:2], rslt_n[3], rslt[3])
  }
  names(rslt)[1] <- "Item"
  
  prepare_output(rslt, caption = caption, note="", N=X$N)
}




.desc_long <- function(X,
                       measure.name = "m",
                       fun = NULL,
                       digits = NULL,
                       type =  "auto_kurz",
                       include.label=FALSE)
{
  rslt <- NULL
  
  for (i in seq_len(length(X$measure))) {
    if (X$measure[i] == "factor") {
      if (!is.factor(X$data[[X$measure.vars[i]]])) {
        X$data[[X$measure.vars[i]]] <- factor(X$data[[X$measure.vars[i]]])
        # warning("Konvertiere die Variable ", X$measure.vars[i], " zu Factor!")
      }
      X$row_name[i] <- paste0(X$row_name[i], " (",
                              paste0(levels(X$data[[X$measure.vars[i]]]),
                                     collapse = "/"), ")")
    }
    else if (X$measure[i] %in%  c("mean", "numeric")) {
      X$row_name[i] <- paste0(X$row_name[i], " (mean)")
    }
    else if (X$measure[i] == "median") {
      X$row_name[i] <- paste0(X$row_name[i], " (median)")
    }
    else if (X$measure[i] == "logical") {
      X$data[[X$measure.vars[i]]] <-
        factor(X$data[[X$measure.vars[i]]], c(TRUE, FALSE))
      X$measure[i] <- "factor"
      X$row_name[i] <- paste0(X$row_name[i], " (",
                              paste0(levels(X$data[[X$measure.vars[i]]]),
                                     collapse = "/"), ")")
    }
    
    rslt[[X$measure.vars[i]]] <-
      berechne_all(
        X$data,
        X$measure.vars[i],
        X$by,
        X$measure[i],
        type,
        fun = fun,
        digits = if (is.null(digits)) X$digits[i] else digits,
        measure.name = measure.name
      )
  }
  
  rslt <- plyr::ldply(rslt)
  if (include.label)
    rslt[[1]] <-
    as.character(factor(rslt[[1]], names(X$row_name), X$row_name))
  rslt
  
}




