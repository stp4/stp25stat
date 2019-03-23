#' is_  Irgenwas
#' 
#' Prueft ob objekt bestimmte Eigenschaften aufweist.
#' Fuer Dataframe gibt es \code{is_all_identical2()}
#' @param x zu pruefendes Objekt
#' @return Die \code{is_all_} gibt generel einen Wert zurueck die \code{is_} einen Vector in gleicher Laenge wie der Input-Vector.
#' @examples
#'  isFALSE(TRUE)
#'   x<-c(F, T, F, F)
#'   is_false2(x)
#'   is_all_logical(x)
#'   is_all_0_1(x)
#'   is_all_identical2(data.frame(y=1:3, x=factor(c("a", "b", "c"))))
is_irgendwas<- function(x) !is.null(x)

#' @rdname is_irgendwas
#' @export
#' @description  is_formula2 Prueft ob es eine Formel ist
#' @examples
#' is_formula2(a~b+v)
is_formula2<- function (x)
  inherits(x, "formula")

#' @rdname is_irgendwas
#' @description is_empty2 wird in prepare_data genutzt als test ob ein Elemen  leer ist
#' @export
#' @examples
#'  is_empty2(c("freq", "mean"))
#'  is_empty2("freq")
is_empty2 <- function (x) {
  # print(x)
  if (length(x) == 0)
    TRUE
  else if (length(x) == 1) {
    if (is.null(x))
      TRUE
    else if (is.na(x))
      TRUE
    else if (x == "")
      TRUE
    else FALSE
  }
  else
    FALSE
}


#' @rdname is_irgendwas
#' @export
is_all_dichotom<- function(x){
  if(is_all_logical(x) | is_all_0_1(x)) TRUE
  else{
    if (ncol(x) < 2) { nlevels(x)==2 }
    else{ all(sapply(x, nlevels)==2) }}
}


#' @rdname is_irgendwas
#' @description is_all_logical is_all_0_1 prufen beide Logical aber is_all_dichotom  kann auch ja/nein
#' @export
is_all_logical <- function(x){
  if (length(x)<=0) FALSE  #-- fuer Melt2
  else if(is.null(x)) FALSE
  else all(sapply(x, is.logical))
}


#' @rdname is_irgendwas
#' @export
is_all_0_1 <- function(x)  {
  is_0_1<- function(z){
    z <- factor(z)
    if (nlevels(z) > 2)
      FALSE
    else if (nlevels(z) == 2 & all(levels(z) == 0:1))
      TRUE
    else if (nlevels(z) == 1 & levels(z)[1] == 0)
      TRUE
    else if (nlevels(z) == 1 & levels(z)[1] == 1)
      TRUE
    else
      FALSE
  }
  if (length(x)<=0) FALSE  #-- fuer Melt2
  else if(is.null(x)) FALSE
  else if(is.data.frame(x)) all(sapply(x, is_0_1 ))
  else if(is.vector(x))  is_0_1(x)
  else     FALSE # class(x)
}


#' @rdname is_irgendwas
#' @description isFALSE analog wie if(x){...} es gibt aber noch base::isFALSE welches leere Werte ignoriert 
isFALSE <- function(x){identical(FALSE, x )}


#' @rdname is_irgendwas
#' @description is_false2 arbeitet mit isFALSE geht aber auch fuer Matris oder Data.frames
#' @export
is_false2<- function(x) sapply(x, identical(FALSE, x ))



#' @rdname is_irgendwas
#' @description is_all_identical2 oder all_identical2 wird in Recast2 verwendet
#' @export
all_identical2 <- function(x) {
  if (ncol(x) < 2) {
    TRUE
  }
  else{
    xs <-
      sapply(x, function(xx)
        if (is.numeric(xx))
          "numeric"
        else if (is.factor(xx))
          "factor"
        else
          NA)
    if (length(xs) <= 1)
      return(TRUE)
    for (i in seq(2, length(xs))) {
      if (!identical(xs[[1]], xs[[i]]))
        return(FALSE)
    }
    TRUE
  }
}


#' @rdname is_irgendwas
#' @export
is_all_identical2 <- function(x) all_identical2(x)




#' @rdname is_irgendwas
#' @param data Daten wenn Formeln gepruft werden
#' @description is_vars_in_data Prueft ob ded data.frame auch die Fariablen enthaelt.
#' @export
is_vars_in_data<- function(x, data=NULL){
  
  if(length(data)==0) return(FALSE)
  if(is_formula(x))  {
    x<- all.vars(x)
    if( any(x==".") ) x <- x[ -which(x==".") ]
  }
  
  if(length(x)>0) return(all(is.element(x, names(data))))
  else return(TRUE)
}