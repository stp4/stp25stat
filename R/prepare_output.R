#' prepare_output
#'
#' Ergebnis fuer Output vorbereiten
#'
#' @param x data.frame
#' @param caption Uberschrift
#' @param note Anmerkung
#' @param N Dezimalstellen bei zB Mean2
#' @param labels Label
#' @param ... Weitere Argumente
#' @return tibble mit attributen
#' @export
prepare_output<- function(x, #Objekt Liste oder df
                          caption=NULL,
                          note=NULL,
                          N=NULL,
                          labels=NA,
                          ...

){
  
  # if (is.data.frame(x) & (!tibble::is_tibble(x))) {
  #   x<- tibble::as_tibble()
  # }
  #  
  # 
  # 
 # if (!tibble::is_tibble(x)) {
  #  x<- tibble::as_tibble()
   #}
  
 if(is.null(caption)) caption<- ""
 if(is.null(note)) note<- ""
 if(is.null(N)) N <- model_info(x)[["N"]]

  attr(x, "caption") =  caption
  attr(x, "note") = note
  attr(x, "N") = N
  attr(x, "labels") = labels

  x
}



