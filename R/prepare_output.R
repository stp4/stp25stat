#' prepare_output
#'
#' Ergebnis fuer Output vorbereiten
#'
#' @param x data.frame Objekt Liste oder df
#' @param caption Uberschrift
#' @param note Anmerkung
#' @param N Dezimalstellen bei zB Mean2
#' @param labels Label
#' @param rgroup,n.rgroup an htmlTable {htmlTable}
#' @param ... Weitere Argumente
#' @return tibble mit attributen
#' @export
prepare_output <- function(x,
                           
                           caption = NULL,
                           note = NULL,
                           N = NULL,
                           labels = NA,
                           include.n =  get_my_options()$caption,
                           
                           rgroup = NULL,
                           n.rgroup = NULL,
                           ...) {
  if (is.null(N))
    N <- model_info(x)[["N"]]
  if (is.null(note))
    note <- ""
  if (is.null(caption))
    caption <- ""
  
  if (!is.null(include.n))
    caption <- paste0(caption, " (N=", N, ")")
  
  attr(x, "caption") =  caption
  attr(x, "note") = note
  attr(x, "N") = N
  attr(x, "labels") = labels
  attr(x, "rgroup") = rgroup
  attr(x, "n.rgroup") = n.rgroup
  
  x
}
