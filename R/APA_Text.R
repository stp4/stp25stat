#' Freie Texte Ausgeben
#'
#' Funktion feur offene Texte diese werden roh als Tabelle ausgeben.
#'
#' @param ... an prepare_data2()
#' @param na.string  missing
#' @return invisible(data.frame) mit pos und answer
#' @export
APA_Text <- function(...,
                     caption = NULL,
                     note = "",
                     na.string = NULL,
                     output = which_output()) {
  X <- stp25formula::prepare_data2(...)
  
  res <- NULL
  for (i in seq_len(length(X$measure.vars))) {
    x <- X$data[[X$measure.vars[i]]]
    pos <- which(!is.na(x))
    sep <- if (grepl("html", output))
      "<br>"
    else
      NULL
    
    ans <- wrap_sentence(gsub("\r?\n|\r", " ", trimws(x[pos])),
                         100, sep = sep)
    
    
    
    if (!is.null(na.string)) {
      na <- which(ans  %in%  na.string)
      print(na)
      if (length(na) > 0) {
        pos <- pos[-na]
        ans <- ans[-na]
      }
    }
    
    
    if (!is.null(X$group.vars)) {
      gr <- X$data[pos, X$group.vars]
      gr <- apply(gr, 1, paste, collapse = ", ")
      r <- data.frame(Pos = pos,
                      Group = gr,
                      Answer = ans)
      r <- r[order(r$Group),]
    } else
      r <- data.frame(Pos = pos,
                      Answer = ans)
    
    
    
    if (is.null(caption))
      caption <- X$row_name[i]
    
    
    Output(r, caption , note, output = output)
    res <-   rbind(res, r)
  }
  invisible(res)
}



