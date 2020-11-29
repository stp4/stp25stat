#' combine_data_frame
#'
#' @param ... data.frame
#' @param item default = 1 kann auch NULL sein
#' @param prefix names
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#'
#' m <- data.frame(
#'   Item = 1:3,
#'   a = (1:3),
#'   b = (1:3) * 2,
#'   c = (1:3) * 3
#' )
#' sd <- data.frame(
#'   Item = (1:3),
#'   a = (1:3) * 4,
#'   b = (1:3) * 5,
#'   c = (1:3) * 6
#' )
#' combine_data_frame(m, sd)
#' combine_data_frame(m, sd, item = NULL)
#' 
#' 
combine_data_frame <- function(..., item = 1, prefix = NULL) {
  tmp <- list(...)
  lng <-  lengths(tmp)
  if (length(unique(lng)) != 1L)
    stop("unequal input")
  if (!is.null(item)) {
    if (length(unique(sapply(tmp, "[", item))) != 1L)
      stop("unequal Items")
    measure <- seq_len(lng[1])[-item]
    rslt <- tmp[[1]][item]
  } else{
    measure <- seq_len(lng[1])
    rslt <- data.frame(row.names = seq_len(nrow(tmp[[1]])))
  }
  if (is.null(prefix))
    prefix <- as.character(substitute(list(...)))[-1]
  
  for (i in measure) {
    rslt <- cbind(
      rslt,
      as.data.frame(
        sapply(tmp, "[", i),
        col.names = paste(names(tmp[[1]])[i], prefix, sep =
                            "_"),
        fix.empty.names = FALSE,
        stringsAsFactors = FALSE
      )
    )
  }
  rslt
}

 
