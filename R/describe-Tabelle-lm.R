# Mittelwerte -------------------------------------------------------------

#' @rdname Tabelle
#' @export
Tabelle.lmerModLmerTest <- function(x,caption=NULL, note="",
                                    digits = 2,
                                    fun = function(x) {
                                      c(
                                        n = length(x),
                                        M = mean(x, na.rm = TRUE),
                                        SD = sd(x, na.rm = TRUE)
                                      )
                                    },
                                    ...) {
  Tabelle.lm(x, caption, note, digits, fun)
}



#' @rdname Tabelle
#' @export
Tabelle.glm <- function(x, caption=NULL, note="",
                        digits = 2,
                        fun = function(x) {
                          f__n = length(x)
                          
                        },
                        ...)
{
  if (x$family[1] == "binomial")
    fun <- function(x) {
      c(f__n = length(x),
        f__Anteil = paste(table(x), collapse = "/"))
    }
  else
    fun <- function(x) {
      x <-  as.numeric(x)
      c(f__n = length(x),
        f__M = mean(x, na.rm = TRUE))
    }
  Tabelle.lm(x, caption, note, digits, fun)
}



#' @rdname Tabelle
#' @export
Tabelle.lm <- function(x, caption=NULL, note="",
                       digits = 2,
                       fun = function(x) {
                         c(
                           f__n = length(x),
                           f__M = mean(x, na.rm = TRUE),
                           f__SD = sd(x, na.rm = TRUE)
                         )
                       },
                       ...) {
  res_list <- NULL
  myeff <- effects::allEffects(x, ...)
  
  for (i in names(myeff)) {
    info <- model_info(myeff[[i]])
    ans <- aggregate_effect(myeff[[i]], info$y, info$x, fun)
    
    AV <- ifelse(is.na(info$labels[info$y]),
                 info$y, info$labels[info$y])
    
    ans <-
      data.frame(plyr::llply(ans, function(x)
        if (is.numeric(x))
          round(x, digits)
        else
          x),
        stringsAsFactors = FALSE)
    
    names(ans)<- gsub("\\.f__", "_", names(ans))
   
    if(is.null(caption))   caption <- paste0("AV: ", AV)
    
    res_list[[i]] <- prepare_output(ans,
                                    caption, note,
                                    info$N,  
                                    info$labels)
  }
  res_list
}


aggregate_effect <- function(eff,
                             y,
                             x,
                             fun = function(x)
                               length(x)) {
  fm <- formula(paste(y, "~", paste(x, collapse = "+")))
  df <- eff$data
  #-- Faktoren fuer N berechnung vorbereiten
  for (j in names(eff$variables)) {
    if (!eff$variables[[j]]$is.factor)
      df[, j] <- cut(df[, j], length(eff$variables[[j]]$levels))
  }
  
  res <- try(aggregate(fm, df, fun, drop = FALSE))
  if (class(res) == "try-error")
    data.frame(NULL)  #  wegen ncol im weiteren progammverlauf
  else
    do.call(data.frame, res)
}