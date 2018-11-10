#' Rangreihe
#'
#' Rangordnungen von Objekten koennen durch eine Transformation der Rangreihen in
#' Intervallskalierte Merkmale ueberfuehrt werden. Die Grundidee dieser Methode geht
#' auf Thurstone (1927) nach dem "Law of Categorical Judgement" zurueck.
#'
#' Dabei werden
#' die kumulierten Haeufigkeiten in Normalverteilte z-Werte uebergefuehrt und aus diesen
#' die Intervallskalierten Markmalsauspraegungen gebildet.
#'
#' Literatur: Bortz, J. & Doering, N. (2006). Forschungsmethoden und Evaluation fuer Human-und Sozialwissenschaftler (4. Auflage). Berlin: Springer. Seite 155
#'
#' @name Rangreihe
#' @param ... Weitere Argumente
#' @return Vector
#' @export
#' @examples
#'
#' #require(HH)
#' #require(stpvers)
#'
#' n <- 2 * 3 * 4 * 1
#'
#' set.seed(n)
#' kaffee <- c(
#'   "Cubanischer Arabica Filter",
#'   "Cubanischer Arabica Kaltextrakt",
#'   "Dallmayr Prodomo Kaltextrakt",
#'   "Dallmayr Prodomo Filter"
#' )
#'
#'
#'
#' DF <-
#'   data.frame(
#'     Geschlecht = gl(2, n / 2, labels = c("Maennlich", "Weiblich")),
#'     Alter = gl(4, n / 4,   labels = c("20-29", "30-39", "40-49", "50-59")),
#'     Konsum = gl(3, n / 3,   labels = c("weniger als 3 T.", "3 bis 6 T.", "mehr als 6 T.")),
#'     Kaffeeform = gl(3, n / 3, labels = c(
#'       "Espresso", "Filterkaffee", "Milchkaffee"
#'     )),
#'     FavA = gl(4, n / 4,  labels = kaffee)[sample.int(n)],
#'     FavB = gl(4, n / 4,  labels = kaffee)[sample.int(n)],
#'     FavC = gl(4, n / 4,  labels = kaffee)[sample.int(n)],
#'     FavD = gl(4, n / 4,  labels = kaffee)[sample.int(n)]
#'   )
#'
#'
#' #some(DF)
#' # Beispieldaten.Borz <-
#' #   matrix(c(
#' #      2,8,10,13,17,
#' #      5,10,15,18,2,
#' #      10,12,20,5,3,
#' #       15,20,10,3,2,
#' #       22,18,7,2,1)
#' #            , nrow = 5, ncol=5, byrow=TRUE,
#' #            dimnames = list(c("A", "B", "C", "D", "E"),1:5))
#' ans <- Rangreihe(~ FavA + FavB + FavC + FavD, DF)
#' APA2(ans, caption = "Alle")
#'
#' ans <-
#'   Rangreihe(FavA + FavB + FavC + FavD ~ Geschlecht + Kaffeeform, DF)
#' APA2(ans, caption = "Alle")
#'
#'
#' #-- DF1 und DF2 sind identisc
#' DF1 <-  data.frame(
#'   A = c(1, 1, 1, 2, 3, 1),
#'   B = c(2, 2, 2, 3, 2, 3),
#'   C = c(3, 3, 3, 1, 1, NA),
#'   D = c(NA, NA, NA, NA, NA, 2)
#' )
#' DF2 <-   data.frame(
#'   R1 = factor(Hmisc::Cs(A, A, A, C, C, A)),
#'   R2 = factor(Hmisc::Cs(B, B, B, A, B, D)),
#'   R3 = factor(Hmisc::Cs(C, C, C, B, A, B))
#' )
#' Rangreihe(DF1)
#' Rangreihe(~ R1 + R2 + R3, DF2)
#' #windows(6,3)
#' #dotplot( reorder(Items, Skalenwert)~ Skalenwert|"Kaffeeform", ans$result, groups=Kaffeeform , xlab="",
#' #        xlim=range( ans$result$Skalenwert)*1.10 , auto.key=list(), cex=1)
#' #         SaveData("Kaffeeform")
#'
Rangreihe <-
  function(...) {
    UseMethod("Rangreihe")
}


#' @rdname Rangreihe
#' @param x Objekt Vector oder auch Formel
#' @param caption,note,output an Output
#'
#' @export
APA2.rangreihe <- function(x,
                           caption = "",
                           note = "",
                           output=which_output(),
                           ...) {
  x$results$mittlerer.Rang <- Format2(x$results$mittlerer.Rang, digits = 2)
  x$results$Skalenwert <- Format2(x$results$Skalenwert, digits = 2)

  Output( prepare_output(x$results,
                 caption = paste0(caption, " (N = ", x$N , ")"),
                 note=note,
                 N=x$N),
         output=output,
         ...)
}

#' @rdname Rangreihe
#' @param data Objekt
#' @param order,decreasing sortieren
#' @param digits Nachkommastellen
#' @param exclude,subset,na.action an Formula_Data
#' @export
Rangreihe.formula <- function(x,
                              data = NULL,
                              order = TRUE,
                              digits = 2,  # options()$stp4$apa.style$prozent$digits,
                              decreasing = TRUE,
                              #transpose = FALSE,
                              exclude = NA,
                              subset,
                              na.action = na.pass,
                              ...) {
  X <- Formula_Data(x, data, subset, na.action)
  if (is.null(X$xname)) {
    Rangreihe.default(
      X$Y_data ,
      grouping = NULL,
      order = order,
      digits = digits,
      decreasing = decreasing,
      ....
    )
  } else{
    Rangreihe.default(
      X$Y_data,
      X$X_data,
      order = order,
      digits = digits,
      decreasing = decreasing,
      ....
    )
  }
}


#' @rdname Rangreihe
#' @param items,grouping intern Rangreihe.default
#' @param labels intern Rangreihe.default
#' @param RankByRow Methode wie aufdroeseln
#' @param N Rangreihe.default
#'
#' @export
#'
Rangreihe.default <- function (items,
                               grouping = NULL,
                               order = TRUE,
                               # digits = options()$stp4$apa.style$prozent$digits,
                               decreasing = TRUE,
                               #transpose = FALSE,
                               labels = NULL,
                               RankByRow = if (is.factor(items[, 1]) | is.character(items[, 1])) FALSE else TRUE,
                               N = if (is.null(grouping))nrow(items) else nrow(na.omit(grouping)),
                              # info = FALSE,
                               ...)
{
  if (stpvers::is_all_identical2(items))
    warning("Das Skalenniveau in der Rangreihe ist unterschiedlich. Moeglicherweise stimmen die Ergebnisse nicht!")
  # if (info)
  #   Text("Rangordnungen von Objekten koennen durch eine Transformation der Rangreihen in Intervallskalierte Merkmale ueberfuehrt werden. Die Grundidee dieser Methode geht  auf Thurstone (1927) nach dem Law of Categorical Judgement zurueck. Dabei werden die kumulierten Haeufigkeiten in Normalverteilte z-Werte uebergefuehrt und aus diesen die Intervallskalierten Markmalsauspraegungen gebildet. Literatur: Bortz, J. & Doering, N. (2006). Forschungsmethoden und Evaluation fuer Human-und Sozialwissenschaftler (4. Auflage). Berlin: Springer. Seite 155")
  #

  mylabels <- GetLabelOrName(items)
  my_ranks <- unique(
                unlist(lapply(items,
                              function(x)  levels(factor(x)))
                       )
                )

  My_table <- function(items, my_ranks) {
    sapply(items, function(x, ...) {
      table(factor(x, ...))
    },
    levels = my_ranks, simplify = TRUE)
  }


  Calc_Rank <- function(items) {

    if (nrow(items) < 1) {
      return(NULL)
    }
    my_table <- My_table(items, my_ranks)
    if (RankByRow)
      my_table <- t(my_table)
    rel_feq <- prop.table(my_table, 1)
    if (ncol(my_table) < nrow(my_table)) {
      if (RankByRow)
        my_ranks <- c(my_ranks, "n.a.")
      else
        items$n.a. <- NA

      my_table <- My_table(items, my_ranks)
      if (RankByRow)
        my_table <- t(my_table)
      my_table[, ncol(my_table)] <-
        nrow(items) - rowSums(my_table)
      rel_feq <- prop.table(my_table, 1)
    }
    kum_feq <- t(apply(rel_feq, 1, cumsum))
    z.wert <- qnorm(kum_feq[,-ncol(kum_feq)])
    z.wert[which(is.infinite(z.wert))] <- NA
    zeilen.mittel <- rowMeans(z.wert, na.rm = TRUE)
  #  spalten.mittel <- colMeans(z.wert, na.rm = TRUE)

    ANS <- rndr_percent(rel_feq * 100, my_table, return_as_vector=FALSE)

    dimnames(ANS) <- dimnames(my_table)
    ANS <- data.frame(ANS)
    ANS$mittlerer.Rang <- rowSums(rel_feq *
                                    matrix(rep(1:ncol(rel_feq),
                                               each = nrow(rel_feq)),
                                           nrow = nrow(rel_feq)))
    ANS$Skalenwert <-
      (mean(zeilen.mittel, na.rm = T) - zeilen.mittel)*-1
    ANS <- cbind(Items = rownames(ANS), ANS)

    if (RankByRow)
      ANS$Items <- factor(ANS$Items, names(mylabels), mylabels)
    ANS
  }


  if (!is.null(grouping)) {
    data_by_group <- split(items, grouping, sep = "___") #-- seperator fuer mehr als ein Faktor
    g_res <- lapply(data_by_group, Calc_Rank)
    is_null <- which(sapply(g_res, function(x)! is.null(x)))
    g_res <- g_res[is_null]

    Group <- rep(gl(length(g_res), 1, labels = names(g_res)),
                 sapply(g_res, nrow))
    if (ncol(grouping) > 1)
      Group <- reshape2::colsplit(Group, "___", names(grouping))

    else{
      Group <- data.frame(Group)
      names(Group) <- names(grouping)
    }
    r <- list(
      results = cbind(Group, do.call(rbind, g_res)),
      labels = mylabels,
      groups = names(g_res)
    )
  }
  else{
    r <- Calc_Rank(items)
    if (order)
      r <-
        r[order(r$Skalenwert,
                na.last = TRUE,
                decreasing = decreasing),]
    r <- list(
      results = r,
      Skalenwert = data.frame(
        names = rownames(r),
        Items = r$Items  ,
        mean = r$mittlerer.Rang,
        Skalenwert = r$Skalenwert
      ),
      labels = mylabels,
      groups = NULL
    )
  }
  r$N <- N
  class(r) <- "rangreihe"
  return(r)
  }
