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
#' Literatur: Bortz, J. & Doering, N. (2006). Forschungsmethoden und
#'  Evaluation fuer Human-und Sozialwissenschaftler (4. Auflage). Berlin: Springer. Seite 155
#'
#' @name Rangreihe
#' @param ... Weitere Argumente
#' @return Vector
#' @export
#' @examples
#'
#' #'
#' 
#' 
#' 
#' 
#' DF1 <-  data.frame(
#'   A = c(1, 1, 1, 2, 3, 1),
#'   B = c(2, 2, 2, 3, 2, 3),
#'   C = c(3, 3, 3, 1, 1, NA),
#'   D = c(NA, NA, NA, NA, NA, 2)
#' )
#' DF2 <-   data.frame(
#'   R1 = factor(c("A", "A", "A", "C", "C", "A"),   c("A", "B", "C", "D")),
#'   R2 = factor(c("B", "B", "B", "A", "B", "D"),   c("A", "B", "C", "D")),
#'   R3 = factor(c("C", "C", "C", "B", "A", "B"),   c("A", "B", "C", "D"))
#' )
#' 
#' 
#' Rangreihe(DF1)$mean
#' Rangreihe(DF2)$mean
#' 
#' dat_bortz<-
#'   as.table(matrix(c(
#'     2,8,10,13,17,
#'     5,10,15,18,2,
#'     10,12,20,5,3,
#'     15,20,10,3,2,
#'     22,18,7,2,1)
#'     , nrow = 5, ncol=5, byrow=TRUE,
#'     dimnames = list(c("A", "B", "C", "D", "E"),1:5)))  
#' 
#' Rangreihe(dat_bortz)
#' 
#' 
#' 
#' # dat_table <-
#' #   as.table(matrix(c(
#' #     50,0,0,0,0,
#' #     0,50,0,0,0,
#' #     0,0,50,0,0,
#' #     0,0,0,50,0,
#' #     0,0,0,0,50
#' #      )
#' #     , nrow = 5, ncol=5, byrow=TRUE,
#' #     dimnames = list(c("A", "B", "C", "D", "E"),1:5)))
#' #  # Calc_Rank(dat_table)
#' 
#' 
#' 
#' n <- 2 * 3 * 4 * 1
#' 
#' set.seed(n)
#' kaffee <- c("Guatemala", "Vietnam", "Honduras", "Äthiopien")
#' sex<- c("male", "female")
#' age<- c("20-29", "30-39", "40-49", "50-59")
#' kaffe<- c("Espresso", "Filterkaffee", "Milchkaffee")
#' 
#' DF <-
#'   data.frame(
#'     sex = factor("male",sex),
#'     Alter = factor("20-29",age ),
#'     Kaffeeform = factor("Espresso", kaffe),
#'     R1 = factor(kaffee[1], kaffee),
#'     R2 = factor(kaffee[2], kaffee),
#'     R3 = factor(kaffee[3], kaffee),
#'     R4 = factor(kaffee[4], kaffee)
#'   )
#' 
#' DF<- rbind(DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF)
#' 
#' for(i in 1:n){
#'   DF<- rbind(DF,
#'              c(sample(sex)[1],
#'                sample(age)[1],
#'                sample(kaffe)[1],
#'                sample(kaffee)
#'              ))
#' }
#' 
#' x <- DF[4:7]
#' Rangreihe(x, include.percent=FALSE, groups=DF$sex)
#' x<-Rangreihe(R1 + R2 +R3 ~sex, DF, include.percent=FALSE, output=FALSE)
#' 
#' 
#' names( x)
#' x$mean
#' 
#' #' 
#' #lattice::dotplot( reorder(Items, mean)~ mean|"Kaffee", 
#'                   x$mean, groups=group , xlab="",
#' #                  xlim=range(x$mean$mean)*1.10 , auto.key=list(), cex=1)
#'  
Rangreihe <-
  function(...) {
    UseMethod("Rangreihe")
  }



#' @rdname Rangreihe
#' @param x formula
#' @param data Data.frame Objekt
#' @param order,decreasing sortieren
#' @param exclude,subset,na.action an Formula_Data
#' @export
Rangreihe.formula <- function(x,
                              data = NULL,
                              caption = "Rangreihe",
                              note = "Law of Categorical Judgement",
                              output=stp25output::which_output(),
                              exclude = NA,
                              subset,
                              na.action = na.pass,
                              ...) {
  X <- Formula_Data(x, data, subset, na.action)
  if (is.null(X$xname)) {
    Rangreihe.default(
      items = X$Y_data,
      caption = caption,
      note = note,
      output=output,
      groups = NULL,
      ...
    )
  } else{
    Rangreihe.default(
      items = X$Y_data,
      caption = caption,
      note = note,
      output=output,
      groups=X$X_data,
      ...
    )
  }
}



#' @rdname Rangreihe
#'
#' @param items data.frame
#' @param include.percent,include.freq,include.mean,include.z Auswerten
#' @param groups gruppen
#' @param order,decreasing sortieren
#' @param N Stichprobe
#' @param digits.mean digits
#' @export
Rangreihe.default <- function (items,
                               caption = "Rangreihe",
                               note = "Law of Categorical Judgement",
                               output=stp25output::which_output(),
                               include.percent=TRUE,
                               include.freq=TRUE,
                               include.mean=TRUE,
                               include.z=TRUE,
                               groups=NULL,
                               order = TRUE,
                               decreasing = TRUE,
                               N = if (is.null(groups))
                                 nrow(items)
                               else
                                 nrow(na.omit(groups)),
                               # info = FALSE,
                               digits.mean = 2,
                               ...)
{
  if (!stp25stat::is_all_identical2(items))
    warning(
      "Das Skalenniveau in der Rangreihe ist unterschiedlich. Moeglicherweise stimmen die Ergebnisse nicht!"
    )
  
  if (!is.null(groups)) {
    data_by_group <-
      split(items, groups, sep = "___") #-- seperator fuer mehr als ein Faktor
    r <- lapply(data_by_group, Calc_Rank)
    res<- NULL
    
    for( i in names(r)) {
      res_1 <- format_rank(
        r[[i]],
        include.mean, include.z,
        include.percent, include.freq,
        digits.mean, order, decreasing
      )
      
      res<- rbind(res, cbind(Group=i, res_1))  
      r$mean =   rbind(   cbind(r[[i]]$mean, group=i),  r$mean   )
      
    }
  }
  else{
    r  <- Calc_Rank(items)
    res <- format_rank(
      r,
      include.mean,
      include.z,
      include.percent,
      include.freq,
      digits.mean,
      order,
      decreasing
    )
    
  }
  stp25output::Output( 
    stp25stat::prepare_output(res,
                              caption = paste0(caption, " (N = ", N , ")"),
                              note=note,
                              N=N),
    output=output,
    ...)
  
  r$res <- res
  invisible(r)
  
}




format_rank <- function(r,
                        include.mean,
                        include.z,
                        include.percent,
                        include.freq,
                        digits.mean,
                        order,
                        decreasing) {
  
  if (include.mean & include.z)
    rmean <- r$mean[, c( "mean","sd", "z.score")]
  else if (include.z)
    rmean <- r$mean[, "z.score"]
  else if (include.mean)
    rmean <- r$mean[, c( "mean","sd")]
  else
    rmean <- NULL
  
  res <- as.data.frame(
    cbind(
      Items = r$labels,
      if (include.percent & include.freq)
        stp25rndr::rndr_percent(r$rel.freq * 100, r$freq, return_as_vector = FALSE)
      else if (include.percent)
        stp25rndr::rndr_percent(r$rel.freq * 100, return_as_vector = FALSE)
      else
        r$freq,
      stp25rndr::Format2(rmean, digits.mean)
    ),
    stringsAsFactors = FALSE
  )
  
  if (order)
    res[order(r$mean[, "z.score"],
              na.last = TRUE,
              decreasing = decreasing),]
  else
    res
}

#' Range transortieren
#'
#' @param x data.frame
#'
#' @return data.frame
#' @examples
#'
#'   DF2 <-   data.frame(
#'   R1 = factor(c("A", "A", "A", "C", "C", "A"),   c("A", "B", "C", "D")),
#'   R2 = factor(c("B", "B", "B", "A", "B", "D"),   c("A", "B", "C", "D")),
#'   R3 = factor(c("C", "C", "C", "B", "A", "B"),   c("A", "B", "C", "D"))
#'   )
#'   transpose(DF2)
#' @noRd
transpose <- function(x) {
  last <- nrow(x)
  x <- cbind(x, id = 1:last)
  last_column <- ncol(x)
  
  x <- stp25aggregate::Melt2(x,
                             id.vars = last_column,
                             key = "variable",
                             value = "rang")
  
  x <- tidyr::spread(x, rang, variable)
  stp25stat:::dapply1(x[-1])
  
}


#' @param x dataframe
#' @param my_ranks levels
#'
#' @return matrix
#' @noRd

table_apply <- function(x, my_ranks) {
  tab <- t(sapply(x, function(x, ...) {
    table(factor(x, ...))
  },
  levels = my_ranks, simplify = TRUE))
  colnames(tab) <- paste0("R", colnames(tab))
  tab
}




#' @param x Items
#' @param RankByRow Methode 
#' @param sicherheit,q_wert Das LOCJ gilt nur wenn keine 0 vorhanden sind daher hier mit 99.9% wahrscheinlichkeit
#' @param n Anzahl
#'
#' @return list("freq",     "rel.freq", "z.value",  "mean",     "labels"  )
#' @noRd


Calc_Rank <-
  function(x,
           RankByRow = if (is.factor(x[, 1]) |
                           is.character(x[, 1]))
             FALSE
           else
             TRUE,
           sicherheit = .001,
           q_wert = qnorm(1 - sicherheit),
           n = nrow(x),
           mylabels=NULL
  ) {
    
    if (n < 1) {
      return(NULL)
    }
    
    if (!is.table(x)) {
      if (!RankByRow)
        x <- transpose(x)
      
      mylabels <- stp25aggregate::GetLabelOrName(x)
      
      if (!stp25stat::is_all_identical2(x)) {
        my_ranks <- unique(unlist(lapply(x,
                                         function(x)
                                           levels(factor(
                                             x
                                           )))))
      }
      else if (is.factor(x[, 1]))
        my_ranks <- levels(x[, 1])
      else
        my_ranks <- levels(factor(x[, 1]))
      
      tbl <- table_apply(x, my_ranks)
      
      # Test ob fehlende Werte
      if (ncol(tbl) < nrow(tbl)) {
        my_ranks <- c(my_ranks, ".NA")
        tbl <- table_apply(x, my_ranks)
        tbl[, ncol(tbl)] <- nrow(x) - rowSums(tbl)
      }
      rel_feq <- prop.table(tbl, 1)
      
      x_mean <- sapply(x,  function(x) {
        x[is.na(x)] <- length(my_ranks)
        c(m = mean(x, na.rm = TRUE),
          sd = sd(x, na.rm = TRUE),
          min = min(x, na.rm = TRUE),
          max = max(x, na.rm = TRUE),
          median = median(x, na.rm = TRUE)
          
        )
      } )
      
    }
    else
    {
      mylabels<- rownames(x)
      names(mylabels)<- mylabels
      my_ranks <- seq_len(ncol(x))
      tbl <- x
      rel_feq <- prop.table(tbl, 1)
      mm <- rel_feq * matrix(rep(1:ncol(rel_feq),
                                 each = nrow(rel_feq)),
                             nrow = nrow(rel_feq))
      mms <- rowSums(mm)
      x_mean <- rbind(m=mms, sd=NA, se=NA , min=NA, max=NA, median=NA)
    }
    
    # Das LOCJ gilt nur wenn keine 0 vorhanden sind daher hier mit 99.9% wahrscheinlichkeit
    # qnorm(.001)= -3.090232
    kum_feq <- t(apply(rel_feq, 1, cumsum))
    kum_feq[which(kum_feq[, ] == 0)] <- sicherheit
    kum_feq[which(kum_feq[, ] == 1)] <- 1 - sicherheit
    
    #The Normal Distribution
    z.wert <- qnorm(kum_feq[,-ncol(kum_feq)])
    # z.wert[which(is.infinite(z.wert))] <- qnorm(1-sicherheit)  # NA
    
    zeilen.sum <- rowSums(z.wert, na.rm = TRUE)
    zeilen.mittel <- zeilen.sum / (ncol(z.wert))
    z_score <- (mean(zeilen.mittel, na.rm = T) - zeilen.mittel) * -1
 
    my_mean <- data.frame(Items=mylabels,
                          mean = x_mean[1L, ],
                          sd = x_mean[2L, ],
                          se = x_mean[2L, ] / sqrt(n),
                          min = x_mean[3L, ],
                          max = x_mean[4L, ],
                          median = x_mean[5L, ],
                          z.score = z_score,
                          # z.rang = length(my_ranks) - ((z_score + q_wert) / 
                          #(q_wert * 2) * (length(my_ranks) - 1)),
                          stringsAsFactors = FALSE
                          
    )
    list(
      freq = tbl,
      rel.freq = rel_feq,
      z.value = cbind(z.wert,
                      z.sum = zeilen.sum, z.mittel = zeilen.mittel),
      mean = my_mean,
      labels = mylabels
    )
    
  }






# 
# APA2.rangreihe <- function(x,
#                            caption = "",
#                            note = "",
#                            output=which_output(),
#                            ...) {
#   x$results$mittlerer.Rang <- Format2(x$results$mittlerer.Rang, digits = 2)
#   x$results$Skalenwert <- Format2(x$results$Skalenwert, digits = 2)
# 
#   Output( prepare_output(x$results,
#                  caption = paste0(caption, " (N = ", x$N , ")"),
#                  note=note,
#                  N=x$N),
#          output=output,
#          ...)
# }

