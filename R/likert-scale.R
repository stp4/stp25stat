#' Likert
#'
#'  Analyse von Likertskalen.
#'
#' @name Likert
#' @param ... Alle weiteren Argumente
#' @param reverse.labels logical levels umderehen
#' @param labels  andere Ordung der  levels
#' @return a likert class list with the following elements:
#'   results: Ergebnisse der Haufigkeiten fuer zB Grafik
#'   names: Dataframe mit Namen
#'   freq: Data.frame mit nur den Haufigkeiten ohne NAs
#'   freq.na:Data.frame mit nur den Haufigkeiten mit NAs
#'   N: Stichprobengroesse als Zahl
#'   n: Anzahl der Gueltigen Werte je Item (Vektor)
#'   m: Mittelwerte (Vektor)
#'   sd: SD (Vektor)
#'   Mittelwert: M und SD Formatiert (Vektor)
#'   items: Daten
#'   grouping: data frame mit Gruppe (kann auch laenge Null haben)
#'   nlevels Anzahl an Levels
#'   levels: Labels fuer zb scale =list( at=nlevels, ...)
#' @export
#' @examples
#'
#'
#' set.seed(1)
#' n<-100
#' lvs<-c("--","-","o","+","++")
#' DF2<- data.frame(
#'   Magazines=gl(length(lvs),1,n,lvs),
#'   Comic.books=gl(length(lvs),2,n,lvs),
#'   Fiction=gl(length(lvs),3,n,lvs),
#'   Newspapers=gl(length(lvs),5,n,lvs))
#'
#' DF2$Comic.books[sample.int(n/2)]<- lvs[length(lvs)]
#' DF2$Newspapers[sample.int(n/2)]<- lvs[1]
#' DF2$Magazines[sample.int(n/2)]<- lvs[2]
#'
#' DF2<- transform(DF2, Geschlecht= cut( rnorm(n), 2, Hmisc::Cs(m, f)))
#' Res1 <- Likert(~., DF2[,-5] )
#' APA2(Res1)
#' APA2(Res2 <- Likert(.~ Geschlecht, DF2 ))
#'
#' #require(HH)  # ?likertplot
#'
#' #windows(7,3)
#'  #likertplot( Item   ~ .| Geschlecht , data=Res2$results,
#'  #            main = '',
#'  #            ylab = "",
#'  #            sub = "" ,
#'  #            xlab = "Prozent",
#'  #            col = brewer_pal_likert(5),
#'  #            rightAxis = FALSE,
#'  #            positive.order = TRUE,
#'  #            as.percent = TRUE,
#'  #            auto.key = list(space = "top", columns = 2) ,
#'  #            layout=c(2,2)
#'  #)
#'
#'
#'
#'
#' # data<- Res2$names
#' # data$mean<- Res2$m
#' #  barchart( Item~ mean |Geschlecht, Mymean2, xlim=c(1,5))
#' # windows(3,6)
#' # dotplot(Item ~ mean, data,
#'  #       groups=Geschlecht, xlim=c(.085, 5.15),
#'  #       type=c("p", "l"))
#'
#'
#' APA2(Res1, ReferenceZero=3)
#' APA2(Res1, ReferenceZero="-")
#' APA2(Res2, ReferenceZero=3, na.exclude=TRUE, type="freq")
#'
#'
#'
#'
Likert <- function(...,
                   labels = NULL,
                   reverse.labels = FALSE) {
  if (!reverse.labels) {
    if (is.null(labels)) {
      results <- Summarise(
        ...,
        fun = function(x)
          table(x, useNA = "always"),
        key = "Item"
      )
    }
    else {
      results <- Summarise(
        ...,
        fun = function(x) {
        #  levels(x) <- labels
          x <- factor(x, labels)
          
          table(x, useNA = "always")
        },
        key = "Item"
      )
    }
    
    item_mean <-
      Summarise(
        ...,
        fun = function(x)
          mean(as.numeric(x), na.rm = TRUE),
        key = "Item"
      )$value
    
    item_sd <-
      Summarise(
        ...,
        fun = function(x)
          sd(as.numeric(x), na.rm = TRUE),
        key = "Item"
      )$value
    
  } else  {
    results <-
      Summarise(
        ...,
        fun = function(x) {
          x <- factor(x, rev(levels(x)))
          table(x, useNA = "always")
        },
        key = "Item"
      )
    
    item_mean <-
      Summarise(
        ...,
        fun = function(x) {
          mean(1 + nlevels(x) - as.numeric(x), na.rm = TRUE)
        },
        key = "Item"
      )$value
    item_sd <- Summarise(
      ...,
      fun = function(x) {
        sd(1 + nlevels(x) - as.numeric(x), na.rm = TRUE)
      },
      key = "Item"
    )$value
  }
  
  
  nms <-  sapply(results, is.integer)
  ncl <- ncol(results)
  names(results)[ncl] <- "NA"
  
  
  col_names <- names(results[-ncl])
  pos_col_names <- grep("Item", col_names)
  
  
  rslt <- list(
    results = results[-ncl],
    names =  results[-c(which(nms), ncl)],
    freq =    results[which(nms[-ncl])],
    freq.na = results[which(nms)],
    N =   sum(results[which(nms)]) / nlevels(results$Item),
    n =   as.vector(rowSums(results[which(nms[-ncl])])),
    m =   item_mean,
    sd = item_sd,
    Mittelwert =  stp25rndr::rndr_mean(item_mean, item_sd),
    # items =  data.frame(),
    #  grouping = NULL,
    formula =   if (pos_col_names == 1) {
      Item ~ .
    } else{
      formula(paste("Item ~ .|", paste(col_names[1:(pos_col_names - 1)], collapse = "+")))
    },
    
    nlevels = sum(nms) - 1,
    levels =  names(nms[-ncl])[nms[-ncl]]
  )
  class(rslt) <- c('likert', class(rslt))
  rslt
}



#' Tbll_likert
#'
#' @param ...  Likert-Objekt oder data+Formula
#' @param caption,note an prepare Data
#' @param ReferenceZero,labels   ReferenceZero=2 Neutrales Element in Kombination mit
#'  labels = c("low", "neutral", "high")
#' @param include.mean,include.n,include.na,include.order,include.percent,include.count Zusatz

#' @param reverse.labels an Likert
#'
#' @return
#' @export
#'
#' @examples
#' 
#' set.seed(1)
#' n <- 100
#' lvs <- c("--", "-", "o", "+", "++")
#' DF2 <- data.frame(
#'   Magazines = cut(rnorm(n), 5, lvs),
#'   Comic.books = cut(rnorm(n), 5, lvs),
#'   Fiction = cut(rnorm(n), 5, lvs),
#'   Newspapers = cut(rnorm(n), 5, lvs),
#'   Geschlecht = cut(rnorm(n), 2, c("m", "f"))
#' )
#' 

#' DF2 %>% Tbll_likert(Magazines, Comic.books, Fiction, Newspapers)
#' 
#' # Tabelle + Grafik
#' 
#' #  APA_Likert( ~ ., DF2[, -5])
#' #  APA_Likert(. ~ Geschlecht, DF2 , layout = c(1, 2))
#' 
#' 
Tbll_likert <- function(...){
  UseMethod("Tbll_likert")
}


#' @rdname Tbll_likert
#' @export
#' 
Tbll_likert.default <- function(...,
             caption = "" ,
             note = NULL,
             ReferenceZero = NULL,
             include.mean = TRUE,
             include.n = FALSE,
             include.na = FALSE,
             include.order = TRUE,
             include.percent = TRUE,
             include.count =TRUE,
             labels = c("low", "neutral", "high"),
             reverse.labels = FALSE) {
  
  Tbll_likert.likert(
  Likert(..., reverse.labels = reverse.labels),
  caption = caption,
  note = note,
  ReferenceZero = ReferenceZero,
  include.mean = include.mean,
  include.n = include.n,
  include.na = include.na,
  include.order = include.order,
  include.percent = include.percent,
  include.count =include.count,
  labels = labels
  )
 
}
 
#' @rdname Tbll_likert
#' @export
#' 
Tbll_likert.likert <- function(x,
                        caption = "" ,
                        note = NULL,
                        ReferenceZero = NULL,
                        # type = "percent",
                        include.mean = TRUE,
                        include.n = FALSE,
                        include.na = FALSE,
                        include.order = TRUE,
                        #  na.exclude = FALSE,
                        include.percent = TRUE,
                        include.count =TRUE,
                        labels = c("low", "neutral", "high")
                        ) {


  if (!is.null(ReferenceZero)) {  
    # x$freq und x$freq.na werden neu zudammengefasst
    if (is.character(ReferenceZero))
      ReferenceZero <- which(x$levels %in% ReferenceZero)
    else if (!is.numeric(ReferenceZero))
      ReferenceZero <- median(1:x$nlevels)
    
    if (ceiling(ReferenceZero) == floor(ReferenceZero)) {
      lowrange <- 1:(ReferenceZero - 1)
      neutral <- ReferenceZero
      highrange <- (ReferenceZero + 1):x$nlevels
      
      freq <- cbind(
        lowrange = RowSums2(x$freq[, lowrange]),
        neutral = x$freq[, neutral],
        highrange = RowSums2(x$freq[, highrange])
      )
      if (is.null(note))
        note <-
        paste(
          "lowrange:",
          paste(x$levels[lowrange], "\n", collapse = "|"),
          "neutral:",
          paste(x$levels[neutral], "\n", collapse = "|"),
          "highrange:",
          paste(x$levels[highrange], collapse = "|")
        )
      
      colnames(freq) <-
        c(
          paste0(labels[1], "(1:", ReferenceZero - 1, ")"),
          paste0(labels[2], "(", ReferenceZero, ")"),
          paste0(labels[3], "(", ReferenceZero + 1, ":", x$nlevels, ")")
        )
      x$freq <- freq
      
    } else{
      lowrange <- 1:floor(ReferenceZero)
      highrange <- ceiling(ReferenceZero):x$nlevels
      
      freq <-
        cbind(lowrange = RowSums2(x$freq[, lowrange]),
              highrange = RowSums2(x$freq[, highrange]))
      colnames(freq) <-
        c(
          paste0(labels[1], "(1:", floor(ReferenceZero), ")"),
          paste0(labels[3], "(", ceiling(ReferenceZero), ":", x$nlevels, ")")
        )
      if (is.null(note))
        note <-
        paste(
          "lowrange:",
          paste(x$levels[lowrange], "\n", collapse = "|"),
          "highrange:",
          paste(x$levels[highrange], collapse = "|")
        )
      x$freq <- freq
      
    }
    
    x$freq.na <- if (names(x$freq.na)[ncol(x$freq.na)] == "NA")
      cbind(freq, x$freq.na[ncol(x$freq.na)])
    else
      freq
  }
  
  
  if (include.na) {
    x$freq <- x$freq.na
  }
  
  if(include.percent){
    if(include.count)
      x$freq <- stp25rndr::rndr_percent2(x$freq / x$n * 100, x$freq)[-1]
    else  
      x$freq <- stp25rndr::rndr_percent2(x$freq / x$n * 100)[-1]
  } else if( !include.count ){
    x$freq<- ""
  }
  
  if (include.n) {
    x$freq <- cbind(n = x$n, x$freq)
  }
  
  if (include.mean) {
    x$freq <- cbind(x$freq,
                    'M(SD)' = x$Mittelwert)
  }
  
  ans <- cbind(x$names, x$freq)
  
  if (!is.logical(include.order) & include.order == "decreasing")
    ans <- ans[order(x$m,  decreasing = TRUE), ]
  else if (include.order)
    ans <- ans[order(x$m), ]
  
  prepare_output(
    ans,
    caption = paste0(caption, " (N = ", x$N, ")")  ,
    note = note ,
    N = x$N
    )
}

RowSums2 <- function(x)
  if (is.vector(x)) x else rowSums(x, na.rm = TRUE)


#' @rdname Likert
#' @description
#' Likert type : c(1, 2), oder c("Freq", "Precent")
#'
#' @param ReferenceZero,labels,na.exclude,labels,include.mean Likert: ReferenceZero=2 Neutrales Element in Kombination mit
#'  labels = c("low", "neutral", "high")
#'   Mittelwerte T/F
#' @export
APA2.likert <- function(x,
                        caption = "" ,
                        note = NULL,
                        type = "percent",
                        order = FALSE,
                        na.exclude=FALSE,
                        ReferenceZero = NULL,
                        include.mean = TRUE,
                        include.n = FALSE,
                        include.na = !na.exclude,
                        include.order = order,
                        include.percent = type == "percent",
                        include.count =TRUE,
                        labels = c("low", "neutral", "high"),
                        output = which_output(),
                        ...) {
  rslt <- Tbll_likert.likert(
    x,
    caption = caption,
    note = note,
    ReferenceZero = ReferenceZero,
    include.mean = include.mean,
    include.n = include.n,
    include.na = include.na,
    include.order = include.order,
    include.percent = include.percent,
    include.count = include.count,
    labels = labels
  )
  Output(rslt, output = output, ...)
  invisible(rslt)
}



# {
# 
#   if (!is.null(ReferenceZero)) {
#     if (is.character(ReferenceZero))
#       ReferenceZero <- which(x$levels %in% ReferenceZero)
#     else if (!is.numeric(ReferenceZero))
#       ReferenceZero <- median(1:x$nlevels)
# 
#     #  cat(ReferenceZero, "\\n")
# 
#     if (ceiling(ReferenceZero) == floor(ReferenceZero)) {
#       freq <- cbind(
#         lowrange = RowSums2(x$freq[, 1:(ReferenceZero - 1)]),
#         neutral = x$freq[, ReferenceZero],
#         highrange = RowSums2(x$freq[, (ReferenceZero + 1):x$nlevels])
#       )
#       colnames(freq) <-
#         c(
#           paste0(labels[1], "(1:", ReferenceZero - 1, ")"),
#           paste0(labels[2], "(", ReferenceZero, ")"),
#           paste0(labels[3], "(", ReferenceZero + 1, ":", x$nlevels, ")")
#         )
#       x$freq <- freq
#       x$freq.na <- if (names(x$freq.na)[ncol(x$freq.na)] == "NA")
#         cbind(freq, x$freq.na[ncol(x$freq.na)])
#       else
#         freq
# 
#     } else{
#       freq <-
#         cbind(lowrange = RowSums2(x$freq[, 1:floor(ReferenceZero)]),
#               highrange = RowSums2(x$freq[, ceiling(ReferenceZero):x$nlevels]))
#       colnames(freq) <-
#         c(
#           paste0(labels[1], "(1:", floor(ReferenceZero), ")"),
#           paste0(labels[3], "(", ceiling(ReferenceZero), ":", x$nlevels, ")")
#         )
#       x$freq <- freq
#       x$freq.na <- if (names(x$freq.na)[ncol(x$freq.na)] == "NA")
#         cbind(freq, x$freq.na[ncol(x$freq.na)])
#       else
#         freq
#     }
#   }
# 
#   if (!na.exclude)
#     x$freq <- x$freq.na
# 
#   results2Prozent <- x$freq / x$n * 100
# 
#   if (type == "percent" |  type == 2)
#     x$freq <- rndr_percent2(results2Prozent, x$freq)[-1]
# 
#   if (include.mean)
#     ans <- cbind(x$names,
#                  x$freq,
#                  n = x$n,
#                  Mittelwert = x$Mittelwert)
#   else
#     ans <- cbind(x$names, x$freq)
# 
# 
#   if (!is.logical(order) & order == "decreasing")
#     ans <- ans[order(x$m,  decreasing = TRUE), ]
#   else if (order)
#     ans <- ans[order(x$m),]
#   else
#     (NULL)
# 
#   ans <-
#     prepare_output(
#       ans,
#       caption = paste0(caption, " (N = ", x$N, ")")  ,
#       note = note ,
#       N = x$N)
#   
#   Output(ans, output=output, ...)
#   invisible(ans)
# }



#' @param save.plot,w,h Grafik speichern
#' @param include.table Output Tabelle
#' @param include.plot,auto.key,layout,wrap_sentence  Output plot wrap_sentence =list
#'
#' @rdname APA_
#' @export
#'
#'
APA_Likert <- function(...,
                       caption = "",
                       note = NULL,
                       ReferenceZero = NULL,
                       include.mean = TRUE,
                       include.n = FALSE,
                       include.na = FALSE,
                       include.order = TRUE,
                       include.percent = TRUE,
                       include.count = TRUE,
                       labels = c("low", "neutral", "high"),
                       reverse.labels = FALSE,
                       w = 8,
                       h = NULL,
                       include.table = TRUE,
                       include.plot = TRUE,
                       save.plot =FALSE,
                       auto.key = list(space = "top", columns = 2),
                       layout=NULL,
                       wrap_sentence=list(width = 45, sep =  "\n", max.lines = 2)
                       ) {
  x <- Likert(...,
              reverse.labels = reverse.labels,
              labels = NULL)
  

  if (include.table) {
    Output(
      Tbll_likert(
        x,
        caption = caption,
        note = note,
        ReferenceZero = ReferenceZero,
        include.mean = include.mean,
        include.n = include.n,
        include.na = include.na,
        include.order = include.order,
        include.percent = include.percent,
        include.count = include.count,
        labels = labels
      )
    )
  }
  

  
  if (include.plot) {
    if (is.null(h))
      h <- 2.3 + length(x$n) * (2 / 5)
   if(!is.null(wrap_sentence))
     levels(x$results$Item) <- 
       stp25plot::wrap_sentence(levels(x$results$Item),
                                wrap_sentence$width,
                                wrap_sentence$sep,
                                wrap_sentence$max.lines)
    print(
      HH:::plot.likert.formula(
        x$formula,
        data = x$results,
        main = caption,
        ylab = "",
        sub = "",
        xlab = "Prozent",
        col = brewer_pal_likert(x$nlevels),
        rightAxis = FALSE,
        ReferenceZero = ReferenceZero,
        positive.order = TRUE,
        as.percent = TRUE,
        auto.key = auto.key,
        layout=layout
      )
    )
   if(save.plot) SavePlot(caption, w = w, h = h)
  }
  invisible(x)
}


#  function(...){
#   res<- Likert(...)
#   res[[ "table"]] <- APA2(res)
#  invisible( res )
# }

# Tab_plot <-


#' @rdname Likert
#' @export
print.likert<-function(x, ...){
  cat("\nnames: ", paste(names(x), collapse=", "),"\n")
  cat("\nresults:  \n ")
  print( head(x$results))
  cat("\nlevels: ", paste(x$levels, collapse=", "),"\n")
}


#' @rdname Likert
#' @description
#' Farb-Palette für Likert-plots die Funktion ist eine  eine Kopie von \code{brewer.pal.likert} kann aber auch schwarz-weiss.
#' und hat blau-rot als default
#' @export
#' @param n,name,middle.color,min_gray,max_gray brewer_pal_likert Default 5 Stufen mit Grau in der Mitte
#' @examples
#'
#' brewer_pal_likert( ) #Default 5 Stufen mit Grau in der Mitte
#' #  HH::brewer.pal.likert(5, "RdBu", "gray80")
#'
brewer_pal_likert <- function(n = 5,
                              name = "RdBu",
                              middle.color = "gray80",
                              min_gray = 10,
                              max_gray = 60) {
  if (tolower(name) == "gray") {
    min_gray <- 1 - min_gray / 100
    max_gray <- 1 - max_gray / 100
    if (n %% 2 == 0) {
      n <- (n) / 2
      mycols <- gray(seq(min_gray,  max_gray, length.out = n))
      c(rev(mycols), mycols)
    }
    else{
      n <- (n - 1) / 2 + 1
      mycols <- gray(seq(min_gray,  max_gray, length.out = n))
      c(rev(mycols), mycols[-1])
    }
  } else
    HH::brewer.pal.likert(n, name, middle.color)
}


