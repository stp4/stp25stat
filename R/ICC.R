#' ICC Intra-Klassen-Korrelation
#'
#'  Kopie von \link{ICC} aus dem Packet \code{psych}.
#'  psych erwartet eine Matrix in form von:
#'
#'  ICC und ICC2 erlaubt die Verwendung von Formeln \code{ICC(~a+b+c+g, data)}
#'  wobei hier auch die Rater bzw Judge
#'
#' @param ... Weitere Argumente al psych missing=TRUE, alpha=.05
#' @return ICC gibt ein psych -Objetht retur ICC2 einen data.frame
#' @export
#' @examples
#' #'
#' #Quelle:James Demaree Woolf http://sci-hub.ac/10.1037/0021-9010.69.1.85
#' data<-stp25aggregate::GetData(
#'   "Judge   item1 item2 item3 item4 item5 item6
#'   1       3     4     3     2     4     3
#'   2       2     3     3     2     4     4
#'   3       4     3     4     3     4     3
#'   4       3     3     2     2     2     4
#'   5       3     2     2     4     2     3
#'   6       4     2     4     3     2     3
#'   7       2     3     2     3     3     4
#'   8       3     4     4     3     3     2
#'   9       4     2     3     4     3     2
#'   10      2     4     3     4     3     2")
#'
#'
#'
#'
#' AD_Index2(~item1+item2+item3+item4+item5+item6, data, type="item", A=4)
#'
#' data2<- data.frame(t(data[,-1]))
#' names(data2)<-  c("J1", "J2", "J3", "J4", "J5", "J6", "J7", "J8", "J9", "J10")
#' AD_Index2(~J1+J2+J3+J4+J5+J6+J7+J8+J9+J10, data2, type="judge", A=4)
#'
#'
#'
#' data<- stp25aggregate::GetData("
#'                                Item J1 J2 J3 J4 J5 J6 J7 J8 J9 J10
#'                                1  5  4  5  4  5  4  5  4  5  4
#'                                2  4  5  4  5  4  5  4  5  4  5
#'                                3  5  5  5  5  5  4  4  4  4  4
#'                                4  5  4  4  5  5  5  4  5  4  5")
#'
#'
#' AD_Index2(~J1+J2+J3+J4+J5+J6+J7+J8+J9+J10, data)
#' #Quelle:James Demaree Woolf
#' ICC2(~J1+J2+J3+J4+J5+J6+J7+J8+J9+J10, data)
#' ##Head("personality-project")
#' #Quelle  http://www.personality-project.org/r/book/Chapter7.pdf
#' sf <- stp25aggregate::GetData("
#'                               J1 J2 J3 J4 J5 J6
#'                               1  1  6  2  3  6
#'                               2  2  7  4  1  2
#'                               3  3  8  6  5 10
#'                               4  4  9  8  2  4
#'                               5  5 10 10  6 12
#'                               6  6 11 12  4  8")
#' sf  # Intraclass Correlation Coefficient (ICC)
#' ICC2(sf)
#'
#' #AD_Index2(~., data)
#'
ICC <- function(...) {
  UseMethod("ICC")
}


#' @rdname APA_
#' @description APA_ICC:Intra-Klassen-Korrelation (mit psych).
#' @export
APA_ICC <- function(...,
                    caption = "ICC",
                    note = "",
                    type = c(1, 4),
                    output = which_output()) {
  ans <- ICC(...)
  APA2.ICC(ans,
           caption = caption,
           type = type,
           output = output)
}

#' @rdname ICC
#' @param caption,note,output an Output
#' @param type 1 bis 6 type = c("all", "ICC1", "ICC2")
#'  c("ICC1" "ICC2", "ICC3" "ICC1k" "ICC2k"  "ICC3k")
#' @export
ICC2 <- function(...,
                 caption = "ICC",note="",
                 type = c(1, 4),
                 output = which_output()) {
  APA_ICC(
    ...,
    caption = caption,
    note = note,
    type = type,
    output = output
  )
}


#' @rdname APA2
#' @export
APA2.ICC <- function(x,
                     caption = "ICC",
                     note = NULL,
                     type = c(1, 4),
                     output = which_output()) {
  n <- paste0("obs=", x$n.obs, ", judge=", x$n.judge)
  if (is.null(note))
    note <- paste("Number of subjects: ", n)
  
  ans <-  prepare_output(x$results[type, ],
                         caption = caption,
                         N = n)
  Output(fix_format(ans),
         ote = note,
         output = output)
  
  invisible(ans)
}


#' @rdname ICC
#' @export
ICC.matrix <- function(data) {
  psych::ICC(data)
}

#' @rdname ICC
#' @export
ICC.data.frame <- function(data) {
  psych::ICC(as.matrix(data))
}

#' @rdname ICC
#' @param x   Formula
#' @param data dataframe oder Matrix an psych::ICC
#' @export
ICC.formula <- function(x, data) {
  X <- Formula_Data(x, data)
  psych::ICC(as.matrix(X$Y_data))
}


#' @rdname ICC
#' @description AD_index  Hypothese Jeder Judge beurteilt das Item gleich Estimating Interrater Agreement With the
#' Average Deviation Index: A User’s Guide MICHAEL J. BURKE WILLIAM P. DUNLAP
#' Basiert auf 642 Raphaela Willitsch
#' wo Estimating Interrater Agreement With the Average Deviation Index: A User’s Guide MICHAEL J. BURKE
#' WILLIAM P. DUNLAP
#' die Methode beschreibt.
#'
#' Estimating within-group interrater reliability with and without response bias.
#' By James, Lawrence R.; Demaree, Robert G.; Wolf, Gerrit
#' Journal of Applied Psychology, Vol 69(1), Feb 1984, 85-98.
#' Abstract
#' Presents methods for assessing agreement among the judgments made by a single group of judges on a single variable in regard to a single target. For example, the group of judges could be editorial consultants, members of an assessment center, or members of a team. The single target could be a manuscript, a lower level manager, or a team. The variable on which the target is judged could be overall publishability in the case of the manuscript, managerial potential for the lower level manager, or a team cooperativeness for the team. The methods presented are based on new procedures for estimating interrater reliability. For such situations, these procedures furnish more accurate and interpretable estimates of agreement than estimates provided by procedures commonly used to estimate agreement, consistency, or interrater reliability. The proposed methods include processes for controlling for the spurious influences of response biases (e.g., positive leniency and social desirability) on estimates of interrater reliability. (49 ref) (PsycINFO Database Record (c) 2016 APA, all rights reserved)
#'
#' Hypothese Jeder Judge beurteilt das Item gleich.
#'
#' rwg: oeherer Wertbessere Uebereinstimmung
#'
#'
#' @param A in AD_Index A=5 Anzahl der Stufen der Likertskala
#'  na.rm normalerweise bna.rm==TRUE soll nicht geaendert werden
#' @return Ein data.frame
#' @export


AD_Index <- function(x, ...) {
  UseMethod("AD_Index")
}

#' @rdname ICC
#' @export
AD_Index2 <- function(x,
                      data,
                      ... ,
                      A = NULL,
                      #A Anzahl stufen der  Likertskala
                      type = "judge",
                      #lang oder breit
                      caption = "Interrater Agreement",
                      note = "AD index (based on deviations from the item medians)",
                      output = which_output()) {
  X <- Formula_Data(x, data)
  
  if (is.factor(X$Y_data[, 1])) {
    #prüfen wie viele stufen
    A <- nlevels(X$Y_data[, 1])
    X$Y_data <- dapply2(X$Y_data)
  } else{
    if (is.null(A)) {
      A <- 5
      warning(
        "Fuer Interrater Agreement muss
        A  die Anzahl an Stufen der Likertskala angegeben werden!
        Default ist 5 Stufen"
      )
    }
    }
  
  if (type == "judge")
    X$Y_data <- data.frame(t(X$Y_data))
  N <- nrow(X$Y_data)
  AD.Index <- sapply(X$Y_data, AD_Index_helper, ...)
  Mean <- sapply(X$Y_data, mean_na_rm, ...)
  SD <- sapply(X$Y_data, var_na_rm, ...)
  
  J <- length(X$yname)
  sx <- mean(SD, na.rm = TRUE)
  
  # seite 91 triangular Distribution
  ET <-
    ifelse(A %% 2 == 0, (A ^ 2 + 2 * A - 2) / 24, ((A - 1) * (A + 3)) /
             24)
  ET_rwg1 <- 1 - sx / ET
  ET_rwgj <- (J * ET_rwg1) / (J * ET_rwg1 +  sx / ET)
  
  # Expected Error uniform Distribution
  EU <- (A ^ 2 - 1) / 12
  EU_rwg1 <- 1 - sx / EU
  EU_rwgj <- (J * EU_rwg1) / (J * EU_rwg1 + sx / EU)
  
  Overall <- data.frame(
    Item = "Overall",
    Mean = NA,
    SD = NA,
    AD.Index = mean(AD.Index)
  )
  ##Ausgabe
  means <-
    fix_format(rbind(
      data.frame(
        Item = stp25aggregate::GetLabelOrName(X$Y_data),
        Mean,
        SD,
        AD.Index,
        stringsAsFactors = FALSE
      ),
      Overall
    ))
  Output(
    means,
    "Estimating Interrater Agreement With the Average Deviation Index",
    note = paste("Number of rater =", N),
    output = output
  )
  #Expected Error
  exer <-  fix_format(data.frame(
    Expected.Error = c("uniform Distribution", "triangular Distribution"),
    rwg1 = c(EU_rwg1, ET_rwg1),
    rwgj = c(EU_rwgj, ET_rwgj),
    stringsAsFactors = FALSE
  ))
  Output(
    exer,
    caption = "Estimating Within-group interrater reliability",
    note = paste("Number of rater =", N),
    output = output
  )
  
  invisible(list(
    AD.Index = AD.Index,
    expected.error = exer,
    mean = means
  ))
    }

#' @rdname ICC
#' @export
AD_Index.data.frame <- function(data, ...) {
  AD_Index.formula( ~ ., data, ...)
}


#' @rdname ICC
#' @export
AD_Index.formula <- function(x,
                             data,
                             type = "judge",
                             ...) {
  X <-  Formula_Data(x,  data)
  if (is.factor(X$Y_data[, 1])) {
    #prüfen wie viele stufen
    A <- nlevels(X$Y_data[, 1])
    X$Y_data <- dapply2(X$Y_data)
  }  else{
    warning(
      "Fuer Interrater Agreement muss
      A  die Anzahl an Stufen der Likertskala angegeben werden!
      Default ist 5 Stufen"
    )
  }
  
  if (type == "judge")
    X$Y_data <- data.frame(t(X$Y_data))
  
  data.frame(
    Item = stp25aggregate::GetLabelOrName(X$Y_data),
    AD.Index = sapply(X$Y_data,
                      AD_Index_helper, ...)
  )
  
  }

#' @rdname ICC
#' @export
AD_Index.default <- function(x, ...) {
  data.frame(Item = deparse(substitute(x)),
             AD.Index = AD_Index_helper(x, ...))
}

#- Helper ------------------------------------
AD_Index_helper <- function(x,
                            na.rm = TRUE) {
  #- Interrater Agreement
  if (is.factor(x))
    x <- as.numeric(x)
  if (na.rm)
    x <- na.omit(x)
  sum(abs(x - mean(x))) / length(x)
}

mean_na_rm <- function(x, na.rm = TRUE)
  mean(x, na.rm = na.rm)
# im Artikel sird sd verwendet
var_na_rm <- function(x, na.rm = TRUE)
  var(x, na.rm = na.rm)
