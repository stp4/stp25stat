
#' Tbll_desc
#' 
#' @param ...  an prepare_data2
#' @param caption 
#' @param include.n,include.nr,include.total,include.multiresponse 
#' @param include.test,include.normality.tests Test
#' @param include.label Labels ja-nein
#' @param exclude,exclude.level,max_factor_length fuer factor
#' @param include.custom eigene Funktion mit (x, by, fun) return kann ein Vector oder eine Matrix sein
#'  function(x , by){
#' x <- scale(as.numeric(x))
#' diff(sapply(split(x, by), mean, na.rm=TRUE))})
#' 
#' @return data.frame
#' @export
#' @examples
#' 
#' Tbll_desc(
#'   warpbreaks,
#'   "H1",
#'   breaks,
#'   tension,
#'   by = ~ wool,
#'   include.total = TRUE,
#'   include.n = FALSE,
#'   include.test = TRUE)
#'   
#'   
#'    warpbreaks2 <- Label(warpbreaks,
#' breaks	=	"The number of breaks",
#' wool	=	"The type of wool",
#' tension	=	"The level of tension")
#' warpbreaks2$tension2 <- as.numeric(warpbreaks2$tension)
#' 
#' warpbreaks2 %>%
#'   Tbll_desc(breaks + tension ~ wool)
#' warpbreaks2 %>%
#'   Tbll_desc_long(breaks + tension ~ wool)
#' Tbll_corr(breaks ~ tension2, warpbreaks2,  groups = ~ wool)
#' 
#' Tbll_xtabs( ~ tension + wool, warpbreaks2, include.label = FALSE)
#' 
#' lm1 <- lm(breaks ~ wool + tension, data = warpbreaks2)
#' lm2 <- lm(breaks ~ wool * tension, data = warpbreaks2)
#' 
#' # aufruf von APA_Table()
#' Tbll_reg(
#'   lm1,
#'   lm2,
#'   include.p = FALSE,
#'   include.ci = TRUE,
#'   include.se = FALSE,
#'   caption = "Regression Table"
#' )
#' 
#' # aufruf von APA2()
#' Tbll_reg(lm1, lm2)
#'   
#'   
#'   
Tbll_desc <- function (...,
                       caption="",
                       include.label=TRUE,
                       include.n = TRUE,
                       include.nr = FALSE,
                       include.total = FALSE,
                       include.test = FALSE,
                       include.normality.tests=FALSE,
                       include.multiresponse=FALSE,
                       include.custom = NULL
                       ) {
  note<-""
  rslt_all <- NULL
 
  
  X <- stp25formula::prepare_data2(...)
 # print(X)
  n <- length(X$measure.vars)
  any_missing_measure <- sapply(X$data[X$measure.vars], function(x) length(na.omit(x))) 
 
   if(!include.label) X$row_name <- X$measure.vars
 
  if ( include.n & sum(any_missing_measure[X$measure!="header"]-X$N) == 0 ) {
    # keine fehlenden dann nur erste Zeile mit N
    include.n <- FALSE
    include.nr <- TRUE
  }
  
  if (include.multiresponse)
      X$measure <- rep("multi", length(X$measure))
  
  if (is.null(X$group.vars) | include.total) {
    rslt_all <-
      list_rbind(purrr::pmap(list(
        x = X$data[X$measure.vars],
        digits = X$digits,
        measure = X$measure,
        row_name = X$row_name), .calc_desc_mean))
    
    if (include.total)
      names(rslt_all)[3:4] <-
      paste0("Total_", names(rslt_all)[3:4])
  }
  
  if (!is.null(X$group.vars)) {
    if (length(X$group.vars) > 1) {
      X$data$group <- interaction(X$data[X$group.vars])
     caption <- paste(paste(X$group.vars, collapse=", "), caption)
      X$group.vars <- "group"
    } else  caption <- paste( X$group.vars, caption)
    ans <- NULL
    data <-
      split(X$data[X$measure.vars], X$data[[X$group.vars]])
    
    for (i in names(data)) {
      ans_i <-
        list_rbind(purrr::pmap(list(
          x = data[[i]],
          digits = X$digits,
          measure = X$measure,
          row_name = X$row_name), .calc_desc_mean))
      
      if (is.null(ans))
        ans <- ans_i[1:2]
      names(ans_i) <-  paste0(i, "_", names(ans_i))
      ans <- cbind(ans, ans_i[-c(1:2)])
    }

    
    if (include.total)
      rslt_all <- cbind(rslt_all, ans[-c(1:2)])
    else
      rslt_all <- ans
    
  }
  
  if (include.nr) {
    if (is.null(X$group.vars)) {
      n.out <- c("(N)", "", "", X$N)
      names(n.out) <- names(rslt_all)
      rslt_all <- rbind(n.out, rslt_all)
    }
    else {
      tsum <- table(X$data[[X$group.vars]])
      if (include.total) {
        n.out <- c("(N)", rep("", ncol(rslt_all) + 4))
        n.out[grep("_m", names(rslt_all))] <-
          as.character(c(sum(tsum), as.vector(tsum)))
      } else{
        n.out <- c("(N)", rep("", ncol(rslt_all) + 2))
        n.out[grep("_m", names(rslt_all))] <- as.character(tsum)
      }
      rslt_all <- rbind(n.out, rslt_all)
    }
  }
  
  if (!include.n) {
    length.out <- if(is.null(X$group.vars)) 1 else nlevels(X$data[[X$group.vars]]) + include.total
    rslt_all <-
      rslt_all[-(seq(
        from = 3,
        by = 2,
        length.out = length.out
      ))]
    names(rslt_all) <- gsub("_m", "", names(rslt_all))
  }
  
  if (!is.null(include.custom)) {
    rslt_custom <- NULL
    for (i in seq_len(n)) {
      tmp <- do.call(include.custom,
                     list(X$data[[X$measure.vars[i]]],
                          X$data[[X$group.vars[1]]]))
      if (is.vector(tmp)) {
        rslt_custom <- append(rslt_custom, tmp)
        if (X$measure[i] == "factor")
          rslt_custom <-
            append(rslt_custom, rep("", nlevels(X$data[[X$measure.vars[i]]])))
      }
      else{
        rslt_custom <- rbind(rslt_custom, tmp)
        if (X$measure[i] == "factor") {
          rslt_custom <- rbind(rslt_custom,
                               matrix(
                                 "",
                                 ncol = ncol(rslt_custom),
                                 nrow = nlevels(X$data[[X$measure.vars[i]]])
                               ))
        }
      }
    }
    if (is.vector(rslt_custom)) {
      if (include.nr)
        rslt_custom <-  append(rslt_custom, "", after = 0)
      rslt_all$custom <- rslt_custom
    }
    else {
      rslt_custom <- rbind(rep("", ncol(rslt_custom)), rslt_custom)
      rslt_all <- cbind(rslt_all, rslt_custom)
    }
  }
  
  
  if (is.character(include.test)) {
    include.test <- gsub("[^[:alpha:]]", "", tolower(include.test))
    which_test <-
      match.arg(include.test,
                c(contest, cattest, notest, ordtest, disttest, cortest))
    X$measure.test <- rep(which_test, length(X$measure.test))
    if (which_test %in% disttest) {
      include.test <- FALSE
      include.normality.tests <- TRUE
    } else{
      include.test <- TRUE
    }
  }  
  
  if (include.test) {
    rslt_test <- NULL
    for (i in seq_len(n)) {
      temp <- NULL
      fm_chi <-
        formula(paste("~", X$measure.vars[i], "+", X$group.vars[1]))
      fm_aov <-
        formula(paste(X$measure.vars[i], "~", X$group.vars[1]))
      
      if (X$measure.test[i] == "notest") {
        rslt_test <- append(rslt_test,  "")
      }
      else if (X$measure.test[i] == "contest") {
        if (X$measure.class[i] == "factor") {
          temp <- X$data[[X$measure.vars[i]]]
          X$data[[X$measure.vars[i]]] <-
            as.numeric(X$data[[X$measure.vars[i]]])
        }
        rslt_test <-
          append(rslt_test,  conTest(fm_aov, X$data))
      }
      else if (X$measure.test[i] == "cattest") {
        rslt_test <- append(rslt_test, catTest(fm_chi, X$data))
      }
      else if (X$measure.test[i] %in% contest) {
        if (X$measure.class[i] == "factor") {
          temp <- X$data[[X$measure.vars[i]]]
          X$data[[X$measure.vars[i]]] <-
            as.numeric(X$data[[X$measure.vars[i]]])
        }
        rslt_test <-
          append(rslt_test, conTest(fm_aov, X$data, X$measure.test[i]))
      }
      else if (X$measure.test[i] %in% cattest) {
        rslt_test <-
          append(rslt_test, catTest(fm_chi, X$data, X$measure.test[i]))
      }
      if (!is.null(temp))
        X$data[[X$measure.vars[i]]] <- temp
      if (X$measure[i] == "factor")
        rslt_test <-
        append(rslt_test, rep("", nlevels(X$data[[X$measure.vars[i]]])))
    }
    
   
    if(include.nr)   rslt_test <-  append(rslt_test, "", after = 0)
    note <- paste(unique(names(rslt_test)[nzchar(names(rslt_test))]), collapse = ", ")
    rslt_all$statistics <- rslt_test
  }
  
  
  if (include.normality.tests) {
    rslt_disttest <- NULL
    for (i in seq_len(n)) {
      if (X$measure[i] %in% c("numeric", "mean", "median")) {
        ix <- na.omit(as.numeric(X$data[[X$measure.vars[i]]]))
        if (X$measure.test[i] == "kstest")
          r <- APA(stats::ks.test(ix, "pnorm", mean(ix), sd(ix)))
        else
          r <- APA(stats::shapiro.test(ix))
      } else if (X$measure[i] == "factor")
        r <- rep("", nlevels(X$data[[X$measure.vars[i]]]))
      else
        r <- ""
      rslt_disttest <- append(rslt_disttest, r)
    }
    if (include.nr)
      rslt_disttest <-  append(rslt_disttest, "", after = 0)
    
    note<- X$measure.test[1]
    rslt_all$normality.tests <- rslt_disttest
  }
  
  
  rslt_all[[1]] <- paste(rslt_all[[1]], rslt_all[[2]])
  prepare_output(rslt_all[-2], caption = caption, note=note, N=X$N)
}


#' @rdname Tbll_desc
#' @export
Tbll_desc_multi <- function(...) {
  Tbll_desc(..., include.multiresponse=TRUE)
}



#' @rdname Tbll_desc
#' @export
#' @examples 
#' 
#' #' require(stpvers)
#' n <- 2 * 20
#' e <- rnorm(n)
#' dat <- stp25aggregate::Label(
#'   data.frame(
#'     a = rnorm(n) + e / 2,
#'     b = rnorm(n) + e,
#'     c = rnorm(n),
#'     d = rnorm(n) + e * 10,
#'     g = gl(2, 20, labels = c("Control", "Treat"))
#'   ),
#'   a = "Alpha",
#'   b = "Beta",
#'   c = "Gamma"
#' )
#' 
#' 
#' Tbll_corr( ~ a + b + c, dat)
#' Tbll_corr(a ~ c, dat)
#' Tbll_corr(a + b + c ~ d, dat)
#' Tbll_corr(a + b + c ~ d, dat, groups = ~ g)
#' 
#' 
Tbll_corr <-
  function(...,
           caption="",
           include.label=TRUE,
           include.mean = FALSE,
           include.n = TRUE,
           include.stars = TRUE,
           include.p = FALSE,
           cor_diagonale_up = TRUE,
           type = c("pearson", "spearman")) {
  Hmisc_rcorr(
      ...,
      cor_diagonale_up = cor_diagonale_up,
      include.stars = include.stars,
      include.p  = include.p,
      include.mean = include.mean,
      include.n = include.n,
      type = type,
      caption = caption,
      note = ""
    )
}




#' @rdname Tbll_desc
#' @export
#' 
#' @examples 
#' 
#' 
#'  data(infert, package = "datasets")
#' infert$case  <- factor(infert$case , 1:0, c("case", "control"))
#' 
#' infert$spontaneous <- factor(infert$spontaneous)
#' infert$induced2    <- factor(infert$induced == 0)
#' 
#' Tbll_xtabs( ~  case, infert)
#' Tbll_xtabs( ~ induced2 + case, infert)
#' Tbll_xtabs( ~ induced + case, infert)
#' Tbll_xtabs( ~ induced + education, infert)
#' 
#' 
#' Tbll_xtabs( ~ induced + education + case,
#'             infert,
#'             margin = "case",
#'             #  add.margins = c("education", "induced"),
#'             include.count = FALSE)
#' 
#' Tbll_xtabs(
#'   ~ induced + education + case,
#'   infert,
#'   margin = "case",
#'   add.margins = c("case"),
#'   include.count = FALSE
#' )
#' 
#' 
#' 
Tbll_xtabs <- function(...,
                       caption="",
                       include.label=TRUE,
                       include.count = TRUE,
                       include.percent = TRUE,
                       include.total = FALSE,
                       
                       include.prop.chisq = FALSE,
                       include.chisq = FALSE,
                       include.fisher = FALSE,
                       include.test = any(c(include.fisher,
                                            include.chisq,
                                            include.prop.chisq)),
                       
                       include.correlation = FALSE,
                       include.diagnostic = FALSE,
                       
                       margin = NULL,
                       add.margins = NA) {
  rslt <- APA_Xtabs(
    ...,
    caption=caption,
    label=include.label,
    include.percent = include.percent,
    include.count = include.count,
    include.total = include.total,
    
    margin = margin,
    add.margins = add.margins,
    
    include.prop.chisq = include.prop.chisq,
    include.chisq = include.chisq,
    include.fisher = include.fisher,
    include.test = include.test,
    include.correlation = include.correlation,
    include.diagnostic = include.diagnostic,
    
    output = FALSE
  )
  rslt
}









#' Leerer Data.Frame
#' 
#' @noRd
.emty_tbll <- function() {
  data.frame(
    lev = "",
    n = "",
    m = "",
    stringsAsFactors = FALSE
  )
}


#' Die Berechnung
#' 
#' @noRd
.calc_desc_mean <- function(x,
                           digits,
                           measure,
                           row_name,
                           sep = paste(stp25rndr::symbol_nbsp(), stp25rndr::symbol_nbsp()),
                           exclude = NA,
                           max_factor_length = 35,
                           ...) {
 
  x  <- na.omit(x)
  n  <- length(x)
  rslt <- NULL
  
  res <- switch(
    measure,
    numeric = Mean2default(x, digits, n),
    integer = Mean2default(x, digits, n),
    factor =  Prozent2default(x, digits, n, exclude, max_factor_length),
    logical = Prozent2default(x, digits, n, exclude, max_factor_length),
    mean =    Mean2default(x, digits, n),
    median =  Median2default(x, digits, n),
    multi =   Multi2default(x, digits, n),
    header =  .emty_tbll(),
    .emty_tbll()
  )

  if (measure == "factor") {
    x0 <- data.frame(
      Item = row_name,
      lev = "",
      n = res$n[1] ,
      m = "",
      stringsAsFactors = FALSE
    )
    res$n <- ""
    x1 <- cbind(Item = sep, res)
    
    rslt <- rbind(x0, x1)
  } else
    rslt <-
    cbind(Item = c(row_name, rep("", nrow(res) - 1)), res)
  
  rslt
}





#' liste als DF
#' 
#' @noRd
list_rbind <- function(l)
  as.data.frame(do.call(rbind, (l)))
