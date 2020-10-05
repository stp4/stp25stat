#' @rdname Tbll
#' @export
#' @examples
#' 
#'
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
Tbll_desc <- function (...,
                       caption="",
                       #note=""
                       # type = c("2", 
                       #          "1", 
                       #          "freq","freq.ci",
                       #          "mean", "median", "ci","cohen.d","effsize",
                       #          "multiresponse",  "pearson", "spearman"),
                       na.action = na.pass,
                       exclude = NA,
                       include.n = TRUE,
                       # Anzahl bei Gruppe je Spalte
                        include.nr = FALSE,
                       # erste zeile Anzahl der Gruppen
                       include.total = FALSE,
                       # Total Anzahl + Statistik bei Gruppe
                       include.test = FALSE,
                       include.normality.tests=FALSE, #distribution-Test
                       include.multiresponse=FALSE,
                       exclude.level = FALSE,
                       max_factor_length = 35
                       ) {

  sep <- paste(stp25rndr::symbol_nbsp(), stp25rndr::symbol_nbsp())
  note<-""
  rslt_all <- NULL
  X <- stp25formula::prepare_data2(..., na.action = na.action)
  n <- length(X$measure.vars)
  
  any_missing_measure <- sapply(X$data[X$measure.vars], function(x) length(na.omit(x))) 
  X$measure <- ifelse(X$measure == "logical"  & (any_missing_measure == 0), "header", X$measure)
  # keine fehlenden
  if ( include.n & sum(any_missing_measure[X$measure!="header"]-X$N) == 0 ) {
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
        row_name = X$row_name,
        sep = rep(sep, n),
        exclude = rep(exclude, n),
        exclude.level = if(is.null(exclude.level)) rep(NA, n) else rep(exclude.level, n),
        max_factor_length = rep(max_factor_length, n)
      )
      , 
      calc_desc_mean))
    
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
          row_name = X$row_name,
          sep = rep(sep, n),
          exclude = rep(exclude, n),
          exclude.level = if(is.null(exclude.level)) rep(NA, n) else rep(exclude.level, n),
          max_factor_length = rep(max_factor_length, n)
        ),   
        calc_desc_mean))
      
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


#' @rdname Tbll
#' @export
Tbll_desc_multi <- function(...) {
  Tbll_desc(..., include.multiresponse=TRUE)
}

emty_tbll <- function() {
  data.frame(
    lev = "",
    n = "",
    m = "",
    stringsAsFactors = FALSE
  )
}


 
calc_desc_mean <- function(x,
                           digits,
                           measure,
                           row_name,
                           sep = " ",
                           exclude = NA,
                           exclude.level = NULL,
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
    freq =    Prozent2default(x, digits, n, exclude, max_factor_length),
    mean =    Mean2default(x, digits, n),
    median =  Median2default(x, digits, n),
    multi =   Multi2default(x, digits, n),
    header =  emty_tbll(),
    emty_tbll()
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
    
    #if (!all(exclude.level))    exclude.level <- NULL
    
    if (all(is.na(exclude.level)) | ( !is.null(exclude.level) & length(x1$lev) == 2)) {
      excld <- which(x1$lev %in% exclude.level)
      if (length(excld) > 0)
        x1 <- x1[-excld, ]
    }
    
    rslt <- rbind(x0, x1)
  } else
    rslt <-
    cbind(Item = c(row_name, rep("", nrow(res) - 1)), res)
  
  rslt
}


list_rbind <- function(l)
  as.data.frame(do.call(rbind, (l)))
