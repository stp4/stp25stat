#' @rdname Tabelle
#'
#' @examples
#'
#'  Tabelle(
#' m1[median] + m2[median] + m3[median] + m4[median] ~ geschl,
#' varana,
#' APA = TRUE,
#' include.n = FALSE,
#' test = TRUE
#' )
#'
#'  c(
#' "wilcox"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="wilcox")[[1]]$statistics[1],
#' "h.test"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="h.test")[[1]]$statistics[1],
#' "anova"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="anova")[[1]]$statistics[1],
#' "t.test"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="t.test")[[1]]$statistics[1],
#' "hmisc"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="Hmisc")[[1]]$statistics[1]
#' )
#'
#'
#'    Tabelle(alter ~ geschl, varana, APA=TRUE)
#'
#' Tabelle(alter ~ geschl, varana, include.n=FALSE, APA=TRUE)
#' Tabelle(alter ~ geschl, varana, include.nr=TRUE, APA=TRUE)
#' Tabelle(alter ~ geschl, varana, include.total=TRUE, APA=TRUE)
#' Tabelle(alter ~ geschl, varana, include.test=TRUE, APA=TRUE)
errate_statistik3 <-
  function (...,
            type = NULL, # "muliresponse" "pearson", "spearman"
            caption = "",
            note = "",
            na.action = na.pass,
            exclude = NA,
            include.n = TRUE,     # Anzahl bei Gruppe je Spalte
            include.nr = FALSE,   # erste zeile Anzahl der Gruppen
            include.total = FALSE,# Total Anzahl + Statistik bei Gruppe
            include.test = test,
            exclude.level=NULL,
            max_factor_length = 35,
            order = FALSE
         )
  {
    mySep<-  stp25rndr::symbol_nbsp()
    mySep2  <-  paste0(mySep,mySep)
 
    Emty_res <- function(...) { data.frame(lev="", 
                                           n="", 
                                           m="", 
                                           stringsAsFactors = FALSE)}

    Mittelwert_Einzel <- function(i, x) {

      x  <- na.omit(x)
      n  <- length(x)
      rr <- NULL

      if(n==0 & X$measure[i]=="logical")  X$measure[i] <- "header"

      res <- switch(
        X$measure[i],
        numeric = Mean2default(x, X$digits[i], n),
        integer = Mean2default(x, X$digits[i], n),
        factor =  Prozent2default(x, X$digits[i], n, exclude, max_factor_length),
        logical = Prozent2default(x, X$digits[i], n, exclude, max_factor_length),
        freq =    Prozent2default(x, X$digits[i], n, exclude, max_factor_length),
        mean =    Mean2default(x, X$digits[i], n),
        median =  Median2default(x, X$digits[i], n),
        multi =   Multi2default(x, X$digits[i], n),
        header =  Emty_res(),
        Emty_res()
      )

      if (X$measure[i] == "factor") {
        x0 <- data.frame(
          Item = X$row_name[i],
          lev = "",
          n = res$n[1] ,
          m = "",stringsAsFactors=FALSE
        )
        res$n <- ""
        x1 <- cbind(Item = mySep2, res)
        if (!is.null(exclude.level) & length(x1$lev) == 2) {

          excld <- which(x1$lev %in% exclude.level)
          if (length(excld) > 0)
            x1 <- x1[-excld, ]
        }
        
        rr <- rbind(x0, x1)
      } else
        rr <-
        cbind(Item = c(X$row_name[i], rep("", nrow(res) - 1)), res)

      rr
    }

    Mittelwert_Gruppe <- function(i, j, x = NULL) {
      groups <- droplevels(X$data[[j]])
      tabel_header <- paste0(mySep, names(table(groups)))

      ans <- NULL
      for (lev in seq_len(nlevels(groups))) {
        xx <- x[which(groups == levels(groups)[lev])]
        rr <- Mittelwert_Einzel(i, xx)

        if (is.null(ans))
          ans <- rr
        else
          ans <- cbind(ans, rr[-c(1, 2)])
      }
      tabel_header <- rep(tabel_header, each = 2)
      names(ans)[-c(1, 2)] <-
        paste0(tabel_header, "_", names(ans)[-c(1, 2)])
      ans
    }

    Total_Gruppe <- function(i, j) {
      groups <- droplevels(X$data[[j]])
      res <- t(as.matrix(table(groups)))
      res_n <- NULL
      for (i in seq_len(ncol(res)) ) {
        res_n <-  cbind(res_n, cbind(n = "", res[, i]))
      }

      colnames(res_n) <- paste0(mySep,
                                rep(colnames(res), each = 2),
                                "_",
                                rep(c("n", "m"), length.out = ncol(res)))

      cbind(data.frame(Item = "Total",
                       lev = "(N)",
                       stringsAsFactors=FALSE
                       ), res_n)
    }

    Test <- function(i, j) {
      fm_chi <- formula(paste("~", measure.vars[i], "+",  j))
      fm_aov <- formula(paste(measure.vars[i], "~", j))

      if( X$group.class[j] == "factor") {
        if (X$measure.class[i] == "numeric" ) {
          conTest(fm_aov, X$data, which_test)
        }
        else if (X$measure.class[i] == "factor" ) {
          if (is.logical(which_test)){
            catTest(fm_chi, X$data,  "chisq.test")
          }
          else if (is.character(which_test)){
            catTest(fm_chi, X$data, which_test )
          }
          else{
            X$data[[measure.vars[i]]] <- as.numeric(X$data[[measure.vars[i]]])
            conTest(fm_aov, X$data, which_test)
          }
        }
        else if (X$measure.class[i] == "median" ) {
          conTest(fm_aov, X$data, which_test)
        }
        else if (X$measure.class[i] == "multi" ) {
          catTest(fm_chi, X$data, which_test)
        }
        else{
          # Zwischen-Ueberschrift
          " "
        }

      }
      else{
        # keine Gruppen eventuel Korrelation
        " "
      }
    }

    # Vorbereiten der Daten
    ANS <- NULL
    X <- prepare_data2(..., na.action = na.action)
    if(order){X<- order_by_mean( X )}
    

    group.vars   <- X$group.vars
    measure.vars <- X$measure.vars
    N            <- nrow(X$data)

    if(is.character(include.test)){
     which_test <-  match.arg(include.test, c("wilcox.test",
                                              "u.test",
                                              "kruskal.test",
                                              "h.test",
                                              "chisq.test",
                                              "fisher.test",
                                              "t.test",
                                              "aov", 
                                              "anova",
                                              "SPSS", 
                                              "Hmisc",
                                              "shapiro.test", 
                                              "ks.test"
                                              # Kolmogorov-Smirnov-Anpassungstest wie SPSS
                                              ))
     if (any(which_test %in% c("shapiro.test", "KS.test"))) {
       include.test <- FALSE # Einzeltest
     } else {
       include.test <- TRUE
     }
    }  else {
      which_test <- TRUE}

    if (type[1] == "multiresponse")
      X$measure <- rep("multi", length(X$measure))

    # Einzelvergleich
    if (is.null(group.vars)) {

      if (include.nr)
        ANS <-
          data.frame(
            Item = "Total",
            lev = "(N)",
            n = "",
            m = X$N,
            stringsAsFactors=FALSE
          )
      for (i in seq_len(length(measure.vars)))
      {
        mymeans<- Mittelwert_Einzel(i, X$data[[i]])
        if( which_test == "shapiro.test" )
              mymeans$shapiro.test <- APA(
                         stats::shapiro.test(na.omit(as.numeric(X$data[[i]]))))
        if( which_test == "ks.test" ){
          ix <- na.omit(as.numeric(X$data[[i]]))
          mymeans$ks.test <- APA(
            stats::ks.test(ix,"pnorm", mean(ix), sd(ix)))
        }
        ANS <- rbind(ANS, mymeans)
      }
      ANS$Item <-
        paste(ANS$Item, ANS$lev) # Spalte Characteristics entfernen
      if (include.n)
        ANS <- prepare_output(ANS[-2], caption, note, N)
      else
        ANS <- prepare_output(ANS[-c(2, 3)], caption, note, N)
    }
    # Gruppenvergleich
    else{
      for (j in group.vars) {
        #- jeder Eintrg getrennt
        if (X$group.class[j] == "factor") {
          # Kontrolle
          caption <- paste(X$col_name[j], caption)
          ans_in  <- NULL
          if (include.nr) {
            ans_in <- Total_Gruppe(i, j)
            if (include.total)
              # Item|lev|n||All|G1_n|G1_m|G2_n|G2_m|.._n|.._m|
              ans_in <- cbind(ans_in[1:2],
                              "Total_n" = "",
                              "Total_m" = X$N,
                              ans_in[3:ncol(ans_in)])

           if ( include.test ) ans_in$statistics <- ""
          }
          for (i in seq_len(length(measure.vars))) {
            ans <- Mittelwert_Gruppe(i, j, X$data[[measure.vars[i]]])
            if (include.total) {
              total <- Mittelwert_Einzel(i, X$data[[measure.vars[i]]])[-c(1, 2)]
              names(total)[] <- paste0("Total_", names(total))
              ans <- cbind(ans[1:2], total, ans[3:ncol(ans)])
            }
           if (include.test){
               ans$statistics <- ""
               ans$statistics[1] <- Test(i, j)
           }
            ans_in <- rbind(ans_in, ans)
          }
          if (!include.n) {
            if (ncol(ans_in) %% 2)
              ans_in <-
                ans_in[c(1, 2, seq(4, ncol(ans_in), by = 2), ncol(ans_in))]
            else
              ans_in <-
                ans_in[c(1, 2, seq(4, ncol(ans_in), by = 2))]

            names(ans_in) <- gsub("_m", "", names(ans_in))
          }
          ans_in$Item <- paste(ans_in$Item, ans_in$lev)
          ANS[[j]] <- prepare_output(ans_in[-2], caption, note, N)
        }
        else {
          # Das geht nicht bzw vieleicht als eigene Funktion
          stop("Benutze die Funktion APA_Correlation")
        }
      }
    }
    ANS
  }


#' measure.vars nach groesse sortieren
#' 
#' @noRd
#'
order_by_mean <- function(X) {
  if (length(X$yname) == 1) {
    return(X)
  }
  
  my_order <- order(sapply(X$data[X$measure.vars],
                           function(x) {
                             if (is.numeric(x))
                               mean(x, na.rm = TRUE)
                             else if (is.factor(x))
                               mean(as.numeric(x), na.rm = TRUE) / 100
                             else
                               0
                           })
                    , decreasing = TRUE)
  
  
  X$data <-
    X$data[c(X$measure.vars[my_order], X$group.vars, X$condition.vars)]
  X$measure.vars <- X$measure.vars[my_order]
  X$measure <- X$measure[my_order]
  X$row_name <- X$row_name[my_order]
  X$measure.class <- X$measure.class[my_order]
  X$digits <- X$digits[my_order]
  
  
  X
}


