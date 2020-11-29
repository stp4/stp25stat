#  Manova

 



#' @rdname APA2
#' @description MANOVA: APA2(x, , test="Wilks") test : "Wilks", "Pillai"
#' @param include.eta die Manova wird ueber heplots::etasq berechnet und die anova mit den SS eta2=SS/SS_total
#' @export
#' @examples
#'
#' #- manova ---------------------------------------------
#'
#' ## Set orthogonal contrasts.
#' op <- options(contrasts = c("contr.helmert", "contr.poly"))
#' ## Fake a 2nd response variable
#' npk2 <- within(npk, foo <- rnorm(24))
#' npk2 <- within(npk2, foo2 <- rnorm(24))
#' npk2.aov <- manova(cbind(yield, foo, foo2) ~ block + N * P * K, npk2)
#'
#' APA2(npk2.aov) #wilks
#' APA2(npk2.aov, "Pillai")
#'
#' #npk2.aovE <- manova(cbind(yield, foo) ~  N*P*K + Error(block), npk2)
#' #APA2(npk2.aovE)
#'
#'
#'
#'  DF<-stp25aggregate::GetData(
#' "C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25data/extdata/manova.sav"
#' )
#'
#' #information from
#' DF$GROUP<- factor(DF$GROUP, 1:3, Cs("website", "nurse ", "video tape" ))
#' #DF %>% Tabelle2(USEFUL, DIFFICULTY, IMPORTANCE, by=~GROUP )
#'
#' z<- as.matrix(DF[,-1])
#' fit1<- manova(z ~ DF$GROUP)
#' APA2(fit1)
#' summary(fit1)$Eigenvalues
#'
#' # SPSS
#' # Multivariate Tests of Significance (S = 2, M = 0, N = 13 )
#' #
#' # Test Name       Value  Approx. F Hypoth. DF   Error DF  Sig. of F
#' #
#' # Pillais          .48    3.02       6.00      58.00       .012
#' # Hotellings       .90    4.03       6.00      54.00       .002
#' # Wilks            .53    3.53       6.00      56.00       .005
#' # Roys             .47
#' # Note.. F statistic for WILKS' Lambda is exact.
#' # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' # Univariate F-tests with (2,30) D. F.
#' #
#' # Variable   Hypoth. SS   Error SS Hypoth. MS   Error MS          F  Sig. of F
#' #
#' # USEFUL       52.92424  293.96544   26.46212    9.79885    2.70053       .083
#' # DIFFICUL      3.97515  126.28728    1.98758    4.20958     .47216       .628
#' # IMPORTAN     81.82969  426.37090   40.91485   14.21236    2.87882       .072
#' #
#' # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' #   Eigenvalues and Canonical Correlations
#' #
#' # Root No.    Eigenvalue        Pct.   Cum. Pct.  Canon Cor.
#' #
#' # 1          .892      99.416      99.416        .687
#' # 2          .005        .584     100.000        .072
#' #
#' # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'
#'
#'
#'

APA2.manova <-
  function(x,
           test = "Wilks",
           caption = "MANOVA",
           note = "",
           type = c("anova", "manova"),
           include.eta = TRUE,
           #   include.summary=TRUE,
           output = which_output()) {
    aov_result <- NULL
    res <- summary.aov(x)
    
    for (i in names(res)) {
      rs <- res[[i]]
     
      resp_name <- data.frame(
        Source = i,
        F = NA,
        df = NA,
        p.value = NA,
        stringsAsFactors = FALSE
      )
      rs <- cbind(Source = gsub(".*\\$", "", rownames(rs)),
                  rs[, c("F value", "Df", "Pr(>F)")],
                  stringsAsFactors = FALSE)
      names(rs) <- c("Source", "F", "df", "p.value")
      
      if (include.eta) {
        # Eta-quadrat Ueberprueft und soweit ist es korrekt
        ss <- res[[i]][, 2]
        ss1 <- ss[-length(ss)]
        sst <- ss[length(ss)]
        eta_ss <- ss1 / sst
        #   cat("\neta^2=\n")
        #
        #   print(ss)
        #   print(eta_ss)
        # df2<- rs$df[nrow(rs)]
        # dfF<- rs$df*rs$F
        # eta<- (dfF)/(dfF+ df2)
        rs <- cbind(rs[1:3], part.eta2 =  c(eta_ss, NA),
                    rs[4])
        resp_name <- cbind(resp_name[1:3],
                           part.eta2 = NA,
                           resp_name[4])
        
      }
      #html wird anderst ausgegeben markdown_html ist noch nicht implementiert
      if( !grepl("html", output) )
        rs <- rbind(resp_name, rs) 
      
      if (is.null(aov_result))
        aov_result <- rs
      else
        aov_result <- rbind(aov_result, rs)
    }
    # names(aov_result)[length(names(aov_result))] <-   "p.value"
    aov_result <- prepare_output(fix_format(aov_result),
                                 caption = caption,
                                 note = note)
    if ("anova"  %in%  type)
      Output(
        aov_result,
        rgroup = names(res),
        n.rgroup = rep(nrow(res[[1]]), (length(res) - 1)),
        output = output
      )
    #
 
    maov_result <- summary(x, test = test)
    maov_result <-  stp25tools::fix_to_df(maov_result$stats)
    
    maov_result$Source <- gsub(".*\\$", "", maov_result$Source)
    
    if (include.eta) {
      ## OK und Korrekt
      eta <-  heplots::etasq(x, test = test)
      n <- ncol(maov_result)
      maov_result <-
        cbind(maov_result[1:(n - 1)],  part.eta2 = c(eta[, 1], NA),  maov_result[n])
      
    }
    maov_result <- prepare_output(fix_format(maov_result),
                                  caption = paste(test, "Test"))
    names(maov_result)[length(names(maov_result))] <-   "p.value"
    if ("manova" %in% type)
      Output(maov_result,
             output = output)
    
    invisible(list(manova = aov_result, test = maov_result))
  }






 


#' @rdname APA2
#' @description Canonical Discriminant Analysis (MANOVA)
#'
#' @param LRtests an candisc::Wilks Wilks Lambda Tests for Canonical Correlations
#' @export
APA2.candisc <- function (x,
                          caption = NA,
                          note = "",
                          output = which_output(),
                          LRtests = TRUE,
                          ...){
  table <- candisc:::canrsqTable(x)
  if (is.na(caption)) {
    caption_eigen <-
      paste("Canonical Discriminant Analysis for ", x$term, sep = "")
    caption_wilks <-
      "Test of H0: The canonical correlations in the current row and all that follow are zero"
  } else{
    caption_eigen <- caption_wilks <- caption
  }
  tests <- NULL
  Output(prepare_output(fix_format(table, digits = c(2, 2, 2, 1, 1)),
                        caption = caption_eigen),
         output = output)
  
  if (LRtests) {
    # Wilks Lambda Tests for Canonical Correlations
    tests <- candisc::Wilks(x)
    Output(prepare_output(fix_format(tests),
                          caption = caption_wilks),
           output = output)
    
  }
  invisible(list(eigenvalues = table, LRTest = as.data.frame(tests)))
}



#' @rdname APA2
#' @description Anova- Methode (MANOVA)
#' @export
APA2.Anova.mlm <- function(x,
                           caption = NA,
                           note = "",
                           output = which_output(),
                           ...) {
  if ((!is.null(x$singular)) && x$singular)
    stop(
      "singular error SSP matrix; multivariate tests unavailable\ntry summary(object, multivariate=FALSE)"
    )
  test <- x$test
  repeated <- x$repeated
  ntests <- length(x$terms)
  tests <- matrix(NA, ntests, 4)
  if (!repeated)
    SSPE.qr <- qr(x$SSPE)
  for (term in 1:ntests) {
    eigs <-
      Re(eigen(qr.coef(if (repeated)
        qr(x$SSPE[[term]])
        else
          SSPE.qr,
        x$SSP[[term]]), symmetric = FALSE)$values)
    tests[term, 1:4] <- switch(
      test,
      Pillai = car:::Pillai(eigs, x$df[term], x$error.df),
      Wilks = car:::Wilks(eigs, x$df[term],x$error.df),
      `Hotelling-Lawley` = car:::HL(eigs, x$df[term],x$error.df),
      Roy = car:::Roy(eigs, x$df[term], x$error.df)
    )
  }
  ok <- tests[, 2] >= 0 & tests[, 3] > 0 & tests[, 4] > 0
  ok <- !is.na(ok) & ok
  tests <- cbind(x$df, tests, pf(tests[ok, 2], tests[ok, 3],
                                 tests[ok, 4], lower.tail = FALSE))
  rownames(tests) <- x$terms
  colnames(tests) <- c("Df", "test stat", "approx F", "num Df", "den Df", "Pr(>F)")
  
  
  if( is.na(caption) )
    caption <- paste(
      "Type ",
      x$type,
      if (repeated)
        " Repeated Measures",
      " MANOVA Tests: ",
      test,
      " test statistic",
      sep = ""
    ) 
  
  tests<- cbind(Source=rownames(tests), as.data.frame(tests))
  
  
  # invisible(x)
  
  Output(prepare_output(fix_format(tests),
                        caption=caption, note=note), output=output)
  invisible( tests )
  
  
}


#' @rdname APA2
#' @param fit_predict lda: MASS predict.lda
#' @param newdata lda:model.frame
#' @description LDA (linear discriminants analysis) Erweiterung der MANOVA
#'
#' @export
#' @examples
#'
#' #-- LDA -------------------------
#'
#' library(MASS)

#'
#' require(stp25output)
#'
#'
#' DF2 <- stp25aggregate::GetData(
#' "C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25data/extdata/discrim.sav")
#' #--https://stats.idre.ucla.edu/spss/dae/discriminant-function-analysis/
#' DF2$Job <- factor(DF2$JOB, 1:3, Cs("customer service", "mechanic","dispatcher"))
#' DF2$Job2 <- factor(DF2$JOB, c(2,3,1), Cs( "mechanic","dispatcher","customer service"))
#'
#' #APA2(.~JOB ,DF2)
#' #DF2 %>%  APA_Correlation(OUTDOOR,SOCIAL,CONSERVATIVE )
#'
#' fit2 <- lda(Job ~ OUTDOOR+SOCIAL+CONSERVATIVE, data=DF2)
#' fit3 <- lda(Job2 ~ OUTDOOR+SOCIAL+CONSERVATIVE, data=DF2)
#'
#' APA2(fit2)
#' APA2(fit3)
#'
#' #' #fit2 <- lda(GROUP ~ ., data=DF )
#' #APA2(fit2)
#' #plot(fit2)
APA2.lda <- function(x,
                     fit_predict = MASS:::predict.lda(x),
                     newdata = model.frame(x),
                     caption = "",
                     note = "",
                     output = which_output(),
                     ...) {
  means <- prepare_output( stp25tools::fix_to_df(t(x$means)),
                          caption = paste("Means:", caption))
  
  Output(fix_format(means),
         output = output)
  
  scaling <-  stp25tools::fix_to_df(x$scaling)
  scaling <- prepare_output(scaling[order(-scaling[, 2]),],
                            caption = paste("Coefficients of linear discriminants:", caption))
  
  Output(fix_format(scaling),
         output = output)
  svd <- x$svd
  names(svd) <- dimnames(x$scaling)[[2L]]
  if (length(svd) > 1L) {
    svd <- prepare_output(data.frame(t(data.frame(
      svd ^ 2 / sum(svd ^ 2)
    ))),
    caption = paste("Proportion of trace:", caption))
    Output(fix_format(svd),
           output = output)
  }
  
  cTab <- table(newdata[, 1], fit_predict$class,
                dnn = c(names(newdata)[1], "Predict"))
  
  Output(
    addmargins(cTab),
    "Kontingenztafel tatsaechlicher und vorhergesagter Kategorien",
    output = output
  )
  
  
  cTotal <- c(diag(prop.table(cTab, 1)),
              Total = sum(diag(prop.table(cTab)))) * 100
  
  cTotal <-  stp25tools::fix_to_df(cTotal)
  Output(fix_format(cTotal),
         "prozentuale Uebereinstimmung",
         output = output)
  
  invisible(list(
    mean = means,
    scal = scaling,
    svd = svd,
    cTab = cTab,
    cTotal = cTotal
  ))
}
