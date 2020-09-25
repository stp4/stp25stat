# #  # not-used-functions
# 
# 
# 

# APA2.formula ------------------------------------------------------------


# APA2_formula <- function(x,
#                          data = NULL,
#                          caption = "",
#                          fun = NULL,
#                          type = c(
#                            "auto",
#                            "freq",
#                            "mean",
#                            "median",
#                            "ci",
#                            "multiresponse",
#                            "cohen.d",
#                            "effsize",
#                            "freq.ci",
#                            "describe"
#                          ),
#                          note = "",
#                          na.action = na.pass,
#                          test = FALSE,
#                          corr_test = "pearson",
#                          cor_diagonale_up = TRUE,
#                          direction = "long",
#                          order = FALSE,decreasing = TRUE,
#                          
#                          use.level = 1,
#                          include.n = TRUE,
#                          include.all.n = NULL,
#                          include.header.n = TRUE,
#                          include.total = FALSE,
#                          include.test = test,
#                          include.p = FALSE,
#                          include.stars = TRUE,
#                          include.names = FALSE,
#                          include.labels = TRUE,
#                          digits = NULL,
#                          digits.mean = if (!is.null(digits)) c(digits, digits)  else  NULL,
#                          digits.percent = if (is.null(digits))  options()$stp25$apa.style$prozent$digits else c(digits, 0),
#                          output = which_output(),
#                          ...) {
#   
#   if (include.names & include.labels) {
#     nms <- names(data)
#     lbl <- GetLabelOrName(data)
#     lbl <- paste(nms, lbl)
#     names(lbl) <- nms
#     data <- label_data_frame(data, lbl)
#   } else if (!include.labels) {
#     nms <- names(data)
#     names(nms) <- nms
#     data <- label_data_frame(data, nms)
#   }
#   
#   
#   
#   type <-  match.arg(type, several.ok = TRUE)
#   if (!is.null(fun))
#     type <-  "recast"
#   if (length(type) > 2)
#     type <- type[1] # Fehler abfangen
#   #cat("\n APA2(..., type =", type, ")\n")
#   result <- switch(
#     type[1],
#     recast = Recast2_fun(
#       x,
#       data,
#       caption,
#       fun,
#       note = note,
#       include.n = include.n,
#       direction = direction,
#       ...
#     ),
#     multiresponse =  APA_multiresponse(
#       x,
#       data,
#       caption = caption,
#       note = note,
#       test = test,
#       order = order,
#       decreasing = decreasing,
#       na.action = na.action,
#       use.level = use.level,
#       output=FALSE
#     )$tab,
#     cohen.d = cohen_d_formula(x, data, ...),
#     # effsize = Effsize( x, data, ..., type="cohen.d"),
#     describe = Describe2(x, data, stat = c("n", "mean", "sd", "min", "max")),
#     errate_statistik2(
#       x,
#       data = as.data.frame(data),
#       caption = caption,
#       note = note,
#       na.action = na.action,
#       type = if (length(type) > 1 | type[1] != "auto") type else NULL,
#       include.n = include.n,
#       include.all.n = include.all.n,
#       include.header.n = include.header.n,
#       include.total = include.total,
#       include.test = include.test,
#       include.p = include.p,
#       include.stars = include.stars,
#       order = order,
#       decreasing = decreasing,
#       corr_test = corr_test,
#       cor_diagonale_up = cor_diagonale_up,
#       
#       digits.mean = digits.mean,
#       digits.percent = digits.percent,
#       ...
#     )
#   )
#   
#   
#   if (is.data.frame(result))
#     Output(result, output=output)
#   else if (is.list(result))
#     for (rst in result)
#       Output(rst, output=output)
#   else
#     Text(Tab(), class(result), " ", result)
#   
#   invisible(result)
# }
# 
# 
# 
# 
# 
# 
# Recast2_fun <- function(x,
#                         data,
#                         caption = "",
#                         fun,
#                         direction = "long",
#                         note = "",
#                         include.n = FALSE,
#                         ...) {
#   ANS <-  Recast2(x,
#                   data,
#                   fun,
#                   drop = FALSE)
#   if (include.n) {
#     ans_n <-
#       Recast2(
#         x,
#         data,
#         fun = function(x)
#           length(na.omit(x)),
#         drop = FALSE
#       )
#     ANS <- data.frame(ANS[-ncol(ANS)],
#                       n = ans_n$value,
#                       value = ANS[, ncol(ANS)])
#   }
#   ANS <- prepare_output(ANS,
#                         caption, note, nrow(data))
#   
#   if (direction != "long")
#     prepare_output(reshape2::dcast(ANS,
#                                    as.formula(paste(
#                                      "variable", paste(x[-2], collapse = "")
#                                    )))
#                    , caption, note, nrow(data))
#   else
#     ANS
#   
# }
# 
# 
# 

# errate_statistik2 -------------------------------------------------------

 
# errate_statistik2 <- function(Formula,
#                               data,
#                               caption = "caption",
#                               
#                               note = "note",
#                               type = NULL,
#                               
#                               na.action = na.pass,
#                               exclude = NA,
#                               include.n = TRUE,
#                               include.all.n = NULL,
#                               include.header.n = TRUE,
#                               include.total = FALSE,
#                               include.test = FALSE,
#                               include.p = TRUE,
#                               include.stars = FALSE,
#                               corr_test = "pearson",
#                               cor_diagonale_up = TRUE,
#                               max_factor_length = 35,
#                               order = FALSE,
#                               decreasing = FALSE,
#                               useconTest = FALSE,
#                               normality.test = FALSE,
#                               digits.mean = options()$stp25$apa.style$m$digits,
#                               digits.percent = options()$stp25$apa.style$prozent$digits[1],
#                               test_name = "Hmisc",
#                               ...)
# {
#   Stat_Mean_Freq <- function(x, ...,
#                              default_numeric = "mean") {
#     index_zaeler <<- index_zaeler + 1
#     if (is.list(digits.mean))
#       digits.mean <-
#         digits.mean[[index_zaeler]] # lebt nur in dieser Funktion
#     if (is.list(type))
#       type <- type[[index_zaeler]] # lebt nur in dieser Funktion
#     # Formula_ data muss ~ m1[3]+ m2 aufdroeseln
#     # und digits uebergeben,
#     # und Formel zusammenbauen
#     
#     type_switch <- tolower(type)
#     #Funktion definieren fuer  'auto'
#     if (is.na(type_switch[1]) | any(type_switch %in% "auto")) {
#       if (any(type_switch %in% "median"))
#         default_numeric <- "median"
#       if (is.factor(x))
#         type_switch <- "freq"
#       else if (is.logical(x))
#         type_switch <- "freq_logical"
#       else if (is.numeric(x))
#         type_switch <- default_numeric
#       else{
#         x <- as.numeric(x)
#         type_switch <- default_numeric
#       }
#     }
#     
#     x_NA <- x
#     #  N    <- length(x)
#     x    <- na.omit(x)
#     n    <- length(x)
#     
#     
#     mydf <- function(n, m, name = "")
#       data.frame(Characteristics = "",
#                  n = as.character(n),
#                  Statistics = m,
#                  stringsAsFactors=FALSE)
#     
#     if (all(is.na(x)))
#       type_switch <- "all_NA"
#     
#     result <- switch(
#       type_switch,
#       mean = mydf(n, Mean2(x, digits = digits.mean, ...), "(mean)"),
#       median = mydf(n, Median2(x, digits = digits.mean[1], ...), "(median)"),
#       ci = mydf(n, Meanci2(x, digits = digits.mean, ...), "(CI)"),
#       meanci = mydf(n, Meanci2(x, digits = digits.mean, ...), "(CI)"),
#       freq = Prozent2APA(x_NA, exclude, digits.percent, max_factor_length),
#       freq_logical = Prozent2APA(x_NA, exclude, digits.percent, max_factor_length)[1,],
#       freq.ci = Prop_Test(x_NA)[, c("Characteristics", "n", "Statistics")],
#       n = mydf(n, n),
#       all_NA =  mydf(0, "n.a."),
#       mydf(n, class(x)) # nur eine Zeile ausgeben# Fehler abfangen
#     )
#     if (include.all.n)
#       result
#     else
#       result[,-2, drop = FALSE]
#   }
#   
#   # Liste zu Dataframe 
#   return_data_frame <- function(ans) {
#     ANS <- NULL
#     for (var in names(ans)) {
#       var_name <- ifelse(is.null(attr(X$Y_data[, var], "label")),
#                          var,
#                          attr(X$Y_data[, var], "label"))
#       n_var <- length(ans[[var]]$Characteristics) - 1
#       ans[[var]] <-
#         cbind(Item = c(var_name, rep("", n_var)), ans[[var]])
#       if (is.null(ANS)) {
#         ANS <- ans[[var]]
#       } else {
#         ANS <- rbind(ANS, ans[[var]])
#       }
#     }
#     ANS
#   }
#   
#   
#   # Start der Funktion 
#   X      <- Formula_Data(Formula, data, na.action = na.action)
#   N      <- nrow(data)
#   
#   
#   if (!is.logical(include.test)) {
#     if (include.test == "conTest")
#       useconTest <- TRUE
#     else if (include.test == "shapiro.test")
#       normality.test <- TRUE
#     else {
#       test_name <- include.test
#       useconTest <- TRUE
#     }
#     include.test <- TRUE
#   }
#   
#   if (is.null(type))  type <- X$type
#   if (is.null(digits.mean))  digits.mean <- X$digits
#   if (!is.null(X$condition)) {warning("errate_statistik2: condition weden noch nicht unterstuetzt")}
#   
#   # Beginn der Auswertung
#   if (is.null(include.all.n)) {
#     
#     if (is.null(X$X_data)) {
#       if (!any(is.na(X$Y_data)))
#         include.all.n <- FALSE
#       else
#         include.all.n <- TRUE
#     }
#     else{
#       if (!any(is.na(cbind(X$X_data, X$Y_data))))
#         include.all.n <- FALSE
#       else
#         include.all.n <- TRUE
#     }
#   }
#   if (order & (length(X$yname) > 1)) {
#     my_order <- order(
#       apply(X$Y_data, 2,
#             function(x) if (is.numeric(x) | is.factor(x)) mean2(x) else  0),
#       decreasing = decreasing)
#     X$Y_data <- X$Y_data[, my_order, drop = FALSE]
#   }
#   
#   # Einzelvergeich Pruefen ob Gruppe (also ~a+b+c oder a+b+c~d+e)
#   
#   if (is.null(X$xname)) {
#     index_zaeler <- 0
#     ANS <- return_data_frame(
#       lapply(X$Y_data, Stat_Mean_Freq))
#     if (include.test & !normality.test) {
#       mycorrtable <- Corr1(X$Y_data, nrow(ANS),
#                            corr_test, include.p, include.stars, cor_diagonale_up)
#       note <- paste("Korrelation nach" , Hmisc::upFirst(type))
#       if (nrow(ANS) != nrow(mycorrtable)) ANS <-  cbind(ANS, Error = "gemischtes Skalenniveau")
#       else ANS <- cbind(ANS, mycorrtable)
#     } else if (include.test & normality.test) {
#       ANS <- cbind(ANS,
#                    "shapiro test" = unlist(
#                      lapply(X$Y_data,
#                             function(x) {
#                               if (is.numeric(x)) {
#                                 APA(stats::shapiro.test(x))
#                               } else {
#                                 rbind(paste(
#                                   APA(
#                                     stats::shapiro.test(as.numeric(x)))
#                                   ,  class(x)),
#                                   rep("", nlevels(x) - 1))
#                               }})))
#     } else {NULL}
#     ANS <- prepare_output(ANS, caption, note, N)
#     return(ANS)
#     
#     # GRUPPENVERGLEICH
#   } else {
#     ANS_list <- list() # antwortliste
#     for (ix in X$xname) {
#       ANS <- NULL
#       #  Mehere Gruppenvariablen aufschluesseln
#       caption <- paste(ix, caption)
#       Xi <- X$X_data[, ix]  # Gruppe ist X'
#       x_name <- ifelse(is.null(attr(X$X_data, "label")), ix, attr(X$X_data, "label")) 
#       y_name <-  sapply(X$xname, function(y)
#         ifelse(is.null(attr(X$Y_data, "label")),
#                y, attr(X$Y_data, "label")))
#       my_levels <- levels(Xi)
#       # Test ob Gruppen cat("\n\nAchtung Gruppe ist kein Factor!\n\n")
#       if (is.null(my_levels)) {
#         #  Gruppe ist Numeric also Correlation
#         if (corr_test %in% c("pearson", "spearman")) {
#           note <- paste(note, "Korrelation nach", Hmisc::upFirst(corr_test))
#           ANS <- Corr2(X$Y_data, Xi, corr_test, include.stars)
#           ANS[, 1] <- rownames(ANS)
#           colnames(ANS)[1] <- x_name
#           ANS <-
#             if (include.test)
#               ANS[, c(1, 2, 6)]
#           else
#             ANS[, c(1, 2, 5)]
#         }
#       } else{
#         # Gruppe ist Faktor  also Freq oder Mean
#         Xi <- factor(Xi)
#         # sicherstellen das keine leeren Faktorstufen esistieren
#         tabel_header <-
#           if (include.header.n)
#             paste0(names(table(Xi)), " (n=", table(Xi), ")")
#         else
#           names(table(Xi))
#         my_levels <- levels(Xi)
#         # alle Faktor-Stufen Auswerten mean/Freq
#         for ( lev in seq_len(length(my_levels)) ) {
#           index_zaeler <- 0
#           my_subset <- which(Xi == my_levels[lev])
#           ans <- return_data_frame(lapply(X$Y_data[my_subset, , drop = FALSE], Stat_Mean_Freq))
#           
#           colnames(ans)[include.all.n + 3] <- tabel_header[lev]
#           if (is.null(ANS))
#             ANS <- ans
#           else if (include.all.n)
#             ANS <- cbind(ANS, ans[,-c(1:2)])
#           else
#             ANS <- cbind(ANS, ans[3])
#         }
#         
#         if (include.total | include.n) {
#           Total <-
#             errate_statistik2(
#               Formula = formula(paste0(
#                 "~", paste(X$yname, collapse = "+")
#               )),
#               data = X$Y_data,
#               type = type,
#               include.test = FALSE,
#               include.all.n = TRUE,
#               include.header.n = FALSE,
#               include.total = FALSE,
#               max_factor_length = max_factor_length
#             )
#           
#           nncol <- ncol(Total)
#           names(Total)[c(nncol - 1, nncol)] <- c("N", "Total")
#           names_ans <- names(ANS)
#           
#           if (include.total) {
#             if (include.all.n | include.n) {
#               ANS  <-  cbind(ANS[1:2],
#                              Total[c(nncol - 1, nncol)],
#                              ANS[3:ncol(ANS)])
#               names(ANS)[-c(1:4)] <- names_ans[-c(1:2)]
#             }
#             else{
#               ANS  <-  cbind(ANS[1:2],
#                              Total[nncol],
#                              ANS[3:ncol(ANS)])
#               names(ANS)[-c(1:3)] <- names_ans[-c(1:2)]
#             }
#           }
#           else{
#             ANS <- cbind(ANS[1:2], N = Total[, nncol - 1], ANS[3:ncol(ANS)])
#             names(ANS)[-c(1:3)] <- names_ans[-c(1:2)]
#           }
#         }
#         
#         if (include.test) {
#           inference_test_result <- c()
#           for (y in X$yname) {
#             fm_aov <- formula(paste(y, "~", ix))
#             fm_xtab <- formula(paste("~", ix, "+", y))
#             
#             if (is.factor(X$Y_data[, y])) {
#               if (useconTest) {
#                 X$Y_data[, y] <- as.numeric(X$Y_data[, y])
#                 cctest       <-
#                   conTest(fm_aov, cbind(X$X_data, X$Y_data), test_name)
#               } else{
#                 cctest    <- catTest(fm_xtab, cbind(X$X_data, X$Y_data))
#               }
#               
#               inference_test_result <-
#                 c(inference_test_result,
#                   cctest,
#                   rep("", nlevels(data[, y]) - 1))
#             } else{
#               # Zielvariable Zahl
#               X$Y_data[, y] <- as.numeric(X$Y_data[, y])
#               data_aov   <- cbind(X$X_data, X$Y_data)
#               cctest     <- conTest(fm_aov, data_aov, test_name)
#               
#               inference_test_result <-
#                 c(inference_test_result, cctest)
#             }
#           }
#           ANS$sig.Test <- inference_test_result
#         }
#       }
#       ANS <- prepare_output(ANS, caption, note, N)
#       ANS_list[[ix]]  <-  (ANS)
#     }
#     return(ANS_list)
#   }
# }
# 
# 
# 
# 

# Prozent2APA -------------------------------------------------------------


# Prozent2APA <- function(x,
#                         exclude = NA,
#                         digits = 1,
#                         max_factor_length = 35,
#                         ...) {
#   Non_Factor_To_Factor <- function(x) {
#     if (is.logical(x)) {
#       x <- factor(x, c(TRUE, FALSE))
#     } else if (is.numeric(x)) {
#       if (is_all_0_1(x))
#         x <- factor(x, c(0, 1))
#       else{
#         x <- as.numeric(x)
#         xf <- factor(x)
#         if (nlevels(xf) > 7)
#           x <- cut(x, quantile(x, na.rm = TRUE))
#         else
#           x <- xf
#       }
#     } else
#       x <- rep(NA, length(x))
#     x
#   }
#   
#   if (!is.factor(x))
#     x <- Non_Factor_To_Factor(x)
#   
#   x_NA <- x  
#   x    <- na.omit(x)
#   n    <- length(x)
#   
#   if (n == 0) {
#     result <- ""
#     ans <- rep(NA, nlevels(x_NA))
#     names(ans) <- levels(x_NA)
#   } else {
#     if (is.null(exclude))
#       x <- x_NA
#     
#     
#     ans <- table(x, exclude = exclude)
#     
#     # seltener fall das sehr viele levels vorhanden sind
#     if (length(ans) > max_factor_length) {
#       naLev <- levels(x)[-(1:max_factor_length)]
#       Text("NA = ", paste(naLev, collapse = ", "))
#       
#       x <- factor(x, levels(x)[1:max_factor_length], exclude = NULL)
#       x <-
#         addNA(x)  #- addNA modifies a factor by turning NA into an extra level
#       n <- length(x)
#       ans <- table(x)
#     }
#     
#     result <-
#       rndr_percent(prop.table(ans) * 100, ans,  digits = digits)
#   }
#   
#   data.frame(
#     Characteristics = names(ans),
#     n = c(n, rep("", length(ans) - 1)),
#     Statistics = result,
#     stringsAsFactors = FALSE
#   )
# }
# 

# Describe2 ---------------------------------------------------------------


# Describe2 <- function(...,
#                       output = FALSE) {
#   UseMethod("Describe2")
# }
# 
# Describe2.data.frame <- function(data,
#                                  ...,
#                                  by = NULL,
#                                  caption = "",
#                                  note = "",
#                                  stat = c("n", "mean", "sd", "min", "max"),
#                                  output = which_output(),
#                                  digits = 2) {
#   
#   measure <-
#     sapply(lazyeval::lazy_dots(...), function(x)
#       as.character(x[1]))
#   
#   if(length( measure)==0) measure<-names(data)
#   cat("\n Noch nicht getestet!\n")
#   
#   Describe2.formula(
#     formula(paste("~",
#                   paste(
#                     measure, collapse = "+"
#                   ))),
#     data = data,
#     by = by,
#     caption = caption,
#     note = note,
#     stat = stat,
#     output = output,
#     digits = digits
#   )
#   
#   
# }
# 
# Describe2.formula <- function(x,
#                               data,
#                               by = NULL,
#                               caption = "",
#                               note = "",
#                               stat = c("n", "mean", "sd", "min", "max"),
#                               output = which_output(),
#                               digits = 2,
#                               ...) {
#   vars <- which(names(data) %in% all.vars(x))
#   stat <- c(
#     "vars",
#     "n",
#     "mean",
#     "sd" ,
#     "median",
#     "trimmed",
#     "mad",
#     "min",
#     "max",
#     "range",
#     "skew",
#     "kurtosis",
#     "se" ,
#     stat
#   )
#   stat <- unique(stat[duplicated(stat)])
#   
#   if (is.null(by)) {
#     data <- data[vars]
#     result <-  as.data.frame(psych::describe(data),
#                              stringsAsFactors = FALSE)
#     
#     
#     which_class <- sapply(data, class)
#     result <- cbind(Item = GetLabelOrName(data),
#                     class = which_class,
#                     result)
#     
#     res <- result[c("Item", "class", stat)]
#     res[-1] <- Format2(res[-1], digits = digits)
#     
#     
#     
#   } else{
#     names_groups <- which(names(data) %in% all.vars(by))
#     groups <- data[names_groups]
#     if(ncol(groups)>1){
#       groups<- interaction(groups,  sep = " / ")
#     }
#     
#     data <- data[vars]
#     results_list <- psych::describeBy(data, groups)
#     result <- res <- NULL
#     
#     for (i in   names(results_list)) {
#       r1 <- as.data.frame(results_list[[i]],
#                           stringsAsFactors = FALSE)
#       
#       r1 <- cbind(Item = GetLabelOrName(data), Group = i, r1)
#       result <- rbind(result, r1)
#     }
#     res <- result[c("Item", "Group", stat)]
#     res[-c(1:3)] <- Format2(res[-c(1:3)], digits = digits)
#   }
#   
#   
#   
#   prepare_output(res,
#                  caption,
#                  note,
#                  nrow(data))
#   
# }
# 
# 
# 
# 
# 
  
# # regression output -------------------------------------------------------
  
# 
# 
# 
# ## @rdname APA_Table
# ## @description  APA_Table(..., type="long") ist ein Workaround von texreg
# ##
# ## include.pseudo = FALSE Preudo R
# ##
# ##  Cox und Snell R2: [ 0.2 = akzeptabel, 0.4 = gut ]
# ##
# ##  Nagelkerke R2: [ 0.2 = akzeptabel, 0.4 = gut, 0.5 = sehr gut]
# ##
# ##  McFaddens R2: [ 0.2 = akzeptabel, 0.4 = gut ]
# ##
# ## include.ftest = FALSE  noch nicht fertig
# ## include.loglik = FALSE noch nicht fertig
# ##
# ## include.CI=FALSE leicht unterschiedlich zu confint
# ## texreg berechnet über die SE und qnorm (Normal Distribution)
# ## confint bei lm über qt (student-T-Distribution)
# ##
# ##  z = qnorm(1 - ((1 - ci.level)/2))
# ##   coef + (z * se) und coef - (z * se)
# ##
# ##
# ## rgroup = c("Parameter", "Goodness of fit")
# ## col_names = c("b", "SE", "p")
# ## 
#  
# 
# # regression_output  <-
# #   function (fits,
# #             # Liste mit lm, glm, usw
# #             caption = "",
# #             note = "",
# #             custom.model.names = NULL,
# #             digits = 2,
# #             p.value = TRUE,
# #             # Sternchen oder p-Werte
# #             col_names = NULL,
# #             rgroup = c("Parameter", "Goodness of fit"),
# #             # Parameter Goodness of fit
# #             
# #             include.pseudo = FALSE,
# #             #Preudo R
# #             include.ftest = FALSE,
# #             # noch nicht fertig
# #             include.loglik = FALSE,
# #             # noch nicht fertig
# #             include.CI = FALSE,
# #             ...)
# #   {
# #     #cat("\ninclude.pseudo: ")
# #     #  print(include.pseudo)
# #     #  cat("\n")
# #     # Extract Parameter -------------------------------------------------------
# #     models <- texreg:::get.data(fits)
# #     gof.names <-
# #       texreg:::get.gof(models) #return:  gof.names[1] "R$^2$"      "Adj. R$^2$" "Num. obs."  "RMSE"
# #     models <- texreg:::correctDuplicateCoefNames(models)
# #     gofs <- texreg:::aggregate.matrix(
# #       models,
# #       gof.names,
# #       custom.gof.names = NULL,
# #       digits = 2,
# #       returnobject = "gofs"
# #     )
# #     m <- texreg:::aggregate.matrix(
# #       models,
# #       gof.names,
# #       custom.gof.names = NULL,
# #       digits = 2,
# #       returnobject = "m"
# #     )
# #     m <- texreg:::rearrangeMatrix(m)
# #     
# #     
# #     if (include.CI) {
# #       
# #       #Das mit den CIs noch ändern
# #       
# #       
# #       models2 <- texreg:::get.data(fits)
# #       #models2 <-
# #       #texreg:::ciforce(models2, ci.force = TRUE, ci.level = 0.95)
# #       #Kopie ciforce ohne die Fehlerprüfung
# #       ci.level<- .95
# #       note <- "95%-CI based on asymptotic normality"
# #       for (i in  seq_len(length(models2))) {
# #         if (length(models2[[i]]@se) > 0) {
# #           z <- qnorm(1 - ((1 - ci.level)/2))
# #           upper <- models2[[i]]@coef + (z * models2[[i]]@se)
# #           lower <- models2[[i]]@coef - (z * models2[[i]]@se)
# #           models2[[i]]@ci.low <- lower
# #           models2[[i]]@ci.up <- upper
# #           models2[[i]]@se <- numeric(0)
# #           models2[[i]]@pvalues <- numeric(0)
# #         }
# #       }
# #       
# #       models2 <- texreg:::correctDuplicateCoefNames(models2)
# #       
# #       m_cis <- texreg:::aggregate.matrix(
# #         models2,
# #         gof.names,
# #         custom.gof.names = NULL,
# #         digits = 2,
# #         returnobject = "m"
# #       )
# #       m_cis <- texreg:::rearrangeMatrix(m_cis)
# #       
# #     }
# #     
# #     
# #     
# #     #- fuer Output Zwi Ueberschriftenebenen
# #     modnames <- gsub("_",
# #                      " ",
# #                      texreg:::modelnames(fits, models, custom.model.names))
# #     if (include.pseudo) {
# #       whichR2 <- sapply(fits, function(fitx) {
# #         if (any(class(fitx) %in% "lm")) {
# #           if (any(class(fitx) %in% "glm"))
# #             3 # Cox + Nagek
# #           else
# #             0
# #         } else
# #           2  # Magrinal + Cond
# #       })
# #       
# #       resR2 <- NULL
# #       
# #       if (any(whichR2 == 2)) {
# #         for (i in fits) {
# #           if (any(class(i) %in% "lm"))
# #             R2i <- c(NA, NA)
# #           else
# #             R2i <- R2(i)
# #           names(R2i) <-
# #             c("Pseudo R2 (Marginal)", "Pseudo R2 (Conditional)")
# #           if (is.null(resR2))
# #             resR2 <- R2i
# #           else
# #             resR2 <- rbind(resR2, R2i)
# #         }
# #         gofs <- rbind(gofs, t(resR2))
# #       }
# #       
# #       if (any(whichR2 == 3)) {
# #         for (i in fits) {
# #           if (!any(class(i) %in% "glm"))
# #             R2i <- c(NA, NA, NA)
# #           else
# #             R2i <- R2(i)
# #           # McFadden's pseudo r-squared
# #           
# #           # r2ML Cox & Snell
# #           # Maximum likelihood pseudo r-squared
# #           
# #           # r2CU Nagelkerke
# #           # Cragg and Uhler's pseudo r-squared
# #           names(R2i) <- c("McFadden R2", "Cox & Snell R2", "Nagelkerke")
# #           if (is.null(resR2))
# #             resR2 <- R2i
# #           else
# #             resR2 <- rbind(resR2, R2i)
# #         }
# #         gofs <- rbind(gofs, t(resR2))
# #       }
# #     }
# #     
# #     
# #     # Gof ---------------------------------------------------------------------
# #     # sonderzeichen entfernen #"[^[:alnum:] :()]"[^[:alnum:]]
# #     rownames(gofs) <- gsub("[^[:alnum:] :().]", "", rownames(gofs))
# #     Numobs <-
# #       which(grepl("Num", rownames(gofs))) #  which(rownames(gofs)=="Numobs")
# #     
# #     if (length(fits) == 1) {
# #       gofs <- c(gofs[-Numobs, ], Num.obs = gofs[Numobs, ])
# #       gofs <- matrix(gofs, ncol = 1 , dimnames = list(names(gofs)))
# #     }
# #     else{
# #       gofs <- rbind(gofs[-Numobs, ], Num.obs = gofs[Numobs, ])
# #     }
# #     
# #     
# #     
# #     gofs[1:(nrow(gofs) - length(Numobs)), ] <- stp25rndr::Format2(gofs[1:(nrow(gofs) -
# #                                                                             length(Numobs)),], 2)
# #     
# #     # p-Werte -----------------------------------------------------------------
# #     est_vars <- seq(1, ncol(m), by = 3)
# #     se_vars  <- seq(2, ncol(m), by = 3)
# #     p_vars   <- seq(3, ncol(m), by = 3)
# #     
# #     p_stars  <- stp25rndr::rndr_Stars(m[, p_vars])
# #     p_val    <- stp25rndr::rndr_P(m[, p_vars])
# #     
# #     m[, c(est_vars, se_vars)] <-
# #       stp25rndr:::Format2.matrix(m[, c(est_vars, se_vars)], digits)
# #     
# #     if (include.CI) {
# #       ci_vars <- 2:3
# #       for (i in seq_len(length(est_vars))) {
# #         
# #         m[, se_vars[i]] <- rndr_CI(m_cis[, ci_vars], digits)
# #         ci_vars <- ci_vars + 3
# #       }
# #       if (is.null(col_names))
# #         col_names <- c("b", "95%-CI ", "p")
# #     } else{
# #       if (is.null(col_names))
# #         col_names <- c("b", "SE", "p")
# #     }
# #     
# #     
# #     
# #     
# #     # Sternchen
# #     if (p.value) {
# #       n_param <- 3
# #       m[, p_vars] <- p_val
# #       colnames(m) <-  c(t(
# #         outer(modnames, paste0("_", col_names), FUN=paste0)))
# #     }
# #     else{
# #       m[, est_vars] <- mapply(paste0, m[, est_vars], p_stars)
# #       n_param <- 2
# #       m <- m[, -p_vars]
# #       colnames(m) <- c(t(
# #         outer(modnames,
# #               paste0("_", col_names)[1:2],FUN=paste0)))
# #     }
# #     
# #     
# #     # Gofs --------------------------------------------------------------------
# #     ngofs <- nrow(gofs)
# #     emptygofs <- rep(NA, ngofs * (n_param - 1))
# #     newgofs <- rownames(gofs)
# #     
# #     for (i in seq_len(length(modnames)))
# #       gofs <- append(gofs, emptygofs, after = ngofs * (1 + n_param * (i -
# #                                                                         1)))
# #     
# #     gofs <- matrix(gofs , nrow = ngofs)
# #     rownames(gofs) <- newgofs
# #     
# #     result <- prepare_output(fix_to_data_frame(rbind(m, gofs)),
# #                              caption , note)
# #     
# #     Output(result, rgroup = rgroup, n.rgroup = nrow(m))
# #     invisible(result)
# #   }
# 
# 
# 
# 
# 
# 
# # berecne -----------------------------------------------------------------
# 
# # roxygen   16-10-2018
# 
# # berechneMean <- function(data = NULL,
# #                          measurevar,
# #                          by = NULL,
# #                          na.rm = TRUE,
# #                          conf.interval = .95,
# #                          .drop = TRUE) {
# #   Text("berechneMean: Achtung die Funktion wird bals geloescht!")
# #   # This does the summary. For each group's data frame, return a vector with
# #   # N, mean, and sd
# #   if (length(measurevar) != 1)
# #     return(measurevar)
# #   
# #   datac <- plyr::ddply(
# #     data,
# #     by,
# #     .fun = function(xx, col) {
# #       c(
# #         variable = NA,
# #         N    = length2(xx[[col]], na.rm = na.rm),
# #         mean = mean   (xx[[col]], na.rm = na.rm),
# #         sd   = sd     (xx[[col]], na.rm = na.rm),
# #         min  = min    (xx[[col]], na.rm = na.rm),
# #         max  = max    (xx[[col]], na.rm = na.rm)
# #       )
# #     },
# #     measurevar,
# #     .drop = .drop
# #   )
# #   
# #   # Rename the "mean" column
# #   #  datac <- plyr::rename(datac, c("mean" = measurevar))
# #   datac$se <-
# #     datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
# #   
# #   # Confidence interval multiplier for standard error
# #   # Calculate t-statistic for confidence interval:
# #   # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
# #   ciMult <- qt(conf.interval / 2 + .5, datac$N - 1)
# #   datac$ci <- datac$se * ciMult
# #   datac$ci.low <-    datac$mean - datac$ci
# #   datac$ci.hig <-    datac$mean + datac$ci
# #   datac$variable <- GetLabelOrName(data[measurevar])
# #   return(datac)
# # }
# 
# #  #   berechne: berechne Mittelwerte
# #  ##
# #  #   Die Lagemasse werden ueber die Standard-Funktionen berechnet unterschied ist nur dass
# #  #   Faktoren zu Zahlen transformiert werden und das \code{na.rm=TRUE} gesetzt wird.
# #  #   CI = Hmisc::smean.cl.normal
# #  ##
# #  ##
# #  #   @return  ein dataframe Objekt oder ein Character-String
# #  #   @param ... alles weitere
# #  #   @export
# #   
# #   berechne <- function(...) {
# #     UseMethod("berechne")
# #   }
# #   
# #  #   @rdname berechne
# #  #   @param na.rm NAs
# #  #   @param conf.interval,ci Grenzen der Konfidenzintervalle CIs
# #  #   @param .drop anplyr::ddply
# #  #   @export
# #  ##
# #  #   @examples
# #  #   # erlaubt:  varana %>% berechne(4, 5, by= ~geschl )
# #  #   #  berechne(hyper, "chol0" )
# #  #   #  names(hyper)
# #  #   #  hyper %>% berechne(chol0,chol1,chol6,chol12, by=~med+g)
# #   berechne.data.frame <- function(data,
# #                                   ...,
# #                                   by = "1",
# #                                   type = 1,
# #                                   na.rm = TRUE,
# #                                   conf.interval = .95,
# #                                   .drop = TRUE) {
# #     measure <-
# #       sapply(lazyeval::lazy_dots(...), function(x) {
# #         as.character(x[1])
# #       })
# #     
# #     meAsNum <- grep("^[[:digit:]]", measure)
# #     if (length(meAsNum) != 0) {
# #       measure[meAsNum] <- names(data[as.numeric(measure[meAsNum])])
# #     }
# #     
# #     if (is_formula2(by))
# #       by <- all.vars(by)
# #     
# #     
# #     res <- NULL
# #     for (i in measure) {
# #       res <- rbind(
# #         res,
# #         berechneMean(
# #           data,
# #           i,
# #           by,
# #           na.rm = na.rm,
# #           conf.interval = conf.interval,
# #           .drop = .drop
# #         )
# #       )
# #     }
# #     res$variable <- factor(res$variable, unique(res$variable))
# #     
# #     res
# #   }
# # library(psycho) ---------------------------------------------------------
# 
# 
# #  #  APA2
# #  ##
# #  #  @param x lm object.
# #  #  @param include.ci Confidence interval
# #  #  @param include.effect Text zu Effect_Size
# #  #  @export
# #  ##
# #  #  @examples
# #  ##
# #  #  library(psycho)
# #  ##
# #  ##
# #  #   df <- psycho::affective  # Load a dataset from the psycho package
# #  #   #df <- standardize(df)  # Standardize all numeric variables
# #  ##
# #  #   fit <- lm(Age ~ Salary, data=df)  # Fit a Bayesian linear model
# #  #   # results <- analyze(fit)  # Format the output
# #  #   #APA2(results )
# #  ##
# #  ##
# #  ##
# #  #   library(lmerTest)
# #  #   fit <- lmerTest::lmer(Sepal.Length ~ Sepal.Width + (1|Species), data=iris)
# #  ##
# #  #   #results <- analyze(fit)
# #  #   #APA2(results)
# #  APA2.psychobject <- function(x,
# #                               caption = "",
# #                               note = NULL,
# #                               # paste("contrasts: ", paste(options()$contrasts, collapse=", ")),
# #                               include.ci = FALSE,
# #                               include.effect = FALSE,
# #                               output = stp25output::which_output(),
# #                               ...) {
# #    # class(x)
# #    
# #    res <-
# #      fix_format(summary(x),
# #                 pattern_pval = "p",
# #                 pattern_est = c("SE", "SE.std"))
# #    
# #    if (!include.ci) {
# #      ci <- which(names(res) %in% c("CI_lower", "CI_higher"))
# #      res <- res[-ci]
# #      
# #    }
# #    
# #    if (!include.effect) {
# #      eff <- which(names(res) == "Effect_Size")
# #      res <- res[-eff]
# #      
# #    }
# #    if (is.null(note)) {
# #      r2s <- x$values$model
# #      note <- ""
# #      for (i in names(r2s)) {
# #        note <- paste(note, i, "=", rndr_r(r2s[[i]], FALSE))
# #        if (names(r2s)[1] == i)
# #          note <- paste0(note, ",")
# #      }
# #      note
# #    }
# #    res <-  prepare_output(res, caption, note)
# #    
# #    Output(res, output = output)
# #    invisible(res)
# #  }
# #  
# #  
# #  #  @rdname APA2
# #  #  @export
# #  ##
# #  APA2.psychobject <- function(...) {
# #    Output(...)
# #  }
# #  
# #  
# #  
# #  #  @rdname APA
# #  #  @export
# #  APA2.psychobject <- function(x, ...) {
# #    x$text
# #  }



# is_irgendwas ------------------------------------------------------------


# is_  Irgenwas
# 
# Prueft ob objekt bestimmte Eigenschaften aufweist.
# Fuer Dataframe gibt es \code{is_all_identical2()}
# @param x zu pruefendes Objekt
# @return Die \code{is_all_} gibt generel einen Wert zurueck die \code{is_} einen Vector in gleicher Laenge wie der Input-Vector.
# @examples
#  isFALSE(TRUE)
#   x<-c(F, T, F, F)
#   is_false2(x)
#   is_all_logical(x)
#   is_all_0_1(x)
#   is_all_identical2(data.frame(y=1:3, x=factor(c("a", "b", "c"))))
# is_irgendwas<- function(x) !is.null(x)

# @rdname is_irgendwas
# @export
# @description  is_formula2 Prueft ob es eine Formel ist
# @examples
# is_formula2(a~b+v)
#is_formula2<- function (x)
#  inherits(x, "formula")

# @rdname is_irgendwas
# @description is_empty2 wird in prepare_data genutzt als test ob ein Elemen  leer ist
# @export
# @examples
#  is_empty2(c("freq", "mean"))
#  is_empty2("freq")
# is_empty2 <- function (x) {
#   # print(x)
#   if (length(x) == 0)
#     TRUE
#   else if (length(x) == 1) {
#     if (is.null(x))
#       TRUE
#     else if (is.na(x))
#       TRUE
#     else if (x == "")
#       TRUE
#     else FALSE
#   }
#   else
#     FALSE
# }


# @rdname is_irgendwas
# @export
# is_all_dichotom<- function(x){
#   if(is_all_logical(x) | is_all_0_1(x)) TRUE
#   else{
#     if (ncol(x) < 2) { nlevels(x)==2 }
#     else{ all(sapply(x, nlevels)==2) }}
# }


# @rdname is_irgendwas
# @description is_all_logical is_all_0_1 prufen beide Logical aber is_all_dichotom  kann auch ja/nein
# @export
# is_all_logical <- function(x){
#   if (length(x)<=0) FALSE  #-- fuer Melt2
#   else if(is.null(x)) FALSE
#   else all(sapply(x, is.logical))
# }


# @rdname is_irgendwas
# @export
# is_all_0_1 <- function(x)  {
#   is_0_1<- function(z){
#     z <- factor(z)
#     if (nlevels(z) > 2)
#       FALSE
#     else if (nlevels(z) == 2 & all(levels(z) == 0:1))
#       TRUE
#     else if (nlevels(z) == 1 & levels(z)[1] == 0)
#       TRUE
#     else if (nlevels(z) == 1 & levels(z)[1] == 1)
#       TRUE
#     else
#       FALSE
#   }
#   if (length(x)<=0) FALSE  #-- fuer Melt2
#   else if(is.null(x)) FALSE
#   else if(is.data.frame(x)) all(sapply(x, is_0_1 ))
#   else if(is.vector(x))  is_0_1(x)
#   else     FALSE # class(x)
# }


# @rdname is_irgendwas
# @description isFALSE analog wie if(x){...} es gibt aber noch base::isFALSE welches leere Werte ignoriert 
#isFALSE <- function(x){identical(FALSE, x )}


# @rdname is_irgendwas
# @description is_false2 arbeitet mit isFALSE geht aber auch fuer Matris oder Data.frames
# @export
#is_false2<- function(x) sapply(x, identical(FALSE, x ))



# @rdname is_irgendwas
# @description is_all_identical2 oder all_identical2 wird in PCA und ranking verwendet
# @export
# all_identical2 <- function(x) {
#   if (ncol(x) < 2) {
#     TRUE
#   }
#   else{
#     xs <-
#       sapply(x, function(xx)
#         if (is.numeric(xx))
#           "numeric"
#         else if (is.factor(xx))
#           "factor"
#         else
#           NA)
#     if (length(xs) <= 1)
#       return(TRUE)
#     for (i in seq(2, length(xs))) {
#       if (!identical(xs[[1]], xs[[i]]))
#         return(FALSE)
#     }
#     TRUE
#   }
# }


# @rdname is_irgendwas
# @export
#is_all_identical2 <- function(x) all_identical2(x)




# @rdname is_irgendwas
# @param data Daten wenn Formeln gepruft werden
# @description is_vars_in_data Prueft ob ded data.frame auch die Fariablen enthaelt.
# @export
# is_vars_in_data<- function(x, data=NULL){
#   
#   if(length(data)==0) return(FALSE)
#   if(is_formula(x))  {
#     x<- all.vars(x)
#     if( any(x==".") ) x <- x[ -which(x==".") ]
#   }
#   
#   if(length(x)>0) return(all(is.element(x, names(data))))
#   else return(TRUE)
# }
