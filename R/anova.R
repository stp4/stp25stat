# Analysis of Variance Model



#' Anova Tabelle
#'
#'
#' Anova mit car Anova
#' @param ... fits
#' @param caption,note,output,names output
#' @param include.eta,include.sumsq,include.meansq,include.df,include.fvalue Statistik
#' @param type ausgabe einzeln oder kompakt
#' @return list mit data.frame
#' @export
#'
#' @examples
#'
#' #' APA_Table
#'
#' #' require(stpvers)
#' require(emmeans)
#' require(car)
#' #Projekt()
#' set.seed(1)
#' #Head("auto.noise")
#'
#' auto.noise$power <-
#'   scale(auto.noise$noise) + scale(as.numeric(auto.noise$side)) +  rnorm(nrow(auto.noise), 0, .25)
#' auto.noise$power <-
#'   (auto.noise$power -  min(auto.noise$power)) * 100 + 50
#' summary(auto.noise)
#'
#' fit1 <- lm(noise ~ size +  type +  side, data = auto.noise)
#' fit2 <-
#'   lm(noise ~ size * side +  type * side +  side, data = auto.noise)
#' fit3 <- lm(noise ~ size * side +  type * side +   size * type * side, data = auto.noise)
#' fit4 <- lm(noise ~ size + power + type + side, data = auto.noise)
#'
#'
#' #fit4 <- aov(noise ~ size +  type +  side, data = auto.noise)
#' #APA_Table_Anova(Anova(fit1))
#'
#'
#' #Anova(fit4)
#'
#' #
#' # # Estimated marginal means
#' # emmeans(noise.lm, pairwise ~ size)
#' # # Interaction-style plots for estimated marginal means
#' # emmip(noise.lm, type ~ size | side)
#' #
#' # emmip(noise.lm, type ~ size | side)
#' #
#' # #post.hoc.test( )
#'
#'
#'
#'
#'
#' APA_Table_Anova(fit1, fit2, fit3,fit4,
#'                 include.eta = FALSE,
#'                 include.sumsq = FALSE ,include.fvalue = FALSE ,output=FALSE,
#'                 include.df= FALSE)  %>% stp25tools::list_to_df(last="Residuals" )
APA_Table_Anova  <-
  function(...,
           caption = "Anova",
           note = NULL,
           output = stp25output::which_output(),
           names = NULL,
           include.eta = TRUE,
           include.sumsq = TRUE ,
           include.meansq = FALSE,
           include.df = TRUE,
           include.fvalue = TRUE,
           type.anova = "II",
           #  test.statistic= NULL,#  "LR",# , "F")
           #  error,
           # type = c("II", "III", 2, 3),
           #  white.adjust = c(FALSE, TRUE, "hc3", "hc0", "hc1", "hc2", "hc4"),
           # vcov. = NULL,
           # singular.ok
           type = "wide")
  {
    fits <- list(...)
    if ("list"  %in% class(fits[[1]]))
      fits <- fits[[1]]
    
    rslt <- NULL
    n <- length(fits)
    
    if (is.null(names))
      names <-  paste("M", seq_len(n))
    
    if (n != 1)
      caption <- paste(caption, names)
    
    for (i in seq_len(n))  {
      fit <- fits[[i]]
      my_input <- model_info(fit)
      
      if (length(my_input$x) == 0) {
        rslt[[names[i]]] <- "Null-Model"
      }
      
      else{
        if (class(fit)[1] == "lm") {
          rslt[[names[i]]] <-
            APA2.anova(
              car::Anova(fit, type = type.anova),
              caption = caption[i],
              note = note,
              output = FALSE,
              include.eta = include.eta,
              include.sumsq = include.sumsq ,
              include.meansq = include.meansq,
              include.df = include.df,
              include.fvalue = include.fvalue
            )
        }
        else if (class(fit)[1] == "glm") {
          rslt[[names[i]]] <-
            APA2.anova(
              car::Anova(fit, type = type.anova),
              caption = caption[i],
              note = note,
              output = FALSE,
              include.eta = FALSE,
              include.sumsq = include.sumsq ,
              include.meansq = include.meansq,
              include.df = include.df,
              include.fvalue = include.fvalue
            )
        }
        else if (class(fit)[1] == "anova") {
          rslt[[names[i]]] <-
            tbll_extract.anova(
              fit,
              caption = caption[i],
              note = note,
              include.eta = include.eta,
              include.sumsq = include.sumsq ,
              include.meansq = include.meansq,
              include.df = include.df,
              include.fvalue = include.fvalue
            )
        }
        else if (class(fit)[1] == "aov") {
          rslt[[names[i]]] <-
            tbll_extract.aov(
              fit,
              type.anova = type.anova,
              caption = caption[i],
              note = note,
              include.eta = include.eta,
              include.sumsq = include.sumsq ,
              include.meansq = include.meansq,
              include.df = include.df,
              include.fvalue = include.fvalue
            )
        }
        else {
          rslt[[names[i]]] <- paste(class(fit), sep = ", ")
          warning(paste(class(fit), sep = ", "))
          
        }
      }
    }
    
    if (type == "wide")
      Output(rslt, output = output)
    else
      Output(stp25tools::list_to_df(rslt,
                                    last = "Residuals")
             , output = output)
    
    invisible(rslt)
  }


#
#
# APA_Table_Anova <- function(...,
#                             caption = "Anova",
#                             note = NULL,
#                             output = stp25output::which_output(),
#                             names = NULL,
#                             include.eta = TRUE,
#                             include.sumsq = TRUE ,
#                             include.meansq = FALSE,
#                             include.df = TRUE,
#                             include.fvalue=TRUE,
#                             type="wide")
# {
#   fits <- list(...)
#   rslt <- NULL
#   n <- length(fits)
#
#   if (is.null(names))
#     names <-  paste("M", seq_len(n))
#
#   if (n != 1)
#     caption <- paste(caption, names)
#
#   for (i in seq_len(n))  {
#     fit <- fits[[i]]
#     my_input <- model_info(fit)
#
#     if (length(my_input$x) == 0) {
#       rslt[[names[i]]] <- "Null-Model"
#     }
#
#     else{
#       if (class(fit)[1] == "lm") {
#         rslt[[names[i]]] <-  APA2.anova(
#           car::Anova(fit),
#           caption = caption[i],
#           note = note,
#           output = FALSE,
#           include.eta = include.eta,
#           include.sumsq = include.sumsq ,
#           include.meansq = include.meansq,
#           include.df = include.df,
#           include.fvalue=include.fvalue
#         )
#       }
#
#       else if (class(fit)[1] == "anova") {
#         rslt[[names[i]]] <-  APA2.anova(
#           fit,
#           caption = caption[i],
#           note = note,
#           output = FALSE,
#           include.eta = include.eta,
#           include.sumsq = include.sumsq ,
#           include.meansq = include.meansq,
#           include.df = include.df,
#           include.fvalue=include.fvalue
#         )
#
#       }
#       else if (class(fit)[1] == "aov") {
#         rslt[[names[i]]] <-  APA2.anova(
#           car::Anova(fit),
#           caption = caption[i],
#           note = note,
#           output = FALSE,
#           include.eta = include.eta,
#           include.sumsq = include.sumsq ,
#           include.meansq = include.meansq,
#           include.df = include.df,
#           include.fvalue=include.fvalue
#         )
#
#       }
#       else {
#         rslt[[names[i]]] <- paste(class(fit), sep = ", ")
#         warning(paste(class(fit), sep = ", "))
#
#       }
#     }
#   }
#
#   if(type=="wide") Output(rslt, output=output)
#   else   Output(stp25tools::list_to_df(rslt,
#                            last="Residuals")
#                 , output=output)
#
#   invisible(rslt)
# }



#' @rdname Tbll
#' @export
#'
tbll_extract.aov <- function(x,
                             type.anova = "II",
                             ...) {
  tbll_extract.anova(
    car::Anova(x, type = type.anova), 
    ...)
}



#' @rdname Tbll
#' @export
#'
tbll_extract.anova <- function(x,
                               caption =  attr(x, "heading")[1],
                               note = NULL,
                               include.eta = FALSE,
                               include.sumsq = TRUE,
                               include.meansq = FALSE,
                               include.df = TRUE,
                               include.fvalue = TRUE,
                               ...) {
  #info <- model_info(x)
 # cat("\ntbll_extract.anova\n")
  #print(class(x))
 # print(attributes(x))
  if (grepl( "Anova" ,attr(x, "heading")[1]) ) {
   # cat("\ncar::Anova\n")
    # rslt <- fix_format(broom::tidy(x))
    # names(rslt)[4]<- "F.value"
    rslt <- x # wegen eta2
    nms <- gsub(" ", ".", names(x))
    nms[grep('>', nms)] <- "p.value"
    names(rslt) <- nms
    rslt <-   tibble::as_tibble(cbind("term" = rownames(rslt), rslt))
    
    if (include.eta)
      rslt <-
      tibble::as_tibble(cbind(rslt[-ncol(rslt)],
                              etaSquared2(x),
                              rslt[ncol(rslt)]))
    
    if (include.meansq)
      rslt <-
      tibble::as_tibble(cbind(rslt[1],
                              Mean.Sq = rslt$Sum.Sq / rslt$df,
                              rslt[-1]))
    
    if (!include.sumsq)
      rslt <- rslt[-which(names(rslt) == "Sum.Sq")]
    if (!include.df)
      rslt <- rslt[-which(names(rslt) == "Df")]
    if (!include.fvalue)
      rslt <- rslt[-which(names(rslt) == "F.value")]
    #
    # rslt[ ncol(rslt)] <- stp25rndr::rndr_P(rslt[ ncol(rslt)],
    #                                        include.symbol = FALSE)
    if (is.null(note))
      note <-  paste("contrasts: ", paste(options()$contrasts, collapse = ", "))
  }
  
  else{
    # "Analysis of Variance Table\n"
   # cat("\nanova(...) \n")
    rslt <- broom::tidy(x)
    rslt<- cbind(model=row.names(rslt), 
                 rslt)
    if (is.null(note))
      note <- attr(x, "heading")[2]
  }
  
  prepare_output(fix_format(rslt), caption, note = note)
  
}


#' @rdname Tbll
#' @export
#'
tbll_extract.summary.aov <- function(x,
                                     caption = "ANOVA",
                                     note = paste("contrasts: ", paste(options()$contrasts, collapse = ", ")),
                                     ...) {
  rslt <- stp25output::fix_format(broom::tidy(x[[1]]))
  prepare_output(rslt,
                 caption = caption,
                 note = note)
}

#' @rdname Tbll
#' @export
#'
tbll_extract.aovlist <- function(x,
                                 caption = "ANOVA",
                                 note = paste("contrasts: ", paste(options()$contrasts, collapse = ", ")),
                                 ...) {
  rslt <- summary(x)
  
  rslt1 <-  fix_data_frame2(rslt[[1]][[1]])
  rslt1 <- cbind(Source = rownames(rslt1), rslt1)
  
  rslt2 <- fix_data_frame2(rslt[[2]][[1]])
  rslt2 <- cbind(Source = rownames(rslt2), rslt2)
  
  list(
    rslt1 = prepare_output(rslt1, caption = names(rslt[1]), note = note),
    rslt2 = prepare_output(rslt2, caption = names(rslt[2]), note = note)
  )
  
  
}

#' @rdname APA_Table_Anova
#' @description APA2.anova() Ausgabe  alle anova
#' @export
#'
APA2.anova <- function(x,
                       caption = "ANOVA",
                       note = paste("contrasts: ", paste(options()$contrasts, collapse = ", ")),
                       output = stp25output::which_output(),
                       include.eta = TRUE,
                       include.sumsq = TRUE,
                       include.meansq = FALSE,
                       include.df = TRUE,
                       include.fvalue = TRUE,
                       ...) {
  rslt <-  tbll_extract.anova(
    x,
    caption = "caption",
    note = note,
    include.eta = include.eta,
    include.sumsq = include.sumsq,
    include.meansq = include.meansq,
    include.df = include.df,
    include.fvalue = include.fvalue
  )
  if (!is.logical(output))
    Output(rslt, output = output, ...)
  
  invisible(rslt)
  

}


#  info <- model_info(x)
# # rslt <-  broom::tidy(x)
#
# # names(rslt)[4]<- "F.value"
#  rslt <- x # wegen eta2
#  nms<- gsub(" ", ".", names(x))
#  nms[grep('>', nms)] <- "p.value"
#  names(rslt) <- nms
#  rslt <-   tibble::as_tibble(cbind("term"= rownames(rslt), rslt))
#
#
#  if(include.eta) rslt <-
#    tibble::as_tibble(cbind(rslt[-ncol(rslt)],
#                               etaSquared2(x),
#                               rslt[ncol(rslt)]))
#
#  if(include.meansq) rslt <-
#    tibble::as_tibble(cbind(rslt[1],
#                            Mean.Sq = rslt$Sum.Sq/rslt$df,
#                            rslt[-1] ))
#
#  if(!include.sumsq) rslt <- rslt[ -which(names(rslt)=="Sum.Sq") ]
#  if(!include.df) rslt <- rslt[ -which(names(rslt)=="Df") ]
#  if(!include.fvalue) rslt <- rslt[ -which(names(rslt)=="F.value") ]
# #
# # rslt[ ncol(rslt)] <- stp25rndr::rndr_P(rslt[ ncol(rslt)],
# #                                        include.symbol = FALSE)
#  rslt <- prepare_output(fix_format(rslt),
#                         caption,
#                         note,
#                         info$N,
#                         info$labels)

# if (!is.logical(output))
#   Output(rslt, output = output, ...)
#
# invisible(rslt)
# APA2.anova <- function(x,
#                        caption = gsub("\\n", "", paste(attr(x, "heading"), collapse =
#                                                          ", ")),
#                        note = paste("contrasts: ", paste(options()$contrasts, collapse =
#                                                            ", ")),
#                        output = stp25output::which_output(),
#                        include.eta = FALSE,
#                        include.sumsq = TRUE,
#                        include.meansq = FALSE,
#                        ...) {
#   res <-  extract(
#     x,
#     include.eta = include.eta,
#     include.sumsq = TRUE,
#     include.meansq = FALSE
#   )
#
#   if (include.meansq) {
#     res <-
#       cbind(res[1:2], meansq = res$sumsq / res$df, res[3:ncol(res)])
#   }
#   if (!include.sumsq)
#     res <- res[-2]
#
#   res <-  stp25output::fix_format(res)
#   stp25output::Output(res,
#                       caption = caption,
#                       note = note,
#                       output = output)
#   invisible(res)
#
# }


#' @rdname APA_Table_Anova
#' @description anova: APA2.aov(x, include.eta = TRUE)
#' @export
#' @examples
#'
#' #- ANOVA ---------
#' # op <- options(contrasts = c("contr.helmert", "contr.poly"))
#' # npk.aov <- aov(yield ~ block + N*P*K, npk)
#' #summary(npk.aov)
#' #coefficients(npk.aov)
#'
#' #APA2(npk.aov, include.eta = FALSE)
#'
APA2.aov <- function(x,
                     caption = "ANOVA",
                     note = paste("contrasts: ", paste(options()$contrasts, collapse = ", ")),
                     output = stp25output::which_output(),
                     #  names = NULL,
                     type.anova = "II",
                     include.eta = TRUE,
                     include.sumsq = TRUE,
                     include.meansq = FALSE,
                     include.df = TRUE,
                     include.fvalue = TRUE,
                     ...)
{
  rslt <- tbll_extract.aov(
    x,
    caption = "caption",
    note = note,
    type.anova = type.anova,
    include.eta = include.eta,
    include.sumsq = include.sumsq,
    include.meansq = include.meansq,
    include.df = include.df,
    include.fvalue = include.fvalue
  )
  if (!is.logical(output))
    Output(rslt, output = output, ...)
  
  invisible(rslt)
  
}

                     
                     # rslt <- APA_Table_Anova(x,
                     #          caption=caption,
                     #          note=note,
                     #          output =  output,
                     #          include.eta = include.eta,
                     #          include.sumsq = include.sumsq,
                     #          include.meansq = include.meansq,
                     #          include.df = include.df,
                     #          include.fvalue=include.fvalue,
                     #          type="wide")[[1]]
                     #
                     #
                     #  invisible(rslt)

# APA2.lm <- function(x,
#                      caption = "Fitting Linear Models",
#                      note =  "",
#                      output = stp25output::which_output(),
#                     include.b = TRUE,
#                     include.se = TRUE,
#                     include.beta = FALSE,
#                     include.ci = FALSE,
#                     include.statistic = TRUE,
#                     include.p = TRUE,
#                     include.stars = FALSE) {
#   #APA2.default -> extract.lm -> extract_param
#   APA2.default(x,
#                   caption,
#                   note,
#                   output =  output,
#                   include.b = include.b,
#                   include.se = include.se,
#                   include.beta = include.beta,
#                   include.ci = include.ci,
#                   include.statistic = include.statistic,
#                   include.p = include.p,
#                   include.stars = include.stars
#                   )
# }

#' @rdname APA_Table_Anova
#' @export
APA2.summary.aov <- function(x,
                             caption = "ANOVA",
                             note = paste("contrasts: ", paste(options()$contrasts, collapse = ", ")),
                             output = stp25output::which_output(),
                             ...) {
  rslt <- tbll_extract.summary.aov(x, caption = caption, note = note)
  

  if (!is.logical(output))
    Output(rslt, output = output, ...)
  
  invisible(rslt)
}

  # rslt <- stp25output::fix_format(broom::tidy(x[[1]]))
  # rslt <- prepare_output(cbind(Source = rownames(rslt), rslt),
  #                       caption = caption,
  #                       note = note)

#' @rdname APA_Table_Anova
#' @export
#' @examples
#'
#' #- One way repeated Measures ---------------------
#'
#' #datafilename="http://personality-project.org/r/datasets/R.appendix3.data"
#' #data.ex3=read.table(datafilename,header=T)   #read the data into a table
#'# #data.ex3                                      #show the data
#'
#' #aov.ex3 = aov(Recall~Valence+Error(Subject/Valence),data.ex3)
#' #
#' #APA2(aov.ex3)
#'
APA2.aovlist <- function(x,
                         caption = "ANOVA",
                         note = paste("contrasts: ", paste(options()$contrasts, collapse = ", ")),
                         output = stp25output::which_output(),
                         
                         ...) {
  rslt <- tbll_extract.aovlist(x, caption, note)
  if (!is.logical(output)) {
    Output(rslt[[1]], output = output, ...)
    Output(rslt[[2]], output = output, ...)
  }
  
  invisible(rslt)

}


  
  # rslt <- summary(x)
  #
  # rslt1 <-
  #   stp25output::fix_data_frame2(rslt[[1]][[1]])
  # rslt1 <- cbind(Source = rownames(rslt1), rslt1)
  # #APA_Table(npk.aov, type="anova")
  # stp25output::Output(rslt1 , caption = names(rslt[1]), output = output)
  #
  # rslt2 <- stp25output::fix_data_frame2(rslt[[2]][[1]])
  # rslt2 <- cbind(Source = rownames(rslt2), rslt2)
  # stp25output::Output(rslt2 , caption = names(rslt[2]), output = output)
  #
  # invisible(rslt
