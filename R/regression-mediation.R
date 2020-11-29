

# "Fri Nov 16 07:45:40 2018"

#'  Causal Mediation Analysis
#'
#'
#' Einfache Mediation und Moderationsanalyse mit Sobel-Test
#' Mediation ist gegeben wenn alle Modelle signifikant (1) und (4)
#'
#'   (1) Y~X
#'   (2) Y~X+M  (wenn hier M signifikant ist => partielle Mediation)
#'   (4) M~X
#'
#'   Moderation ist gegeben wenn die Interaktion (X:M) signifikant ist
#'
#'   (3) Y~ X + M + X:M
#'   
#'   Ein Beispiel mit Laavan findet sich unter
#'   https://paolotoffanin.wordpress.com/2017/05/06/multiple-mediator-analysis-with-lavaan/
#'
#' @param y_x   y~x formula oder lm-Objekt
#' @param y_xm   y~x+m formula oder lm-Objekt
#' @param m_x   m~x formula oder lm-Objekt
#' @param treat,mediator   Einflussvariable und Moderator  
#' @param caption,note,output an Output
#' @param y_inter_xm m~x*m formula oder lm-Objekt
#' @param data Daten bei der Verwendendung von Formeln
#' @param include.parameter,include.sobl was soll Ausgegeben werden
#' @param z.transform Transformation bei der Verwendendung von Formeln
#' @param type,include.p,...   fuer Regression APA_Tabelle()
#'
#' @return list(param, sobel, methode)
#' @export
#' @examples
#'#' 
#' ## https://paolotoffanin.wordpress.com/2017/05/06/multiple-mediator-analysis-with-lavaan/
#' ## simpleMediation
#' #Projekt("html", "Test")
#' set.seed(1234)
#' Motivation <- rnorm(100)
#' Lerndauer <- 0.5 * Motivation + rnorm(100)
#' Note <- 0.7 * Lerndauer + rnorm(100)
#' dat <- data.frame(Note = Note,
#'                   Lerndauer = Lerndauer,
#'                   Motivation = Motivation)
#' 
#' dat <- stp25aggregate::Label(dat,
#'                              Note = "Note (Ziel-Variable)" ,
#'                              Lerndauer = "Lerndauer (Mediator)",
#'                              Motivation = "Motivation (Einfluss-Variable)")
#' Tabelle2(dat, Note, Lerndauer, Motivation)
#' Mediation(
#'   Note ~ Motivation,
#'   Note ~ Motivation + Lerndauer,
#'   Lerndauer ~ Motivation,
#'   Note ~ Motivation * Lerndauer,
#'   dat,
#'   digits = 2,
#'   caption = "Einfache Mediation und Moderationsanalyse mit Sobel-Test"
#' )
#' 
#' model <- ' # direct effect
#' Note ~ c*Motivation
#' # mediator
#' Lerndauer ~ a*Motivation
#' Note ~ b*Lerndauer
#' # indirect effect (a*b)
#' indir := a*b
#' # total effect
#' total := c + (a*b)
#' '
#' 
#' #xyplot(Note ~ value |
#' #         variable,
#' #       melt2(dat, Motivation, Lerndauer, by =  ~ Note))
#' 
#' 
#' fit <-  lavaan::sem(model, data = dat)
#' APA2(fit)
#' #End()
Mediation <- function(y_x,
                      y_xm,
                      m_x,
                      y_inter_xm = NULL,
                      data = NULL,
                      caption = "",
                      note = "",
                      output = which_output(),
                      include.parameter = TRUE,
                      include.p=TRUE,
                      include.sobl = TRUE,
                      z.transform = TRUE,
                      treat = model_info(m_x)$x[1],
                      mediator = model_info(m_x)$y,
                      type = "long",
                      ...) {
  if (inherits(y_x, "formula")) {
    data <- data[unique(c(
      all.vars(y_x),
      all.vars(y_xm),
      all.vars(m_x),
      all.vars(y_inter_xm)
    ))]
    
    if (z.transform) {
      data <- dapply1(data, function(x)
        scale(as.numeric(x)))
    }
    y_x <- lm(y_x, data)
    y_xm <- lm(y_xm, data)
    m_x <- lm(m_x, data)
    if (!is.null(y_inter_xm))
      y_inter_xm <- lm(y_inter_xm, data)
    
  }
  
  res <- list()
  
  if (!is.null(y_inter_xm))
    res$parameter <-
    APA_Table (
      y_x,
      y_xm,
      y_inter_xm,
      m_x,
      caption = caption,
      type = type,
      names = c("(1) Y~X", "(2) Y~X+M", "(3) Y~X*M" , "(4) M~X"),
      output = FALSE,
      include.p=include.p,
      ...
    )
  else
    res$parameter <-
    APA_Table (
      y_x,
      y_xm,
      m_x,
      caption = caption,
      type = type,
      names = c("(1) Y~X", "(2) Y~X+M", "(4) M~X"),
      output = FALSE,include.p=include.p,
      ...
    )
  
  
  res$sobel <-
    Sobel_Test(y_x, y_xm, m_x, treat, mediator)
  
  res$model <- paste0("treat = ", treat, ",  mediator = ", mediator)
  
  if (include.parameter)
    Output(res$parameter, caption = caption, output = output)
  if (include.sobl)
    Output(
      res$sobel,
      caption = "Sobel's Test for Mediation",
      note = res$model,
      output = output
    )
  
  invisible(res)
}


 
#' @rdname Mediation
#'
#' @param y_x,y_xm,m_x  in Sobel-Test lm-Objekte
#' @param treat,mediator Einfluss und  Mediatorals string
#' @param digits Nachkommastellen 
#'
#' @return data.frame
#' @export
#' @examples 
#' #' 
#' 
#' # Daten -------------------------------------------------------------------
#' 
#' 
#' set.seed(1234)
#' n <- 2 * 10
#' 
#' Motivation <- rnorm(n)
#' Lerndauer <- 0.5 * Motivation + rnorm(n)
#' Note <- 0.7 * Lerndauer + rnorm(n)
#' 
#' t0 <- data.frame(
#'   id = 1:n,
#'   sex = gl(2, n / 2, labels = c("m", "f")),
#'   time = 0,
#'   Note = Note,
#'   Lerndauer = Lerndauer,
#'   Motivation = Motivation
#' )
#' 
#' t1 <- rnorm(n, 0.1, 0.1)
#' t1 <- data.frame(
#'   id = 1:n,
#'   sex = gl(2, n / 2, labels = c("m", "f")),
#'   time = 1,
#'   Note = Note * .1 + t1,
#'   Lerndauer = Lerndauer * .1 + t1,
#'   Motivation = Motivation * .1 + t1
#' )
#' 
#' t2 <- rnorm(n, 0.2, 0.5)
#' t2 <- data.frame(
#'   id = 1:n,
#'   sex = gl(2, n / 2, labels = c("m", "f")),
#'   time = 2,
#'   Note = Note * .15 + t2  ,
#'   Lerndauer = Lerndauer * .15 + t2,
#'   Motivation = Motivation * .15 + t2
#' )
#' dat <- rbind(t0, t1, t2)
#' 
#' 
#' 
#' 
#' 
#' #model1 <- lm(Note ~ Motivation +time, dat)
#' #model2 <- lm(Note ~ Motivation + Lerndauer+time, dat)
#' #model3 <- lm(Lerndauer ~ Motivation+time, dat)
#' 
#' 
#' model1 <- lmerTest::lmer(Note ~ Motivation +time+ (1 | id), dat)
#' model2 <- lmerTest::lmer(Note ~ Motivation + Lerndauer +time+ (1 | id), dat)
#' model3 <- lmerTest::lmer(Lerndauer ~ Motivation +time+ (1 | id), dat)
#' 
#' 
#' Sobel_Test(model1, model2, model3,
#'            treat="Motivation",
#'            mediator="Lerndauer" )



Sobel_Test <- function(y_x, y_xm, m_x,
                       treat, mediator,
                       digits = 2) {
  
  if( (length(mediator)>1) | (length(treat)>1) ){
    stop()
  }

  mod1.out <- summary(y_x)$coef
  mod2.out <- summary(y_xm)$coef
  mod3.out <- summary(m_x)$coef
  
  pred <- which(rownames(mod3.out) == treat)
  med <- which(rownames(mod2.out) == mediator)
  
  

  indir <- mod3.out[pred, 1] * mod2.out[med, 1]
  
  effvar <- (mod3.out[pred, 1]) ^ 2 * (mod2.out[med, 2]) ^ 2 +
    (mod2.out[med, 1]) ^ 2 * (mod3.out[pred, 2]) ^ 2
  serr <- sqrt(effvar)
  zvalue = indir / serr
  
  data.frame(
    Source = "indir",
    B = Format2(indir, digits = digits),
    SE = Format2(serr, digits = digits),
    Z = rndr_Test_Statistic(zvalue),
    p.value = rndr_P(2 * pnorm(-abs(zvalue)), FALSE),
    row.names = "Sobel",
    stringsAsFactors = FALSE
  )
  
}

 