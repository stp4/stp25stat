
#' @rdname Tbll
#' 
#' @export
#'
#' @examples 
#'  
#'  
#' require(stpvers)
#' require(emmeans)
#' require(car)
#' 
#' set.seed(1)
#' 
#' 
#' auto.noise$power <- scale(auto.noise$noise) + scale(as.numeric(auto.noise$side)) +  rnorm(nrow(auto.noise), 0, .25)
#' auto.noise$power <-  (auto.noise$power -  min(auto.noise$power)) * 100 + 50
#' 
#' 
#' fit1 <- lm(noise ~ size +  type +  side, data = auto.noise)
#' fit2 <- lm(noise ~ size * side +  type * side +  side, data = auto.noise)
#' fit3 <- lm(noise ~ size * side +  type * side +   size * type * side, data = auto.noise)
#' fit4 <- lm(noise ~ size + power + type + side, data = auto.noise)
#' fit5 <- aov(noise ~ size +  type +  side, data = auto.noise)
#' fit6 <- (Anova(fit1))
#' 
#' a0 <- aov(noise ~ 1, data = auto.noise)
#' a1 <- aov(noise ~ size, data = auto.noise)
#' a2 <- aov(noise ~ size +  type +  side, data = auto.noise)
#' fit7<-anova(a0, a1, a2)
#' 
#' tbll_extract(fit1)
#' tbll_extract(aov(fit1))
#' tbll_extract(fit2)
#' tbll_extract(fit3)
#' tbll_extract(fit4)
#' tbll_extract(fit5)
#' tbll_extract(summary(fit5))
#' tbll_extract(fit6)
#' 
#' 
#' tbll_extract(fit7)
#' 
tbll_extract.lm <- function( x,                          
                             caption =NULL,
                             note = "", 
                             ...) {
  
 if (is.null(caption))
   caption <- paste0("Linear Model ",  " Obs: ", attr(model_info(x), "N"))
 
  rslt <- extract_param(x, ... )
  prepare_output(fix_format(rslt), caption=caption, note = note)

  # if (is.null(note))
  #   note <- attr(rslt, "note")
  
}


#' @rdname Tbll
#' @export
#'
tbll_extract.glm <- function( x,                          
                              caption =NULL,
                              note = "", 
                              ...) {
  
  if (is.null(caption)){
    x_attr<-model_info(x)
    caption <- paste0("Generalized Linear Model ", 
                     "(",  attr(x_attr, "family")[1], ") ", 
                     "Obs: ",  attr(x_attr, "N"))
    }
  
  rslt <- extract_param(x, ... )
  prepare_output(fix_format(rslt), caption=caption, note = note)
  
  
}