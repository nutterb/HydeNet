#' @name writeJagsFormula
#' @export
#' 
#' @title Write the JAGS Formula for a Hyde Node
#' @description Based on the information provided about the node,
#'   an appropriate JAGS model is written in text.  This is combined with 
#'   the other node models to generate the complete network.
#'   
#' @param fit a model object
#' @param ... Additional arguments to be passed to other methods
#' 
#' @details Methods for different model objects can be written so that 
#'   this function can be extended as desired.
#'   
#'   In the \code{writeJagsFormula.glm} method, appropriate transformations
#'   exist for the following combinations:
#'   \enumerate{
#'     \item family = binomial; link = logit
#'     \item family = poisson; link = log
#'     \item family = gaussian; link = identity (calls \code{writeJagsFormula.lm})
#'   }
#'   
#' @author Jarrod Dalton and Benjamin Nutter

writeJagsFormula <- function(fit, ...) UseMethod("writeJagsFormula")

#' @rdname writeJagsFormula
#' @export
#' @importFrom stringr str_trim
#' 

writeJagsFormula.glm <- function(fit, ...){
  if (fit$family$family == "gaussian" & fit$family$link == "identity") 
    return(writeJagsFormula.lm(fit))
  
  fm <- as.character(fit$call$formula)
  out_fm <- paste(fm[2], fm[1])
  fm <- stringr::str_trim(unlist(strsplit(fm[-(1:2)], "[+]")))
  fm <- unlist(sapply(fm, function(x){
    if (! x %in% names(attributes(fit$terms)$dataClasses)){
      return(NULL)
    }
    if (attributes(fit$terms)$dataClasses[x] == "factor"){
      return(paste0("(", x, " == ", 2:nlevels(fit$model[, x]), ")"))
    }
    else return(x)
  }))
  fm <- {if (is.null(fm)) coef(fit)[1]
         else paste0(round(coef(fit)[1], getOption("Hyde_maxDigits")), 
                     " + ", 
                     paste(round(coef(fit)[-1], getOption("Hyde_maxDigits")), 
                           fm, sep="*", collapse=" + "))}
  
  #* Binomial Proportion
  if (fit$family$family == "binomial" & fit$family$link == "logit"){
    fm <- paste0("ilogit(", fm, ")")
  }
  
  #* Poisson Regression
  if (fit$family$family == "poisson" & fit$family$link == "log"){
    fm <- paste("exp(", fm, ")")  
  }
  
  out_fm <- paste0(out_fm, fm)
  rToJags(as.formula(out_fm)) 
}

#' @rdname writeJagsFormula
#' @export
#' @importFrom stringr str_trim
#' 

writeJagsFormula.lm <- function(fit, ...){
  fm <- as.character(fit$call$formula)
  out_fm <- paste(fm[2], fm[1])
  fm <- stringr::str_trim(unlist(strsplit(fm[-(1:2)], "[+]")))
  fm <- unlist(sapply(fm, function(x){
    if (! x %in% names(attributes(fit$terms)$dataClasses)){
      return(NULL)
    }
    if (attributes(fit$terms)$dataClasses[x] == "factor"){
      return(paste0("(", x, " == ", 2:nlevels(fit$model[, x]), ")"))
    }
    else return(x)
  }))
  fm <- {if (is.null(fm)) round(coef(fit)[1], getOption("Hyde_maxDigits"))
         else paste0(round(coef(fit)[1], getOption("Hyde_maxDigits")), 
                     " + ", 
                     paste(round(coef(fit)[-1], getOption("Hyde_maxDigits")), 
                           fm, sep="*", collapse=" + "))}
  out_fm <- paste0(out_fm, fm)
  rToJags(as.formula(out_fm))
}

#' @rdname writeJagsFormula
#' @export
#' @import nnet
#'

writeJagsFormula.multinom <- function(fit, ...){
  if (is.null(fit$model)) fit <- update(fit, model=TRUE)
  fm <- as.character(fit$call$formula)
  out_fm <- paste0("pi.", fm[2])
  fm <- stringr::str_trim(unlist(strsplit(fm[-(1:2)], "[+]")))
  
  fm
  
  fm <- unlist(sapply(fm, function(x){
    if (! x %in% names(attributes(fit$terms)$dataClasses)){
      return(NULL)
    }
    if (attributes(fit$terms)$dataClasses[x] == "factor"){
      return(paste0("(", x, " == ", 2:nlevels(fit$model[, x]), ")"))
    }
    else return(x)
  }))
  
  fm <- lapply(1:nrow(coef(fit)), 
               function(r){
                 if (is.null(fm)) coef(fit)[r, 1]
                 else paste0(round(coef(fit)[r, 1], getOption("Hyde_maxDigits")),
                             " + ", 
                             paste(round(coef(fit)[r, -1], getOption("Hyde_maxDigits")), 
                                   fm, sep="*", collapse=" + "))})
  
  fm <- sapply(fm, function(x) paste0("exp(", x, ") / (1 + ", 
                                      paste(sapply(fm, function(x) paste0("exp(", x, ")")), collapse=" + "),
                                      ")"))
  fm <- c(paste0("1 - (", paste(fm, collapse=" + "), ")"), fm)
  fm <- paste0(out_fm, "[", 1:length(fm), "] <- ", fm, collapse="; ")
  
  return(fm) 
}

#' @rdname writeJagsFormula
#' @export

writeJagsFormula.xtabs <- function(fit, ...){
  fm <- attributes(fit)$call$formula
  out_fm <- paste(fm[2], fm[1])
  
  pi <- fit/sum(fit)
  names(pi) <- 1:length(pi)
  pi <- paste0("pi.", 
               fm[2], 
               "[", 
               names(pi), 
               "] <- ", 
               round(pi, getOption("Hyde_maxDigits")), 
               collapse="; ")
  
  return(pi)
}