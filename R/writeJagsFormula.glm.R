#' @rdname writeJagsFormula
#' @export writeJagsFormula.glm
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
