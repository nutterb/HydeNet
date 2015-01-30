#' @rdname writeJagsFormula
#' @export writeJagsFormula.multinom
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
           else paste0(coef(fit)[r, 1], " + ", 
                       paste(coef(fit)[r, -1], fm, sep="*", collapse=" + "))})
  
  fm <- sapply(fm, function(x) paste0("exp(", x, ") / (1 + ", 
                                      paste(sapply(fm, function(x) paste0("exp(", x, ")")), collapse=" + "),
                                      ")"))
  fm <- c(paste0("1 - (", paste(fm, collapse=" + "), ")"), fm)
  fm <- paste0(out_fm, "[", 1:length(fm), "] <- ", fm, collapse="; ")
        
  return(fm) 
}
