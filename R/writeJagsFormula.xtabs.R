#' @rdname writeJagsFormula
#' @export writeJagsFormula.xtabs
#' 

writeJagsFormula.xtabs <- function(fit, ...){
  fm <- attributes(fit)$call$formula
  out_fm <- paste(fm[2], fm[1])
  
  pi <- fit/sum(fit)
  names(pi) <- 1:length(pi)
  pi <- paste0("pi.", fm[2], "[", names(pi), "] <- ", pi, collapse="; ") 
 
  return(pi)
}