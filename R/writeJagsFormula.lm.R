#' @rdname writeJagsFormula
#' @export writeJagsFormula.lm
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
  fm <- {if (is.null(fm)) coef(fit)[1]
         else paste0(coef(fit)[1], " + ", paste(coef(fit)[-1], fm, sep="*", collapse=" + "))}
  out_fm <- paste0(out_fm, fm)
  rToJags(as.formula(out_fm))
}
