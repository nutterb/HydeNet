#' @name TranslateFormula
#' @importFrom stringr str_trim
#' 
#' @title Translate R Formula to JAGS
#' @description While most functions available in JAGS have equivalents in R,
#'   they don't always use the exact same names.  R formulas are converted
#'   to character strings, function names translated, and the corresponding
#'   JAGS formula is returned.
#'   
#' @param f R formula object
#' 
#' @details Only a limited subset of R functions are recognized here, but no 
#'   attempt is made to restrict the user to functions that will be recognized
#'   by JAGS.  For now, the user should remain aware of what functions
#'   are available in JAGS and only use the corresponding functions in R.
#'   The JAGS functions may be referenced in the JAGS user manual (see 
#'   References).  The corresponding R functions are listed in the 
#'   \code{jagsFunctions} data set (use \code{data(jagsFunctions)} to 
#'   review).
#'   
#' @author Jarrod Dalton and Benjamin Nutter
#' @references \url{http://people.stat.sc.edu/hansont/stat740/jags_user_manual.pdf}

rToJags <- function(f)
{
  f <- as.character(f)
  f <- unlist(strsplit(x = f, 
                       split = "[+]"))
  f <- trimws(f)
  
  #* Strip Surv 
  surv_factor <- grepl(pattern = "Surv[(]", 
                       x = f)
  f[surv_factor] <- sub(pattern = "^Surv[(]", 
                        replacement = "", 
                        x = f[surv_factor])
  f[surv_factor] <- sub(pattern = ",.+$", 
                        replacement = "", 
                        x = f[surv_factor])
  
  #* Easy translations
  f <- gsub(pattern = "acos[(]", 
            replacement = "arccos(", 
            x = f)
  f <- gsub(pattern = "acosh[(]", 
            replacement = "arccosh(", 
            x = f)
  f <- gsub(pattern = "asin[(]", 
            replacement = "arcsin(", 
            x = f)
  f <- gsub(pattern = "asinh[(]", 
            replacement = "arcsinh(", 
            x = f)
  f <- gsub(pattern = "atan[(]", 
            replacement = "arctan(", 
            x = f) 
  f <- gsub(pattern = "pnorm[(]", 
            replacement = "phi(", 
            x = f) 
  f <- gsub(pattern = "ceiling[(]", 
            replacement = "round(", 
            x = f) 
  f <- gsub(pattern = "floor[(]", 
            replacement = "trunc(", 
            x = f) 

  #* convert caret to pow()
  convertCaret <- function(x){
    if (grepl(pattern = "^", 
              x = x, 
              fixed=TRUE))
    {
      x <- trimws(unlist(strsplit(x = x, 
                                  split = "^", 
                                  fixed=TRUE)))
      for (i in length(x):2)
      {
        x[i-1] <- paste0('pow(', x[i-1], ", ", x[i], ")")
        x <- x[1:(i-1)]
      }
      return(x)
    }
    else return(x)
  }
  f <- sapply(X = f, 
              FUN = convertCaret)
  
  #* Convert logit with inverse to ilogit
  convertLogit <- function(x)
  {
    x <- gsub(pattern = " ", 
              replacement = "", 
              x = x)
    if (grepl(pattern = "logit[(]", 
              x = x))
    {
      if (grepl(pattern = "inverse[=]T", 
                x = x))
      {
        x <- gsub(pattern = "(logit[(]|qlogis[(])", 
                  replacement = "ilogit(", 
                  x = x)
      }
      x <- gsub(pattern = ",[[:print:]]+[)]", 
                replacement = ")", 
                x = x)
      return(x)
    }
    else
    {
      return(x)
    }
  }
  f <- sapply(X = f, 
              FUN = convertLogit)
  
  return(paste(f[2], f[1], paste(f[-(1:2)], collapse=" + ")) )
}
