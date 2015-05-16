#' @name cpt
#' @export cpt
#' 
#' @title Compute a conditional probablity table for a factor given other factors
#' @description The function \code{cpt} operates on sets of factors. Specifically,
#'   it computes the conditional probability distribution of one of the factors
#'   given other factors, and stores the result in a multidimensional \code{array}. 
#'   
#' @param formula a formula with the following structure: \emph{response ~ var1 + var2 + ...}
#' @param data a data frame containing all the factors represented by the \code{formula}
#'   parameter.
#' @param wt (optional) a numeric vector of observation weights.
#'   
#' @author Jarrod Dalton and Benjamin Nutter
#' 
#' @examples
#' # a very imbalanced dice example
#' 
#' n <- 50000 
#' data <- data.frame(
#' di1=as.factor(1:6 %*% rmultinom(n,1,prob=c(.4,.3,.15,.10,.03,.02))),
#' di2=as.factor(1:6 %*% rmultinom(n,1,prob=rev(c(.4,.3,.15,.10,.03,.02)))),
#' di3=as.factor(1:6 %*% rmultinom(n,1,prob=c(.15,.10,.02,.3,.4,.03)))
#' )
#' cpt <- cpt(di3 ~ di1 + di2, data)
#' plyr::aaply(cpt, c(1,2), sum)
#'


cpt <- function(formula, data, wt){
  err.flag <- 0
  err.msg <- ""
  
  wrn.flag <- 0
  wrn.msg <- ""
  
  if(!"formula" %in% class(formula)){
    err.flag <- err.flag + 1
    err.msg <- c(err.msg, paste0(err.flag,": Object 'formula' must be of class 'formula'"))
  }
  variables <- as.character(attr(terms(formula), "variables"))[-1]
  dependentVar <- variables[1]
  independentVars <- variables[-1]
  
  if(!is.data.frame(data)){
    err.flag <- err.flag + 1
    err.msg <- c(err.msg, paste0(err.flag,": Object 'data' must be of class 'data.frame'"))
  }
  n <- nrow(data)
  
  missingVariables <- which(!variables %in% names(data))
  if(length(missingVariables)>0){
    tmp <- paste0("'",paste0(variables[missingVariables], collapse="', '"),"'")
    err.flag <- err.flag + 1
    err.msg <- c(err.msg,
                 paste0(err.flag,
                        ": These variables do not exist in the inputted data object: ",tmp,".")
    )
    if(err.flag) stop(paste(err.msg, collapse="\n"))
  }
  
  
  if(!all(unlist(lapply(data[,variables],function(x) "factor" %in% class(x))))){
    err.flag <- err.flag + 1
    err.msg <- c(err.msg, paste0(err.flag, ": All variables in 'formula' must be of class 'factor'"))
  }
  
  if(missing(wt)) wt <- rep(1,n) else {
    if(is.character(wt)){
      if(length(wt)>1){
        wrn.flag <- wrn.flag + 1
        wrn.msg <- c(wrn.msg,
                     paste0(wrn.flag, 
                            ": Character vector of length >1 given for 'wt'. Using only the first element.")
        )
        wt <- wt[1]
      }
      if(wt %in% names(data)){
        wt <- data[,"wt"]
      } else{
        err.flag <- err.flag + 1
        err.msg <- c(err.msg,
                     paste0(err.flag,
                            ": 'wt' must be a numeric vector or the name of a variable in 'data'")
        )
      }
    }
    
    if(!is.numeric(wt)){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg,
                   paste0(err.flag,
                          ": 'wt' must be a numeric vector or the name of a variable in 'data'")
      )
    } else if(length(wt) != n){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg,paste0(err.flag,": Length of 'wt' not equal to number of rows in 'data'"))
    }
  }
  
  if(wrn.flag) warning(paste(wrn.msg, collapse="\n"))
  if(err.flag)  stop(paste(err.msg, collapse="\n"))
  
  vars  <- c(dependentVar,independentVars)  
  data  <- data.frame(data[,vars], wt)  
  joint <- plyr::daply(data, vars, function(df) sum(df$wt), .drop_i=FALSE)
  cpt   <- plyr::aaply(joint, seq_along(vars)[-1], function(x) x/sum(x))
  
  names(dimnames(cpt))[length(c(dependentVar,independentVars))] <- dependentVar
  class(cpt) <- "cpt"
  
  return(cpt)
}
