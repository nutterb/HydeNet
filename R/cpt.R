#' @name cpt
#' @export cpt
#' 
#' @title Compute a conditional probability table for a factor given other factors
#' @description The function \code{cpt} operates on sets of factors. Specifically,
#'   it computes the conditional probability distribution of one of the factors
#'   given other factors, and stores the result in a multidimensional \code{array}. 
#'   
#' @param vars either a formula or a list containing the names of the variables 
#'   used to compute the conditional probability table. See details.
#' @param data a data frame containing all the factors represented by the \code{formula}
#'   parameter.
#' @param wt (optional) a numeric vector of observation weights.
#' 
#' @details If a \code{formula} object is entered for the \code{vars} parameter, the
#'   formula must have the following structure: \emph{response ~ var1 + var2 + etc.}.
#'   The other option is to pass a named \code{list} containing two elements \code{y}
#'   and \code{x}. Element \code{y} is a character string containing the name of the 
#'   factor variable in \code{data} to be used as the dependent variable, and 
#'   element \code{x} is a character vector containing the name(s) of the factor
#'   variable(s) to be used as independent (or conditioning) variables.
#'   
#' @author Jarrod Dalton and Benjamin Nutter
#' 
#' @examples
#' # a very imbalanced dice example
#' 
#' n <- 50000 
#' data <- data.frame(
#'   di1 = as.factor(1:6 %*% rmultinom(n,1,prob=c(.4,.3,.15,.10,.03,.02))),
#'   di2 = as.factor(1:6 %*% rmultinom(n,1,prob=rev(c(.4,.3,.15,.10,.03,.02)))),
#'   di3 = as.factor(1:6 %*% rmultinom(n,1,prob=c(.15,.10,.02,.3,.4,.03)))
#' )
#' 
#' cpt1 <- cpt(di3 ~ di1 + di2, data)
#' cpt1[di1 = 1, di2 = 4, ]  # Pr(di3 | di1 = 1, di2 = 4)
#' cpt1["1","4",]
#' cpt1[1,4,]
#' 
#' plyr::aaply(cpt1, c(1,2), sum) # card(di1)*card(di2) matrix of ones
#' 
#' l <- list(y = "di3", x = c("di1","di2"))
#' all(cpt(l, data) == cpt1)


cpt <- function(vars, data, wt){
  err.flag <- 0
  err.msg <- ""
  
  wrn.flag <- 0
  wrn.msg <- ""
  
  if("formula" %in% class(vars)){
    
    variables       <- as.character(attr(terms(vars), "variables"))[-1]
    dependentVar    <- variables[1]
    independentVars <- variables[-1]
    
  } else if("list" %in% class(vars)){
    
    if(!all(c("y","x") %in% names(vars))){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg, paste0(err.flag,
                                   ": List object 'vars' must contain character vectors ",
                                   "'y' and 'x'. See help('cpt')."))
    }
    
    if(!all(unlist(lapply(vars,is.character)))){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg, paste0(err.flag,
                                   ": List object 'vars' must contain character vectors ",
                                   "'y' and 'x'. See help('cpt')."))
    }
    
    if(length(vars[["y"]]) != 1){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg, paste0(err.flag,
                                   ": Element 'y' of list object 'vars' must be a character ",
                                   "vector of length 1. See help('cpt')."))
    }
    
    variables       <- c(vars[["y"]], vars[["x"]])
    dependentVar    <- vars[["y"]]
    independentVars <- vars[["x"]]
    
  } else {
    
    err.flag <- err.flag + 1
    err.msg <- c(err.msg, paste0(err.flag,
                                 ": Parameter 'vars' must be either a formula or list. ",
                                 "See help('cpt')."))
    
  }
  
  if(!is.data.frame(data)){
    err.flag <- err.flag + 1
    err.msg <- c(err.msg, paste0(err.flag,": Object 'data' must be of class 'data.frame'"))
  }
  n <- nrow(data)
  
  if(err.flag)  stop(paste(err.msg, collapse="\n"))
  
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
    err.msg <- c(err.msg, paste0(err.flag, ": All variables must be of class 'factor'"))
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
  # I'D LIKE TO USE THE VECTORS vars, dependentVar, AND independentVars
  # IN THE CALLS TO THE dplyr FUNCTIONS, BUT DON'T KNOW HOW. 
  # THE dplyr FUNCTIONS WANT VARIABLE NAMES BUT I HAVE CHARACTER
  # STRINGS. HOW DO YOU DO THIS?

  data        <- dplyr::bind_cols(dplyr::tbl_df(data[,vars]),
                                  dplyr::tbl_df(data.frame(wt=wt)))
  
  joint       <- data %>% dplyr::group_by(di3,di1,di2) %>%  # using 'vars' doesn't work
                       dplyr::summarise(wt = sum(wt))
  
  marginal    <- joint %>% dplyr::group_by(di1,di2) %>% # using 'independentVars' doesn't work
                       dplyr::summarise(sumWt = sum(wt))
  
  conditional <- dplyr::left_join(joint,marginal) %>%
                       dplyr::mutate(p = wt / sumWt) %>% 
                       dplyr::select(-c(wt,sumWt))
  
  # using 'c(independentVars,dependentVar)' doesn't work:
  cpt <- plyr::daply(conditional, .(di1,di2,di3), function(x) x$p) 
  cpt[is.na(cpt)] <- 0

  class(cpt) <- "cpt"
  return(cpt)
}
