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
#' @param factorLevels (optional) a named list with the following structure: 
#'   Variable names for the factors specified in \code{vars} comprise the names
#'   of the list elements, and each list element is a character vector containing
#'   the levels of the respective factor. See examples.
#' @param reduce set to \code{TRUE} if \code{inputCPT()} is to compute probabilities
#'   for the first level of the dependent variable as the complement of the 
#'   inputted probabilities corresponding to the other levels of the dependent
#'   variable. For example, \code{reduce = TRUE} with a binary dependent variable
#'   \code{y} (say, with levels \code{'no'} and \code{'yes'}) will ask for the
#'   probabilities of \code{'yes'} at each combination of the independent variables,
#'   and compute the probability of \code{'no'} as their respective complements.
#'   See details.
#' 
#' @details If a \code{formula} object is entered for the \code{vars} parameter, the
#'   formula must have the following structure: \emph{response ~ var1 + var2 + etc.}.
#'   The other option is to pass a named \code{list} containing two elements \code{y}
#'   and \code{x}. Element \code{y} is a character string containing the name of the 
#'   factor variable in \code{data} to be used as the dependent variable, and 
#'   element \code{x} is a character vector containing the name(s) of the factor
#'   variable(s) to be used as independent (or conditioning) variables.
#'   
#'   In \code{inputCPT()}, when the parameter \code{reduce} is set to \code{FALSE},
#'   any non-negative number (e.g., cell counts) is accepted as input. Conditional
#'   probabilities are then calculated via a normalization procedure. However, when
#'   \code{reduce} is set to \code{TRUE}, a) only probabilities in [0,1] are accepted
#'   and b) all inputted probabilities for each specific combination of independent
#'   variable values must not sum to a value greater than 1 (or the calculated 
#'   probability for the first level of the dependent variable would be negative).
#'   
#'   The \code{cpt()} function with a weight vector passed to parameter \code{wt}
#'   works analogously to \code{inputCPT(reduce = FALSE)}, i.e., it accepts any
#'   non-negative vector, and computes the conditional probability array by 
#'   normalizing sums of weights.
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
#' 
#' \dontrun{
#' inputCPT(wetGrass ~ rain + morning) 
#' 
#' inputCPT(wetGrass ~ rain + morning,
#'          factorLevels <- list(wetGrass = c("dry","moist","VeryWet"),
#'                               rain     = c("nope","yep"),
#'                               morning  = c("NO","YES")),
#'          reduce = FALSE)
#' }


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
      err.msg <- c(err.msg,
                   paste0(err.flag,
                          ": Length of 'wt' not equal to number of rows in 'data'"))
    } else if(min(wt) < 0){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg,
                   paste0(err.flag,
                          ": Negative values in parameter 'wt' not allowed"))
    }
  }
  
  if(wrn.flag) warning(paste(wrn.msg, collapse="\n"))
  if(err.flag)  stop(paste(err.msg, collapse="\n"))
  
  vars  <- c(dependentVar, independentVars)  
  ..vars <- lapply(vars, as.symbol)
  ..independentVars <- lapply(independentVars, as.symbol)

  data     <- dplyr::bind_cols(dplyr::tbl_df(data[,vars]),
                               dplyr::tbl_df(data.frame(wt = wt)))
  
  joint    <- data %>% dplyr::group_by_(.dots = ..vars) %>%  
                  dplyr::summarise(wt = sum(wt))
  
  marginal <- joint %>% dplyr::group_by_(.dots = ..independentVars) %>% 
                  dplyr::summarise(sumWt = sum(wt))
  
  cpt      <- dplyr::left_join(joint, marginal, by = independentVars) %>%
                  dplyr::mutate(p = wt / sumWt) %>% 
                  dplyr::select(-c(wt, sumWt)) %>%
                  plyr::daply(c(vars[-1], vars[1]), function(x) x$p)
  
  cpt[is.na(cpt)] <- 0

  class(cpt) <- "cpt"
  return(cpt)
  
} #end function cpt


#' @rdname cpt
#' @export inputCPT

inputCPT <- function(vars, factorLevels, reduce=TRUE){
  
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
  
  
  hbar <- paste(paste(rep("-",80),collapse=""),"\n",sep="")
  factorEntryCommand <- function(variableName){
    cat(hbar, "Enter Factor Levels for node '", variableName,"':\n\n",
        "If this is a binary variable, enter '<yn>' as a shortcut.\n",
        "When finished, enter '<z>'.\n",
        "To repeat entry of the last inputted factor level, enter '<b>'.\n",
        "To start over entirely, enter '<s>'\n", hbar, sep="")
  }
  
  if(missing(factorLevels)){  # solicit the names of factor levels from the console
    factorLevels <- vector(mode="list")
    for(i in seq_along(variables)){
      escapeFlag <- 0
      levelIndex <- 1
      tmp <- vector("character")
      factorEntryCommand(variables[i])
      while(!escapeFlag){
        IO <- readline(paste0("Level ",levelIndex," of '",variables[i],"':   "))
        if(IO == "<yn>"){
          if(levelIndex == 1) {
            tmp <- c("No","Yes")
            escapeFlag <- 1
          } else cat("(NOTE: <yn> only works when entering the FIRST factor level.)\n")
        } else if(IO == "<s>") {
          levelIndex <- 1
          tmp <- vector("character")
          factorEntryCommand(variables[i])
        } else if(IO == "<b>"){
          levelIndex <- max(c(1,levelIndex-1))
          if(levelIndex == 1) tmp <- vector("character") else tmp <- tmp[1:(levelIndex-1)]
        } else if(IO == "<z>"){
          escapeFlag <- 1
        } else if(IO == "<q>"){
          stop("User requested termination.")
        }
        else {
          tmp <- c(tmp, IO)
          levelIndex <- levelIndex + 1
        }
      }
      factorLevels[[variables[i]]] <- tmp
    }
  } else{
    if(!all(variables %in% names(factorLevels))){
      stop(paste("Variables",paste(variables,collapse=", "),
                 "not all in parameter 'factorLevels'."))
    }
    factorLevels <- factorLevels[variables]
    if(!all(unlist(lapply(factorLevels, is.character)))) {
      stop("Incompatible 'factorLevels' argument. See help('readCPTFromConsole').")
    }
  }
  facValWidths <- unlist(lapply(factorLevels, function(x) max(nchar(x))))
  
  # input the conditional probabilities
  data <- expand.grid(factorLevels)
  
  
  if(reduce){
    cat(hbar,
        "NOTE: parameter 'reduce' is set to TRUE in readCPTFromConsole().\n",
        "      Conditional probabilities Pr(",dependentVar,"=",
        factorLevels[[dependentVar]][1]," | ", paste(independentVars,collapse=", "),
        ") will be calculated\n",
        "      as the complement of the inputted probabilities Pr(", dependentVar,
        " != ",factorLevels[[dependentVar]][1]," | ",
        paste(independentVars,collapse=", "), ").\n", hbar,sep="")
    data <- data[data[,dependentVar] %in% levels(data[,dependentVar])[-1],]
    cat("Enter the following conditional probabilities:\n")
  } else {
    cat(hbar, "Enter the following conditional probabilities, or positive\n",
        "numbers proportional to them (e.g., counts):\n")
  }
  cat("Use '<q>' to halt execution.\n",
      "To go back one step and re-enter, enter '<b>'.\n", hbar, sep="")
  
  formattedDepVarLvls <- format(as.character(data[,dependentVar]),
                                width = facValWidths[dependentVar])
  
  noNegativeProbs <- FALSE
  i <- 1
  optWarn <- options()$warn
  options(warn = -1)
  while(!noNegativeProbs){
    while(i <= nrow(data)){
      valid.IO <- FALSE;
      while(!valid.IO){
        formattedIndepVarLvls <- data[i, independentVars]
        formattedIndepVarLvls <- format(unlist(formattedIndepVarLvls),
                                        width=facValWidths[names(formattedIndepVarLvls)])
        prompt <- paste("Pr(",dependentVar,"=", formattedDepVarLvls[i], " | ",
                        paste(apply(cbind(names(data[i,independentVars]),
                                          formattedIndepVarLvls),
                                    1, paste, collapse="="),
                              collapse=", "),
                        "):   ", sep="")
        IO <- readline(prompt)
        if(IO == "<q>") stop("User requested termination.") else if(IO != "<b>"){
          IO.n <- as.numeric(IO)
          if(is.na(IO.n)) cat("Invalid numeric data entry. Try again:\n") else {
            if(reduce & (IO.n<0 | IO.n>1)){
              cat("Invalid probability given. Enter a number in [0,1]:\n")  
            } else if(IO.n<0){
              cat("Invalid count/probability given. Enter a non-negative number:\n")
            } else{
              valid.IO <- TRUE
              data[i,"wt"] <- IO.n
              i <- i + 1
            }
          }
        } else i <- max(i -1 , 1)
      }
    }
    options(warn = optWarn)
    
    if(reduce){
      # Add complement rows to the conditional probability data frame
      # if reduce=TRUE was used; check for errors involving sum of entered
      # conditional probabilities greater than 1
      complementProbs <- plyr::ddply(data, independentVars,
                                     function(data) c("wt"=1-sum(data$wt)))
      complementProbs[,dependentVar] <- levels(data[,dependentVar])[1]
      data <- rbind(data,complementProbs)
      if(min(data$wt)>=0) noNegativeProbs <- TRUE else{
        cat(hbar,"Invalid set of conditional probabilities given. There exists\n",
            "some combination of conditioning variables such that\n",
            "the sum of Pr(",dependentVar," != ",factorLevels[[dependentVar]][1]," | ",
            paste(independentVars,collapse=", "), ") is greater than 1.\n",
            "Please re-enter the conditional probabilities.\n",
            hbar, sep="")
      }
    } else noNegativeProbs <- TRUE
  } #end while(!noNegativeProbs) loop
  
  return(cpt(vars = list(y = dependentVar, x = independentVars), 
             data = data, wt = data$wt))
} #end function inputCPT()


