#' @name HydeUtilities
#' @importFrom plyr ddply
#' 
#' @title Hyde Network Utility Functions 
#' @description The functions described below are unexported functions that 
#'   are used internally by \code{HydeNet} to prepare modify network objects
#'   and prepare JAGS code.
#'   
#' @details 
#'   \code{termName}: In most model objects, factors in the model are 
#'   represented as [variable][level].  This utility function pulls out the 
#'   [variable] component.  This is typically called from within 
#'   \code{makeJagsReady}.
#'   
#'   \code{makeJagsReady}: Manages the presence of factors in interactions and
#'   makes sure the proper numeric factor value is given to the JAGS code.  
#'   This is called from within a \code{writeJagsFormula} call.
#'   
#'   \code{matchLevelNumber}: Assigns the correct numeric value of a level to 
#'   a factor variable in a model.  This is called from within 
#'   \code{makeJagsRead}.
#'   
#'   \code{validateParameters}: Users may pass parameters to the JAGS code using the 
#'   \code{setNode} function.  If a faulty parameter is given, such as 
#'   \code{lambda = -10} for a poisson distribution (\code{lambda} must be
#'   positive in a Poisson distribution), the error returned by JAGS may not
#'   clear enough to diagnose a problem that presented several steps earlier
#'   in the code.  \code{validateParamters} allows the user to receive instant
#'   feedback on the appropriateness of the code.
#'   
#'   Logical expressions for comparisons are stored in the \code{jagsDists}
#'   data object (\code{data(jagsDists, package='Hyde')}).  This is a utility
#'   function used only within \code{setNode} and is not visible to the user.
#'   
#' @author Jarrod Dalton and Benjamin Nutter

#' @rdname HydeUtilities
#' @importFrom stringr str_extract
#'   
#' @param term Usually the \code{term} column from the output of 
#'   \code{broom::tidy()}
#' @param reg A regular expression, usually provided by \code{factorRegex}

termName <- function(term, reg){
  if (!is.null(reg)){
    return(sapply(term, 
                  function(t, reg){
                    t <- unlist(strsplit(t, ":"))
                    t <- ifelse(grepl(reg, t),
                                stringr::str_extract(t, reg),
                                t)
                    t <- paste(t, collapse=":")
                  },
                  reg))
  }
  else return(term)   
}

#' @rdname HydeUtilities 
#' @param mdl Output from \code{broom::tidy()}
#' @param regex A regular expression, usually returned by \code{factorRegex}

makeJagsReady <- function(mdl, regex){
  term_name <- NULL # avoids global binding NOTE on check
  
  mdl$term_name <- termName(mdl$term, regex)
  mdl$level_name <- if (!is.null(regex)) gsub(regex, "", mdl$term) else mdl$term
  mdl$factor <- if (!is.null(regex)) grepl(regex, mdl$term) else FALSE
  
  factorRef <- mdl[mdl$factor & !grepl(":", mdl$term_name), "term_name", drop=FALSE]
  factorRef <- plyr::ddply(factorRef,
                           "term_name",
                           transform,
                           level_value = 2:(length(term_name)+1))
  
  mdl$jagsVar <- sapply(mdl$term_name, matchLevelNumber, Ref=factorRef)
  
  mdl
}

#' @rdname HydeUtilities
#' @param t Usually the \code{term_name} column generated within 
#'   \code{makeJagsReady}
#' @param Ref usually the levels reference data frame generated within
#'   \code{makeJagsReady}

matchLevelNumber <- function(t, Ref){
  t <- unlist(strsplit(t, ":"))
  t <- ifelse(!t %in% Ref$term_name, t,
              paste0("(", t, " == ", Ref$level_value[Ref$term_name == t], ")"))
  paste0(t, collapse="*")
}
  
#' @rdname HydeUtilities
#' @param params The list of parameters given in the \code{...} argument of 
#'   \code{setNode}
#' @param dist The JAGS distribution function name.  Appropriate names are
#'   in the \code{FnName} column of the \code{jagsDists} data object.

validateParameters <- function(params, dist){
  expr <- jagsDists$paramLogic[jagsDists$FnName == dist]
  valid <- sapply(expr, function(e) with(params, eval(parse(text=e))))  
  valid[sapply(params, function(p) p %in% c("fromData", "fromFormula"))] <- TRUE
  return(valid)
}