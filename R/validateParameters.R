#' @name validateParameters
#' 
#' @title Validate JAGS Distributional Parameters
#' @description Users may pass parameters to the JAGS code using the 
#'   \code{setNode} function.  If a faulty parameter is given, such as 
#'   \code{lambda = -10} for a poisson distribution (\code{lambda} must be
#'   positive in a Poisson distribution), the error returned by JAGS may not
#'   clear enough to diagnose a problem that presented several steps earlier
#'   in the code.  \code{validateParamters} allows the user to receive instant
#'   feedback on the appropriateness of the code.
#'   
#' @param params The list of parameters given in the \code{...} argument of
#'   \code{setNode}
#' @param dist The JAGS distribution function name.  Appropriate names are
#'   in the \code{FnName} column of the \code{jagsDists} data object.
#'   
#' @details Logical expressions for comparisons are stored in the \code{jagsDists}
#'   data object (\code{data(jagsDists, package='Hyde')}).  This is a utility
#'   function used only within \code{setNode} and is not visible to the user.
#'   
#' @author Jarrod Dalton and Benjamin Nutter

validateParameters <- function(params, dist){
  expr <- jagsDists$paramLogic[jagsDists$FnName == dist]
  valid <- sapply(expr, function(e) with(params, eval(parse(text=e))))  
  valid[sapply(params, function(p) p %in% c("fromData", "fromFormula"))] <- TRUE
  return(valid)
}