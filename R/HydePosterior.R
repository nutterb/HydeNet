#' @name HydePosterior
#' @export HydePosterior
#' 
#' @title Posterior Distributions of a Decision Network
#' @description The posterior distributions of the decision network can be 
#'   evaluated to determine the probabilistic outcomes based on the decision
#'   inputs in the model as well as subject specific factors.
#'   
#' @param cHN A \code{compiledHydeNetwork} object as returned by 
#'   \code{compileJagsNetwork}.
#' @param variable.names a character vector giving the names of variables to be monitored.
#' @param n.iter number of iterations to monitor.
#' @param thin thinning interval for monitors.
#' @param ... options arguments that are passed to the update method for 
#'   jags model objects.
#' @param monitor_observed If TRUE, the observed or fixed variables (those
#'   passed to the \code{data} argument in \code{compileJagsNetwork}) are 
#'   forced into \code{variable.names} if not already provided.  This is 
#'   recommended, especially if you will be binding multiple JAGS runs 
#'   together.
#'   
#' @details This is essentially a wrapper around \code{coda.samples} that 
#'   returns in a list the output for each run of \code{coda.samples} over 
#'   the rows of the policy/decision matrix given in the \code{data} argument 
#'   of \code{compileJagsNetwork}.
#'   
#' @return A list of class \code{HydePosterior} with elements \code{codas} 
#'   (the MCMC matrices from \code{coda.samples}), \code{observed} (the values
#'   of the variables that were observed), \code{dag} (the dag object for 
#'   convenience in displaying the network), and \code{factorRef} (giving the
#'   mappings of factor levels to factor variables).  
#'   
#'   The only rationale for giving this object its own class was because it 
#'   produces an enormous amount of material to be printed.  A distinct 
#'   \code{print} method has been written for this object.
#'   
#' @author Jarrod Dalton and Benjamin Nutter
#' 

HydePosterior <- function(cHN, variable.names, n.iter, thin=1, ...,
                          monitor_observed=TRUE){
  if (monitor_observed) variable.names <- unique(c(variable.names, 
                                                   names(cHN$observed)))
  
  codas <- lapply(cHN$jags, coda.samples, 
                  variable.names = variable.names,
                  n.iter = n.iter, 
                  thin = thin, 
                  ...)
  
  HydePost <- list(codas = codas,
                   observed = cHN$observed,
                   dag = cHN$dag,
                   factorRef = cHN$factorRef)
  
  class(HydePost) <- "HydePosterior"
  HydePost
  
}