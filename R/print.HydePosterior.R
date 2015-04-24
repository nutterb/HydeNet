#' @name print.HydePosterior
#' @export 
#' @method print HydePosterior
#' 
#' @title Print a Hyde Posterior Distribution Object
#' @details Prints a brief description of a HydePosterior object.
#' 
#' @param x a \code{HydePosterior} object
#' @param ... additional arguments to be passed to print methods.  Currently 
#'   none in use.
#'   
#' @details Prints the number of posterior distributions, chains, and 
#'   iterations, as well as the observed values.
#'   
#' @author Jarrod Dalton and Benjamin Nutter
#' 

print.HydePosterior <- function(x, ...){
  n_distributions <- length(x$codas)
  n_chains <- length(x$codas[[1]])
  n_iterations <- nrow(x$codas[[1]][[1]])
  
  cat(paste0("Posterior distributions of a Hyde Network\n",
             "number of posterior distributions: ", n_distributions, "\n",
             "number of chains: ", n_chains, "\n",
             "number of iterations: ", n_iterations, "\n"))
  
  if (!is.null(x$observed)){
    cat("\nObserved at the values:\n")
    print(x$observed)
  }
}
