#' @name bindPosterior
#' @importFrom dplyr bind_rows
#' @export bindPosterior
#' 
#' @title Bind Posterior Distributions
#' @description After determining the posterior distributions are satisfactory,
#'   it can be advantageous to bind the posterior distributions together in
#'   order to aggregate values and perform other manipulations and analyses.
#'   
#' @param hydePost An object of class \code{HydePosterior}
#' @param relabel_factor Logical.  If \code{TRUE}, factors that had been 
#'   converted to integers for the JAGS code can be relabelled as factors 
#'   for additional analysis in R.
#'   
#' @details For the purposes of this function, it is assumed that if the 
#'   posterior distributions are satisfactory, the multiple chains in a run 
#'   can be bound together.  Subsequently, the multiple runs are bound 
#'   together.  Lastly, the factors are relabeled, if requested.
#'   
#' @author Jarrod Dalton and Benjamin Nutter
#' 
bindPosterior <- function(hydePost, relabel_factor=TRUE){

  #* first, bind chains within an mcmc object together
  bind_chains <- function(mcmc){
    m <- lapply(mcmc, as.data.frame)
    dplyr::bind_rows(m)
  }
  
  bound <- dplyr::bind_rows(lapply(hydePost$codas, bind_chains))
  
  factors_to_relabel <- names(bound)[names(bound) %in% names(hydePost$factorRef)]
  
  for(i in factors_to_relabel){
    bound[i] <- factor(bound[[i]], 
                       levels=hydePost$factorRef[[i]]$value,
                       labels=hydePost$factorRef[[i]]$label)
  }

  as.data.frame(bound)
}