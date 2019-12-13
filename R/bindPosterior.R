#' @name bindSim
#' @importFrom dplyr bind_rows
#' @export bindSim
#' 
#' @title Bind Simulated Distributions
#' @description After determining the simulated distributions are satisfactory,
#'   it can be advantageous to bind the simulated distributions together in
#'   order to aggregate values and perform other manipulations and analyses.
#'   
#' @param hydeSim An object of class \code{HydeSim}
#' @param relabel_factor Logical.  If \code{TRUE}, factors that had been 
#'   converted to integers for the JAGS code can be relabelled as factors 
#'   for additional analysis in R.
#'   
#' @details For the purposes of this function, it is assumed that if the 
#'   simulated distributions are satisfactory, the multiple chains in a run 
#'   can be bound together.  Subsequently, the multiple runs are bound 
#'   together.  Lastly, the factors are relabeled, if requested.
#'   
#' @author Jarrod Dalton and Benjamin Nutter
#' 
#' @examples
#' #' data(PE, package="HydeNet")
#' Net <- HydeNetwork(~ wells + 
#'                      pe | wells + 
#'                      d.dimer | pregnant*pe + 
#'                      angio | pe + 
#'                      treat | d.dimer*angio + 
#'                      death | pe*treat,
#'                      data = PE) 
#'   
#'                  
#' compiledNet <- compileJagsModel(Net, n.chains=5)
#' 
#' #* Generate the simulated distribution
#' Simulated <- HydeSim(compiledNet, 
#'                      variable.names = c("d.dimer", "death"), 
#'                      n.iter=1000)
#' 
#' Bound <- bindSim(Simulated)
#' 
#' #* Bind a Decision Network
#' #* Note: angio shouldn't really be a decision node.  
#' #*       We use it here for illustration
#' Net <- setDecisionNodes(Net, angio, treat)
#' compiledDecision <- compileDecisionModel(Net, n.chains=5)
#' SimulatedDecision <- HydeSim(compiledDecision, 
#'                              variable.names = c("d.dimer", "death"),
#'                              n.iter = 1000)
#' 
bindSim <- function(hydeSim, relabel_factor=TRUE)
{
  if (class(hydeSim$codas) == "mcmc.list")
  {
    bound <- 
      dplyr::bind_rows(
        lapply(seq_along(hydeSim[["codas"]]), 
               bind_chains_mcmclist, 
               hydeSim
        )
      )
  }
  else 
  {
    bound <- 
      dplyr::bind_rows(
        lapply(hydeSim[["codas"]], 
               bind_chains_list
        )
    )
  }
  
  #* JAGS returns integers in place of factors.  If requested, 
  #* replace the integers as factors.
  if (relabel_factor)
  {
    factors_to_relabel <- names(bound)[names(bound) %in% names(hydeSim$factorRef)]
    for(i in factors_to_relabel)
    {
      bound[i] <- factor(bound[[i]], 
                         levels=hydeSim$factorRef[[i]]$value,
                         labels=hydeSim$factorRef[[i]]$label)
    }
  }

  as.data.frame(bound)
}



#**** UTILITY FUNCTIONS
#**** bind_chains_mcmclist is used when there is a single network (not a decision network)
#**** bind_chains_list is used when a list of mcmclists is being bound, such as 
#****                  when a decision network was run.
bind_chains_mcmclist <- function(mcmc, hydeSim)
{
  as.data.frame(hydeSim$codas[[mcmc]]) %>%
    dplyr::mutate_(
      chain_index = ~mcmc,
      obs_index = seq_along(mcmc)
    )
}

bind_chains_list <- function(mcmc)
{
  lapply(1:length(mcmc),
         function(chain)
         {
           as.data.frame(mcmc[[chain]]) %>%
             dplyr::mutate_(
               chain_index = ~chain,
               obs_index = seq_along(chain)
             )
         }
  ) %>%
  dplyr::bind_rows()
}


#' @rdname bindSim
#' @export

bindPosterior <- function(hydeSim, relabel_factor=TRUE)
{
  message("`bindPosterior` is deprecated and replaced by `bindSim`")
  bindSim(hydeSim, relabel_factor=relabel_factor)
}