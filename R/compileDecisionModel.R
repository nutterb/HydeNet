#' @name compileDecisionModel
#' @export
#' 
#' @title Compile JAGS Models to Evaluate the Effect of Decisions in a Network
#' @description Nodes at which a decision can be made, such as the decision to 
#'   test or not test; treat or not treat; or use open or robotic surgery may 
#'   impact the outcome for a subject.  These types of decisions may not be 
#'   truly random and understanding how these decisions may impact downstream
#'   outcomes may be beneficial to making the decision.  Compiling the decision
#'   network permits the network to be evaluate under the conditions of each 
#'   set of decisions separately.
#'   
#' @param network A HydeNet object with decision nodes defined.
#' @param ... Additional arguments to pass to \code{jags.model}, excepting
#'   the \code{data} argument.  The \code{data} argument created by 
#'   \code{compileDecisionModel}, and cannot be passed manually.
#'   
#' @details \code{compileDecisionModel} only accepts nodes of type \code{"dbern"}
#'   (Bernoulli random variable taking either 0 or 1) or \code{"dcat"} 
#'   (categorical variables).  When the node is type \code{"dcat"}, the 
#'   decision options are extracted from the JAGS statement returned by 
#'   \code{writeJagsModel}.
#'   
#'   The options for each decision nodes (if there are multiple nodes) are 
#'   combined via \code{expand.grid} to make a table of all possible decisions.
#'   Each row of this table is passed as a list to the \code{data} argument 
#'   of \code{jags.model} (via \code{compileJagsModel}) and a list of JAGS
#'   model objects is returned.  \code{coda.samples} may be run on each of these
#'   models.
#'   
#' @return Returns a list of \code{compiledHydeNetwork} objects.
#' 
#' @author Jarrod Dalton and Benjamin Nutter
#' 
compileDecisionModel <- function(network, ...){
  dots <- list(...)
  if ("data" %in% names(dots)) 
    stop("'data' is not an accepted argument in 'compileDecisionModel'")
  
  decisionNodes <- names(network$nodeDecision)[sapply(network$nodeDecision, any)]
  if (length(decisionNodes) == 0) 
    stop("No decision nodes indicated in the network")
  
  validDecision <- sapply(network$nodeType[decisionNodes], 
                          function(x) x %in% c("dbern", "dcat", "dbin"))
  if (!all(validDecision))
    stop(paste0("Only nodes of type 'dcat', and 'dbin' may be decision nodes.\n  ",
                paste0(names(validDecision)[!validDecision], collapse=", "),
                " cannot be used as decision nodes."))
  
  options <- lapply(decisionNodes, decisionOptions, network)
  names(options) <- decisionNodes
  
  options <- expand.grid(options, stringsAsFactors=FALSE) 
  options <- lapply(1:nrow(options), 
                    function(i) as.list(options[i, , drop=FALSE]))
  
  lapply(options,
         function(o, ...) compileJagsModel(network, data=o, ...),
         ...)
}