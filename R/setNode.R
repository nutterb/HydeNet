#' @name setNode
#' @export setNode
#' @importFrom plyr is.formula
#' 
#' @title Set Node Relationships
#' @description The relationship between a node and its parents must be defined
#'   before the appropriate JAGS model statement can be constructed.  
#'   \code{setNode} is the utility by which a user can define the distribution
#'   of the node and its relationship to its parents (usually through a model
#'   of some sort).
#'   
#' @param network A \code{HydeNetwork}.
#' @param node A node within \code{network}.  This does not have to be quoted.
#' @param nodeType a valid distribution function from JAGS.  See the data set
#'   in \code{data(jagsDists)} for a complete list.
#' @param nodeFitter the fitting function, such as \code{lm} or \code{glm}.  This
#'   will probably only be needed when \code{fromData = TRUE}.
#' @param nodeFormula A formula object specifying the relationship between a 
#'   node and its parents.  It must use as a term every parent of \code{node}.
#' @param fitterArgs Additional arguments to be passed to \code{fitter}.  This does not 
#'   yet have any effect as I haven't yet decided out where to store this and 
#'   how to implement the fitting.
#' @param decision A value of either \code{"current"} or a logical value.  
#'   If \code{"current"}, the current value of the setting is retained.  This allows 
#'   decision nodes set by \code{setDecisionNode} to retain the classification as a 
#'   decision node if \code{setNode} is run after \code{setDecisionNode}.
#'   If \code{TRUE}, the node will be considered a
#'   decision node in \code{compileDecisionNetwork}.  This is only a valid
#'   option when the node is of type \code{"dbern"} or \code{"dcat"}. Note: if any 
#'   character value other than \code{"current"} is given, \code{setNode} will assume
#'   you intended \code{"current"}.  
#' @param utility A value of either \code{"current"} or a logical value.  
#'   If \code{"current"}, the current value of the setting is retained.  This allows 
#'   utility nodes set by \code{setUtilityNode} to retain the classification as a 
#'   utility node if \code{setNode} is run after \code{setUtilityNode}. 
#'   If \code{TRUE}, the node will be considered a 
#'   utility node.  This is only a valid option when the node is of type 
#'   \code{"determ"} and it has no children.
#'   Note: if any 
#'   character value other than \code{"current"} is given, \code{setNode} will assume
#'   you intended \code{"current"}.  
#' @param fromData Logical.  Determines if a node's relationship is calculated 
#'   from the data object in \code{network}.  Defaults to \code{TRUE} whenever
#'   \code{network} has a data object.
#' @param ... parameters to be passed to the JAGS distribution function.  Each parameter
#'   in the distribution function must be named.  For 
#'   example, the parameters to pass to \code{dnorm} would be \code{mu='', tau=''}.
#'   The required parameters can be looked up using the 
#'   \code{expectedParameters} function.  If parameters are to be estimated 
#'   from the data, the functions \code{fromData} and \code{fromFormula} may 
#'   be used as placeholders.
#' @param validate Logical.  Toggles validation of parameters given in \code{...}.
#'   When passing raw JAGS code (ie, character strings), this should be turned off, 
#'   as the validation is applicable to numerical/formula values.
#' @param fitModel Logical. Toggles if the model is fit within the function call.
#'   This may be set globally using \code{options('Hyde_fitModel')}.  See Details
#'   for more about when to use this option.
#'   
#' @details #'   
#'   The functions \code{fromFormula()} and \code{fromData()} help to control
#'   how \code{Hyde} determines the values of parameters passed to JAGS.  If the 
#'   parameters passed in \code{params} argument are to be calculated from the
#'   data or inferred from the formula, these functions may be used as placeholders
#'   instead of writing JAGS code in the \code{params} argument.
#'   
#'   By default, \code{options(Hyde_fitModel=FALSE)}.  This prevents \code{setNode}
#'   from fitting any models.  Instead, the fitting is delayed until the user 
#'   calls \code{writeJagsModel} and all of the models are fit at the same time.
#'   When using large data sets that may require time to run, it may be better to
#'   leave this option \code{FALSE} so that the models can all be compiled together
#'   (especially if you are working interactively).  Using \code{fitModel=TRUE} 
#'   will cause the model to be fit and the JAGS code for the parameters to be
#'   stored in the \code{nodeParams} attribute.
#'   
#' @author Jarrod Dalton and Benjamin Nutter
#'   
#' @examples
#' data(PE, package="HydeNet")
#' Net <- HydeNetwork(~ wells + 
#'                      pe | wells + 
#'                      d.dimer | pregnant*pe + 
#'                      angio | pe + 
#'                      treat | d.dimer*angio + 
#'                      death | pe*treat,
#'                      data = PE) 
#' print(Net, d.dimer)
#' 
#' #* Manually change the precision
#' Net <- setNode(Net, d.dimer, nodeType='dnorm', mu=fromFormula(), tau=1/2.65, 
#'                   nodeFormula = d.dimer ~ pregnant * pe,
#'                   nodeFitter='lm')
#' print(Net, d.dimer)
#' 

setNode <- function(network, node, nodeType, 
                    nodeFitter, nodeFormula, 
                    fitterArgs = list(),
                    decision = "current",
                    utility = "current",
                    fromData=!is.null(network$data), ...,
                    validate=TRUE, fitModel=getOption("Hyde_fitModel")){
  
  network.t <- as.character(substitute(network))
  node.t <- as.character(substitute(node))
  
#   data(jagsDists, package='Hyde')
  
  err.flag <- 0
  err.msg <- ""
  
  wrn.flag <- 0
  wrn.msg <- ""
  
  if (is.character(decision))
  {
    if (decision != "current")
    {
      wrn.flag <- wrn.flag + 1
      wrn.msg <- c(wrn.msg,
                   paste0(wrn.flag, ": 'decision' must be logical or 'current'.  You provided ",
                          "an unrecognized character value.  'HydeNet' is assuming you mean ",
                          "'current'."))
      decision <- "current"
    }
    decision <- network$nodeDecision[[node.t]]
  }
  
  if (is.character(utility))
  {
    if (utility != "current")
    {
      wrn.flag <- wrn.flag + 1
      wrn.msg <- c(wrn.msg,
                   paste0(wrn.flag, ": 'utility' must be logical or 'current'.  You provided ",
                          "an unrecognized character value.  'HydeNet' is assuming you mean ",
                          "'current'."))
      utility <- "current"
    }
    utility <- network$nodeUtility[[node.t]]
  }

  if (!missing(nodeType)){
    if (length(nodeType) > 1){
      wrn.flag <- wrn.flag + 1
      wrn.msg <- c(wrn.msg,
                   paste0(wrn.flag, ": nodeType must have length 1. The first element is being used."))
      nodeType <- nodeType[1]
    }
  }
  
  if (!missing(nodeType)){
    if (!nodeType %in% jagsDists$FnName){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg,
                   paste0(err.flag, ": nodeType must be one of the following -\n    ",
                          paste(unique(jagsDists$FnName), collapse=", ")))
    }
  }

  if (!missing(nodeType)) network$nodeType[[node.t]] <- nodeType
 
  exp_param <- eval(substitute(expectedParameters(network, node, TRUE)))
  params <- list(...)[exp_param]
  
  if (!all(exp_param %in% names(params))){
    err.flag <- err.flag + 1
    err.msg <- c(err.msg,
                 paste0(err.flag, ": Nodes of type ", network$nodeType[[node.t]], 
                        " must have all of the following parameters--",
                        paste(exp_param, collapse=", "), "."), collapse="\n")
  }

  if (validate){
    valid <- validateParameters(params, network$nodeType[[node.t]]) 

    if (!all(valid)){
      not_valid <- which(!valid)
      msg <- paste0("Please define ", names(params)[not_valid], " such that ", names(valid)[not_valid], 
                    " (or use validate=FALSE).")
      msg <- paste(msg, collapse="\n")
      err.flag <- err.flag + 1
      err.msg <- c(err.msg,
                   paste0(err.flag, ": ", msg)) 
    }
  }

  if (decision){
    if (!nodeType %in% c("dbern", "dcat")){
      wrn.flag <- wrn.flag + 1
      wrn.msg <- c(wrn.msg,
                   paste0(wrn.flag, 
                          ": Only nodes of type 'dbern' and 'dcat' may be decision nodes. ",
                          "'decision' has been set to FALSE"))
      decision <- FALSE
    }
  }

  if (utility){
    if (!nodeType %in% c("determ")){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg,
                   paste0(err.flag, 
                          ": Utility nodes must be of type 'determ'."))
    }
    
    if (any(sapply(network$parents, function(p, t) t %in% p, node.t))){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg,
                   paste0(err.flag,
                          ": Utility nodes may not have children."))
    }
  }

  if (length(list(...))) network$nodeParams[[node.t]] <- list(...)
  if (!missing(nodeFormula)) network$nodeFormula[[node.t]] <- nodeFormula
  if (!missing(nodeFitter)) network$nodeFitter[[node.t]] <- nodeFitter
  if (length(fitterArgs)) network$nodeFitterArgs[[node.t]] <- fitterArgs
  network$nodeDecision[[node.t]] <- decision
  network$nodeUtility[[node.t]] <- utility

  if (fitModel) {
    fit <- do.call(network$nodeFitter[[node.t]],
                   c(list(formula = network$nodeFormula[[node.t]],
                          data = if (is.null(network$nodeData[[node.t]])) network$data else network$nodeData[[node.t]]),
                     network$nodeFitterArgs[[node.t]]))
    network$nodeModel[[node.t]] <- fit
    
    if (network$nodeType[[node.t]] == "dbern"){
      network$nodeParams[[node.t]]$p <- writeJagsFormula(fit)  
    }
    else if (network$nodeType[[node.t]] == "dcat"){
      network$nodeParams[[node.t]]$pi <- writeJagsFormula(fit)
    }
    else if (network$nodeType[[node.t]] == "dnorm"){
      network$nodeParams[[node.t]]$mu <- writeJagsFormula(fit)
      network$nodeParams[[node.t]]$tau <- 1/summary(fit)$sigma
    }
    else if (network$nodeType[[node.t]] == "dpois"){
      network$nodeParams[[node.t]]$lambda <- writeJagsFormula(fit)
    }
                   
  }
  
  if (wrn.flag) warning(paste(wrn.msg, collapse="\n"))
  if (err.flag) stop(paste(err.msg, collapse="\n"))
  return(network)  
}

#' @rdname setNode
#' @export fromData

fromData <- function(){ "fromData" }

#' @rdname setNode
#' @export fromFormula

fromFormula <- function(){ "fromFormula" }

