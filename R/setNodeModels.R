#' @name setNodeModels
#' @export setNodeModels
#' 
#' @title Set Node Properties Using Model Objects
#' @description Set node properties using pre-defined model objects.  Model 
#'   objects may be imported from other programs, but need to be valid 
#'   model objects with the additional restriction that the responses and 
#'   independent variables must be named nodes in the network.  This will 
#'   NOT create a network from a list of models.  For that, see 
#'   \code{HydeNetwork}
#'   
#' @param network A \code{HydeNetwork} object
#' @param ... Model objects to be incorporated into \code{network}
#' 
#' @author Jarrod Dalton and Benjamin Nutter
#' 
#' @examples
#' data(PE, data="HydeNet")
#' Net <- HydeNetwork(~ wells + 
#'                      pe | wells + 
#'                      d.dimer | pregnant*pe + 
#'                      angio | pe + 
#'                      treat | d.dimer*angio + 
#'                      death | pe*treat) 
#' print(Net)
#' 
#' g1 <- lm(wells ~ 1, data=PE)
#' g2 <- glm(pe ~ wells, data=PE, family="binomial")
#' g3 <- lm(d.dimer ~ pe + pregnant, data=PE)
#' g4 <- xtabs(~ pregnant, data=PE)
#' g5 <- glm(angio ~ pe, data=PE, family="binomial")
#' g6 <- glm(treat ~ d.dimer + angio, data=PE, family="binomial")
#' g7 <- glm(death ~ pe + treat, data=PE, family="binomial")
#' 
#' Net <- setNodeModels(Net, g1, g2, g3, g4, g5, g6, g7) 
#' print(Net)
#' 
#' writeNetworkModel(Net, pretty=TRUE)

setNodeModels <- function(network, ...){
  models <- list(...)
  if (length(models) == 0) stop("No objects passed in '...' argument.")
  
  Attrs <- lapply(models, modelToNode)
  #* assign names to list elements
  for(i in 1:length(Attrs)){
    names(Attrs)[i] <- Attrs[[i]]$nodes  
  }
  
  #* 1. check that network is a HydeNetwork
  #* 2. Check that the response is the name of a node in 'network'
  #* 3. Check that all regression variables are parents of the response
  
  err.flag <- 0
  err.msg <- ""
  
  wrn.flag <- 0
  wrn.msg <- ""
  
  #* 1. check that network is a HydeNetwork
  if (class(network) != "HydeNetwork"){
    err.flag <- err.flag + 1
    err.msg <- c(err.msg,
                 paste0(err.flag, ": 'network' must be of class 'HydeNetwork'"))
  }
  
  #* 2. Check that the response is the name of a node in 'network'
  if (!all(names(Attrs) %in% network$nodes)){
    not_nodes <- paste(names(Attrs)[!names(Attrs) %in% network$nodes], collapse=", ")
    err.flag <- err.flag + 1
    err.msg <- c(err.msg,
                 paste0(err.flag, ": The following model responses are not nodes in 'network': ",
                        not_nodes))
  }
  
  #* 3. Check that all regression variables are parents of the response
  equalParents <- rep(NA, length(Attrs))
  names(equalParents) <- names(Attrs)
  for(i in 1:length(Attrs)){
    equalParents[i] <- setequal(Attrs[[i]]$parents, network$parents[[names(Attrs)[i]]])
  }
  
  if (!all(equalParents)){
    err.flag <- err.flag + 1
    err.msg <- c(err.msg,
                 paste0(err.flag, ": The following model independent variables ",
                        "are not identical to the node parent list: ",
                        paste(names(equalParents)[!equalParents], collapse=", ")))
  }
  
  if (wrn.flag) warning(paste(wrn.msg, collapse="\n"))
  if (err.flag) stop(paste(err.msg, collapse="\n"))
  
  #* Translate new node features into network object  
  for (i in names(Attrs)){
    network$parents[[i]] <- Attrs[[i]]$parents
    network$nodeType[[i]] <- Attrs[[i]]$nodeType
    network$nodeFormula[[i]] <- Attrs[[i]]$nodeFormula
    network$nodeFitter[[i]] <- Attrs[[i]]$nodeFitter
    network$nodeFitterargs[[i]] <- Attrs[[i]]$nodeFitterArgs
    network$nodeParams[[i]] <- Attrs[[i]]$nodeParams
    network$nodeData[[i]] <- Attrs[[i]]$nodeData
    network$nodeModel[[i]] <- Attrs[[i]]$nodeModel
  }

  return(network) 
}
