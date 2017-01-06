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
#' data(PE, package="HydeNet")
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
#' Net2 <- setNodeModels(Net, g1, g2, g3, g4, g5, g6, g7) 
#' print(Net)
#' 
#' writeNetworkModel(Net, pretty=TRUE)

setNodeModels <- function(network, ...)
{
  coll <- checkmate::makeAssertCollection()
  
  models <- list(...)
  
  if (!length(models))
  {
    coll$push("No objects passed in '...' argument.")
  }
  
  checkmate::reportAssertions(coll)
  
  Attrs <- lapply(X = models, 
                  FUN = modelToNode, 
                  nodes=network[["nodes"]])
  #* assign names to list elements
  for(i in seq_along(Attrs))
  {
    names(Attrs)[i] <- Attrs[[i]][["nodes"]]
  }
  
  #* 1. check that network is a HydeNetwork
  #* 2. Check that the response is the name of a node in 'network'
  #* 3. Check that all regression variables are parents of the response
  
  #* 1. check that network is a HydeNetwork
  checkmate::assertClass(x = network,
                         classes = "HydeNetwork",
                         add = coll)
  
  #* 2. Check that the response is the name of a node in 'network'
  checkmate::assertSubset(x = names(Attrs),
                          choices = network[["nodes"]],
                          add = coll)
  
  #* 3. Check that all regression variables are parents of the response
  equalParents <- rep(NA, length(Attrs))
  names(equalParents) <- names(Attrs)
  for(i in seq_along(Attrs))
  {
    equalParents[i] <- setequal(Attrs[[i]][["parents"]], 
                                network[["parents"]][[names(Attrs)[i]]])
  }
  
  if (!all(equalParents))
  {
    coll$push(paste0("The following model independent variables ",
                     "are not identical to the node parent list: ",
                     paste(names(equalParents)[!equalParents], collapse=", ")))
  }
  
  checkmate::reportAssertions(coll)

  #* Translate new node features into network object  
  for (i in names(Attrs)){
    network[["parents"]][[i]] <- Attrs[[i]][["parents"]]
    network[["nodeType"]][[i]] <- Attrs[[i]][["nodeType"]]
    network[["nodeFormula"]][[i]] <- Attrs[[i]][["nodeFormula"]]
    network[["nodeFitter"]][[i]] <- Attrs[[i]][["nodeFitter"]]
    network[["nodeFitterArgs"]][[i]] <- Attrs[[i]][["nodeFitterArgs"]]
    network[["nodeParams"]][[i]] <- Attrs[[i]][["nodeParams"]]
    network[["nodeData"]][[i]] <- Attrs[[i]][["nodeData"]]
    network[["nodeModel"]][[i]] <- Attrs[[i]][["nodeModel"]]
    network[["factorLevels"]][[i]] <- Attrs[[i]][["factorLevels"]]
  }

  network
}
