#' @name HydeNetwork
#' @export HydeNetwork
#' 
#' @title Define a Probablistic Graphical Network
#' @description Using a directed acyclic graph (DAG), define a probabilistic
#'   graphical network to serve as the basis of building a model.  This function
#'   only defines the network, but not the relationships that characterize the 
#'   network.  In other words, this function will define where the relationships
#'   exist, but will not quantify those relationships.
#'   
#' @param nodes Either a formula that defines the network as passed to 
#'   \code{gRbase::dag} or a list of model objects.
#' @param data A data frame with the data for estimating node parameters.
#' @param ... additional arguments to \code{gRbase::dag}.
#' 
#' @details The DAG becomes only one element of the object returned by 
#'   \code{HydeNetwork}. The dag object is used to extract the node names
#'   and a list of parents for each node.  These will be used to help quantify
#'   the relationships.
#'   
#' @author Jarrod Dalton and Benjamin Nutter
#' @examples
#' gm <- HydeNetwork( ~ cyl + 
#'   disp | cyl + 
#'   hp | disp + 
#'   wt + 
#'   gear + 
#'   mpg | disp*hp*wt*gear)
#'   
#' graph::plot(gm$dag)
#' gm$nodes
#' gm$parents
#' 

HydeNetwork <- function(nodes, ...) UseMethod("HydeNetwork")

#' @rdname HydeNetwork
#' @export


HydeNetwork.formula <- function(nodes, data=NULL, ...){
  #* Build the DAG object
  network <- gRbase::dag(nodes) 
  
  #* Node names
  node_names <- graph::nodes(network)
  
  #* Parents
  adjMat <- gRbase::graphNEL2adjMAT(network)  
  parents <- lapply(1:ncol(adjMat), function(x) rownames(adjMat)[adjMat[, x] == 1])
  parents <- lapply(parents, function(x) if (length(x) == 0) NULL else x)
  names(parents) <- node_names
  
  #* fromData
  #* returns TRUE if the node and its parents are in 'data'
  #* returns FALSE if any node or parent is missing from 'data'
  fromData <- lapply(node_names, 
                     function(x){
                       if (is.null(data)) return(FALSE)
                       if (all(c(x, parents[[x]]) %in% names(data)))
                         return(TRUE)
                       else return(FALSE)
                     })
  names(fromData) <- node_names
  
  #* nodeFormula
  nodeFormFn <- function(x, parents){
    if (is.null(parents[[x]])){
      if (fromData[[names(parents)[x]]] & !is.numeric(data[, names(parents)[x]]))
        f <- paste("~ ", names(parents)[x])
      else f <- paste(names(parents)[x], "~ 1")
    }
    else f <- paste(names(parents)[x], "~", paste(parents[[x]], collapse=" + "))
    return(as.formula(f))
  }
  nodeFormula <- lapply(1:length(parents), nodeFormFn, parents)
  names(nodeFormula) <- node_names
  
  #* nodeFitter
  #* returns 'lm' for continuous variables
  #* returns 'glm' for categorical variables
  #* returns NULL for variables not in the data
  nodeFitter <- lapply(node_names, 
                       function(x){
                         if (is.null(data)) return(NULL)
                         if (!x %in% names(data)) return(NULL)
                         else if (is.numeric(data[, x])) return("lm")
                         else if (is.factor(data[, x]) & is.null(parents[[x]])) return("xtabs")
                         else if (is.factor(data[, x]) & nlevels(data[, x]) == 2) return("glm")
                         else if (is.factor(data[, x]) & nlevels(data[, x]) > 2) return("multinom")
                         else return("glm")
                       }
  )
  names(nodeFitter) <- node_names
  
  #* nodeTypes
  #* returns 'dcat' if categorical and has no parents
  #* returns 'dnorm' otherwise
  nodeType <- lapply(node_names, 
                     function(x){
                       if (is.null(data)) return('dnorm')
                       if (x %in% names(data)){
                         if ((is.null(parents[[x]]) && !is.numeric(data[, x])) || 
                               (!is.null(parents[[x]]) && !is.numeric(data[, x]) && nlevels(data[, x]) > 2))
                           return('dcat')
                         else if ((is.null(parents[[x]]) && !is.numeric(data[, x])) || 
                                    (!is.null(parents[[x]]) && !is.numeric(data[, x]) && nlevels(data[, x]) == 2))
                           return('dbern')
                         else return('dnorm')
                       }
                       else return('dnorm')
                     }  
  )
  names(nodeType) <- node_names
  
  #* nodeParameters
  data(jagsDists, envir=environment())
  nodeParams <- lapply(node_names, 
                       function(x){
                         parm <- jagsDists$Parameters[jagsDists$FnName == nodeType[[x]]]
                         if (fromData[[x]]) 
                           parm <- paste0("c(",
                                          paste(parm, "fromData()", sep="=", collapse=", "),
                                          ")")
                         else 
                           parm <- paste0("c(",
                                          paste(parm, "'Unspecified'", sep="=", collapse=", "),
                                          ")")
                         return(eval(parse(text=parm)))
                       }
  )
  names(nodeParams) <- node_names
  
  #* fitterArgs
  nodeFitterArgs <- lapply(1:length(node_names), function(x) return(NULL))
  if (any(sapply(nodeFitter, function(x) if (is.null(x)) FALSE else x == "glm")))
    #     return(nodeFitterArgs[which(sapply(nodeFitter, function(x) x == "glm"))])
    nodeFitterArgs[which(sapply(nodeFitter, function(x) x == "glm"))] <- list(family='binomial')
  names(nodeFitterArgs) <- node_names
  
  nodeData <- lapply(1:length(node_names), function(x) return(NULL))
  names(nodeData) <- node_names
  
  nodeModel <- lapply(1:length(node_names), function(x) return(NULL))
  names(nodeModel) <- node_names  
  
  #* Define the HydeNetwork object
  network <- list(nodes = node_names, parents=parents, nodeType=nodeType,
                  nodeFormula=nodeFormula,
                  nodeFitter=nodeFitter, nodeFitterArgs=nodeFitterArgs,
                  nodeParams=nodeParams, 
                  fromData=fromData, 
                  nodeData = nodeData,
                  nodeModel = nodeModel,
                  dag=network)
  
  
  network$data <- if (!is.null(data)) data else NULL
  network$network_formula <- nodes
  class(network) <- c("HydeNetwork")
  return(network)
}

#' @rdname HydeNetwork
#' @export

HydeNetwork.list <- function(nodes, ...){
  #* convert models to nodes
  Attrs <- lapply(nodes, modelToNode)
  
  #* assign names to list elements
  for(i in 1:length(Attrs)){
    names(Attrs)[i] <- Attrs[[i]]$nodes  
  }
  
  #* Generate the DAG formula and build the network
  dag.form <- sapply(Attrs, 
                     function(x) paste0(x$nodes, 
                                        if (!is.null(x$parents)) " | " else "", 
                                        paste(x$parents, collapse=" * ")))
  dag.form <- paste0("~ ", paste(dag.form, collapse = " + "))
  network <- HydeNetwork(as.formula(dag.form))
  
  #* Reassign parameters from the models
  for (i in names(Attrs)){
#     network$parents[[i]] <- if (length(Attrs[[i]]$parents) == 0) NULL else Attrs[[i]]$parents
    network$nodeType[[i]] <- Attrs[[i]]$nodeType
    network$nodeFormula[[i]] <- Attrs[[i]]$nodeFormula
    network$nodeFitter[[i]] <- Attrs[[i]]$nodeFitter
    network$nodeFitterArgs[[i]] <- Attrs[[i]]$nodeFitterArgs
    network$nodeParams[[i]] <- Attrs[[i]]$nodeParams
    network$nodeData[[i]] <- Attrs[[i]]$nodeData
    network$nodeModel[[i]] <- Attrs[[i]]$nodeModel
    network$fromData[[i]] <- TRUE
  }
  
  return(network)
}
