#' @rdname HydeNetwork
#' @export HydeNetwork.formula
#' 


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
