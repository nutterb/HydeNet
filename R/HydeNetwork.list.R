#' @rdname HydeNetwork
#' @export HydeNetwork.list
#' 

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
