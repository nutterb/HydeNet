#' @name compileJagsModel
#' @export compileJagsModel
#' @import rjags
#' 
#' @title Compile Jags Model from a Hyde Network
#' @description Generates the JAGS code from the Hyde network and uses it to 
#'   create an object representing a Bayesian graphical model.
#'   
#' @param network An object of class \code{HydeNetwork}
#' @param data A list of data values to be observed in the nodes.  It is
#'   passed to the \code{data} argument of \code{rjags::jags}.  Alternatively,
#'   a data frame representing a policy matrix may be provided to compile
#'   multiple JAGS models.
#' @param ... Additional arguments to be passed to \code{jags.model}
#' 
#' @details \code{compileJagsModel} is a partial wrapper for 
#'   \code{jags.model}. Running \code{compileJagsModel(network)} is 
#'   equivalent to running \code{jags.model(textConnection(writeNetworkModel(network)))}.
#'   
#' @return Returns a \code{compiledHydeNetwork} object.  The \code{jags} element
#'   of this object is suitable to pass to \code{coda.samples}.  Otherwise, 
#'   the primary function of the object is plotting the network with 
#'   observed data shown.   
#'   
#' @author Benjamin Nutter
#' @seealso \code{jags.model} 
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
#'   
#'                  
#' compiledNet <- compileJagsModel(Net, n.chains=5)
#' 
#' #* Generate the posterior distribution
#' Posterior <- HydePosterior(compiledNet, 
#'                            variable.names = c("d.dimer", "death"), 
#'                            n.iter = 1000)
#' Posterior
#' 
#' #* For a single model (ie, not a decision model), the user may choose to 
#' #* use the \code{rjags} function \code{coda.samples}.
#' #* However, this does not have a succinct print method
#' s <- coda.samples(compiledNet$jags, 
#'                   variable.names = c("d.dimer", "death"), 
#'                   n.iter=1000)
#'                 

compileJagsModel <- function(network, data=NULL, ...){
  
  factorRef <- makeFactorRef(network)
  
  #* convert label to value
  
  msg <- ""
  for (i in names(data)){
    if (!is.numeric(data[[i]]))
    {
      if (!i %in% names(factorRef)){
        msg <- c(msg,
                 paste0("'", i, "' was not numeric and no matching factor could be found in the data."))
      }
      else if (!all(data[[i]] %in% factorRef[[i]]$label))
      {
        msg <- c(msg,
                 paste0("Values observed in '", i, "' must be one of ",
                        paste0(c(FactorRef[[i]]$label, FactorRef[[i]]$value), collapse = ", ")))
      }
      else data[[i]] <- factorRef[[i]]$value[which(factorRef[[i]]$label == data[[i]])]
    }
  }
  
  if (length(msg) > 1) stop(paste(msg, collapse="\n"))

#   if (!is.null(network$data)){
#     .factors <- names(network$data)[sapply(network$data, is.factor)]
#     .factors <- .factors[.factors %in% network$nodes]
# 
#     if(length(.factors)>0){
#       factorRef <- as.list(network$data[, .factors, drop=FALSE])
#     
#       msg <- ""
#       for (i in .factors){
#         if (i %in% names(data)){
#           if (!is.numeric(data[[i]])){
#             data[[i]] <- as.numeric(factor(data[[i]], levels(network$data[, i])))
#             if (network$nodeType[[i]] == "dbern") data[[i]] <- data[[i]] - 1
#           }
#         }
#         factorRef[[i]] <- data.frame(value = 1:nlevels(network$data[, i]),
#                                      label = levels(network$data[, i]))
#         if (network$nodeType[[i]] == "dbern") 
#           factorRef[[i]]$value <- factorRef[[i]]$value - 1
#         if (!all(data[[i]] %in% c(factorRef[[i]]$value,
#                                   as.character(factorRef[[i]]$label)))){
#           msg <- c(msg,
#                    paste0("Values for '", i, "' must be an integer from ",
#                           min(factorRef[[i]]$value), " to ",
#                           max(factorRef[[i]]$value), " or one of the following: ",
#                           paste0(levels(network$data[, i]), collapse=", ")))
#         }      
#       }
#       if (length(msg) > 1) stop(paste(msg, collapse="\n"))
#     }
#   }
#   else factorRef <- NULL
  
  cpt_arrays <- unlist(network$nodeFitter) == "cpt"
  if(any(cpt_arrays)){
    cpt_arrays <- names(cpt_arrays)[cpt_arrays]
    cpt_arrays <- network$nodeModel[cpt_arrays]
    names(cpt_arrays) <- paste0("cpt.", names(cpt_arrays))
  } else cpt_arrays = list()
  # return(cpt_arrays)
  

  jags <- rjags::jags.model(textConnection(writeNetworkModel(network)), 
                    data = if(is.null(data) & length(cpt_arrays) == 0) 
                      sys.frame(sys.parent()) else c(data, cpt_arrays), ...)
  
  #* cHN for compiled Hyde Network
  cHN <- list(jags=jags, observed=data, dag=network$dag, factorRef=factorRef)
  
  class(cHN) <- c("compiledHydeNetwork")
  cHN
}
