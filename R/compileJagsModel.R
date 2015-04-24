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
#'   equivalent to running \code{jgas.model(textConnection(writeNetworkModel(network)))}.
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
#' mtcars2 <- transform(mtcars, 
#'                      am=factor(am), 
#'                      cyl=factor(cyl), 
#'                      gear=factor(gear))
#' carNet <- HydeNetwork( ~ cyl +
#'                       disp | cyl +
#'                       hp | disp +
#'                       wt +
#'                       gear +
#'                       mpg | disp*hp*wt*gear,
#'                       data=mtcars2)
#'                  
#' compiledCar <- compileJagsModel(carNet, n.chains=5)
#' s <- coda.samples(compiledCar$jags, c("cyl", "mpg"), n.iter=1000)                 

compileJagsModel <- function(network, data=NULL, ...){
  
  if (!is.null(network$data)){
    .factors <- names(network$data)[sapply(network$data, is.factor)]
  
    factorRef <- as.list(network$data[, .factors, drop=FALSE])
  
    msg <- ""
    for (i in .factors){
      if (i %in% names(data)){
        if (!is.numeric(data[[i]])){
          data[[i]] <- as.numeric(factor(data[[i]], levels(network$data[, i])))                            
        }
      }
      factorRef[[i]] <- data.frame(value = 1:nlevels(network$data[, i]),
                                   label = levels(network$data[, i]))
      if (!all(data[[i]] %in% c(1:nlevels(network$data[, i]),
                                levels(network$data[, i])))){
        msg <- c(msg,
                 paste0("Values for '", i, "' must be an integer from 1 to ",
                        nlevels(network$data[, i]), " or one of the following: ",
                        paste0(levels(network$data[, i]), collapse=", ")))
      }      
    }
    if (length(msg) > 1) stop(paste(msg, collapse="\n"))
  }
  else factorRef <- NULL
  
  if (is.list(data)) data <- as.data.frame(data)
  
  jags <- if (is.null(data)) 
    rjags::jags.model(textConnection(writeNetworkModel(network)),
                      data = sys.frame(sys.parent()), ...)
    else lapply(1:nrow(data),
                function(r, network, data, ...){
                  rjags::jags.model(textConnection(writeNetworkModel(network)),
                                    data = data[r, , drop=FALSE], ...)
                },
         network, data, ...)
  
#   jags <- rjags::jags.model(textConnection(writeNetworkModel(network)), 
#                     data = if(is.null(data)) sys.frame(sys.parent()) else data, ...)
  
  #* cHN for compiled Hyde Network
  cHN <- list(jags=jags, observed=data, dag=network$dag, factorRef=factorRef)
  
  class(cHN) <- c("compiledHydeNetwork")
  cHN
}
