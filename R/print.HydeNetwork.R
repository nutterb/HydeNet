#' @name print.HydeNetwork
#' @export print.HydeNetwork
#' 
#' @title Print a HydeNetwork
#' @details Prints a HydeNetwork object with a brief summary of each node.
#' 
#' @param x a \code{HydeNetwork object}
#' @param ... additional arguments to be passed to print methods.  Currently 
#'   none in use.
#'   
#' @details The summary of each node follows the format\cr
#'   node name | parents\cr
#'   node type (parameter)\cr
#'   estimated from data (or not)\cr
#'   formula
#'   
#' @author Jarrod Dalton and Benjamin Nutter
#' @examples
#' carNet <- HydeNetwork( ~ cyl + 
#'                       disp | cyl + 
#'                       hp | disp + 
#'                       wt + 
#'                       gear + 
#'                       mpg | disp*hp*wt*gear,
#'                       data=mtcars)
#' carNet
#' 
#' carNet <- setNode(carNet, mpg, 
#'                   nodeType='dnorm', mu=fromFormula(), tau=1/2.65, 
#'                   nodeFormula = mpg ~ disp + hp + wt + factor(gear),
#'                   nodeFitter='lm')
#' carNet
#'     
print.HydeNetwork <- function(x, ...){
  Hyde.nm <- as.character(substitute(x))
  
  nodeSummary <- function(node){
    nodeName <- if (!is.null(x$parents[[node]]))
                    paste(node, "|", paste(x$parents[[node]], collapse=" * "))
                else node
    
    nodeType <- if (is.null(x$nodeType[[node]])) "Unspecified" else x$nodeType[[node]]
    
    nodeParam <- if (is.null(x$nodeParams[[node]])) "Unspecified" 
                 else{
                   paste(paste(names(x$nodeParams[[node]]), "=", 
                               x$nodeParams[[node]]), collapse=", ")
                 }
    if (nodeType != "Unspecified") nodeType <- paste0(nodeType, "(", nodeParam, ")")
    
    Formula <- paste0(x$nodeFitter[[node]], ": ", deparse(x$nodeFormula[[node]]))
   
    return(paste(nodeName, nodeType, Formula, sep="\n"))
  }
  
  nodeSummaries <- paste(sapply(x$nodes, nodeSummary), collapse="\n\n")

  cat("A Probabilistic Graphical Network", sep=" ")
  cat(paste("\nHas data attached:", if(is.null(x$data)) "No" else "Yes"))
  cat(paste0("\n\n", nodeSummaries))
}
