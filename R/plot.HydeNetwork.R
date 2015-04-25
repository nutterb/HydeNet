#' @name plot.HydeNetwork
#' @aliases plot.HydeNetwork plotHydeNetwork
#' @export 
#' @importFrom graph plot
#' @method plot HydeNetwork
#' 
#' 
#' @title Plot a Probabilistic Graphical Network
#' @details Plots the dag object from a \code{HydeNetwork} class network.
#' 
#' @param x an object of class \code{HydeNetwork}
#' @param ... additional arguments to be passed to \code{graph::plot}
#' 
#' @details A simple wrapper function to plot \code{network$dag}
#' 
#' @author Jarrod Dalton and Benjamin Nutter
#' @note
#'   For customizations, see the \code{Rgraphviz} documentation for 
#'   \code{GraphvizAttributes}.  Most notably, read the sections about 
#'   General Node Attributes to manipulate shapes and positions of the 
#'   nodes and their labels.  These are manipulated through the \code{nodeAttrs}
#'   arguments (passed through \code{...}).  
#'   
#'   General Graph Attributes are passed through \code{attrs} and Edge 
#'   Attributes are passed through \code{edgeAttrs}.
#' 
#' @examples
#' carNet <- HydeNetwork( ~ cyl + 
#'                       disp | cyl + 
#'                       hp | disp + 
#'                       wt + 
#'                       gear + 
#'                       mpg | disp*hp*wt*gear,
#'                       data=mtcars)
#' plot(carNet)
#' 
#' #* Change node colors, shapes, and labels.
#' #* See "Notes" for more details.
#' plot(carNet, 
#'      nodeAttrs=list(fillcolor=list(cyl="green", am="blue"),
#'                     shape=list(am="rect", mpg="ellipse"),
#'                     label=list(cyl="Cylinder", disp="Displacement", 
#'                                mpg="Miles per Gallon")))

plot.HydeNetwork <- function(x, ..., useHydeDefaults=TRUE){
  if (useHydeDefaults){
    decisionNodes <- names(which(unlist(x$nodeDecision)))
    utilityNodes <- names(which(unlist(x$nodeUtility)))
    determNodes <- names(which(unlist(x$nodeType)=="determ"))
    varNodes <- x$nodes[!x$nodes %in% c(decisionNodes, utilityNodes, determNodes)]
    
    shape <- c(rep(getOption("Hyde_plotOptions")$shape$variable, length(varNodes)),
               rep(getOption("Hyde_plotOptions")$shape$determ, length(determNodes)),
               rep(getOption("Hyde_plotOptions")$shape$decision, length(decisionNodes)),
               rep(getOption("Hyde_plotOptions")$shape$utility, length(utilityNodes)))
    names(shape) <- c(varNodes, determNodes, decisionNodes, utilityNodes)
    
    fill <- c(rep(getOption("Hyde_plotOptions")$fill$variable, length(varNodes)),
              rep(getOption("Hyde_plotOptions")$fill$determ, length(determNodes)),
              rep(getOption("Hyde_plotOptions")$fill$decision, length(decisionNodes)),
              rep(getOption("Hyde_plotOptions")$fill$utility, length(utilityNodes)))
    names(fill) <- c(varNodes, determNodes, decisionNodes, utilityNodes)
    
    fontcolor <- c(rep(getOption("Hyde_plotOptions")$fontcolor$variable, length(varNodes)),
                   rep(getOption("Hyde_plotOptions")$fontcolor$determ, length(determNodes)),
                   rep(getOption("Hyde_plotOptions")$fontcolor$decision, length(decisionNodes)),
                   rep(getOption("Hyde_plotOptions")$fontcolor$utility, length(utilityNodes)))
    names(fontcolor) <- c(varNodes, determNodes, decisionNodes, utilityNodes)
    
    linecolor <- c(rep(getOption("Hyde_plotOptions")$linecolor$variable, length(varNodes)),
                   rep(getOption("Hyde_plotOptions")$linecolor$determ, length(determNodes)),
                   rep(getOption("Hyde_plotOptions")$linecolor$decision, length(decisionNodes)),
                   rep(getOption("Hyde_plotOptions")$linecolor$utility, length(utilityNodes)))
    names(linecolor) <- c(varNodes, determNodes, decisionNodes, utilityNodes)
    
    
                   
    nodeAttribs <- list(shape=shape, fillcolor=fill, fontcolor=fontcolor,
                        color=linecolor)
    
    graph::plot(x$dag, nodeAttrs=nodeAttribs, ...)
  }
  else graph::plot(x$dag, ...)
}
