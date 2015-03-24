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

plot.HydeNetwork <- function(x, ...){
  graph::plot(x$dag, ...)
}
