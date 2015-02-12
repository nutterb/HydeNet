#' @name plot.compiledHydeNetwork
#' @aliases plot.compiledHydeNetwork plotcompiledHydeNetwork
#' @export plot.compiledHydeNetwork
#' @importFrom graph plot
#' @method plot compiledHydeNetwork
#' 
#' @title Plot a Compiled Hyde Network
#' @description Compiled Hyde Networks are network objects created by 
#'   \code{compileJagsModel}.  The primary differentiation is that a 
#'   compiled Network may have observed nodes.  In order to simplify the 
#'   work of showing differences in compiled networks (and the observed nodes
#'   within them), this plotting method will change the color and shape of 
#'   observed nodes by default.
#' 
#' @param x An object of class \code{compiledHydeNetwork}
#' @param shape_observed The default shape for observed nodes
#' @param color_observed The default color for observed nodes
#' @param nodeAttrs A list of node attributes to be passed to 
#'   \code{graph::plot}.  See "Notes" for more details.
#' @param ... Other arguments to be passed to \code{graph::plot}.
#' 
#' @details Default colors and shapes are used for observed nodes (nodes for which at 
#'   least one value was given in the \code{compileJagsModel} function's 
#'   \code{data} argument) unless overridden by the user.  To suppress the 
#'   defaults, it is easiest to use \code{shape_observed = 'circle'} and 
#'   \code{color_observed = 'white'}.
#'   
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
#' @author Jarrod Dalton and Benjamin Nutter
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
#' compiledCar <- compileJagsModel(carNet, data=list(gear=4))
#' plot(compiledCar)


plot.compiledHydeNetwork <- 
  function(x, shape_observed = "rect", 
           color_observed = "#8C6BB1", nodeAttrs=list(), ...){
  
  fillcolor = lapply(names(x$observed), function(x) color_observed)
  names(fillcolor) <- names(x$observed)
  
  fillToAdd <- names(fillcolor)[!names(fillcolor) %in% names(nodeAttrs$fillcolor)]
  if (length(fillToAdd) > 0) nodeAttrs$fillcolor <- c(nodeAttrs$fillcolor, fillcolor[fillToAdd])
  
  shape = lapply(names(x$observed), function(x) shape_observed)
  names(shape) <- names(x$observed)
  
  shapeToAdd <- names(shape)[!names(shape) %in% names(nodeAttrs$shape)]
  if (length(shapeToAdd) > 0) nodeAttrs$shape <- c(nodeAttrs$shape, shape[shapeToAdd])
  
  graph::plot(x$dag, 
              nodeAttrs=nodeAttrs, ...)
   
}