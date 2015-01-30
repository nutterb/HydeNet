#' @name update.HydeNetwork
#' @export update.HydeNetwork
#' 
#' @title Update Probabilistic Graphical Network
#' @description Add or remove nodes or add parents within a \code{HydeNetwork}
#'   model.
#'   
#' @param object A \code{HydeNetwork} object
#' @param formula A formula statement indicating the changes to the network.
#' @param ... Additional arguments to be passed to other methods.  Current,
#'   none are used.
#'   
#' @details Adding or removing nodes is fairly straightforward if you are 
#'   removing a complete node (along with its parents).  If you just wish to 
#'   remove a parent, this doesn't work well yet.  I will have to work out 
#'   a solution to remove parent relationships individually.  I had hoped
#'   there would be an \code{update.dag} method, but no luck.
#'   
#' @author Jarrod Dalton and Benjamin Nutter
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
#' plot(carNet)
#' 
#' carNet2 <- update(carNet, ~ . + am|cyl + mpg|am)
#' plot(carNet2)
#'  
                   
update.HydeNetwork <- function(object, formula, ...){
  new_formula <- rewriteHydeFormula(object$network_formula, formula)
  
  NEW <- HydeNetwork(new_formula, data=object$data)
  NEW$nodeType[names(object$nodeType)] <- object$nodeType
  NEW$nodeFormula[names(object$nodeFormula)] <- object$nodeFormula
  NEW$nodeFitter[names(object$nodeFitter)] <- object$nodeFitter
  NEW$nodeFitterArgs[names(object$nodeFitterArgs)] <- object$nodeFitterArgs
  NEW$nodeParams[names(object$nodeParams)] <- object$nodeParams
  NEW$nodeData[names(object$nodeData)] <- object$nodeData
  
  return(NEW)  
}
