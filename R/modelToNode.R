#' @name modelToNode
#' 
#' @title Convert a Model Object to a Network Node
#' @description In cases where model objects may already be fit and established,
#'   they can be used to generate a network without having to refit the 
#'   models or specify the distributions in \code{setNode}.
#'   
#' @param model A model object
#' @param ... Additional arguments to be passed to other functions.  Currently ignored.
#' 
#' @description For models built with \code{xtabs}, although a data frame may be
#'   passed when building the model, it is not stored in the object.  Thus,
#'   the data used to construct the models are not carried with the node.  However,
#'   the JAGS code is built appropriate from the object and this should be of
#'   little concern.

modelToNode <- function(model, ...) UseMethod("modelToNode")