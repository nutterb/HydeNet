#' @name HydeNetwork
#' @export HydeNetwork
#' 
#' @title Define a Probablistic Graphical Network
#' @description Using a directed acyclic graph (DAG), define a probabilistic
#'   graphical network to serve as the basis of building a model.  This function
#'   only defines the network, but not the relationships that characterize the 
#'   network.  In other words, this function will define where the relationships
#'   exist, but will not quantify those relationships.
#'   
#' @param nodes Either a formula that defines the network as passed to 
#'   \code{gRbase::dag} or a list of model objects.
#' @param data A data frame with the data for estimating node parameters.
#' @param ... additional arguments to \code{gRbase::dag}.
#' 
#' @details The DAG becomes only one element of the object returned by 
#'   \code{HydeNetwork}. The dag object is used to extract the node names
#'   and a list of parents for each node.  These will be used to help quantify
#'   the relationships.
#'   
#' @author Jarrod Dalton and Benjamin Nutter
#' @examples
#' gm <- HydeNetwork( ~ cyl + 
#'   disp | cyl + 
#'   hp | disp + 
#'   wt + 
#'   gear + 
#'   mpg | disp*hp*wt*gear)
#'   
#' graph::plot(gm$dag)
#' gm$nodes
#' gm$parents
#' 

HydeNetwork <- function(nodes, ...) UseMethod("HydeNetwork")