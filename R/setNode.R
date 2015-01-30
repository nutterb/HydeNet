#' @name setNode
#' @export setNode
#' 
#' @title Set Node Relationships
#' @details The relationship between a node and its parents must be defined
#'   before the appropriate JAGS model statement can be constructed.  
#'   \code{setNode} is the utility by which a user can define the distribution
#'   of the node and its relationship to its parents (usually through a model
#'   of some sort).  
#'   
#' @param network A \code{HydeNetwork}.
#' @param node A node within \code{network}.  This does not have to be quoted.
#' @param nodeType a valid distribution function from JAGS.  See the data set
#'   in \code{data(jagsDists)} for a complete list.
#' @param params to be passed to the JAGS distribution function.  Each parameter
#'   in the distribution function must be a named element in the vector.  For 
#'   example, the parameters to pass to \code{dnorm} would be \code{c(mu='', tau='')}.
#'   The required parameters can be looked up using the 
#'   \code{expectedParameters} function.  If parameters are to be estimated 
#'   from the data, the functions \code{fromData} and \code{fromFormula} may 
#'   be used as placeholders.
#' @param nodeFitter the fitting function, such as \code{lm} or \code{glm}.  This
#'   will probably only be needed when \code{fromData = TRUE}.
#' @param nodeFormula A formula object specifying the relationship between a 
#'   node and its parents.  It must use as a term every parent of \code{node}.
#' @param fromData Logical.  Determines if a node's relationship is calculated 
#'   from the data object in \code{network}.  Defaults to \code{TRUE} whenever
#'   \code{network} has a data object.
#' @param ... Additional arguments to be passed to \code{fitter}.  This does not 
#'   yet have any effect as I haven't yet decided out where to store this and 
#'   how to implement the fitting.
#'   
#' @details \code{HydeNetwork} doesn't create a space for the \code{params} to be
#'   stored.  I put this in place in case a non-gaussian distribution was desired
#'   that did not lend itself to a modeling estimation.  I don't know what exactly
#'   such a situation would look like.  Another situation where it might be needed
#'   is when \code{fromData=FALSE} and the user specifies the coefficients associated
#'   with each parent-term.  In this case, the precision must be given to JAGS somehow,
#'   but the formula interface doesn't have a good place to do that.  This might 
#'   be the argument in which 'tau' is passed to JAGS while 'mu' is calculated from
#'   the formula specification.
#'   
#'   The functions \code{fromFormula()} and \code{fromData()} help to control
#'   how \code{Hyde} determines the values of parameters passed to JAGS.  If the 
#'   parameters passed in \code{params} argument are to be calculated from the
#'   data or inferred from the formula, these functions may be used as placeholders
#'   instead of writing JAGS code in the \code{params} argument.
#'   
#'   The arguments passed to \code{...} need to be stored somewhere.  Theoretically,
#'   these arguments could be different for each node.  At this point, I'm thinking I'll
#'   need to use \code{setNode} to define the relationships and then use some other 
#'   function to initiate the model (perhaps HydeToJags, or something).
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
#'   
#' carNet$nodeFormula$mpg
#' carNet$nodeFitter$mpg
#' carNet$nodeType$mpg
#' 
#' carNet <- setNode(gm, mpg, nodeType='dnorm', params=c(tau=1/2.65), 
#'                   nodeFormula = mpg ~ disp + hp + wt + factor(gear),
#'                   nodeFitter='lm')
#'
#' carNet$nodeFormula$mpg
#' carNet$nodeFitter$mpg
#' carNet$nodeType$mpg

setNode <- function(network, node, nodeType='dnorm', params=NULL, 
                    nodeFitter=NULL, nodeFormula, 
                    fromData=!is.null(network$data), ...){
  network.t <- as.character(substitute(network))
  node.t <- as.character(substitute(node))
  
#   data(jagsDists, package='Hyde')
  
  err.flag <- 0
  err.msg <- ""
  
  wrn.flag <- 0
  wrn.flag <- ""
  
  #*****************************
  #* nodeType parameter checks
  if (missing(nodeType)){
    err.flag <- err.flag + 1
    err.msg <- c(err.msg, 
                 paste0(err.flag, ": nodeType is not specified."))
  }
  
  if (length(nodeType) > 1){
    wrn.flag <- wrn.flag + 1
    wrn.msg <- c(wrn.msg,
                 paste0(wrn.flag, ": nodeType must have length 1. The first element is being used."))
    nodeType <- nodeType[1]
  }
  
  if (!missing(nodeType)){
    if (!nodeType %in% jagsDists$FnName){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg,
                   paste0(err.flag, ": nodeType must be one of the following -\n    ",
                          paste(unique(jagsDists$FnName), collapse=", ")))
    }
  }

  # translate inputted parameters into JAGS code if the params argument was not specified
  if (missing(params)){
    
    inputtedArgs <- ls()
    
    if(nodeType=="dbern"){
      if("pi" %in% inputtedArgs){
        if(!is.numeric(pi)) {
          err.flag <- err.flag + 1
          err.msg  <- c(err.msg, paste0(err.flag, ": dbern must have 0 <= pi <= 1"))
        } else if(pi<0 | pi>1){
          err.flag <- err.flag + 1
          err.msg  <- c(err.msg, paste0(err.flag, ": dbern must have 0 <= pi <= 1"))
        } else params <- paste0("pi."deparse(substitute(node)) <- ",pi,";")
      } else {
        err.flag <- err.flag + 1
        err.msg  <- c(err.msg, paste0(err.flag, ": parameters missing for dbern.  See help(\"setNode\")."))
      }
    }
    
  }
  
  network$nodeType[[node.t]] <- nodeType
  network$nodeParams[[node.t]] <- params
  if (!missing(nodeFormula)) network$nodeFormula[[node.t]] <- nodeFormula
  network$nodeFitter[[node.t]] <- nodeFitter
  network$nodeFitterArgs[[node.t]] <- list(...)
  
  return(network)  
}
