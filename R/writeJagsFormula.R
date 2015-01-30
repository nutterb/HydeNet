#' @name writeJagsFormula
#' 
#' @title Write the JAGS Formula for a Hyde Node
#' @description Based on the information provided about the node,
#'   an appropriate JAGS model is written in text.  This is combined with 
#'   the other node models to generate the complete network.
#'   
#' @param fit a model object
#' @param ... Additional arguments to be passed to other methods
#' 
#' @details Methods for different model objects can be written so that 
#'   this function can be extended as desired.
#'   
#'   In the \code{writeJagsFormula.glm} method, appropriate transformations
#'   exist for the following combinations:
#'   \enumerate{
#'     \item family = binomial; link = logit
#'     \item family = poisson; link = log
#'     \item family = gaussian; link = identity (calls \code{writeJagsFormula.lm})
#'   }
#'   
#' @author Jarrod Dalton and Benjamin Nutter

writeJagsFormula <- function(fit, ...) UseMethod("writeJagsFormula")
