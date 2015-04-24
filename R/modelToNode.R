#' @name modelToNode
#' @export modelToNode
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

#' @rdname modelToNode
#' @export

modelToNode.default <- function(model, ...){
  fitter <- tail(as.character(as.list(model$call)[[1]]), 1)
  if (!fitter %in% c("lm", "glm", "multinom", "xtabs"))
    stop(paste0("The Hyde package only accepts models built by the following functions:\n",
                "  lm, glm (family=\"binomial\" only), multinom, xtabs"))
}

#' @rdname modelToNode
#' @export

modelToNode.glm <- function(model, ...){
  list(nodes = as.character(terms(model))[2],
       parents = names(attributes(terms(model))$dataClasses)[-1],
       nodeType = "dbern",
       nodeFormula = model$call$formula,
       nodeFitter = as.character(model$call)[1],
       nodeFitterArgs = as.list(model$call)[-c(1, which(names(as.list(model$call)) %in% c("formula", "data")))],
       nodeParams = list(p = gsub("[[:print:]]+~", "", writeJagsFormula(model))),
       fromData = TRUE,
       nodeData = if ("data" %in% names(as.list(model$call)[-c(1, which(names(as.list(model$call)) == "formula"))])){
         if (is.null(model$model)) update(model, model=TRUE)$model
         else model$model
       } else NULL,
       nodeModel = model)
}

#' @rdname modelToNode
#' @export

modelToNode.lm <- function(model, ...){
  list(nodes = as.character(terms(model))[2],
       parents = names(attributes(terms(model))$dataClasses)[-1],
       nodeType = "dnorm",
       nodeFormula = model$call$formula,
       nodeFitter = as.character(model$call)[1],
       nodeFitterArgs = as.list(model$call)[-c(1, which(names(as.list(model$call)) %in% c("formula", "data")))],
       nodeParams = list(mu = gsub("[[:print:]]+~", "", writeJagsFormula(model)),
                         tau = 1/summary(model)$sigma),
       fromData = TRUE,
       nodeData = if ("data" %in% names(as.list(model$call)[-c(1, which(names(as.list(model$call)) == "formula"))])){
         if (is.null(model$model)) update(model, model=TRUE)$model
         else model$model
       } else NULL,
       nodeModel = model)
}

#' @rdname modelToNode
#' @export

modelToNode.multinom <- function(model, ...){
  list(nodes = as.character(terms(model))[2],
       parents = names(attributes(terms(model))$dataClasses)[-1],
       nodeType = "dcat",
       nodeFormula = model$call$formula,
       nodeFitter = as.character(model$call)[1],
       nodeFitterArgs = as.list(model$call)[-c(1, which(names(as.list(model$call)) %in% c("formula", "data")))],
       nodeParams = list(pi = gsub("[[:print:]]+~", "", writeJagsFormula(model))),
       fromData = TRUE,
       nodeData = if ("data" %in% names(as.list(model$call)[-c(1, which(names(as.list(model$call)) == "formula"))])){
         if (is.null(model$model)) update(model, model=TRUE)$model
         else model$model
       } else NULL,
       nodeModel = model)
}

#' @rdname modelToNode
#' @export

modelToNode.xtabs <- function(model, ...){
  list(nodes = names(attributes(model)$dimnames),
       parents = NULL,
       nodeType = "dcat",
       nodeFormula = attributes(model)$call$formula,
       nodeFitter = as.character(attributes(model)$call)[1],
       nodeFitterArgs = as.list(attributes(model)$call)[-c(1, which(names(as.list(attributes(model)$call)) %in% c("formula", "data")))],
       nodeParams = list(pi = writeJagsFormula(model)),
       fromData = FALSE,
       nodeData = NULL,
       nodeModel = model)
}

