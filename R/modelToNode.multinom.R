#' @rdname modelToNode

modelToNode.multinom <- function(model, ...){
  list(nodes = as.character(terms(model))[2],
       parents = names(attributes(terms(model))$dataClasses)[-1],
       nodeType = "dcat",
       nodeFormula = model$call$formula,
       nodeFitter = as.character(model$call)[1],
       nodeFitterArgs = as.list(model$call)[-c(1, which(names(as.list(model$call)) %in% c("formula", "data")))],
       nodeParams = list(pi = writeJagsFormula(model)),
       fromData = TRUE,
       nodeData = if ("data" %in% names(as.list(model$call)[-c(1, which(names(as.list(model$call)) == "formula"))])){
         if (is.null(model$model)) update(model, model=TRUE)$model
         else model$model
       } else NULL,
       nodeModel = model)
}

