#' @rdname modelToNode

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
