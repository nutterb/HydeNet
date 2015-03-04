#' @rdname modelToNode

modelToNode.default <- function(model, ...){
  fitter <- tail(as.character(as.list(model$call)[[1]]), 1)
  if (!fitter %in% c("lm", "glm", "multinom", "xtabs"))
    stop(paste0("The Hyde package only accepts models built by the following functions:\n",
                "  lm, glm (family=\"binomial\" only), multinom, xtabs"))
}
