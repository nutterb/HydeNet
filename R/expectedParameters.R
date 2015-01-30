#' @rdname expectedVariables
#' @export expectedParameters
expectedParameters <- function(network, node, returnVector=FALSE){
  node <- as.character(substitute(node))
  inputs <- network$nodeType[[node]]
  
#   data(jagsDists, package='Hyde')
  params <- jagsDists$Parameters[jagsDists$FnName == inputs]
  
  if (returnVector) return(params)
  else cat(paste("c(", paste(paste0(params, "= "), collapse=", "), ")"))
}
