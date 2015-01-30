#' @name HydePackageDependencies
#' 
#' @title Install package dependencies for \code{Hyde}
#' @description Some of the package dependencies required for \code{Hyde}
#'   are not available on CRAN (\code{gRbase} depends on \code{RBGL} and 
#'   \code{Rgraphviz} depends on \code{graph}).  Running \code{HydePackageDependencies}
#'   will install these packages from Bioconductor.  I've taken this out of the exports
#'   as I think I now understand how to get around it.
#'   
#' @param suppressUpdates Logical.  Determines if other packages in the library should
#'   be updated.  Defaults to suppression of updates.
#' @param ... Additional arguments to be passed to \code{biocLite}.  See References.
#' 
#' @details \code{biocLite} is a function that is recommended for installing 
#'   packages from Bioconductor.  It can be placed into the workspace with
#'   \code{source("http://bioconductor.org/biocLite.R")}.  Documentation of the
#'   function is available on Bioconductor's website. (See References)
#'   
#' @author Bioconductor
#' @references \url{http://www.bioconductor.org/install/}
#' 
#' @examples
#' \dontrun{
#' HydePackageDependencies()
#' }
#' 

HydePackageDependencies <- function(suppressUpdates=TRUE, ...){
  biocLite <- NULL
  source("http://bioconductor.org/biocLite.R")
  biocLite(c("RBGL", "Rgraphviz"), suppressUpdates=suppressUpdates, 
           ...)
}