#' @name factorRegex
#' 
#' @title Produce Regular Expressions for Extracting Factor Names and Levels
#' @description A utility function to produce a regular expression that can
#'   separate factor names and factor levels in the \code{broom::tidy()$term}
#'   output.
#'   
#' @param fit a model object
#' 
#' @author Benjamin Nutter

factorRegex <- function(fit){
  fctr <- attributes(terms(fit))$dataClasses
  if (any(fctr == "factor")){
    fctr <- names(fctr)[fctr == "factor"]
    fctr_regex <- paste0(fctr, collapse="|")
    fctr_regex <- gsub("[(]", "[(]", fctr_regex)
    fctr_regex <- gsub("[)]", "[)]", fctr_regex)
    fctr_regex <- gsub("[.]", "[.]", fctr_regex)
    fctr_regex <- paste0("(", fctr_regex, ")")
    return(fctr_regex)
  }
  else NULL
}




