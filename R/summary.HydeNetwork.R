#' @name HydeNetSummaries
#' @title HydeNet Summary Objects
#' 
#' @description Summaries of \code{HydeNetwork}, compiled network, and
#'   compiled decision network objects.
#'   
#' @param x A \code{HydeNet} object to be summarized
#' @param ... Additional arguments.
#' 
#' @author Jarrod Dalton and Benjamin Nutter
#' 

summary.HydeNetwork <- function(x, ...){

  cat(node_decision(x), 
      "\n\n",
      node_utility(x), 
      "\n\n",
      node_determ(x),
      "\n\n",
      node_random(x),
      sep = "")
}

node_decision <- function(network){
  name <-   names(network$nodeDecision)[sapply(network$nodeDecision, identity)]
  parents <- vapply(network$parents[name],
                    length,
                    numeric(1))
  parents <- ifelse(parents == 1, 
                    paste0(parents, " parent"),
                    paste0(parents, " parents"))
  decision_policy <-
    vapply(network$nodePolicyValues[name],
           paste0,
           character(1),
           collapse = ", ")
  
  decision_policy <- ifelse(decision_policy == "",
                            "(no policies defined)",
                            decision_policy)
  
  paste0("Decision Nodes: \n",
         paste0(pad_summary_str(name), "  |",
                pad_summary_str(parents, "left"), "  |",
                pad_summary_str(decision_policy), collapse = "\n"))
  
}

node_utility <- function(network){
  name <-   names(network$nodeUtility)[sapply(network$nodeUtility, identity)]
  parents <- vapply(network$parents[name],
                    length,
                    numeric(1))
  parents <- ifelse(parents == 1, 
                    paste0(parents, " parent"),
                    paste0(parents, " parents"))
  paste0("Utility Nodes: \n",
         paste0(pad_summary_str(name), "  |",
                pad_summary_str(parents, "left"), 
                collapse = "\n"))
  
}

node_determ <- function(network){
  name <-   names(network$nodeType)[sapply(network$nodeType, 
                                           function(x) x == "determ")]
  parents <- vapply(network$parents[name],
                    length,
                    numeric(1))
  parents <- ifelse(parents == 1, 
                    paste0(parents, " parent"),
                    paste0(parents, " parents"))
  paste0("Deterministic Nodes: \n",
         paste0(pad_summary_str(name), "  |",
                pad_summary_str(parents, "left"), 
                collapse = "\n"))
  
}

node_random <- function(network){
  determ <-   names(network$nodeType)[sapply(network$nodeType, 
                                           function(x) x == "determ")]
  decision <-   names(network$nodeDecision)[sapply(network$nodeDecision, identity)]
  utility <-   names(network$nodeUtility)[sapply(network$nodeUtility, identity)]
  
  name <- network$nodes[!network$nodes %in% c(determ, decision, utility)]
  parents <- vapply(network$parents[name],
                           length,
                           numeric(1))
  parents <- ifelse(parents == 1, 
                    paste0(parents, " parent"),
                    paste0(parents, " parents"))
  
  type <- unlist(network$nodeType[name])
  
  paste0("Random Nodes: \n",
         paste0(pad_summary_str(name), "  |",
                pad_summary_str(parents, "left"), "  |",
                pad_summary_str(type),
                collapse = "\n"))
  
}

pad_summary_str <- function(string, side = "right"){
  paste0("  ", 
         stringr::str_pad(string, 
                   width = max(nchar(string)),
                   side = side,
                   pad = " "))
}