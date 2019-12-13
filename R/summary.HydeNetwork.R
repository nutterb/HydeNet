#' @name HydeNetSummaries
#' @title HydeNet Summary Objects
#' 
#' @description Summaries of \code{HydeNetwork}, compiled network, and
#'   compiled decision network objects.
#'   
#' @param object A \code{HydeNet} object to be summarized
#' @param ... Additional arguments.
#' 
#' @author Jarrod Dalton and Benjamin Nutter
#' 
#' @method summary HydeNetwork
#' @export

summary.HydeNetwork <- function(object, ...)
{
  decision_nodes <- 
    names(object[["nodeDecision"]])[vapply(X = object[["nodeDecision"]], 
                                           FUN = identity,
                                           FUN.VALUE = logical(1))]
  utility_nodes <- 
    names(object[["nodeUtility"]])[vapply(X = object[["nodeUtility"]], 
                                          FUN = identity,
                                          FUN.VALUE = logical(1))]
  deterministic_nodes <- 
    names(object[["nodeType"]])[vapply(X = object[["nodeType"]],
                                       FUN = function(x) x == "determ",
                                       FUN.VALUE = logical(1))]
  
  random_nodes <- object[["nodes"]][!object[["nodes"]] %in% c(decision_nodes, 
                                          utility_nodes,
                                          deterministic_nodes)]
  cat("Decision Nodes: \n",
      decision_node_summary(object, decision_nodes),
      "\n\n",
      "Utility Nodes: \n",
      utility_node_summary(object, utility_nodes),
      "\n\n",
      "Deterministic Nodes: \n",
      utility_node_summary(object, deterministic_nodes),
      "\n\n",
      "Random Nodes: \n",
      random_node_summary(object, random_nodes),
      "\n",
      sep = ""
  )
}

decision_node_summary <- function(object, nodes)
{
  name_summary <- summarise_node_name(nodes)
  ns_charwid <- ifelse(length(name_summary)>0, max(nchar(name_summary)), 0)
  parent_summary <- summarise_parents(object, nodes, ns_charwid)
  ps_charwid <- ifelse(length(parent_summary)>0, max(nchar(parent_summary)), 0)
  policy_summary <- summarise_policy(object, nodes, ns_charwid, ps_charwid)
  paste0(name_summary, parent_summary, policy_summary, collapse = "\n")
}

utility_node_summary <- function(object, nodes)
{
  name_summary <- summarise_node_name(nodes)
  ns_charwid <- ifelse(length(name_summary)>0, max(nchar(name_summary)), 0)
  parent_summary <- summarise_parents(object, nodes, ns_charwid, end_sep = "")
  paste0(name_summary, parent_summary, collapse = "\n")
}

random_node_summary <- function(object, nodes)
{
  name_summary <- summarise_node_name(nodes)
  ns_charwid <- ifelse(length(name_summary)>0, max(nchar(name_summary)), 0)
  parent_summary <- summarise_parents(object, nodes, ns_charwid, end_sep = "")
  type_summary <- summarise_type(object, nodes)
  
  paste0(name_summary, parent_summary, type_summary, collapse = "\n")
}


summarise_node_name <- function(nodes, max.width = 20)
{
  if(length(nodes)>0) max.width <- min(c(max.width, max(nchar(nodes))+3))

  ifelse(test = nchar(nodes) > (max.width - 2),
         yes = paste0(substr(nodes, 1, 14), "...  |  "),
         no = paste0(stringr::str_pad(string = nodes, 
                                      width = max.width - 2, 
                                      side = "right"), 
                     "  |  "))
}

summarise_parents <- function(object, nodes, name_width, end_sep = "  |  ")
{
  max.width <- floor((getOption("width") - name_width) / 2)
  parents <- vapply(object[["parents"]][nodes],
                    paste0,
                    character(1),
                    collapse = ", ")
  nparents <- vapply(object[["parents"]][nodes],
                     length,
                     numeric(1))
  
  parents <- 
    ifelse(nchar(parents) > (max.width - 2),
           ifelse(test = nparents == 1,
                  yes = "1 parent",
                  no = paste0(nparents, " parents  ")),
           parents)
  parents <- 
    stringr::str_pad(string = parents, 
                     width = ifelse(length(parents)>0, max(nchar(parents)), 0)+1, 
                     side = "right")
  
  if(length(parents)>0) paste0(parents, end_sep) else ""
}

summarise_policy <- function(object, nodes, name_width, parent_width)
{
  max.width <- getOption("width") - name_width - parent_width
  decision_policy <-
    vapply(object[["nodePolicyValues"]][nodes],
           paste0,
           character(1),
           collapse = ", ")
  
  decision_policy <- ifelse(test = decision_policy == "",
                            yes = "(no policies defined)",
                            no = decision_policy)
  
  ifelse(test = nchar(decision_policy) > max.width,
         yes = paste0(substr(decision_policy, 1, max.width - 3), "..."),
         no = decision_policy)
}

summarise_type <- function(object, nodes)
{
  unlist(object[["nodeType"]][nodes])
}
