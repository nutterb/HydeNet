#' @name plot.HydeNetwork
#' @aliases plot.HydeNetwork plotHydeNetwork
#' @export 
#' @method plot HydeNetwork
#' 
#' 
#' @title Plotting Utilities for Probabilistic Graphical Network
#' @description Generate and customize plots of a \code{HydeNetwork} 
#'   class network. \code{HydeNet} provides some initial defaults for standard 
#'   variable nodes, deterministic nodes, decision nodes, and utility nodes.
#'   Since these nodes are assumed to be of inherent difference and interest, 
#'   the options are defined in a way to make these easier to identify in 
#'   a plot.  The default options may be altered by the user to their liking
#'   by invoking \code{HydePlotOptions}.  Node attributes are more fully 
#'   explained in the documentation for the \code{DiagrammeR} package.  
#'   Individual nodes may be define with \code{customNode}.
#' 
#' @param x an object of class \code{HydeNetwork}
#' @param customNodes a data frame giving additional specifications for nodes.
#'   The customizations provided here will override the default settings.
#' @param customEdges a data frame giving custom settings for edges (arrows)
#'   between nodes.
#' @param ... for the \code{plot} method, additional arguments to be passed to 
#'   \code{DiagrammeR::render_graph}.  For \code{customNode}, 
#'   named node attributes to assign to a node's plotting characteristics.
#' @param removeDeterm A logical value.  When \code{FALSE} (the default), 
#'   it has no effect.  When \code{TRUE}, deterministic nodes are removed
#'   from the network and a reduced plot with no deterministic nodes
#'   is rendered.
#' @param useHydeDefaults A logical value indicating if the default plot
#'   parameters in \code{options("Hyde_plotOptions")} should be applied
#'   to the plot.
#' 
#' @details GraphViz is an enormous set of resources for customizing and we 
#'   cannot adequately describe them all here.  See 'Sources' for links 
#'   to additional documentation from the \code{DiagrammeR} package and the 
#'   GraphViz website.
#'   
#'   With its default settings, \code{HydeNet} makes use of five node 
#'   attributes for plotting networks.  These are 
#'   \itemize{
#'     \item style: By default, set to 'filled', but may also take 'striped',
#'       'wedged', or 'radial'.
#'     \item fillcolor: The hexadecimal or X11 color name.  In styles 'striped',
#'       'wedged', or 'radial', this may take multiple colors separated by a 
#'       colon (:).
#'     \item shape: the node shape.  May take the values 'oval', 'diamond',
#'       'egg', 'ellipse', 'square', 'triangle', or 'rect'
#'     \item fontcolor: The color of the node label.
#'     \item color: The color of the node's border.
#'    }
#'    
#'   \code{HydeNet} assumes the GraphViz defaults for edge nodes (arrows).
#'   
#'   See the Plotting Hyde Networks vignette (\code{vignette("HydeNetPlots")})
#'   for a more thorough explanation of plotting networks.  
#' 
#' @author Jarrod Dalton and Benjamin Nutter
#'   
#' @source 
#'   \url{http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html}\cr
#'   See especially the section on Attributes
#'   
#'   \url{http://graphviz.org/}
#' 
#' @examples
#' \dontrun{
#' #* Plots may open in a browser.
#' data(BlackJack, package="HydeNet")
#' plot(BlackJack)
#'
#' HydePlotOptions(variable=list(shape = "rect", fillcolor = "#A6DBA0"),
#'                 determ = list(shape = "rect", fillcolor = "#E7D4E8",
#'                               fontcolor = "#1B7837", linecolor = "#1B7837"),
#'                 decision = list(shape = "triangle", fillcolor = "#1B7837",
#'                                 linecolor = "white"),
#'                 utility = list(shape = "circle", fillcolor = "#762A83", 
#'                                fontcolor = "white"))
#' plot(BlackJack)
#' 
#' HydePlotOptions(restorePackageDefaults = TRUE)
#' 
#' plot(BlackJack,
#'      customNodes = customNode(payoff, 
#'                               fillcolor = "purple", shape = "circle", 
#'                               fontcolor = "white", height = "2",
#'                               style="filled"))
#' plot(BlackJack,
#'   customNodes = 
#'     dplyr::bind_rows(
#'       customNode(pointsAfterCard3,
#'                  shape = "circle",
#'                  style = "radial",
#'                  fillcolor = "#1B7837:#762A83",
#'                  fontcolor = "black",
#'                  height = "2"),
#'       customNode(playerFinalPoints,
#'                  shape = "circle",
#'                  style = "wedged",
#'                  height = "3",
#'                  fillcolor = c("red:orange:yellow:green:blue:purple"))))
#' }


plot.HydeNetwork <- function(x, 
                             customNodes = NULL,
                             customEdges = NULL,
                             ..., 
                             removeDeterm = FALSE,
                             useHydeDefaults = TRUE)
{
  if (removeDeterm) x <- plot_nondeterm_only(x)
  
  node_df <- 
    DiagrammeR::create_node_df(n = length(x[["nodes"]]),
                               label = x[["nodes"]])
  # 
  # node_df <- data.frame(nodes = x[["nodes"]],
  #                       stringsAsFactors = FALSE)
  if (useHydeDefaults) node_df <- mergeDefaultPlotOpts(x, node_df)
  
  if (!is.null(customNodes)) node_df <- mergeCustomNodes(node_df, customNodes)
  
  edge_table <- do.call("rbind", 
                        mapply(FUN = mapEdges, 
                               x[["nodes"]], 
                               x[["parents"]],
                               MoreArgs = list(node_df = node_df)))
  
  edge_df <- DiagrammeR::create_edge_df(from = edge_table[, 2], 
                                        to = edge_table[, 1])
  
  if (!is.null(customEdges)) mergeCustomEdges(edge_df, customEdges)
  
  
  
  graph <- DiagrammeR::create_graph(nodes_df = node_df,
                                    edges_df = edge_df,
                                    attr_theme = NULL) 
  DiagrammeR::render_graph(graph)
  
}

#' rdname plot.HydeNetwork
#' @param network a \code{HydeNetwork} object
#' @param node_df A data frame of node attributes.
#' 
mergeDefaultPlotOpts <- function(network, node_df)
{
  nodes <- network[["nodes"]]
  node_df <- node_df %>%
    dplyr::mutate(
      type = 
        ifelse(test = network[["nodeUtility"]][nodes], 
               yes = "utility",
               no = ifelse(test = network[["nodeDecision"]][nodes], 
                           yes = "decision",
                           no = ifelse(test = network[["nodeType"]][nodes] == "determ", 
                                       yes = "determ",
                                       no = "variable"
                           )
               )
        )
    )
  
  node_df <- 
    dplyr::left_join(
      node_df, 
      getOption("Hyde_plotOptions"),
      by="type") %>%
    dplyr::select(-type)
  
  node_df[, -which(names(node_df) == "nodes")] <- 
    lapply(node_df[, -which(names(node_df) == "nodes"), drop=FALSE],
           function(x) ifelse(test = is.na(x), 
                              yes = "", 
                              no = x))
  node_df
}

#' @rdname plot.HydeNetwork
#' @param node_df A data frame of node attributes
#' 
mergeCustomNodes <- function(node_df, customNodes)
{
  #   node_df <- dplyr::mutate(node_df, index=2)
  #   customNodes <- dplyr::mutate(customNodes, index=1)
  node_df <- 
    dplyr::full_join(
      customNodes, 
      node_df,
      by = c("nodes" = "label")
    )
  
  duplicated_names.x <- names(node_df)[grepl(pattern = "[.]x", 
                                             x = names(node_df))]
  if (length(duplicated_names.x) > 0)
  {
    duplicated_names.y <- gsub(pattern = "[.]x", 
                               replacement = ".y", 
                               x = duplicated_names.x)
    for(i in 1:length(duplicated_names.y))
    {
      node_df[[duplicated_names.x[i]]] <- 
        ifelse(test = is.na(node_df[[duplicated_names.x[i]]]),
               yes = node_df[[duplicated_names.y[i]]],
               no = node_df[[duplicated_names.x[i]]])
    }
  }
  
  if (any(grepl(pattern = "[.]y", 
                x = names(node_df))))
    node_df <- dplyr::select(node_df, -dplyr::ends_with('.y'))
  
  names(node_df) <- gsub(pattern = "[.]x", 
                         replacement = "", 
                         x = names(node_df))
  
  node_df[, -which(names(node_df) == "nodes")] <- 
    lapply(X = node_df[, -which(names(node_df) == "nodes")],
           FUN = function(x) ifelse(test = is.na(x), 
                                    yes = "", 
                                    no = x))
  return(node_df)
}

#' @rdname plot.HydeNetwork
#' @param n node names from a network object
#' @param p the list of parents from a network object
mapEdges <- function(n, p, node_df)
{
  n <- node_df[["id"]][match(n, node_df[["label"]])]
  p <- node_df[["id"]][match(p, node_df[["label"]])]
  cbind(rep(n, length(p)),
        p)
}

#' @rdname plot.HydeNetwork
#' @param edge_df The default edge attribute data frame
#' 
mergeCustomEdges <- function(edge_df, customEdges)
{
  edge_df <- dplyr::mutate(edge_df, index = 2)
  customEdges <- dplyr::mutate(customEdges, index = 1)
  
  dplyr::bind_rows(customEdges, edge_df) %>%
    dplyr::group_by(from, to)  %>%
    dplyr::filter(rank(index, ties.method='first')==1) %>%
    dplyr::select(-index)
}

#' @rdname plot.HydeNetwork 
#' @export customNode
#' @param node_id The name of a node in a \code{HydeNetwork} object.
#'   May be quoted or unquoted.
#'   
customNode <- function(node_id, ...)
{
  node_id <- as.character(substitute(node_id))
  nodeAttrs <- as.data.frame(c(list(nodes = node_id),
                               list(...)), 
                             stringsAsFactors=FALSE)
  if (length(nodeAttrs) > 0) return(nodeAttrs)
}

#' @rdname plot.HydeNetwork
#' @export HydePlotOptions
#' @param variable,determ,decision,utility Named lists of attributes to use as 
#'   defaults node attribute settings for each variable type.
#' @param restorePackageDefaults A logical value.  When TRUE, the original 
#'   package defaults are restored.
HydePlotOptions <- function(variable = NULL,
                            determ = NULL,
                            decision = NULL,
                            utility = NULL, 
                            restorePackageDefaults = FALSE)
{
  if (restorePackageDefaults)
    options(Hyde_plotOptions = 
              data.frame(type = c("variable", "determ", "decision", "utility"),
                         fillcolor = c("white", "white", "#6BAED6", "#FFFFB2"),
                         shape = c("ellipse", "ellipse", "rect", "rect"),
                         fontcolor = c("black", "gray70", "black", "black"),
                         color = c("black", "gray70", "black", "black"),
                         style = c("filled", "filled", "filled", "filled"),
                         stringsAsFactors=FALSE))
  else {
    current_options <- getOption("Hyde_plotOptions")
    
    new_options <- dplyr::bind_rows(
      lapply(list(variable, determ, decision, utility),
             as.data.frame,
             stringsAsFactors=FALSE))
    new_options$type <- c(if (is.null(variable)) NULL else "variable", 
                          if (is.null(determ)) NULL else "determ", 
                          if (is.null(decision)) NULL else "decision", 
                          if (is.null(utility)) NULL else "utility")
    
    new_options <- dplyr::full_join(new_options, current_options,
                                    by = c("type" = "type"))
    shared_names <- names(new_options)[grepl(pattern = "[.]x", 
                                             x = names(new_options))]
    if (length(shared_names) > 0)
    {
      for (s in shared_names)
      {
        new_options[[s]] <- 
          as.vector(mapply(FUN = function(x, y) ifelse(is.na(x), 
                                                          y, 
                                                          x),
                              new_options[s],
                              new_options[gsub("[.]x", ".y", s)]))
      }
      new_options <- dplyr::select(new_options, -dplyr::ends_with('.y'))                                      
    }
    
    names(new_options) <- gsub(pattern = "[.]x", 
                               replacement = "", 
                               x = names(new_options))
    
    new_options[, which(names(new_options) == "type")] <- 
      lapply(X = new_options[, which(names(new_options) == "type"), drop=FALSE],
             FUN = function(x) ifelse(test = is.na(x), 
                                      yes = "", 
                                      no = x))
    
    options(Hyde_plotOptions = new_options)
  }
}


utils::globalVariables(c("from", "to", "index", "type"))