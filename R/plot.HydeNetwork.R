#' @name plot.HydeNetwork
#' @aliases plot.HydeNetwork plotHydeNetwork
#' @export 
#' @importFrom graph plot
#' @method plot HydeNetwork
#' 
#' 
#' @title Plotting Utilities Probabilistic Graphical Network
#' @details Generate and customize plots the dag object of a \code{HydeNetwork} 
#'   class network. \code{HydeNet} provides some initial defaults for standard 
#'   variable nodes, deterministic nodes, decision nodes, and utility nodes.
#'   Since these nodes are assumed to be of inherent difference and interest, 
#'   the options are defined in a way to make these easier to identify in 
#'   a plot.  The default options may be altered by the user to their liking
#'   by invoking \code{HydePlotOptions}.  Node attributes are more fully 
#'   explained in the documentation for \code{?GraphvizAttributes}.  Individual
#'   nodes may be customized with \code{customNode}
#' 
#' @param x an object of class \code{HydeNetwork}
#' @param ... additional arguments to be passed to \code{graph::plot}.
#'   This may also contain named elements customizing nodes, as in
#'   \code{node = customNode(...)}.
#' @param useHydeDefaults A logical value indicating if the default plot
#'   parameters in \code{options("Hyde_plotOptions")} should be applied
#'   to the plot.
#' 
#' @details The plot method is a simple wrapper function to plot 
#'   \code{network$dag}.  
#'   
#'   When viewing the documentation for \code{GraphvizAttributes}, only
#'   the section Node Attributes is applicable to the settings used by 
#'   \code{HydeNet}.  The settings for Graph Attributes and Edge Attributes 
#'   may be used, but the user will need to provide those settings in an 
#'   appropriate manner using the \code{...} argument of the \plot{plot} method.
#'   
#'   Current settings may be turned off or set to the \code{GraphvizAttributes}
#'   defaults by supplying \code{NA} for an attribute parameter.  For example,
#'   the \code{HydeNet} default shape for utility nodes is "box".  This may be 
#'   turned off by using \code{utility = list(shape = NA)}.
#'   
#'   The plot settings can be reviewed at any time by calling 
#'   \code{options("Hyde_plotOptions")}.
#'   
#'   See the Plotting Hyde Networks vignette (\code{vignette("HydeNetPlots")})
#'   for a more thorough explanation of plotting networks.
#' 
#' @author Jarrod Dalton and Benjamin Nutter
#' @note
#'   For customizations, see the \code{Rgraphviz} documentation for 
#'   \code{GraphvizAttributes}.  Most notably, read the sections about 
#'   General Node Attributes to manipulate shapes and positions of the 
#'   nodes and their labels.  These are manipulated through the \code{nodeAttrs}
#'   arguments (passed through \code{...}).  
#'   
#'   General Graph Attributes are passed through \code{attrs} and Edge 
#'   Attributes are passed through \code{edgeAttrs}.
#'   
#' @seealso GraphvizAttributes
#' 
#' @examples
#' data(BlackJack, package="HydeNet")
#' plot(BlackJack)
#' 
#' HydePlotOptions(variable=list(shape = "rect", fillcolor = "green"),
#'                 determ = list(shape = "rect", fillcolor = "black",
#'                               fontcolor = "white", linecolor = "white"),
#'                 decision = list(shape = "ellipse", fillcolor = "yellow",
#'                                 linecolor = "red"),
#'                 utility = list(shape = "circle", fillcolor = "red", 
#'                                fontcolor = "white"))
#' plot(BlackJack)
#' 
#' HydePlotOptions(restorePackageDefault = TRUE)
#' 
#' plot(BlackJack,
#'      hit1 = customNode(fillcolor = "purple", shape = "circle", 
#'                        fontcolor = "white", height = "2"))

plot.HydeNetwork <- function(x, ..., useHydeDefaults=TRUE){
  #* If any nodes are being customized via the `customNode` function,
  #* we need to separate the `customeNode` calls from the rest of the 
  #* additional arguments.  
  #* Here, we spilit them into `custom_nodes` and `add_args` (additional args)
  add_args <- list(...)
  if (length(add_args) > 0){
    node_attr <- sapply(add_args,
                        function(a) class(a) == "HydeNodePlotAttributes")
    custom_nodes <- add_args[node_attr]
    add_args <- add_args[!node_attr]
  }
  else custom_nodes <- NULL
  
  if ("nodeAttrs" %in% names(add_args) & length(custom_nodes) > 0){
    stop(paste0("An argument named 'nodeAttrs' was given to `plot.HydeNet`",
                ".\nThis will likely produce conflicts with `customNode`.",
                "\nPlease consider using either 'nodeAttrs' or 'customNode', ",
                "but not both."))
    add_args <- add_args[!"nodeAttrs" %in% names(add_args)]
  }
  
  if ("nodeAttrs" %in% names(add_args) & useHydeDefaults){
    warning(paste0("An argument named 'nodeAttrs' was given to `plot.HydeNet`",
                   ".\nThis would conflict with the use of HydeNet defaults, ",
                   "so it is being ignored.  \nIf you wish to use this ",
                   "argument, please use 'useHydeDefaults=FALSE'"))
    add_args <- add_args[!"nodeAttrs" %in% names(add_args)]
  }
  
  #* If using HydePackageDefaults
  #* 1. Identify the nodes of each type
  #* 2. Make an attribute list for each plotting parameter
  #* 3. Combine the attribute lists into one massive list
  #* 4. Remove any attribute lists that are NULL
  #* 5. Reassign values for customized nodes
  #* 6. Plot the function if using customized nodes (requires do.call)
  #* 7. Plot the function if no customized nodes (can be run without do.call)
  if (useHydeDefaults){
    #* 1. Identify the nodes of each type
    decisionNodes <- names(which(unlist(x$nodeDecision)))
    utilityNodes <- names(which(unlist(x$nodeUtility)))
    determNodes <- names(which(unlist(x$nodeType)=="determ"))
    varNodes <- x$nodes[!x$nodes %in% c(decisionNodes, utilityNodes, determNodes)]

    #* 2. Make an attribute list for each plotting parameter
    makeAttributeList <- function(attr){
      att_list <- c(rep(getOption("Hyde_plotOptions")[[attr]]$variable, length(varNodes)),
                    rep(getOption("Hyde_plotOptions")[[attr]]$determ, length(determNodes)),
                    rep(getOption("Hyde_plotOptions")[[attr]]$decision, length(decisionNodes)),
                    rep(getOption("Hyde_plotOptions")[[attr]]$utility, length(utilityNodes)))
      if (!is.null(att_list)) names(att_list) <- c(varNodes, determNodes, decisionNodes, utilityNodes)
      return(att_list)
    }
    
    shape <- makeAttributeList("shape")
    fillcolor <- makeAttributeList("fillcolor")
    fontcolor <- makeAttributeList("fontcolor")
    linecolor <- makeAttributeList("linecolor")
    edgecolor <- makeAttributeList("edgecolor")
    distortion <- makeAttributeList("distortion")
    fixedsize <- makeAttributeList("fixedsize")
    fontname <- makeAttributeList("fontname")
    fontsize <- makeAttributeList("fontsize")
    height <- makeAttributeList("height")
    width <- makeAttributeList("width")
    sides <- makeAttributeList("sides")
    skew <- makeAttributeList("skew")
    
    #* 3. Combine the attribute lists into one massive list
    nodeAttribs <- list(shape=shape, fillcolor=fillcolor, fontcolor=fontcolor,
                        color=linecolor, edgecolor=edgecolor,
                        distortion = distortion, fixedsize=fixedsize,
                        fontname=fontname, fontsize=fontsize,
                        height=height, width=width,
                        sides=sides, skew=skew)
    
    #* 4. Remove any attribute lists that are NULL
    nodeAttribs <- nodeAttribs[!sapply(nodeAttribs, is.null)]
    

    #* 5. Reassign values for customized nodes
    if (!is.null(custom_nodes)){
      for (n in names(custom_nodes)){
        for (a in names(custom_nodes[[n]])){
          nodeAttribs[[a]][n] <- custom_nodes[[n]][[a]]
        }
      }
    }
    #* 6. Plot the network
    do.call(graph::plot,
            args = c(list(x$dag, nodeAttrs=nodeAttribs), add_args))
  }
  else{
    #* 1. generate a vector of names for which plotting parameters have been 
    #*    stated.
    #* 2. Create a list the same length as the vector of names and give that
    #*    list the names in the vector from 1.
    #* 3. for each parameter named in the list (l), extract any nodes for which
    #*    that parameter was named.
    #* 4. The unlist function appends the name of the list element to the end
    #*    of the node name in the format ".name".  To correctly construct the 
    #*    list, we want to remove all of the characters from the last "." to 
    #*    the end.  We use a perl-style regular expression to do that.
    #* 5. place the node name into the nodeAttribs list element.
    if (length(custom_nodes) > 0){
      #* 1. generate a vector of names for which plotting parameters have been 
      #*    stated.
      list_names <- unique(as.vector(sapply(custom_nodes, names)))
      #* 2. Create a list the same length as the vector of names and give that
      #*    list the names in the vector from 1.
      nodeAttribs <- lapply(list_names, function(x) NULL)
      names(nodeAttribs) <- list_names

      for(l in list_names){
        #* 3. for each parameter named in the list (l), extract any nodes for which
        #*    that parameter was named.
        nodes_with_attrib <- unlist(sapply(custom_nodes, "[", l))
        #* 4. The unlist function appends the name of the list element to the end
        #*    of the node name in the format ".name".  To correctly construct the 
        #*    list, we want to remove all of the characters from the last "." to 
        #*    the end.  We use a perl-style regular expression to do that.
        names(nodes_with_attrib) <- sub("\\.(?=[^.]*$)[[:print:]]+$", "", 
                                        names(nodes_with_attrib), perl=TRUE)
        #* 5. place the node name into the nodeAttribs list element.
        nodeAttribs[[l]] <- nodes_with_attrib
      }
      do.call(graph::plot,
              args = c(list(x$dag, nodeAttrs = nodeAttribs), add_args))
    }
    else graph::plot(x$dag, ...)
  }
}

#' @rdname plot.HydeNetwork
#' @export HydePlotOptions
#' 
#' @param variable A named list of default parameters for standard variables.
#' @param determ A named list of default parameteres for deterministic nodes.
#' @param decision A named list of default parameters for decision nodes.
#' @param utility A named list of default parameters for utility nodes.
#' @param restorePackageDefault When \code{TRUE}, the initial package defaults
#'   are restored.
#'   
HydePlotOptions <- function(variable = NULL,
                            determ = NULL,
                            decision = NULL,
                            utility = NULL, 
                            restorePackageDefault = FALSE){
  if (restorePackageDefault)
    options(Hyde_plotOptions = list(fillcolor = list(variable = "white",
                                                determ = "white",
                                                decision = "#6BAED6",
                                                utility = "#FFFFB2"),
                                    shape = list(variable = "ellipse",
                                                 determ = "ellipse",
                                                 decision = "rect",
                                                 utility = "rect"),
                                    fontcolor = list(variable = "black",
                                                     determ = "gray70",
                                                     decision = "black",
                                                     utility = "black"),
                                    linecolor = list(variable = "black",
                                                     determ = "gray70",
                                                     decision = "black",
                                                     utility = "black"),
                                    edgecolor = NULL,
                                    distortion = NULL,
                                    fixedsize = NULL,
                                    fontname = NULL,
                                    fontsize = NULL,
                                    height = NULL,
                                    width = NULL,
                                    sides = NULL,
                                    skew = NULL))
  else {
    current_options <- getOption("Hyde_plotOptions")
    
    updatePlotOptions <- function(current_opts, new_opts, type){
      for(n in names(new_opts)){
        current_opts[n][[1]][[type]] <- if (is.null(new_opts)) NULL else new_opts[[n]]
      }
      return(current_opts)
    }
    
    for (t in c("variable", "determ", "decision", "utility")){
      if (!is.null(get(t))){
        current_options <- updatePlotOptions(current_options, get(t), t)
      }
    }
    
    options(Hyde_plotOptions = current_options)
  }
}

#' @rdname plot.HydeNetwork
#' @export customNode

customNode <- function(...){
  nodeAttrs <- list(...)
  class(nodeAttrs) <- "HydeNodePlotAttributes"
  if (length(nodeAttrs) > 0) return(nodeAttrs)
}

