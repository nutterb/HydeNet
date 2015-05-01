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
#'   explained in the documentation for \code{?GraphvizAttributes}.
#' 
#' @param x an object of class \code{HydeNetwork}
#' @param ... additional arguments to be passed to \code{graph::plot}
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
#' carNet <- HydeNetwork( ~ cyl + 
#'                       disp | cyl + 
#'                       hp | disp + 
#'                       wt + 
#'                       gear + 
#'                       mpg | disp*hp*wt*gear,
#'                       data=mtcars)
#' plot(carNet)
#' 
#' #* Change node colors, shapes, and labels.
#' #* See "Notes" for more details.
#' plot(carNet, 
#'      nodeAttrs=list(fillcolor=list(cyl="green", am="blue"),
#'                     shape=list(am="rect", mpg="ellipse"),
#'                     label=list(cyl="Cylinder", disp="Displacement", 
#'                                mpg="Miles per Gallon")))

plot.HydeNetwork <- function(x, ..., useHydeDefaults=TRUE){
  if (useHydeDefaults){
    decisionNodes <- names(which(unlist(x$nodeDecision)))
    utilityNodes <- names(which(unlist(x$nodeUtility)))
    determNodes <- names(which(unlist(x$nodeType)=="determ"))
    varNodes <- x$nodes[!x$nodes %in% c(decisionNodes, utilityNodes, determNodes)]
    
    shape <- c(rep(getOption("Hyde_plotOptions")$shape$variable, length(varNodes)),
               rep(getOption("Hyde_plotOptions")$shape$determ, length(determNodes)),
               rep(getOption("Hyde_plotOptions")$shape$decision, length(decisionNodes)),
               rep(getOption("Hyde_plotOptions")$shape$utility, length(utilityNodes)))
    names(shape) <- c(varNodes, determNodes, decisionNodes, utilityNodes)
    
    fill <- c(rep(getOption("Hyde_plotOptions")$fill$variable, length(varNodes)),
              rep(getOption("Hyde_plotOptions")$fill$determ, length(determNodes)),
              rep(getOption("Hyde_plotOptions")$fill$decision, length(decisionNodes)),
              rep(getOption("Hyde_plotOptions")$fill$utility, length(utilityNodes)))
    names(fill) <- c(varNodes, determNodes, decisionNodes, utilityNodes)
    
    fontcolor <- c(rep(getOption("Hyde_plotOptions")$fontcolor$variable, length(varNodes)),
                   rep(getOption("Hyde_plotOptions")$fontcolor$determ, length(determNodes)),
                   rep(getOption("Hyde_plotOptions")$fontcolor$decision, length(decisionNodes)),
                   rep(getOption("Hyde_plotOptions")$fontcolor$utility, length(utilityNodes)))
    names(fontcolor) <- c(varNodes, determNodes, decisionNodes, utilityNodes)
    
    linecolor <- c(rep(getOption("Hyde_plotOptions")$linecolor$variable, length(varNodes)),
                   rep(getOption("Hyde_plotOptions")$linecolor$determ, length(determNodes)),
                   rep(getOption("Hyde_plotOptions")$linecolor$decision, length(decisionNodes)),
                   rep(getOption("Hyde_plotOptions")$linecolor$utility, length(utilityNodes)))
    names(linecolor) <- c(varNodes, determNodes, decisionNodes, utilityNodes)
    
    edgecolor <- c(rep(getOption("Hyde_plotOptions")$edgecolor$variable, length(varNodes)),
                   rep(getOption("Hyde_plotOptions")$edgecolor$determ, length(determNodes)),
                   rep(getOption("Hyde_plotOptions")$edgecolor$decision, length(decisionNodes)),
                   rep(getOption("Hyde_plotOptions")$edgecolor$utility, length(utilityNodes)))
    if (!is.null(edgecolor)) 
      names(edgecolor) <- c(varNodes, determNodes, decisionNodes, utilityNodes)
                   
    nodeAttribs <- list(shape=shape, fillcolor=fill, fontcolor=fontcolor,
                        color=linecolor, edgecolor=edgecolor)
    nodeAttribs <- nodeAttribs[!sapply(nodeAttribs, is.null)]
    
    graph::plot(x$dag, nodeAttrs=nodeAttribs, ...)
  }
  else graph::plot(x$dag, ...)
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
    options(Hyde_plotOptions = list(fill = list(variable = "white",
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

