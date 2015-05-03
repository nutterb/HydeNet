.onLoad <- function(libname,pkgname)
{
  options(Hyde_fitModel=FALSE)
  options(Hyde_maxDigits = 5)
  options(Hyde_plotOptions = 
    data.frame(type = c("variable", "determ", "decision", "utility"),
               fillcolor = c("white", "white", "#6BAED6", "#FFFFB2"),
               shape = c("ellipse", "ellipse", "rect", "rect"),
               fontcolor = c("black", "gray70", "black", "black"),
               color = c("black", "gray70", "black", "black"),
               style = c("filled", "filled", "filled", "filled"),
               stringsAsFactors=FALSE))
#   options(Hyde_plotOptions = list(fillcolor = list(variable = "white",
#                                                    determ = "white",
#                                                    decision = "#6BAED6",
#                                                    utility = "#FFFFB2"),
#                                   shape = list(variable = "ellipse",
#                                                determ = "ellipse",
#                                                decision = "rect",
#                                                utility = "rect"),
#                                   fontcolor = list(variable = "black",
#                                                    determ = "gray70",
#                                                    decision = "black",
#                                                    utility = "black"),
#                                   linecolor = list(variable = "black",
#                                                    determ = "gray70",
#                                                    decision = "black",
#                                                    utility = "black"),
#                                   edgecolor = NULL,
#                                   distortion = NULL,
#                                   fixedsize = NULL,
#                                   fontname = NULL,
#                                   fontsize = NULL,
#                                   height = NULL,
#                                   width = NULL,
#                                   sides = NULL,
#                                   skew = NULL))
}

.onUnload <- function(libPath)
{
  options(Hyde_fitModel=NULL)
  options(Hyde_maxDigits = NULL)
  options(Hyde_plotOptions = NULL)
}