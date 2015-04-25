.onLoad <- function(libname,pkgname)
{
  options(Hyde_fitModel=FALSE)
  options(Hyde_maxDigits = 5)
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
                                                   utility = "black")))
}

.onUnload <- function(libPath)
{
  options(Hyde_fitModel=NULL)
  options(Hyde_maxDigits = NULL)
}