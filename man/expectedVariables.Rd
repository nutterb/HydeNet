% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expectedVariables.R
\name{expectedVariables}
\alias{expectedVariables}
\alias{expectedParameters}
\title{List Expected Parameter Names and Expected Variables Names}
\usage{
expectedVariables(network, node, returnVector = FALSE)

expectedParameters(network, node, returnVector = FALSE)
}
\arguments{
\item{network}{A \code{HydeNetwork} object.}

\item{node}{A node name within \code{network}}

\item{returnVector}{Logical.  If FALSE, the sample string for use in
\code{setNode} is returned.  Otherwise, the vector of parent names
is returned.}
}
\description{
To assist in formula that defines the relationship to a node, 
  \code{expectedVariables} returns to the console
  a sample string that can be pasted into \code{setNode}
  and populated with the desired coefficients.
}
\details{
Each node is calculated as a model of its parents.  If no 
training data are provided to the network, the user is expected to provide
appropriate estimates of the regression coefficients for the model.

\code{returnVector} will generally be set to \code{FALSE} for most uses, 
but can be set to \code{TRUE} for use in error checking.  For example, 
in \code{setNode}, if not all of the parents have been given a coefficient 
(or if too few coefficients have been given), the vector of names is supplied.
}
\examples{
data(PE, package="HydeNet")
Net <- HydeNetwork(~ wells + 
                     pe | wells + 
                     d.dimer | pregnant*pe + 
                     angio | pe + 
                     treat | d.dimer*angio + 
                     death | pe*treat)
  
expectedVariables(Net, wells)
expectedVariables(Net, treat)
expectedVariables(Net, treat, returnVector=TRUE)

expectedParameters(Net, wells)
expectedParameters(Net, wells, returnVector=TRUE)
}
\author{
Jarrod Dalton and Benjamin Nutter
}
