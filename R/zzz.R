.onLoad <- function(libname,pkgname)
{
  options(Hyde_fitModel=FALSE)
}

.onUnload <- function(libPath)
{
  options(Hyde_fitModel=NULL)
}