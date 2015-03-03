.onLoad <- function(libname,pkgname)
{
  options(Hyde_fitModel=FALSE)
  options(Hyde_maxDigits = 5)
}

.onUnload <- function(libPath)
{
  options(Hyde_fitModel=NULL)
}