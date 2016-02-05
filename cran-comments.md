## Test environments
* local Windows install (release)
* local Windows install (devel [2016-02-04 r70088])
* win-builder (devel[2015-10-09 r69501] and release)
* Ubuntu 12.04 LTS (Travis-CI)

## R CMD check results

The local Windows install using the release version
returned the expected NOTE about the package maintainer and
license file.

A NOTE was returned by win-builder (release and devel) regarding 
potential spelling errors; all words in the DESCRIPTION file are 
correctly spelled.

The Ubuntu checks also returned a NOTE about not checking
for cyclic dependencies.  This NOTE was note returned on
any of the other checks.

## Downstream dependencies
There are no downstream dependencies for this package
at this time.

Many thanks, and have a great day.