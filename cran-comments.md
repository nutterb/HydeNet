This update fixes errors introduced with `DiagrammeR` 0.9.0

This is a resubmission to address issues found regarding links in the vignettes.
The links have been updated. The link to another package on CRAN now uses
https.  The other problematic link does not have an https equivalent.  I did,
however, write some tests to check that links to this page (rules for a card
game) are valid so that I may catch this problem before submission in the future.

## Test environments
* win-builder (release)
* local windows install R 3.3.2
* local windows install R-devel 2017-01-05 r71919

## R CMD check results

A NOTE was returned by win-builder (release and devel) regarding 
potential spelling errors; all words in the DESCRIPTION file are 
correctly spelled.

## Downstream dependencies
There are no downstream dependencies for this package
at this time.

Many thanks, and have a great day.