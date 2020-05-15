This update removes dependencies on `gRbase` and `graph`.  
It also ensures all opened textConnection objects are explicitly closed.

## Test environments
* win-builder (devel 2020-05-11 r78411)
* win-builder (R 4.0.0)
* Local Linux install (3.6.1; Ubuntu SMP Wed Apr 1 03:25:46 UTC 2020)
* Remote Linux install (4.0.0; Ubuntu 16.04.6 LTS, Travis CI)

## R CMD check results

There were no findings from the CHECK results

## Downstream dependencies

The reverse dependency check for `wiseR` produces two notes. Both of these notes are also listed in the CRAN check results.  I have filed issues for these findings in the `wiseR` GitHub repository.


Many thanks, and have a great day.