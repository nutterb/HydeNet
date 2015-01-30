#' JAGS Probability Distributions.
#'
#' A dataset listing the JAGS probability distributions and their parameters
#'
#' @format A data frame with 30 rows and 6 variables:
#' \describe{
#'   \item{DistName}{Distribution Name}
#'   \item{FnName}{Function Name}
#'   \item{xLow}{Minimum value for x, the random variable}
#'   \item{xHigh}{Maximum value for x, the random variable}
#'   \item{Parameters}{Names of the parameters}
#'   \item{paramLimit}{Limits on the parameter}
#' }
#' @source \url{http://people.math.aau.dk/~kkb/Undervisning/Bayes14/sorenh/docs/jags_user_manual.pdf}
"jagsDists"