#' Blackjack Dealer Outcome Probabilities
#'
#' A dataset containing the conditional probability of various dealer
#' outcomes given the "upcard". (The dealer and player each get two
#' cards; only one of the dealer's cards is shown, and this is called
#' the "upcard")
#'
#' @format A data frame with 70 rows and 3 variables:
#' \describe{
#'   \item{dealerUpcard}{dealer upcard}
#'   \item{dealerOutcome}{outcome of dealer's hand, under the rule that
#'   cards are drawn until the dealer's hand total is at least 17}
#'   \item{probability}{conditional probability of dealerOutcome given
#'   dealerUpcard}
#'   ...
#' }
#' @source \url{https://www.blackjackinfo.com/dealer-outcome-probabilities}
"BJDealer"