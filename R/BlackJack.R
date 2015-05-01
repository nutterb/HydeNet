#' Black Jack Hybrid Decision Network
#'
#' An object of class \code{HydeNetwork} establishing a graphical model for a game
#' of Black Jack.
#'
#' @format A \code{HydeNetwork} object constructed using the code shown in 
#' the example.  The network has seven random nodes, three ten deterministic
#' nodes, three decision nodes, and one utility node.
#' 
#' @examples
#' \dontrun{
#' BlackJack <- 
#'   HydeNetwork(~ card1.ace | card1 + 
#'                 card2.ace | card2 + 
#'                 initialPoints | card1 * card2 * card1.ace * card2.ace + 
#'                 hit1 | initialPoints * dealerUpcard + 
#'                 card3 | hit1 + 
#'                 card3.ace | card3 + 
#'                 pointsAfterCard3 | initialPoints * card3 * card3.ace + 
#'                 hit2 | pointsAfterCard3 * dealerUpcard + 
#'                 card4 | hit2 + 
#'                 card4.ace | card4 + 
#'                 pointsAfterCard4 | pointsAfterCard3 * card4 * card4.ace + 
#'                 hit3 | pointsAfterCard4*dealerUpcard + 
#'                 card5 | hit3 + 
#'                 card5.ace | card5 + 
#'                 pointsAfterCard5 | pointsAfterCard4 * card5 * card5.ace + 
#'                 playerFinalPoints | initialPoints * hit1 * 
#'                 pointsAfterCard3 * hit2 * 
#'                 pointsAfterCard4 * hit3 * 
#'                 pointsAfterCard5 + 
#'                 dealerFinalPoints | dealerUpcard + 
#'                 payoff | playerFinalPoints * dealerFinalPoints)
#'                 
#' BlackJack <- setNode(BlackJack, node = card1.ace, nodeType = "determ", 
#'                      define = fromFormula(), 
#'                      nodeFormula = card1.ace ~ ifelse(card1==1,1,0))
#' BlackJack <- setNode(BlackJack, node = card2.ace, nodeType = "determ", 
#'                      define = fromFormula(), 
#'                      nodeFormula = card2.ace ~ ifelse(card2==1,1,0))
#' BlackJack <- setNode(BlackJack, node = card3.ace, nodeType = "determ", 
#'                      define = fromFormula(), 
#'                      nodeFormula = card3.ace ~ ifelse(card3==1,1,0))
#' BlackJack <- setNode(BlackJack, node = card4.ace, nodeType = "determ", 
#'                      define = fromFormula(), 
#'                      nodeFormula = card4.ace ~ ifelse(card4==1,1,0))
#' BlackJack <- setNode(BlackJack, node = card5.ace, nodeType = "determ", 
#'                      define = fromFormula(), 
#'                      nodeFormula = card5.ace ~ ifelse(card5==1,1,0))
#' BlackJack <- setNode(BlackJack, node = initialPoints, nodeType = "determ", 
#'                      define = fromFormula(), 
#'                      nodeFormula = initialPoints~card1+card2)
#' BlackJack <- setNode(BlackJack, node = pointsAfterCard3, nodeType = "determ", 
#'                      define = fromFormula(),
#'                      nodeFormula = pointsAfterCard3 ~ initialPoints+card3)
#' BlackJack <- setNode(BlackJack, node = pointsAfterCard4, nodeType = "determ", 
#'                      define = fromFormula(),
#'                      nodeFormula = pointsAfterCard4 ~ pointsAfterCard3+card4)
#' BlackJack <- setNode(BlackJack, node = pointsAfterCard5, nodeType = "determ", 
#'                      define = fromFormula(),
#'                      nodeFormula = pointsAfterCard5 ~ pointsAfterCard4+card5)
#' BlackJack <- setNode(BlackJack, node = playerFinalPoints, nodeType = "determ", 
#'                      define = fromFormula(),
#'                      nodeFormula = playerFinalPoints ~ playerFinalPoints+3)
#'
#' BlackJack <- setDecisionNodes(BlackJack, hit1, hit2, hit3)
#' BlackJack <- setUtilityNodes(BlackJack, payoff)
#' }
#' 
"BlackJack"