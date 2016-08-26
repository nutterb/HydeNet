#' @name writeJagsFormula
#' @export
#' @importFrom stats as.formula
#' @importFrom stats coef
#' @importFrom stats update
#' @importFrom utils tail
#'  
#' @title Write the JAGS Formula for a Hyde Node
#' @description Based on the information provided about the node,
#'   an appropriate JAGS model is written in text.  This is combined with 
#'   the other node models to generate the complete network.
#'   
#' @param fit a model object
#' @param nodes a vector of node names, usually passed from \code{network$nodes}
#' @param ... Additional arguments to be passed to other methods
#' 
#' @details Methods for different model objects can be written so that 
#'   this function can be extended as desired.
#'   
#'   The resulting formulas are based on the coefficient matrix of the fitted
#'   model, and the returned result is the JAGS code representing the 
#'   regression equation of the model.
#'   
#'   In the \code{writeJagsFormula.glm} method, appropriate transformations
#'   exist for the following combinations:
#'   \enumerate{
#'     \item family = binomial; link = logit
#'     \item family = poisson; link = log
#'     \item family = gaussian; link = identity (calls \code{writeJagsFormula.lm})
#'   }
#'   
#' @author Jarrod Dalton and Benjamin Nutter
#' 
#' @seealso \code{\link{writeJagsModel}}, \code{\link{writeNetworkModel}}
#' 
#' @examples
#' data(PE, package="HydeNet")
#' fit <- lm(d.dimer ~ pregnant + pe, data=PE)
#' writeJagsFormula(fit, nodes=c("d.dimer", "pregnant", "pe"))
#' 
#' fit.glm <- glm(death ~ pe + treat, data=PE, family="binomial")
#' writeJagsFormula(fit.glm, nodes=c("death", "pe", "treat"))

writeJagsFormula <- function(fit, nodes, ...) 
{
  UseMethod("writeJagsFormula")
}

#' @rdname writeJagsFormula
#' @export
#' 

writeJagsFormula.cpt <- function(fit, nodes, ...)
{
  form <- paste0(utils::tail(names(dimnames(fit)), 1),
                 " ~ ",
                 paste0(names(dimnames(fit))[-length(names(dimnames(fit)))],
                        collapse = " + "))
  rToJags(stats::as.formula(form)) 
}

#' @rdname writeJagsFormula
#' @export
#' @importFrom stringr str_trim
#' 

writeJagsFormula.glm <- function(fit, nodes, bern = bern, ...)
{
  if (fit[["family"]][["family"]] == "gaussian" & fit[["family"]][["link"]] == "identity")
  {
    return(writeJagsFormula.lm(fit))
  }
  
  mdl <- suppressWarnings(
    pixiedust::dust(fit, 
                    descriptors = c("term", "term_plain", "level")) %>%
      as.data.frame(sprinkled = FALSE)
  )
  
  regex <- factorRegex(fit)
  
  mdl <- makeJagsReady(mdl, 
                       factorRef = factor_reference(model.frame(fit)),
                       bern = bern) %>%
    mutate(term_plain = gsub(pattern = ":", 
                             replacement = "*", 
                             x = term_plain))
  
  #* rhs = right hand side
  rhs <- paste(round(mdl[["estimate"]], 
                     getOption("Hyde_maxDigits")), 
               ifelse(test = is.na(mdl[["term_plain"]]), 
                      yes = "", 
                      no = "*"),
               ifelse(test = is.na(mdl[["term_plain"]]), 
                      yes = "", 
                      no = mdl[["term_plain"]]), 
               collapse=" + ")
  
  #* Binomial Proportion
  if (fit[["family"]][["family"]] == "binomial" & 
      fit[["family"]][["link"]] == "logit")
  {
    rhs <- paste0("ilogit(", rhs, ")")
  }
  
  #* Poisson Regression
  if (fit[["family"]][["family"]] == "poisson" & fit[["family"]][["link"]] == "log")
  {
    rhs <- paste("exp(", rhs, ")")  
  }
  
  out_fm <- paste0(as.character(fit[["call"]][["formula"]])[2], " ~ ", rhs)
  rToJags(stats::as.formula(out_fm)) 
}

#' @rdname writeJagsFormula
#' @export
#' @importFrom stringr str_trim
#' 

writeJagsFormula.lm <- function(fit, nodes, bern, ...)
{
  mdl <- pixiedust::dust(fit, 
                         descriptors = c("term", "term_plain", "level")) %>%
    as.data.frame(sprinkled = FALSE)
  
  regex <- factorRegex(fit)
  
  mdl <- makeJagsReady(mdl, 
                       factorRef = factor_reference(model.frame(fit)),
                       bern = bern) %>%
    mutate(term_plain = gsub(pattern = ":", 
                             replacement = "*", 
                             x = term_plain))
  
  #* rhs = right hand side
  rhs <- paste(round(mdl$estimate, 
                     getOption("Hyde_maxDigits")), 
               ifelse(test = is.na(mdl$term_plain), 
                      yes = "", 
                      no = "*"),
               ifelse(test = is.na(mdl$term_plain), 
                      yes = "", 
                      no = mdl$jagsVar), 
               collapse=" + ")
  
  out_fm <- paste0(as.character(fit[["call"]][["formula"]])[2], " ~ ", rhs)
  rToJags(stats::as.formula(out_fm)) 
}

#' @rdname writeJagsFormula
#' @export
#' @import nnet
#'

writeJagsFormula.multinom <- function(fit, nodes, bern = bern, ...)
{
  mdl <- pixiedust::dust(fit, 
                         exponentiate = FALSE, 
                         descriptors = c("term", "term_plain", "level")) %>%
    as.data.frame(sprinkled = FALSE)
  
  mdl <- makeJagsReady(mdl, 
                       factorRef = factor_reference(model.frame(fit)),
                       bern = bern)
  mdl <- dplyr::arrange(mdl, y.level, term_plain)

  mdl <- split(mdl, mdl$y.level)
  
  fm <- lapply(mdl,
         function(x)
         {
           sprintf("%s %s %s",
                   round(x[["estimate"]], getOption("Hyde_maxDigits")),
                   ifelse(is.na(x[["term_plain"]]), 
                          "",
                          "*"),
                   ifelse(is.na(x[["term_plain"]]),
                          "",
                          x[["jagsVar"]]))
         }
  ) %>%
    lapply(
      paste0,
      collapse = " + "
    ) %>%
    vapply(
      function(x) sprintf("exp(%s) / (1 + exp(%s))", x, x),
      character(1)
    ) 
  
  
   fm <- c(sprintf("(%s)", fm) %>%
             paste0(collapse = " - ") %>%
             sprintf("1 - %s", .),
           fm)
   
   fm <- sprintf("pi.%s[%s] <- %s",
                 names(attributes(fit$terms)$dataClasses)[1],
                 seq_along(fm),
                 fm) %>%
     paste0(collapse = " ")
  
  fm
}

#' @rdname writeJagsFormula
#' @export

writeJagsFormula.survreg <- function(fit, bern = bern, ...)
{
  mdl <- pixiedust::dust(fit, descriptors = c("term", "term_plain", "level")) %>%
    as.data.frame(sprinkled = FALSE)
  
  regex <- factorRegex(fit)
  
  mdl <- makeJagsReady(mdl, 
                       factorRef = factor_reference(model.frame(fit)),
                       bern = bern) %>%
    dplyr::mutate(term_plain = gsub(pattern = ":", 
                                    replacement = "*", 
                                    x = term_plain))
  
  #* rhs = right hand side
  rhs <- paste(round(mdl$estimate, getOption("Hyde_maxDigits")), 
               ifelse(test = is.na(mdl$term_plain), "", "*"),
               ifelse(is.na(mdl$term_plain), "", mdl$jagsVar), 
               collapse=" + ") %>%
    trimws()
  
  inverse_fn <- survival::survreg.distributions[[fit[["dist"]]]][["itrans"]] 
  
  if (!is.null(inverse_fn)) 
  {
    inverse_fn <- 
      inverse_fn %>%
      deparse() %>%
      `[`(2) %>%
      sub(pattern = "[(].+$", 
          replacement = "", 
          x = .)
    
    rhs <- paste0(inverse_fn, "(", rhs, ")")
  }
  
  lhs <- as.character(fit[["call"]][["formula"]])[2] %>%
    sub(pattern = "Surv[(]", 
        replacement = "", 
        x = .) %>%
    sub(pattern = ",.+$", 
        replacement = "", 
        x = .) %>%
    trimws()
  
  out_fm <- paste0(lhs, " ~ ", rhs) %>%
    trimws()
  
  rToJags(stats::as.formula(out_fm)) 
}

#' @rdname writeJagsFormula
#' @export

writeJagsFormula.xtabs <- function(fit, ...)
{
  fm <- attributes(fit)[["call"]][["formula"]]
  out_fm <- paste(fm[2], fm[1])
  
  pi <- fit/sum(fit)
  names(pi) <- 1:length(pi)
  pi <- paste0("pi.", 
               fm[2], 
               "[", 
               names(pi), 
               "] <- ", 
               round(pi, getOption("Hyde_maxDigits")), 
               collapse="; ")
  
  return(pi)
}

utils::globalVariables(c("term_plain", "."))
