#' @name compileDecisionModel
#' @export
#' 
#' @title Compile JAGS Models to Evaluate the Effect of Decisions in a Network
#' @description Nodes at which a decision can be made, such as the decision to 
#'   test or not test; treat or not treat; or use open or robotic surgery may 
#'   impact the outcome for a subject.  These types of decisions may not be 
#'   truly random and understanding how these decisions may impact downstream
#'   outcomes may be beneficial to making the decision.  Compiling the decision
#'   network permits the network to be evaluated under the conditions of each 
#'   set of decisions separately.
#'   
#' @param network A HydeNet object with decision nodes defined.
#' @param policyMatrix A data frame of policies to apply to decision nodes
#'   for comparing networks under different conditions.  See 
#'   \code{\link{policyMatrix}}.
#' @param ... Additional arguments to pass to \code{jags.model}, excepting
#'   the \code{data} argument.  The \code{data} argument is created by 
#'   \code{compileDecisionModel}, and cannot be passed manually.
#' @param data An optional list of data values to be observed in the nodes.  
#'   It is passed to the \code{data} argument of \code{rjags::jags}. Any
#'   values given in \code{data} will override values provided in 
#'   \code{policyMatrix} with a warning.
#'   
#' @details \code{compileDecisionModel} only accepts nodes of type \code{"dbern"}
#'   (Bernoulli random variable taking either 0 or 1) or \code{"dcat"} 
#'   (categorical variables) as decision nodes.  
#'   When the node is type \code{"dcat"}, the 
#'   decision options are extracted from the JAGS statement returned by 
#'   \code{writeJagsModel}.
#'   
#'   The options for each decision node (if there are multiple nodes) are 
#'   combined via \code{expand.grid} to make a table of all possible decisions.
#'   Each row of this table is passed as a list to the \code{data} argument 
#'   of \code{jags.model} (via \code{compileJagsModel}) and a list of JAGS
#'   model objects is returned.  \code{coda.samples} may be run on each of these
#'   models.
#'   
#' @return Returns a list of \code{compiledHydeNetwork} objects.
#' 
#' @author Jarrod Dalton and Benjamin Nutter
#' 
#' @seealso \code{\link{policyMatrix}} \code{\link{compileJagsModel}}
#' 
#' @examples
#' data(PE, package="HydeNet")
#' Net <- HydeNetwork(~ wells + 
#'                      pe | wells + 
#'                      d.dimer | pregnant*pe + 
#'                      angio | pe + 
#'                      treat | d.dimer*angio + 
#'                      death | pe*treat,
#'                      data = PE) 
#'                      
#' 
#'                  
#' Net <- setDecisionNodes(Net, treat)
#' plot(Net)
#' 
#' decision1 <- compileDecisionModel(Net)
#'
#' #* An effectively equivalent call as the previous
#' decision2 <- compileDecisionModel(Net, policyMatrix(Net))
#' 
#' #* Using a customized policy matrix
#' #* Note: this is a bit of nonsense--you can't decide if a test is negative
#' #*       or positive, but we'll do this for illustration.
#' custom_policy <- policyMatrix(Net, 
#'                               treat="No", 
#'                               angio = c("Negative", "Positive"))
#' decision3 <- compileDecisionModel(Net, custom_policy) 
#' 
compileDecisionModel <- function(network, 
                                 policyMatrix = NULL, 
                                 ..., 
                                 data = NULL)
{
  coll <- checkmate::makeAssertCollection()

  dots <- list(...)
  
  options <- 
    makePolicyMatrix(network = network, 
                     policyMatrix = policyMatrix, 
                     data = data, 
                     argcheck = coll
    )

  checkmate::reportAssertions(coll)

  cpt_arrays <- makeCptArrays(network = network)
  
  jags.code <- compileJagsModel(network = network, ...)

  lapply(options,
         runJagsDecisionModel,
         j = jags.code,
         cpt_arrays = cpt_arrays, 
         ...)
  
  
}


#*********** UTILITY FUNCTIONS

makePolicyMatrix <- function(network, policyMatrix, data, argcheck)
{
  if (is.null(policyMatrix))
  {
    decisionNodes <- 
      names(network[["nodeDecision"]])[vapply(X = network[["nodeDecision"]], 
                                              FUN = any, 
                                              FUN.VALUE = logical(1))]
    
    validDecision <- 
      network[["nodeType"]][decisionNodes] %>%
      unlist()

    inSubset <- 
      checkmate::testSubset(validDecision,
                            choices = c("dbern", "dcat", "dbin"),
                            empty.ok = FALSE
      )
    
    if (!inSubset) 
    {
      argcheck$push(
        paste0(
          "Decision nodes must be of type 'dbern', 'dcat', or 'dbin'.\n   ",
          paste0(names(validDecision)[!validDecision], collapse=", "),
          " cannot be used as decision nodes."
        )
      )
    }
    
    checkmate::reportAssertions(argcheck)

    options <- lapply(X = decisionNodes, 
                      FUN = decisionOptions, 
                      network = network) %>%
      stats::setNames(decisionNodes)

    options <- expand.grid(options, stringsAsFactors=FALSE) 
  }
  else
  {
    checkmate::assertDataFrame(policyMatrix,
                               add = argcheck)

    checkmate::reportAssertions(argcheck)
    
    options <- policyMatrix
  }
  
  #* This is the part that pushes values from `data` into the 
  #* policy matrix.
  if (!is.null(data))
  {
    conflicts <- names(data)[names(data) %in% names(options)]
    
    if (length(conflicts) > 0)
    {
      warning("The following variables in 'data' are overriding ",
              "values in 'policyMatrix': ",
              paste0(conflicts, collapse = ", ")
      )
    }
    
    for (i in names(data))
    {
      options[[i]] <- data[[i]]
    }
    #* Remove duplicated rows
    options <- options[!duplicated(options), , drop = FALSE]
  }
  
  options <- 
    lapply(X = 1:nrow(options), 
           FUN = function(i)
                 { 
                    l <- as.list(options[i, , drop=FALSE])
                    nms <- names(l)
                    attributes(l) <- NULL
                    names(l) <- nms
                    l
                  }
           )
  
  options
}

#**



makeCptArrays <- function(network)
{
  cpt_arrays <- unlist(network$nodeFitter) == "cpt"
  if(any(cpt_arrays))
  {
    cpt_arrays <- names(cpt_arrays)[cpt_arrays]
    cpt_arrays <- network[["nodeModel"]][cpt_arrays]
    nms <- names(cpt_arrays)
    cpt_arrays <- 
      lapply(X = names(cpt_arrays),
             FUN = 
               function(ca)
               {
                  if ("cpt" %in% class(cpt_arrays[[ca]]))
                  {
                    return(cpt_arrays[[ca]])
                  }
                  else
                  {
                      args <-
                        list(formula = network[["nodeFormula"]][[ca]],
                             data = if (!is.null(network$nodeData[[ca]]))
                                    {
                                      network$nodeData[[ca]]
                                    }
                                    else 
                                    {
                                      network$data
                                    }
                             )
                      if (!is.null(network[["nodeFitterArgs"]][[ca]]))
                      {
                        args <- c(args, network[["nodeFitterArgs"]][[ca]])
                      }
                      return(do.call("cpt", args))
                  }   
             }
          )
    names(cpt_arrays) <- paste0("cpt.", nms)
  } 
  else 
  {
    cpt_arrays = list()
  }
  return(cpt_arrays) 
}

#****
#* o: element of the options list
#* j: jags code 
#* cpt_arrays: the conditional probability table arrays

runJagsDecisionModel <- function(o, j, cpt_arrays, ...)
{
  con <- textConnection(paste0(j[["jags"]]$model(),
                               collapse="\n")
                        )
  o_character <- vapply(X = o, 
                        FUN = is.character, 
                        FUN.VALUE = logical(1))
  for (i in seq_along(o))
  {
    if (o_character[i])
    {
      o[[i]] <- j[["factorRef"]][[names(o[i])]][["value"]][j[["factorRef"]][[names(o[i])]][["label"]] == o[[i]]]
    }
  }
  
  cHN <- list(jags = rjags::jags.model(con,
                                       data = c(o, cpt_arrays),
                                       ...),
              observed = o,
              dag = j$dag,
              factorRef = j$factorRef)
  class(cHN) <- c("compiledHydeNetwork")
  close(con)
  cHN
}  
