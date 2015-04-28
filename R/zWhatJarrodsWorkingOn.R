#require(dplyr); require(Rcpp)
#require(reshape2); require(plyr);  require(HydeNet);


normalizeCPTData <- function(y, x, data, massVar){
  
  stopifnot(is.character(y) && length(y)==1)
  stopifnot(is.character(x))
  stopifnot(is.data.frame(data))
  stopifnot(y %in% names(data))
  stopifnot(all(x %in% names(data)))
  
  if(missing(massVar)){
    data$mass <- 1
    massVar <- "mass"    
  } else{
    stopifnot(is.character(massVar))
    stopifnot(massVar %in% names(data))     
    stopifnot(is.numeric(data[,massVar]))
    stopifnot(min(data[,massVar]) >= 0)
  }
  
  summedMassArray <- plyr::daply(data, c(y,x), function(x) sum(x[,massVar]), .drop_i=FALSE)
  cpt <- plyr::aaply(summedMassArray, seq_along(c(y,x))[-1], function(x) x/sum(x))
  
  names(dimnames(cpt))[length(c(y,x))] <- y
  return(cpt)
}

readCPTFromConsole <- function(y, x, params, reduce=FALSE){
    
  stopifnot(is.character(y) && length(y)==1)
  stopifnot(is.character(x))
  
  hbar <- paste(paste(rep("-",80),collapse=""),"\n",sep="")
  factorEntryCommand <- function(variableName){
    cat(hbar, "Enter Factor Levels for node '", variableName,"':\n\n",
        "If this is a binary variable, enter '<yn>' as a shortcut.\n",
        "When finished, enter '<z>'.\n",
        "To repeat entry of the last inputted factor level, enter <b>.\n",
        "To start over entirely, enter '<s>'\n", hbar, sep="")
  }
  
  variables <- c(y,x)
    
  if(missing(params)){  # solicit the names of factor levels from the console
    factorLevels <- vector(mode="list")
    for(i in seq_along(variables)){
      escapeFlag <- 0
      levelIndex <- 1
      tmp <- vector("character")
      factorEntryCommand(variables[i])
      while(!escapeFlag){
        cat("Level ",levelIndex," of '",variables[i],"'?", sep="")
        IO <- readline()
        if(IO == "<yn>"){
          if(levelIndex == 1) {
            tmp <- c("No","Yes")
            escapeFlag <- 1
          } else cat("(NOTE: <yn> only works when entering the FIRST factor level.)\n")
        } else if(IO == "<s>") {
          levelIndex <- 1
          tmp <- vector("character")
          factorEntryCommand(variables[i])
        } else if(IO == "<b>"){
          levelIndex <- max(c(1,levelIndex-1))
          if(levelIndex == 1) tmp <- vector("character") else tmp <- tmp[1:(levelIndex-1)]
        } else if(IO == "<z>"){
          escapeFlag <- 1
        } else if(IO == "<q>"){
          stop("User requested termination.")
        }
        else {
          tmp <- c(tmp, IO)
          levelIndex <- levelIndex + 1
        }
      }
      factorLevels[[variables[i]]] <- tmp
    }
  } else{
    factorLevels <- params$factorLevels
    if(!all(variables %in% names(factorLevels))){
      stop(paste("Variables",paste(variables,collapse=", "),
                 "not all in parameter 'factorLevels'."))
    }
    factorLevels <- factorLevels[variables]
    if(!all(unlist(lapply(factorLevels, is.character)))) {
      stop("Incompatible 'factorLevels' argument. See help('readCPTFromConsole').")
    }
  }
  
  # input the conditional probabilities
  data <- expand.grid(factorLevels)
  
  if(reduce){
    cat(hbar,
        "NOTE: parameter 'reduce' is set to TRUE in readCPTFromConsole().\n",
        "      Conditional probabilities Pr(",y,"=",factorLevels[[y]][1]," | ",
        paste(x,collapse=", "), ") will be calculated\n",
        "      as the complement of the inputted probabilities Pr(",
        y," != ",factorLevels[[y]][1]," | ",paste(x,collapse=", "),").\n",
        hbar,sep="")
    data <- data[data[,y] %in% levels(data[,y])[-1],]
    cat("Enter the following conditional probabilities:\n")
  } else {
    cat(hbar, "Enter the following conditional probabilities, or positive\n",
       "numbers proportional to them (e.g., counts):\n")
  }
  cat("(Use '<q>' to halt execution)\n",hbar,sep="")

  noNegativeProbs <- FALSE
  while(!noNegativeProbs){
    for(i in seq_len(nrow(data))){
      cat("Pr(",y,"=",as.character(data[i,y])," | ",
          paste(apply(cbind(names(data[i,x]),
                            unlist(lapply(data[i,x], as.character))),
                      1, paste, collapse="="),
                collapse=", "),
          "):", sep="")
      valid.IO <- FALSE;
      while(!valid.IO){
        IO <- readline()
        if(IO == "<q>") stop("User requested termination.")
        IO.n <- as.numeric(IO)
        if(is.na(IO.n)) cat("Invalid numeric data entry. Try again:\n") else {
          if(reduce & (IO.n<0 | IO.n>1)){
            cat("Invalid probability given. Enter a number in [0,1]:\n")  
          } else if(IO.n<0){
            cat("Invalid count/probability given. Enter a non-negative number:\n")
          } else{
            valid.IO <- TRUE
            data[i,"mass"] <- IO.n
          }
        }
      }
    }
    
    if(reduce){
      # Add complement rows to the conditional probability data frame
      # if reduce=TRUE was used; check for errors involving sum of entered
      # conditional probabilities greater than 1
      complementProbs <- ddply(data, x, function(data) c("mass"=1-sum(data$mass)))
      complementProbs[,y] <- levels(data[,y])[1]
      data <- rbind(data,complementProbs)
      if(min(data$mass)>=0) noNegativeProbs <- TRUE else{
        cat(hbar,"Invalid set of conditional probabilities given. There exists\n",
            "some combination of conditioning variables such that\n",
            "the sum of Pr(",y," != ",factorLevels[[y]][1]," | ",
            paste(x,collapse=", "), ") is greater than 1.\n",
            "Please re-enter the conditional probabilities.\n",
            hbar, sep="")
      }
    } else noNegativeProbs <- TRUE
  } #end while(!noNegativeProbs) loop
  
  return(normalizeCPTData(y,x,data,"mass"))
} #end function readCPTFromConsole()



getCPT <- function(y, x, data=NULL, countVar=NULL, ...){

  params <- list(...);
  
  stopifnot(is.character(y) && length(y)==1)
  
  if(!is.null(data)){
    stopifnot(is.data.frame(data))
    stopifnot(y %in% names(data))
    stopifnot(all(x %in% names(data)))
    if(!is.null(countVar)) stopifnot(countVar %in% names(data)) else {
      countVar <- "..cptCount..";
      data[,"..cptCount.."] <- 1
    }
  } else data <- readCPTFromConsole(y, x, factorLevels=params$factorLevels) 

}







n <- 50000
d <- data.frame(
  di1=as.factor(1:6 %*% rmultinom(n,1,prob=c(.4,.3,.15,.10,.03,.02))),
  di2=as.factor(1:6 %*% rmultinom(n,1,prob=rev(c(.4,.3,.15,.10,.03,.02)))),
  di3=as.factor(1:6 %*% rmultinom(n,1,prob=c(.15,.10,.02,.3,.4,.03)))
)

g <- HydeNetwork(~di3|di1*di2, data=d)
system.time(writeNetworkModel(g, pretty=TRUE))

childNode <- "di3"
parentNodes <- c("di1","di2")

di3.cpt <- normalizeCPTData("di3",c("di1","di2"),d)
plyr::aaply(di3.cpt, 1:2, sum)




