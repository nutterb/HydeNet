################################################
# Make some fake data on blackjack hands.
################################################

require(plyr)

n <- 10000

cardNames <- c(2:9,"10-K","A")
cardProbs <- c(rep(1/13,8),4/13,1/13)
cardPoints <- 2:11
names(cardProbs) <- cardNames
names(cardPoints) <- cardNames

handScore <- function(cards){
  cards <- cards[!is.na(cards)]
  
  cardPts <- unlist(lapply(cards, function(x) as.numeric(x)+1))
  numAces <- sum(cardPts==11)
  
  outPts <- sum(cardPts)
  
  while(numAces>0){
    if(outPts>21) outPts <- outPts - 10
    numAces <- numAces - 1
  }
  return(outPts)
}

hit <- function(points,n=1){
  x <- rbinom(n, 1, plogis(10-.8*points))
  x[points>21] <- NA
  return(x)
}


set.seed(42379820)
d <- data.frame(
  handNum <- 1:n,
  card1 = factor(apply(rmultinom(n,1,prob=cardProbs),2,function(x) which(x==1)), 1:10, cardNames),
  card2 = factor(apply(rmultinom(n,1,prob=cardProbs),2,function(x) which(x==1)), 1:10, cardNames)
)
d <- ddply(d, .(handNum,card1,card2), function(x) handScore(list(x$card1,x$card2)))
names(d)[ncol(d)] <- "initialPoints"

d$hit1 <- hit(d$initialPoints,n)

#card 3
d$card3 <- factor(apply(rmultinom(n,1,prob=cardProbs),2,function(x) which(x==1)), 1:10, cardNames)
d$card3[d$hit1==0] <- NA


tmpd <- ddply(d[,c("handNum","card1","card2","card3")], .(handNum,card1,card2,card3),
              function(x) handScore(list(x$card1,x$card2,x$card3)))[,c("handNum","V1")]
names(tmpd)[2] <- "pointsAfterCard3"

d <- merge(d, tmpd, all=FALSE);  dim(d)

d$hit2 <- hit(d$pointsAfterCard3,n)


ix1 <- which(d$hit1==0)
d$pointsAfterCard3[ix1] <- NA
d$hit2[ix1] <- NA
d$hit2[which(d$pointsAfterCard3>21)] <- NA

#card 4
d$card4 <- factor(apply(rmultinom(n,1,prob=cardProbs),2,function(x) which(x==1)), 1:10, cardNames)
ix2 <- which(d$hit1==0 | d$hit2==0)
d$card4[ix2] <- NA
d$card4[which(is.na(d$hit2))] <- NA

tmpd <- ddply(d[,c("handNum","card1","card2","card3","card4")], .(handNum,card1,card2,card3,card4),
              function(x) handScore(list(x$card1,x$card2,x$card3,x$card4)))[,c("handNum","V1")]
names(tmpd)[2] <- "pointsAfterCard4"
d <- merge(d, tmpd, all=FALSE);  dim(d)

d$hit3 <- hit(d$pointsAfterCard4,n)
d$pointsAfterCard4[ix2] <- NA
d$pointsAfterCard4[which(is.na(d$hit2))] <- NA
d$hit3[ix2] <- NA
d$hit3[which(d$pointsAfterCard4>21)] <- NA


#card 5
d$card5 <- factor(apply(rmultinom(n,1,prob=cardProbs),2,function(x) which(x==1)), 1:10, cardNames)
ix3 <- which(d$hit1==0 | d$hit2==0 | d$hit3==0)
d$card5[ix3] <- NA
d$card5[which(is.na(d$hit3))] <- NA


tmpd <- ddply(d[,c("handNum","card1","card2","card3","card4","card5")],
              .(handNum,card1,card2,card3,card4,card5),
              function(x) handScore(list(x$card1,x$card2,x$card3,x$card4,x$card5)))[,c("handNum","V1")]
names(tmpd)[2] <- "pointsAfterCard5"
d <- merge(d, tmpd, all=FALSE);  dim(d)

d$pointsAfterCard5[ix3] <- NA
d$pointsAfterCard5[which(is.na(d$hit2))] <- NA
d$pointsAfterCard5[which(is.na(d$hit3))] <- NA

d$handNum <- NULL

bjdata <- d
save("bjdata", file="bjdata.RData")




#########################################################
# Generate JAGS code for conditional probability of 
# dealerOutcome given dealerUpcard
#########################################################

require(HydeNetwork)

n <- 5000;

cards <- c(2:9, "10-K", "A")
outcomes <- c("21","20","19","18","17","Bust","Blackjack")

L <- list(
  list(card="2",
       p=rev(c(0.000000001,0.3567,0.1301,0.1365,0.1313,0.1257,0.1196))
  ),
  list(card="3",
       p=rev(c(0.000000001,0.3767,0.1263,0.1320,0.1271,0.1218,0.1162))
  ),
  list(card="4",
       p=rev(c(0.000000001,0.3971,0.1224,0.1273,0.1228,0.1179,0.1126))
  ),
  list(card="5",
       p=rev(c(0.000000001,0.4177,0.1184,0.1229,0.1184,0.1138,0.1089))
  ),
  list(card="6",
       p=rev(c(0.000000001,0.4395,0.1148,0.1148,0.1148,0.1103,0.1057))
  ),
  list(card="7",
       p=rev(c(0.000000001,0.2623,0.3686,0.1378,0.0786,0.0786,0.0741))
  ),
  list(card="8",
       p=rev(c(0.000000001,0.2447,0.1286,0.3593,0.1286,0.0694,0.0694))
  ),
  list(card="9",
       p=rev(c(0.000000001,0.2284,0.1200,0.1200,0.3508,0.1200,0.0608))
  ),
  list(card="10-K",
       p=rev(c(0.0714,0.2134,0.1121,0.1121,0.1121,0.3442,0.0347))
  ),
  list(card="A",
       p=rev(c(0.2353,0.1535,0.0635,0.1582,0.1582,0.1582,0.0732))
  )
)

regFunc <- function(l){
  l$data <- data.frame(dealerUpcard  = l$card,
                       dealerOutcome = factor(apply(rmultinom(n,1,prob=l$p),
                                                    2,
                                                    function(x) which(x==1)),
                                              1:7,
                                              outcomes
                       )
  )
  return(l)
}

getData <- function(l) return(l$data)

L <- lapply(L, regFunc)

d <- data.frame()
for(i in 1:length(L)) d <- rbind(d, L[[i]]$data)


net <- HydeNetwork(~dealerOutcome | dealerUpcard, data=d)

writeNetworkModel(net, pretty=TRUE)
