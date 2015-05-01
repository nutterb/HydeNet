#This code makes some fake data on blackjack hands.

require(plyr)

n <- 1000

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

hit <- function(points,n=1) rbinom(n, 1, plogis(10-.8*points))


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

#card 4
d$card4 <- factor(apply(rmultinom(n,1,prob=cardProbs),2,function(x) which(x==1)), 1:10, cardNames)
ix2 <- which(d$hit1==0 | d$hit2==0)
d$card4[ix2] <- NA
tmpd <- ddply(d[,c("handNum","card1","card2","card3","card4")], .(handNum,card1,card2,card3,card4),
              function(x) handScore(list(x$card1,x$card2,x$card3,x$card4)))[,c("handNum","V1")]
names(tmpd)[2] <- "pointsAfterCard4"
d <- merge(d, tmpd, all=FALSE);  dim(d)

d$hit3 <- hit(d$pointsAfterCard4,n)
d$pointsAfterCard4[ix2] <- NA
d$hit3[ix2] <- NA

#card 5
d$card5 <- factor(apply(rmultinom(n,1,prob=cardProbs),2,function(x) which(x==1)), 1:10, cardNames)
ix3 <- which(d$hit1==0 | d$hit2==0 | d$hit3==0)
d$card5[ix3] <- NA

tmpd <- ddply(d[,c("handNum","card1","card2","card3","card4","card5")],
              .(handNum,card1,card2,card3,card4,card5),
              function(x) handScore(list(x$card1,x$card2,x$card3,x$card4,x$card5)))[,c("handNum","V1")]
names(tmpd)[2] <- "pointsAfterCard5"
d <- merge(d, tmpd, all=FALSE);  dim(d)

d$pointsAfterCard5[ix3] <- NA


d$handNum <- NULL

bjdata <- d
rm(list=ls()[!ls() == "bjdata"])

#save("bjdata", file = "some_path/bjdata.RData")