n <- 10000
set.seed(2348920)

PE <- data.frame(wells = round(rlnorm(n,log(3.5),log(1.5))))
PE$wells[PE$wells<0] <- 0;
PE$wells[PE$wells>10] <- 10;
hist(PE$wells, breaks=seq(-0.5,10.5,1))

PE$pregnant <- rbinom(n,1,0.1)

ff <- function(x) plogis(-4+0.6*x)
#plot(seq(0,10,0.1)->xx, ff(xx), type="l", col=2, ylim=c(0,1))
PE$pe <- rbinom(n,1,ff(PE$wells))


PE$angio <- rbinom(n,1,0.1+0.65*PE$pe)
# sweep(tbl(PE$pe, PE$angio)->tt,1,apply(tt,1,sum),"/")

# PE$d.dimer <- rbinom(n,1,0.32+0.53*PE$pe + 
#                             0.20*PE$pregnant*(1-PE$pe) +
#                             0.05*PE$pregnant*PE$pe)
# ix <- which(PE$pregnant==1);
# tbl(PE$pe[ix], PE$d.dimer[ix])->tt;
# sweep(tt,1,apply(tt,1,sum),"/")

mn <- rep(210,n);  sd <- rep(30,n)
ix <- which(PE$pe==1 & PE$pregnant==0); mn[ix] <- 280
ix <- which(PE$pe==0 & PE$pregnant==1); mn[ix] <- 240
ix <- which(PE$pe==1 & PE$pregnant==1); mn[ix] <- 300
PE$d.dimer <- round(rnorm(n,mn,sd))

#PE$treat <- rbinom(n,1,0.01+0.59*PE$d.dimer + 0.79*PE$angio -
#                        0.44*PE$d.dimer*PE$angio)
#plyr::ddply(PE,.(d.dimer, angio), summarize, mn=mean(treat))   

ff <- function(d,a) plogis(-6+0.02*d+2.5*a-0.003*d*a)
#plot((100:350)->xx, ff(xx,0), type="l",ylim=c(0,1));
#lines(xx,ff(xx,1),col=2,lty=5)

PE$treat <- rbinom(n,1,ff(PE$d.dimer, PE$angio))

PE$death <- rbinom(n,1,0.001+0.049*(1-PE$pe)*PE$treat +
                          0.949*PE$pe*(1-PE$treat) +
                          0.25*PE$pe*PE$treat)
#plyr::ddply(PE,.(pe,treat), summarize, mn=mean(death))   

PE$pregnant <- yn(PE$pregnant, c(0,1), c("No","Yes"))
PE$pe       <- yn(PE$pe      , c(0,1), c("No","Yes"))
PE$angio    <- yn(PE$angio   , c(0,1), c("Negative","Positive"))
PE$treat    <- yn(PE$treat   , c(0,1), c("No","Yes"))
PE$death    <- yn(PE$death   , c(0,1), c("No","Yes"))

samp(PE,15)

save("PE", file="C:/Users/daltonj/Google Drive/Hyde/data/PE.RData")


