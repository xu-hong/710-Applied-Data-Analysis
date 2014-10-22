means <- numeric(10000)
set.seed(1001)

for (i in 1:10000){
  means[i] <- mean(runif(5, min=0, max=10))
}


h <- hist(means, prob = T)
hist(means, las = 1, main= "", xlab = "Sample means", 
     col = 2, prob = T, ylim = c(0, max(h$density)+ 
                                   max(h$density)*0.15))
curve(dnorm(x, mean = mean(means), sd = sd(means)), 
      add=T, col = "darkblue", lwd = 2)


qqnorm(means, las=1)
qqline(means)

dat <- read.csv("AfrPlots.csv", header = T)
attach(dat)

Deads <- dat$Dead[dat$CensusNo == 2]
sort(Deads)

q <- (1:30 - 0.5)/30
qZ <- qnorm(q, mean = 0, sd = 1)
plot(qZ, sort(Deads), xlab="Theorectical Quantiles", ylab="Sample Quantiles of Deads")


# C.I.
mu <- 0
sd <- 1
n <- 50
runs <- 100
Tvalue <- qnorm(0.975)

ci <- matrix(nrow=runs, ncol=2)

for (i in 1:runs){
  samp <- rnorm(n, mu, sd)
  sx <- mean(samp)
  ci[i,] <- c(sx-Tvalue*sd(samp)/sqrt(n), sx+Tvalue*sd(samp)/sqrt(n))
}


par(mfrow=c(1,1))
plot(0,0, xlim=c(-2,2), ylim=c(0,100), type="n",
     xlab="CI's of Samples", ylab="Number of Runs", las=1)
abline(v=0, lwd=2)

cnt <- 0
for(i in 1:runs){
  clr <- 1
  if(ci[i,1]>0)clr=2
  if(ci[i,2]<0)clr=2
  if(clr == 2)cnt=cnt+1
  segments(ci[i,1], i, ci[i,2], i, col=clr)
}
text(-1.75, 95, paste("Count = ", cnt), cex=0.9)