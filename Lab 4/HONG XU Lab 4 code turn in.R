
set.seed(1001)

dat <- read.csv("AfrPlots.csv", header = T)
attach(dat)


# Problem #1
Deads <- dat$Dead[dat$CensusNo == 2]

myqqplot <- function(n, data, xlab, ylab){
  q <- (1:n - 0.5)/n
  qZ <- qnorm(q, mean = 0, sd = 1)
  plot(qZ, sort(data), xlab=xlab, ylab=ylab)
  q1 <- c(qnorm(0.25, mean=0, sd=1),quantile(data, 0.25))
  q3 <- c(qnorm(0.75, mean=0, sd=1),quantile(data, 0.75))
  b <- (q3[[2]] - q1[[2]])/(q3[[1]] - q1[[1]]) # the slope
  a <- q1[[2]] - b*q1[[1]] # the intercept
  abline(a, b, col="red")
}
myqqplot(n=30, data=Deads, xlab="Theorectical Quantiles", ylab="Sample Quantiles of Deads")

qqnorm(Deads)
qqline(Deads)

# Problem #2
# ChaveMoist from CensusNo == 1
CM <- dat$ChaveMoist[dat$CensusNo == 1]
hist(CM, las = 1, main= "Distribution of ChaveMoist", xlab = "ChaveMoist", 
     col = 2, prob = T)
curve(dnorm(x, mean = mean(CM), sd = sd(CM)), 
      add=T, col = "darkblue", lwd = 2)

qqnorm(CM)
qqline(CM)

shapiro.test(CM)

# Recruits from CensusNo == 2
rec <- dat$Recruits[dat$CensusNo == 2]
hist(rec, las = 1, main= "Distribution of Recruits", xlab = "Recruits", 
     col = 2, prob = T, ylim=c(0, 0.08))
curve(dnorm(x, mean = mean(rec), sd = sd(rec)), 
      add=T, col = "darkblue", lwd = 2)

qqnorm(rec)  
qqline(rec)

shapiro.test(rec)

# Problem #3
mu <- 0
sd <- 1
n <- 50
runs <- 100
conf <- qnorm(0.975)

n2 <- 15
conf2 <- qt(0.975, df=14)

ci <- matrix(nrow=runs, ncol=2)

for (i in 1:runs){
  samp <- rnorm(n2, mu, sd)
  sx <- mean(samp)
  ci[i,] <- c(sx-conf2*sd(samp)/sqrt(n2), sx+conf2*sd(samp)/sqrt(n2))
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


# use old conf of 1.96
mu <- 0
sd <- 1
n <- 50
runs <- 100
conf <- qnorm(0.975)

n2 <- 15
conf2 <- qt(0.975, df=14)

ci <- matrix(nrow=runs, ncol=2)

for (i in 1:runs){
  samp <- rnorm(n2, mu, sd)
  sx <- mean(samp)
  ci[i,] <- c(sx-conf*sd(samp)/sqrt(n2), sx+conf*sd(samp)/sqrt(n2))
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


# Problem #4
cm <- dat$ChaveMoist[dat$CensusNo == 2]
cm.n <- length(cm)
cm.mean <- mean(cm)
cm.se <- sd(cm)/sqrt(cm.n)

Ts <- qt(0.975, df=cm.n-1) 
cm.ci <- c(cm.mean-Ts*cm.se, cm.mean+Ts*cm.se)