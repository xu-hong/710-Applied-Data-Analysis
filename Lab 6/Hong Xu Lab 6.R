
setwd("/Users/xuhong/Documents/Duke/Term 1 Courses/ENVIRON 710 Applied data analysis/Lab/Lab 6")
library(boot)

# use boot() function to eliminate the need for looping
set.seed(1001)
x <- rpois(25, lambda=6)
samp1 <- sample(x, size=25, replace=T)
samp2 <- sample(x, size=25, replace=T)
data <- data.frame(cbind(samp1, samp2))

ratio <- function(data, i){
 sum(data$samp1[i])/sum(data$samp2[i])
}

b <- boot(data, statistic=ratio, R=999)
mean(b$t)
b$t0

# Q1
# infer from one sample
set.seed(1001)
mysample <- rnorm(35, mean=20, sd=4)
mymean <- mean(mysample)
mysd <- sd(mysample)
myci <- c(mymean - 1.96 * mysd/sqrt(35), mymean + 1.96 * mysd/sqrt(35))

# sample 1000 times
sample.means <- c()
for (i in 1:1000) { sample.means[i] <- mean(rnorm(35, 20, 4))}
hist(sample.means, las = 1, col = "darkblue", main="")
segments(sort(sample.means)[25], 0, sort(sample.means)[25], 250, col="red", lwd=2)
segments(sort(sample.means)[975], 0, sort(sample.means)[975], 250, col="red", lwd=2)
# use true population mean and sd
sum(sample.means >= 20 - 1.96*4/sqrt(35) & sample.means <= 20 + 1.96*4/sqrt(35))/1000

# use bootstrapping to find ci
boot.means <- c()
for (i in 1:1000){
  boot.means[i] <- mean(sample(mysample, 35, replace=T))
}
hist(boot.means, las = 1, col = "darkblue", main="")
se <- sd(boot.means)
ci <- c(mean(boot.means)-1.96*se, mean(boot.means)+1.96*se)
ci2 <- c(quantile(boot.means, 0.025), quantile(boot.means, 0.975))



# resampling for two variables 
dat <- read.csv("Galapagos.csv", header = T)
attach(dat)
plot(Area, Nspecies, pch=19, col="red")
# linear regression
fit <- lm(Nspecies~Area)
abline(fit, col="darkblue")

# draw the logrithm plot
plot(log10(Area), log10(Nspecies), pch=19, col="red")
#plot(log10(Nspecies)~log10(Area), pch=19, col="red")
# linear regression
Lfit <- lm(log10(Nspecies)~log10(Area))
abline(Lfit, col="darkblue")

Lco <- Lfit$coefficients


# bootstrapping the sample pairs
runs <- 1000
len <- length(Nspecies)
slope <- c()
intercept <- c()
for(i in 1:runs){
  samp <- dat[sample(nrow(dat), size=len, replace=T), ]
  fit <- lm(log10(samp$Nspecies) ~ log10(samp$Area))
  intercept[i] <- fit$coefficients[1]
  slope[i] <- fit$coefficients[2]
}
mean.int <- mean(intercept)
mean.slope <- mean(slope)

par(mfrow=c(1,2))
hist(intercept, las=1)
abline(v=sort(intercept)[25], col="red", lwd=1.5)
abline(v=sort(intercept)[975], col="red", lwd=1.5)
ci.int<-c(quantile(intercept, 0.025),
          quantile(intercept, 0.975))


hist(slope, las=1)
abline(v=sort(slope)[25], col="red", lwd=1.5)
abline(v=sort(slope)[975], col="red", lwd=1.5)
ci.slope<-c(quantile(slope, 0.025),
          quantile(slope, 0.975))



