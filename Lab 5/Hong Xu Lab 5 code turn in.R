
use(pwr)
#Problem #1
young <- c(34, 22, 15, 27, 37, 41, 24, 19, 26, 36)
old <- c(41, 24, 46, 39, 21, 33, 43, 40, 50, 41)

shapiro.test(young)
shapiro.test(old)
var.test(young, old)
#find critical t
qt(0.975, df=length(young)-1)
t.test(old, young, alternative="two.sided", var.equal=T)


#Problem #2
m1 <- mean(old)
m2 <- mean(young)
var1 <- var(old)
var2 <- var(young)
psd <- sqrt((var1+var2)/2)
es <- abs(m1-m2)/psd

pwr.t.test(n=length(young), d=es, sig.level=0.05, type="two.sample")


#Problem #3
nostimulation <- c(18.4, 16.1, 9.2, 32.2, 13.8, 16.1,27.6, 11.5, 11.5, 18.4, 22.3, 21.1, 16.4, 29.5, 27.9)
stimulation <- c(27.6, 16.1, 6.9, 25.3, 18.4, 11.5, 32.2, 16.1, 20.7, 23.0, 24.7, 18.1, 26.5, 23.4, 8.3)
var.test(nostimulation, stimulation)

qt(0.95, df=length(stimulation)-1)

t.test(nostimulation, stimulation, alternative="greater", paired=T)


#Problem #4
#Determine the sample size
pwr.result <- pwr.t.test(n=NULL, d=0.4, sig.level=0.05, power=0.8, alternative="greater")

#Draw the graph
d <- seq(from = 0.3, to = 0.5, by = 0.01)
nd <- length(d)

p <- seq(from = 0.6, to = 0.8, by = 0.1)
np <- length(p)

samsize <- array(numeric(nd*np), dim=c(nd,np))
for (i in 1:np){
  for (j in 1:nd){
    result <- pwr.t.test(n=NULL, d = d[j], sig.level = 0.05, power = p[i], alternative = "greater")
    samsize[j,i] <- ceiling(result$n)
  }
}

xrange <- range(d)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n", xlab="Effect size (d)", ylab="Sample Size (n)", las = 1)


for (i in 1:np) {
  lines(d, samsize[,i], type="l", lwd=2, col=colors[i])
}
abline(v=0, h=seq(0, yrange[2], 10), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1], xrange[2], 0.02), lty=2, col="grey89")
abline(v=0, h=pwr.result$n, col="black")
points(0.4, pwr.result$n, pch=21, cex=2, col=colors[3], bg=colors[3])
title("Sample Size Estimation for T-test\n Sig=0.05 (Onetailed)")
legend("topright", title="Power", as.character(p),
       fill=colors)



#Problem #5
#Draw the graph
n <- seq(from = 10, to = 100, by = 5)
nn <- length(n)

p <- seq(from = 0.6, to = 0.8, by = 0.1)
np <- length(p)

samsize <- array(numeric(nn*np), dim=c(nn,np))
for (i in 1:np){
  for (j in 1:nn){
    result <- pwr.t.test(n=n[j], d = NULL, sig.level = 0.05, power = p[i], alternative = "greater")
    samsize[j,i] <- result$d
  }
}

xrange <- range(n)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n", xlab="Sample size (n)", ylab="Effect Size (d)", las = 1)


for (i in 1:np) {
  lines(n, samsize[,i], type="l", lwd=2, col=colors[i])
}
abline(v=0, h=seq(0, yrange[2], 0.2), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1], xrange[2], 10), lty=2, col="grey89")
title("Effect Size Estimation for T-test\n Sig=0.05 (Onetailed)")
legend("topright", title="Power", as.character(p),
       fill=colors)






