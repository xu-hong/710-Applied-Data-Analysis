array(1:50, dim = c(5,10), dimnames = c("a", "b"))
array(1:24, dim = c(3,4,2))

a <- matrix(8,2,3)
b <- matrix(9,2,3)
my.array <- array(c(a,b), c(2,3,2))


x <- c(1:20)
a <- 3
b <- 2
plot(x, y=a*x^b, xlab="Counts", ylab="Response", las=1, pch=21, bg="darkblue")


d <- seq(from = 0.1, to = 0.9, by = 0.01)
nd <- length(d)

p <- seq(from = 0.4, to = 0.9, by = 0.1)
np <- length(p)

samsize <- array(numeric(nd*np), dim=c(nd,np))
for (i in 1:np){
  for (j in 1:nd){
    result <- pwr.t.test(n=NULL, d = d[j], sig.level = 0.05, power = p[i], alternative = "two.sided")
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
abline(v=0, h=seq(0, yrange[2], 100), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1], xrange[2], 0.02), lty=2, col="grey89")
title("Sample Size Estimation for T-test\n Sig=0.05 (Twotailed)")
legend("topright", title="Power", as.character(p),
       fill=colors)










