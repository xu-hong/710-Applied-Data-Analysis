# Problem 1

setwd("/Users/xuhong/Documents/Duke/Term 1 Courses/ENVIRON 710 Applied data analysis/Lab/Lab 2")
afdat <- read.csv("AfrPlots.csv", header=T, sep=",")
attach(afdat)
mg <- MeanGr
mg <- mg[!is.na(mg)] # delete NA rows

#a) plot a histogram and boxplot of MeanGr
hist(mg, col="yellow", main="")
boxplot(mg)

#b) skewness and kurtosis
#skewness function
myskewness <- function(y){
  n <- length(y)
  skew <- 1/(n*sd(y)^3)*sum((y-mean(y))^3)
  skew
}

#kurtosis function
mykurtosis <- function(x){
  n <- length(x)
  kurt <- 1/(n*sd(x)^4)*sum((x-mean(x))^4) 
  kurt
}

myskewness(mg)
mykurtosis(mg)

#c) write a function for variance
myvar <- function(x) {
  n <- length(x)
  variance <- 1/(n-1)*sum((x-mean(x))^2)
  variance
}

#compare with var(), the output should be TRUE
myvar(mg) == var(mg)


#d) report the mean, median, variance, standard deviation and coefficient of variation
mean(mg)
median(mg)
var(mg)
sd(mg)
sd(mg)/mean(mg)


#Problem 2
set.seed(1001)
p <- 0.5 
n <- 20
#use the dbinom function
pr1 <- sum(dbinom(0:5, size = n, prob = p))
pr1
1-pr1
#use the equation
#create a function of calculating probability based on the equation
mybinom <- function(x){
  probX <- (factorial(n)/(factorial(x)*factorial(n-x)))*p^x*(1-p)^(n-x)
  probX
}
pr2 <- sum(mybinom(0:5))
pr2
1-pr2

#Problem 3
1 - pbinom(q = 16, size = 20, prob = 0.25)

#Problem 4
#1)write out the Poisson formula
mypois <- function(x, m){
  probX <- m^x*exp(1)^(-m)/factorial(x)
  probX
}
sum(mypois(x=9:13, m=4))
#2)use dpois()
sum(dpois(x=9:13, lambda=4))


#Problem 5
#probability of counting 4 species
dpois(x=4, lambda=20)
#probability of counting 15 or more species
1 - sum(dpois(x=0:14, lambda=20))
#graph
plot(0:40, dpois(x=0:40, lambda=20), type="h", col="blue", ylab="Probability", xlab="Number of species")
