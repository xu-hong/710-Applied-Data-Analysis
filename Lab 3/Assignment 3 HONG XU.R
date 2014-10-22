#import data
dat <- read.csv("AfrPlots.csv", header = T)
attach(dat)

#problem 1
qnorm(p=0.9, mean=4, sd=1.4)
pnorm(q=5, mean=4, sd=1.4)
set.seed(1001)
drawl <- rnorm(n=12, mean=4, sd=1.4)


#problem 2
set.seed(1001)
h2o <- as.data.frame(matrix(rnorm(n=10*12, mean=4, sd=1.4), ncol=12))
rownames(h2o) <- paste(rep("Riv", nrow(h2o)), c(1:nrow(h2o)), sep = "")
colnames(h2o) <- paste(rep("Obs", ncol(h2o)), c(1:ncol(h2o)), sep = "")

h2o$Test <- rowSums(ifelse(h2o>6, 1, 0))
length(h2o$Test[h2o$Test>1])

#problem 3
set.seed(1001)
h2o <- as.data.frame(matrix(rnorm(n=10*12, mean=4.5, sd=1.4), ncol=12))
rownames(h2o) <- paste(rep("Riv", nrow(h2o)), c(1:nrow(h2o)), sep = "")
colnames(h2o) <- paste(rep("Obs", ncol(h2o)), c(1:ncol(h2o)), sep = "")

h2o$Test <- rowSums(ifelse(h2o>6, 1, 0))
length(h2o$Test[h2o$Test>1])

#problem 4
set.seed(1001)
h2o <- as.data.frame(matrix(rnorm(n=10*36, mean=4, sd=1.4), ncol=36))
rownames(h2o) <- paste(rep("Riv", nrow(h2o)), c(1:nrow(h2o)), sep = "")
colnames(h2o) <- paste(rep("Obs", ncol(h2o)), c(1:ncol(h2o)), sep = "")

h2o$Test <- rowSums(ifelse(h2o>6, 1, 0))
#cut-off is 10%*36 which means 4 
length(h2o$Test[h2o$Test>=4])

#problem 5
set.seed(1001)
#create a funtion
evaluatefunc <- function(samplesize, rivernumber){
  h2odat <- as.data.frame(matrix(rnorm(n=samplesize*rivernumber, mean=4, sd=1.4), ncol=samplesize))
  rownames(h2odat) <- paste(rep("Riv", nrow(h2odat)), c(1:nrow(h2odat)), sep = "")
  colnames(h2odat) <- paste(rep("Obs", ncol(h2odat)), c(1:ncol(h2odat)), sep = "")
  
  h2odat$Test <- rowSums(ifelse(h2odat>6, 1, 0))
  cutoff = ceiling(samplesize * 0.1)
  impaired = length(h2odat$Test[h2odat$Test>=cutoff])
  
  #give the ratio of "the impaired" 
  impaired/rivernumber
}

#test different sample size from 10,50,100 and 500 rivers
sapply(c(10,50,100,500), evaluatefunc, samplesize=10)
sapply(c(10,50,100,500), evaluatefunc, samplesize=50)
sapply(c(10,50,100,500), evaluatefunc, samplesize=100)
sapply(c(10,50,100,500), evaluatefunc, samplesize=500)

#collection is the collection of the unhealthy ratio in 500 rivers
collection <- c()
for (i in 1:500){
  collection <- c(collection, evaluatefunc(samplesize=i, rivernumber=500))
}

#percentage of ratio below 10%
sum(collection <= 0.1)/500

hist(collection, xlab="ratio")
plot(collection, ylab="ratio of unhealthy results", xlab="sample size")
