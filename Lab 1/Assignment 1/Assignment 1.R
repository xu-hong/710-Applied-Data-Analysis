### Problem 1 
fp = 0.5 #probability of head for a fair coin
flip50 <- rbinom(50, 1, fp)
flip500 <- rbinom(500, 1, fp)
flip5000 <- rbinom(5000, 1, fp)
table(flip50)/50
table(flip500)/500
table(flip5000)/5000

up = 0.57 #probability of head for an unfair coin
flip50 <- rbinom(50, 1, up)
flip500 <- rbinom(500, 1, up)
flip5000 <- rbinom(5000, 1, up)
table(flip50)/50
table(flip500)/500
table(flip5000)/5000

#a coin flipping experiment of 50 flips of a fair coin that is replicated 100 times
fliprep <- rbinom(100, 50, fp)
plot(table(fliprep))


### Problem 2 
require(grid)
require(VennDiagram)
Months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
r <- c("Jan", "Feb", "Mar", "Apr", "Sep", "Oct", "Nov", "Dec")
L <- c("Jan", "Mar", "May", "Jul", "Aug", "Oct", "Dec")
RintersectL <- intersect(r,L)
# a) What is the probability of r?
P_r <- length(r)/length(Months)
# b) What is the probability of RintersectL?
P_RintersectL <- length(RintersectL)/length(Months)
# c) What is P(r|L)?
P_RcondL <- length(RintersectL)/length(L)
# Make a Venn diagram
venn.diagram(list(M=Months, R=r, L=L),fill = c("white","red", "blue"), alpha = c(0.2, 0.5, 0.5), cat.fontface = 4, lty = 1, filename = "RL.tiff")


### Problem 3
# probability for making three points in different scenarios:
# a) shooting a single three-point shot:
P3_1 = 0.358
# b) shooting three free throws 
Pf_3 = 1 - pbinom(1, 3, 0.748)
# c) shooting a two-point field goal and a free throw
P2f = 0.473 * 0.748


### Problem 4
CCO2 <- read.csv("Country+CO2.csv", header=T, sep=",")
boxplot(CCO2$CO2_tons_per_person)
summary(CCO2$CO2_tons_per_person)
sd(CCO2$CO2_tons_per_person)

# order the data
CCO2[order(CCO2$CO2_tons_per_person, decreasing=T),]

# plot the histogram
hist(CCO2$CO2_tons_per_person, main="Histogram of CO2_tons_per_person", xlab="CO2 tons per person",breaks=10, ylim=c(0,20))
