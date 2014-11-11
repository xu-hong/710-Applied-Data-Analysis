setwd("/Users/xuhong/Documents/Duke/Term 1 Courses/ENVIRON 710 Applied data analysis/Lab Github/ada/Midterm")
require("car")

pdat <- read.csv("PetrolConsumption.csv", header=T)
lm.full <- lm(Petrol ~ Tax*Income*Drivers, data=pdat)
lm1 <- update(lm.full, ~.-Tax:Income:Drivers)
summary(lm1)
lm2 <- update(lm1, ~.-Income:Drivers)
summary(lm2)
lm3 <- update(lm2, ~.-Tax:Income)
summary(lm3)
lm4 <- update(lm3, ~.-Tax:Drivers)
summary(lm4)
anova(lm.full, lm1, lm2, lm3, lm4)
# minimum adequate model is lm3

# centering the predictors
pdat$Tax.c <- pdat$Tax - mean(pdat$Tax)
pdat$Income.c <- pdat$Income - mean(pdat$Income)
pdat$Drivers.c <- pdat$Drivers - mean(pdat$Drivers)

lm.final <- lm(Petrol ~ Tax.c + Drivers.c + Income.c + Tax.c:Drivers.c, data=pdat)
summary(lm.final)


# plot the interaction
with(pdat, plot(Drivers.c, Petrol, xlab="proportion of population with driver's license",
                ylab="annual petrol consumption in gallons", col="grey", pch=16))
coef <- lm.final$coefficients
x <- seq(min(pdat$Drivers.c), max(pdat$Drivers.c), length=100)
vals <- c(-2.3, 0, 2.3)

curve(coef[1] + coef[2]*vals[1] + coef[3]*x + coef[4]*0 + coef[5]*vals[1]*x, col="red", lwd=2, add=T)
curve(coef[1] + coef[2]*vals[2] + coef[3]*x + coef[4]*0 + coef[5]*vals[2]*x, col="blue", lwd=2, add=T)
curve(coef[1] + coef[2]*vals[3] + coef[3]*x + coef[4]*0 + coef[5]*vals[3]*x, col="green", lwd=2, add=T)
vals.original <- vals + mean(pdat$Tax)
vals.original <- round(vals.original, digits = 2)
legend("topleft", legend=vals.original, lty = 1, col = c("red", "blue", "green"), lwd = 2, 
       bty = "n", title="petrol tax in dollars")  
