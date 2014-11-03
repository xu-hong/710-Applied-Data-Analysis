setwd("/Users/xuhong/Documents/Duke/Term 1 Courses/ENVIRON 710 Applied data analysis/Lab/Lab 9")

tdata <- read.csv("TreePlots.csv", header=T)
attach(tdata)
# scatterplot
pairs(tdata)

source("pair.fun.R")
pairs(tdata, lower.panel=panel.smooth,
      upper.panel=panel.cor, diag.panel=panel.hist)

# correlation test
cor1 <- with(tdata, cor.test(mH.m, mWD.g.m3))
with(tdata, plot(mH.m, mWD.g.m3, las = 1, xlab="Mean Height, m", ylab="Mean Wood Density, g per m3"))

cor2 <- with(tdata[mH.m < 22,], cor.test(mH.m, mWD.g.m3))
with(tdata[mH.m < 22,], plot(mH.m, mWD.g.m3, las = 1, xlab="Mean Height, m", ylab="Mean Wood Density, g per m3"))

# correlation of entire matrix
cor(tdata)


# linear regression
lm1 <- lm(AGBH.Mg.ha ~ mH.m, data = tdata)
anova(lm1)
summary(lm1)
plot(mH.m, AGBH.Mg.ha, lwd = 2, las = 1, pch = 19,
     col = "grey", cex = 1.2,
     xlab = "Mean height, m",
     ylab = expression(paste("Biomass Mg ", ha^-1)))
abline(lm(AGBH.Mg.ha ~ mH.m), col = "darkblue", lwd = 2)

y.fitted <- lm1$coefficients[1] + lm1$coefficients[2]*tdata$mH.m
cor.test(y.fitted, lm1$fitted)

opar <- par(mfrow=c(2,2))
plot(lm1)
par <- opar