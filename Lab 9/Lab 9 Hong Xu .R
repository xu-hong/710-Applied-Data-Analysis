setwd("/Users/xuhong/Documents/Duke/Term 1 Courses/ENVIRON 710 Applied data analysis/Lab/Lab 9")

tdata <- read.csv("TreePlots.csv", header=T)
attach(tdata)
# scatterplot
pairs(tdata)

source("pair.fun.R")
pairs(tdata, lower.panel=panel.smooth,
      upper.panel=panel.cor, diag.panel=panel.hist)

## (1) mean tree diameter (mDBH.cm) versus plot biomass (AGBH.Mg.ha),
## plot biomass should be your dependent variable

# correlation test
cor1 <- with(tdata, cor.test(mDBH.cm, AGBH.Mg.ha))
with(tdata, plot(mDBH.cm, AGBH.Mg.ha, las = 1))

# data point with highest tree diameter seems to have a large effect on correlation coefficient
cor1.1 <- with(tdata[mDBH.cm < 35,], cor.test(mDBH.cm, AGBH.Mg.ha))
with(tdata[mDBH.cm < 35,], plot(mDBH.cm, AGBH.Mg.ha, las = 1))

# linear regression
lm1 <- lm(AGBH.Mg.ha ~ mDBH.cm, data = tdata)
anova(lm1)
summary(lm1)
plot(mDBH.cm, AGBH.Mg.ha, lwd = 2, las = 1, pch = 19,
     col = "grey", cex = 1.2,
     xlab = "mean tree diameter, cm",
     ylab = expression(paste("Biomass Mg ", ha^-1)))
abline(lm(AGBH.Mg.ha ~ mDBH.cm), col = "darkblue", lwd = 2)

#y.fitted <- lm1$coefficients[1] + lm1$coefficients[2]*tdata$mH.m
#cor.test(y.fitted, lm1$fitted)
require(ggplot2)

ggplot(tdata, aes(x = mDBH.cm, y = AGBH.Mg.ha)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm) +
  xlab("Mean tree diameter, cm") +
  ylab(expression(paste("Biomass Mg ", ha^-1)))

# plot diagnostics
opar <- par(mfrow=c(2,2))
plot(lm1)
par <- opar



## (2) mean height (mH.m) versus mean wood density (mWD.g.m3).
## mean height should be your dependent variable.

# correlation test
cor2 <- with(tdata, cor.test(mH.m, mWD.g.m3))
with(tdata, plot(mH.m, mWD.g.m3, las = 1))

# data point with highest mean height seems to have a large effect on correlation coefficient
cor2.1 <- with(tdata[mH.m < 24,], cor.test(mH.m, mWD.g.m3))
with(tdata[mH.m < 24,], plot(mH.m, mWD.g.m3, las = 1))

# linear regression
lm2 <- lm(mH.m ~ mWD.g.m3, data = tdata)
anova(lm2)
summary(lm2)
plot(mWD.g.m3, mH.m, lwd = 2, las = 1, pch = 19,
     col = "grey", cex = 1.2,
     xlab = "mean wood density, g per m3",
     ylab = "mean height, m")
abline(lm(mH.m ~ mWD.g.m3), col = "darkblue", lwd = 2)

#y.fitted <- lm1$coefficients[1] + lm1$coefficients[2]*tdata$mH.m
#cor.test(y.fitted, lm1$fitted)
require(ggplot2)

ggplot(tdata, aes(x = mWD.g.m3, y = mH.m)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm) +
  xlab("mean wood density, g per m3") +
  ylab("mean height, m")

# plot diagnostics
opar <- par(mfrow=c(2,2))
plot(lm2)
par <- opar

# taking out point 60
lm2 <- lm(mH.m ~ mWD.g.m3, data = tdata[mH.m < 24,])
summary(lm2)

opar <- par(mfrow=c(2,2))
plot(lm2)
par <- opar

ggplot(tdata[mH.m < 24,], aes(x = mWD.g.m3, y = mH.m)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm) +
  xlab("mean wood density, g per m3") +
  ylab("mean height, m")
