setwd("/Users/xuhong/Documents/Duke/Term 1 Courses/ENVIRON 710 Applied data analysis/Lab Github/ada/Lab 10")

source("pair.fun.R")
require("Sleuth3")
require("car")

# load first data from Sleuth3
dat1 <- case0901


# inspect the data
tapply(dat1$Flowers, list(dat1$Intensity, dat1$Time), length)
par0 <- par(mfrow=c(2,2))
pairs(dat1, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist)
par <- par0
hist(log(dat1$Flowers))
qqnorm(dat1$Flowers)
qqline(dat1$Flowers)
qqnorm(log(dat1$Flowers))
qqline(log(dat1$Flowers))

# fit the model
lm0 <- with(dat1, lm(log(Flowers) ~ Intensity*factor(Time)))
summary(lm0)
# interaction term is not significant
lm1 <- update(lm0, ~.-Intensity:factor(Time))
summary(lm1)
lm2 <- update(lm1, ~.-factor(Time))
summary(lm2)
anova(lm0, lm1, lm2)
# or use AIC step
lm.f <- step(lm0)
summary(lm.f)

# plot the final model
par0 <- par(mfrow=c(2,2))
plot(lm.f)

# diagnose the final model
avPlots(lm.f)
mean(residuals(lm.f))
vif(lm.f)
# 9th observation might be an outlier
lm.fo <- with(dat1[-9,], lm(log(Flowers) ~ Intensity + factor(Time)))
summary(lm.fo)


# plot Flowers against Intensity and levels of Time
par(mfrow=c(1,1))
with(dat1, plot(Intensity[Time == 1], Flowers[Time==1],
                pch=21, bg="red", ylab="No. of flowers",
                xlab = "Light intensity", cex=1.2, las=1))
with(dat1, points(Intensity[Time == 2], Flowers[Time==2],
                pch=21, bg="grey", cex=1.2))
x <- unique(dat1$Intensity)
coef <- lm.f$coefficients
curve(exp(coef[1] + coef[2]*x + coef[3]*0), add=TRUE, col="red", lty=2)
curve(exp(coef[1] + coef[2]*x + coef[3]*1), add=TRUE, col="grey", lty=2)
