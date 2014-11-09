setwd("/Users/xuhong/Documents/Duke/Term 1 Courses/ENVIRON 710 Applied data analysis/Lectures")
heights <- read.csv("Heights.csv", header=T)
heights <- heights[!is.na(heights$earn)&heights$earn != 0,]
heights$h <- heights$height1 + heights$height2*1.0/12

h0 <- cbind(log(heights$earn), heights$earn, heights$h, heights$ed)
colnames(h0) <- c("l.earn", "earn", "h", "ed")
pairs(h0)
h0 <- as.data.frame(h0)

hist(h0$earn)



par0 <- par(mfrow=c(2,2))
lm0 <- lm(l.earn ~ h*ed, data=h0)
lm1 <- lm(earn ~ h*ed, data=h0)

# plot
require(car)
avPlots(lm1)
summary(lm1)
plot(lm1)

lm1 <- update(lm0, ~. - h:ed)
lm2 <- update(lm1, ~. - h)
anova(lm0, lm1, lm2)
AIC(lm0, lm1, lm2)

# or just use step()
lm.f <- step(lm0)



