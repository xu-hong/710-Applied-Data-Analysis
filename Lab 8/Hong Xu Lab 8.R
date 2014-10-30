setwd("/Users/xuhong/Documents/Duke/Term 1 Courses/ENVIRON 710 Applied data analysis/Lab/Lab 8")
# Factorial Design
par(mfrow=c(1,1))
feed <- read.csv("Growth.csv", header=T)
with(feed, boxplot(gain~diet+supplement, las=2, ylab='weight gain, pounds'))

cols <- c('darkblue', 'green', 'orange')
with(feed, barplot(tapply(gain, list(diet,supplement), mean), las=1, col=cols, beside=T, ylim=c(0,35)))
labs <- c("barley", "oats", "wheat" )
legend(2.3, 35, labs, fill=cols)
# two way anova
mod1 <- aov(gain~supplement*diet, data=feed)
summary(mod1)
# following produce same results
#lm1 <- lm(gain~supplement*diet, data=feed)
#mod2 <- anova(lm1)

# interation plot: no interaction seen (no crossing)
with(feed, interaction.plot(diet, supplement, gain, col=c(2,3,4,5)))

# taking out the interaction term
mod3 <- aov(gain~supplement + diet, data=feed)
summary(mod3)

# post hoc test
require(graphics)
tm1 <- TukeyHSD(mod3, 'diet', ordered=TRUE)
plot(tm1)

tm2 <- TukeyHSD(mod3, 'supplement', ordered=TRUE)
plot(tm2, las=2)

# evaluate model assumption
par(mfrow=c(2,2))
plot(mod3)
# test homogeneity of variance
bartlett.test(gain~diet, data=feed)
bartlett.test(gain~supplement, data=feed)

# Block Design
lab <- c(rep(1:4, each = 4))
antibiotic <- rep(c(1:4), 4)
results <- (c(9.3, 9.4, 9.6, 10, 9.4, 9.3, 9.8, 9.9, 9.2,
              9.4, 9.5, 9.7, 9.7, 9.6, 10, 10.2))

dlabs <- data.frame(cbind(lab, antibiotic, results))
dlabs$lab <- as.factor(dlabs$lab)
dlabs$antibiotic <- as.factor(dlabs$antibiotic)

# first test without considering blocking
mod2 <- aov(results~antibiotic, data=dlabs)
summary(mod2)
# consider block
mod3 <- aov(results~antibiotic + lab, data=dlabs)
summary(mod3)
with(dlabs, interaction.plot(antibiotic, lab, results, col=c(1,2,3,4)))
TukeyHSD(mod3, 'antibiotic', ordered=TRUE)

# block variable as random variables
mod4 <- aov(results~antibiotic + Error(lab), data=dlabs)
summary(mod4)
# Repeated Measure Design
ad <- read.csv("Sales.csv", header=T)
ad <- with(ad, data.frame(sales, city=factor(city),
                          campaign = factor(campaign), time =
                            factor(time)))

mod5 <- lm(sales ~ campaign * time, data = ad)
anova(mod5)

mod6 <- aov(sales ~ campaign*time + Error(city),
            data = ad)
summary(mod6)
