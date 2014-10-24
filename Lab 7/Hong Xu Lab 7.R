setwd("/Users/xuhong/Documents/Duke/Term 1 Courses/ENVIRON 710 Applied data analysis/Lab/Lab 7")

adat <- read.csv("Afrplots.csv", header=T)
adat$Site <- as.factor(rep(c(rep(1,10), rep(2,10), rep(3,10)), 2))
bdat <- adat[adat$CensusNo == 1, ]
attach(bdat)

par(mfrow=c(2,2))
boxplot(ChaveMoist ~ Site, las=1, ylab="Biomass", xlab="Sites")
points(jitter(rep(1, length(Site[Site==1])), f=4), ChaveMoist[Site==1], col = "darkgreen")
points(jitter(rep(2, length(Site[Site==2])), f=4), ChaveMoist[Site==2], col = "darkblue")
points(jitter(rep(3, length(Site[Site==3])), f=4), ChaveMoist[Site==3], col = "darkred")

# normal assumption?
boxplot(log(ChaveMoist) ~ Site, las=1,
        ylab = "log(Biomass)", xlab = "Sites")
points(jitter(rep(1, length(Site[Site==1])), f=4),
       log(ChaveMoist[Site==1]), col = "darkgreen")
points(jitter(rep(2, length(Site[Site==2])), f=4),
       log(ChaveMoist[Site==2]), col = "darkred")
points(jitter(rep(3, length(Site[Site==3])), f=4),
       log(ChaveMoist[Site==3]), col = "darkblue")

qqnorm(ChaveMoist[Site==1])
qqline(ChaveMoist[Site==1])
# log does not seem to make much difference
qqnorm(log(ChaveMoist[Site==1]))
qqline(log(ChaveMoist[Site==1]))
# do not reject the null hypothesis!
shapiro.test(ChaveMoist[Site==1])


sd(ChaveMoist[Site==1])/sd(ChaveMoist[Site==2])
sd(ChaveMoist[Site==2])/sd(ChaveMoist[Site==3])
sd(ChaveMoist[Site==1])/sd(ChaveMoist[Site==3])


# one way ANOVA
mod1 <- aov(ChaveMoist ~ factor(Site))
summary(mod1)

#To determine which of the means are significantly different from one another
#we conduct a post-hoc test.
TukeyHSD(mod1)
plot(TukeyHSD(mod1))

# add pairwise test
pairwise.t.test(ChaveMoist, Site)


error.bars <- function(yvalues, se, nm){
  xv <- barplot(yvalues, ylim=c(0, (max(yvalues)+max(se))),
            names=nm, ylab=deparse(substitute(yvalues)), las=1)
  g <- (max(xv)-min(xv))/50
  for (i in 1:length(xv)){
    arrows(xv[i], yvalues[i] + se[i], xv[i],
           yvalues[i]-se[i], length=0.1, angle=90,
           code=3)
  }
}


site.mean <- tapply(ChaveMoist, list(Site), mean)
site.n <- tapply(ChaveMoist, list(Site), length)
site.sd <- tapply(ChaveMoist, list(Site), sd)
site.se <- site.sd/sqrt(site.n)
site.se[is.na(site.se)==T] <- 0
labls <- as.character(levels(Site))
yvals <- as.vector(site.mean)

error.bars(yvalues = yvals, se = site.se, nm = labls)