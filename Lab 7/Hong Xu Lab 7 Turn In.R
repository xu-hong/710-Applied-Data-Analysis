setwd("/Users/xuhong/Documents/Duke/Term 1 Courses/ENVIRON 710 Applied data analysis/Lab/Lab 7")

adat <- read.csv("TuskData.csv", header=T)
attach(adat)

# Test for normality
qqnorm(Tusk.kg[Year==1970])
qqline(Tusk.kg[Year==1970])
shapiro.test(Tusk.kg[Year==1970])

qqnorm(Tusk.kg[Year==1990])
qqline(Tusk.kg[Year==1990])
shapiro.test(Tusk.kg[Year==1990])

qqnorm(Tusk.kg[Year==2010])
qqline(Tusk.kg[Year==2010])
shapiro.test(Tusk.kg[Year==2010])


# Test for homogeneity of variance
sd(Tusk.kg[Year==1970])/sd(Tusk.kg[Year==1990])
sd(Tusk.kg[Year==1970])/sd(Tusk.kg[Year==2010])
sd(Tusk.kg[Year==1990])/sd(Tusk.kg[Year==2010])

# one way ANOVA
mod1 <- aov(Tusk.kg ~ factor(Year))
summary(mod1)
plot(mod1)

#To determine which of the means are significantly different from one another
#we conduct a post-hoc test.
TukeyHSD(mod1)
plot(TukeyHSD(mod1))


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


year.mean <- tapply(Tusk.kg, list(Year), mean)
year.n <- tapply(Tusk.kg, list(Year), length)
year.sd <- tapply(Tusk.kg, list(Year), sd)
year.se <- site.sd/sqrt(site.n)
year.se[is.na(year.se)==T] <- 0
labls <- as.character(levels(factor(Year)))
yvals <- as.vector(year.mean)

error.bars(yvalues = yvals, se = year.se, nm = labls)
