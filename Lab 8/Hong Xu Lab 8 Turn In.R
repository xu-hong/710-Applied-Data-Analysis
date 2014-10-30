setwd("/Users/xuhong/Documents/Duke/Term 1 Courses/ENVIRON 710 Applied data analysis/Lab/Lab 8")

##################
# Data 1: biomass
##################
biod <- "obs salt block biomass
1 10 1 11.8
2 15 1 21.3
3 20 1 8.8
4 25 1 10.4
5 30 1 2.2
6 35 1 8.4
7 10 2 15.1
8 15 2 22.3
9 20 2 8.1
10 25 2 8.5
11 30 2 3.3
12 35 2 7.3
13 10 3 22.6
14 15 3 19.8
15 20 3 6.1
16 25 3 8.2
17 30 3 6.1
18 35 3 5.2
19 10 4 7.1
20 15 4 9.9
21 20 4 1.0
22 25 4 2.8
23 30 4 0.7
24 35 4 2.2"
biod <- read.delim(textConnection(biod), sep=" ")
biod$salt <- factor(biod$salt)
biod$block <- factor(biod$block)

par(mfrow=c(1,1))
# check the normal assumption
with(biod, boxplot(biomass~salt))
with(biod, tapply(biomass, salt, shapiro.test))
with(biod, tapply(biomass, block, shapiro.test))
# check the homogeneity of variance
bartlett.test(biomass~salt, data=biod)
bartlett.test(biomass~salt*block, data=biod)

# 
mod0 <- aov(biomass ~ salt + block, data=biod)
mod1 <- aov(biomass ~ salt + Error(block), data=biod)
summary(mod1)
with(biod, interaction.plot(salt, block, biomass, col = c(1,2,3,4)))

# Post hoc test
plot(TukeyHSD(mod0, 'salt'))
require("agricolae")
out <- with(biod, HSD.test(biomass, salt, DFerror = 5,
                         MSerror = 7.69))
bar.group(out$groups, density=6, ylim=c(0,25),
          border="darkblue", las=1)
with(biod, tapply(biomass, list(salt), mean))


# Diagnostic plots
par(mfrow=c(2,2))
plot(mod0)

##################
# Data 2: thickness of scale
# just for test
##################
thk <- read.csv("ScaleThickness.csv", sep=",")

par(mfrow=c(1,1))
# check the normal assumption
with(thk, boxplot(thick~supp+dose))
with(thk, tapply(thick, supp, shapiro.test))
with(thk, tapply(thick, dose, shapiro.test))
# check the homogeneity of variance
bartlett.test(thick~supp, data=thk)
bartlett.test(thick~dose, data=thk)


# 
mod2 <- aov(thick ~ factor(dose)*supp, data=thk)
summary(mod2)
with(thk, interaction.plot(supp, dose, thick, col = c(1,2,3)))

# Post hoc test
plot(TukeyHSD(mod2, factor('dose')))
plot(TukeyHSD(mod2, 'supp'))

# Diagonistic plots
par(mfrow=c(2,2))
plot(mod2)
