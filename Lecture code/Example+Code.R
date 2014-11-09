
# these are the two problems that we started in class on thursday
setwd("/Users/xuhong/Documents/Duke/Term 1 Courses/ENVIRON 710 Applied data analysis/Lab Github/ada/Lecture code")
hdat <- read.csv("Heights.csv", header = T)

 source("pair.fun.R")
  pairs(hdat, lower.panel=panel.smooth, upper.panel=panel.cor,
               diag.panel=panel.hist)
# that's a mess of variables
  hist(hdat$earn, las = 1)
               
# we might (definitely) want to log-transform earnings, but i am going to
# leave it untransformed to keep things simple and focus on plotting
            
################################
# EXAMPLE 1               
# earnings vs. height and sex, where sex is a categorical variable   
# look for the minimum adequate model            
 lm1 <- lm(earn ~ height*factor(sex), data = hdat)
  lm1.a <- update(lm1, ~.-height:factor(sex))
 
# let's see if we would have gotten the same model using step() 
 lm1.final <- step(lm1) 
  summary(lm1.final)

# there is no interaction, so we will plot two lines for sex vs. earnings
# over the range of heights -- we expect the lines to be parallel

# get all the model coefficients for simple plotting
 coef <- coefficients(lm1.final)

# plot the data points, earnings vs. height
 with(hdat, plot(height, earn, las = 1, cex.axis = 0.8, pch=16, cex = 0.6, 
      xlab = "Height, inches", ylab = "Earnings, $"))

# get a range of x-values (height values) to plot the lines for sex
# note that we have to include the na.rm commands b/c there are na's in the data
# we HAVE to call this variable x, because the curve function requires an x term      
  x <- with(hdat, seq(min(height, na.rm=T), max(height, na.rm=T), length = 100))
  
# now use curve to plot the regression equations, one for male and one for female
# look at summary(lm1.final) to see that male is set as the contrast (0)  
# so we multiple the 3rd coefficient by 0 to get male, and by 1 to get female earnings

   curve(coef[1] + coef[2]*x + coef[3]*0, col = "blue", lwd = 2, add=T)
    curve(coef[1] + coef[2]*x + coef[3]*2, col = "pink", lwd = 3, add=T)

# let's add a legend    
     legend("topleft", c("Male", "Female"), col = c("blue", "pink"), lty = 1, lwd = 2, bty="n")

################################
# EXAMPLE 2  
# earnings vs. height and sex, where sex is a categorical variable   
# look for the minimum adequate model
   
 lm2 <- lm(earn ~ height*ed, data = hdat)
  summary(lm2)
# the interaction is significant, so we don't need to reduce the model at all 
# good news that earnings increase with education (since your all are in graduate school)  
  
# plot the interaction of height vs. ed against earn
# since we have a significant interaction, we don't care about the main effects
# here i will plot different values of education across the range of heights

# i am going to choose 6 (low value), 12 (median), and 15 (75%) for education values
# i am choosing these just to show how height influences the effect of different levels of
# education on earnings
 summary(hdat$ed) 
  evals <- c(6, 12, 15)  

# as above, first plot the data 
 par(mar = c(4,5,1,1))
 with(hdat, plot(height, earn, las = 1, xlab = "Height (inches)", ylab = "Earnings, $", cex.axis = 0.8, 
      cex = 0.6, pch = 16))

# now create a sequence of x values across the range of heights
  x <- with(hdat, seq(min(height, na.rm=T), max(height, na.rm=T), length = 100))
  
# extract the model coefficients and use them to create the curves for education at the above
# determine values
  
   coef <- coefficients(lm2)
    curve(coef[1] + coef[2]*x + coef[3]*evals[1] + coef[4]*x*evals[1], add = T, col = "grey", lwd = 2)
    curve(coef[1] + coef[2]*x + coef[3]*evals[2] + coef[4]*x*evals[2], add = T, col = "red", lwd = 2)
    curve(coef[1] + coef[2]*x + coef[3]*evals[3] + coef[4]*x*evals[3], add = T, col = "blue", lwd = 2)

# now add a legend    
    legend("topleft", c("6 yrs", "12 yrs", "15 yrs"), lty = 1, col = c("grey", "red", "blue"), lwd = 2, 
           bty = "n")    
           
# look's like the increase in earnings with height is fastest with higher levels of education           