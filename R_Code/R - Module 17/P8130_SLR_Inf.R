################################################################
#                     Biostatistical Methods I                 #
#             Simple Linear Regression - Inferences            #
################################################################

rm(list = ls())

# Load libraries
library(faraway)
library(broom)
library(dplyr)

# Read data 'Hospitals'
data_hosp<-read.csv("Hospital.csv")
names(data_hosp)

# Look at data structure
str(data_hosp)

# Scatter plot (Y) vs (X)
# LOS: length of stay(Y)
# BEDS: number of beds(X)

data_hosp %>% 
   ggplot(aes(BEDS, LOS)) + geom_point(color='blue') + theme_bw(base_size=20) + 
   labs(x="Number of beds", y="Length of stay (days)")

# Simple linear regression
reg_hos<-lm(data_hosp$LOS~data_hosp$BEDS)

# Analyze the regression results
summary(reg_hos)

# Get the ANOVA table
anova(reg_hos)

# Residual st error: MSE=sigma^2
glance(reg_hos)$sigma


# Scatter plot with regression line overlaid  
data_hosp %>% 
  ggplot(aes(BEDS, LOS)) + geom_point(color='blue') + theme_bw(base_size=20) +
  geom_smooth(method='lm', se=FALSE, color='red') +
  labs(x="Number of beds", y="Length of stay (days)")

# Scatter plot with regression line overlaid and 95% confidence bands
data_hosp %>% 
  ggplot(aes(BEDS, LOS)) + geom_point(color='blue') + theme_bw(base_size=20) +
  geom_smooth(method='lm', se=TRUE, color='red') +
  labs(x="Number of beds", y="Length of stay (days)")


# How do we calculate the 95% CI for the slope?
# Interpretation: 95% CI for the expected/mean difference in LOS for 1 bed differene

# Get the critical t value for alpha=0.05 and n-2 df

qt(0.975,111)  # In data hospital, df=n-2=113-2=111

coef<-summary(reg_hos)$coefficients[2,1] 
err<-summary(reg_hos)$coefficients[2,2] 
slope_int<-coef + c(-1,1)*err*qt(0.975, 111)

# CIs for both slope and intercept
confint(reg_hos)
confint(reg_hos,level=0.95)


# How do we calculate the 95% CI for 100 beds difference?
coef<-summary(reg_hos)$coefficients[2,1] 
err<-summary(reg_hos)$coefficients[2,2] 
slope_int100<-100*coef + c(-1,1)*(100*err)*qt(0.975, 111)
slope_int100


#############################################################################
# Calculate 95% CIs using predict function
# If 'newdata' is omitted the predictions are based on the data used for the fit, like in the case below.

pred.clim <- predict.lm(reg_hos, interval="confidence") 
datapred <- data.frame(cbind(data_hosp$BEDS, data_hosp$LOS, pred.clim))

plot(datapred[,1],datapred[,2],xlab="Number of Beds", ylab="Length of stay (days)")

#abline(reg_hos,lwd=2,col=2)
lines(datapred[,1],datapred[,3], lwd=2)
lines(datapred[,1],datapred[,5], lty=1, col=3, type='l')
lines(datapred[,1],datapred[,4], lty=1, col=3,type='l')

# Calculate 95% PIs for fitted values using predict function
# Compare to prediction intervals: of course that the PIs are wider than CIs
pred.plim <- predict.lm(reg_hos, interval="prediction") 
datapred1 <- data.frame(cbind(data_hosp$BEDS, data_hosp$LOS, pred.plim))

#abline(reg_hos,lwd=2,col=2)
lines(datapred1[,1],datapred1[,3], lwd=2)
lines(datapred1[,1],datapred1[,5], lty=1, col=2, type='l')
lines(datapred1[,1],datapred1[,4], lty=1, col=2,type='l')


##############################################################
# Calculate the correlation coefficient between LOS and BEDS
cor(data_hosp$LOS, data_hosp$BEDS)

# Look at the R_squared. How does it compare to the correlation? Same value, but only for SLR.
cor(data_hosp$LOS, data_hosp$BEDS)^2




