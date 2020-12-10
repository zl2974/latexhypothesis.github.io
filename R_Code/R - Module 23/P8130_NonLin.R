
################################################################
#                 Biostatistical Methods I                     #
#                   Non-linear Models                          #
################################################################


rm(list = ls())

# Load libraries
install.packages(c('dplyr','tidyr','broom','ggplot2','splines'))

library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(splines)


# Generate non-linear data: order 4 polynomial
set.seed(1)

data.nonlin <- data.frame(x = runif(100, 0, 1)) %>%
              mutate(y = -30*(x-.5)^2 + 100*(x-.5)^4 + rnorm(100, 0, .3))

# Data plot
ggplot(data.nonlin, aes(y=y, x=x)) + geom_point() + theme_bw()

# Polynomial regression
data.nonlin <- mutate(data.nonlin, 
                     x.pow2 = x^2, x.pow3 = x^3, x.pow4 = x^4)

# Fit data with quartic (max order: 4) polynomial
quartfit <- lm(y ~ x + x.pow2 + x.pow3 + x.pow4, data = data.nonlin)
summary(quartfit)


# Another way to fit the model (same results)
quartfit_alt <- lm(y ~ x + I(x^2)+I(x^3)+I(x^4), data = data.nonlin)
summary(quartfit_alt)

# What if we use function poly()?
# This creates orthogonal (not correlated polynomials), i.e., the columns of the X matrix are orthogonal. 
# This does not change the fitted values but allows you to see whether a certain order in the polynomial 
#  significantly improves the regression over the lower orders.
# For example X^4 captures only the quartic part not captured by the cubic term.

quartfit_ortho <- lm(y ~ poly(x, 4), data = data.nonlin)
summary(quartfit_ortho)

# Use option raw=T to get original resuls.
# quartfit_ortho1 = lm(y ~ poly(x, 4, raw=T), data = data.nonlin)
# summary(quartfit_ortho1)

# Plot with overlaid quartic fitted line

mutate(data.nonlin, fitted = fitted(quartfit)) %>%
  ggplot(., aes(y=y, x=x)) + geom_point() + 
  geom_line(aes(y = fitted), color = "red") + theme_bw()

# Plot with overlaid quadratic fitted line
quadfit = lm(y ~ x + x.pow2, data = data.nonlin)
mutate(data.nonlin, fitted = fitted(quadfit)) %>%
  ggplot(., aes(y=y, x=x)) + geom_point() + 
  geom_line(aes(y = fitted), color = "red") + theme_bw()

#################################################################

# Piece-wise linear regression: 3 knots at 0.2, 0.5, 0.8
# Create indicator variables for values less and greater than 0.2, 0.5, 0.8
data.nonlin = mutate(data.nonlin, 
                     knot_2 = (x - .2) * (x >= .2),
                     knot_5 = (x - .5) * (x >= .5),
                     knot_8 = (x - .8) * (x >= .8))

piecewise.fit <- lm(y ~ x + knot_2 + knot_5 + knot_8, data = data.nonlin)
summary(piecewise.fit)

mutate(data.nonlin, fitted = fitted(piecewise.fit)) %>%
  ggplot(., aes(y=y, x=x)) + geom_point() + 
  geom_line(aes(y = fitted), color = "red", lwd=1.5) + theme_bw()


# Piece-wise linear regression: 1 knot at Nurse=250

# Read data 'Hospitals.csv'
data_hosp<-read.csv("Hospital.csv")
names(data_hosp)

# Create an indicator variable for values less and greater than 250 (knot)

data_hosp <- mutate(data_hosp, NURSEstar = (NURSE - 250) * (NURSE >= 250))
  
reg_hosp_piece<-lm(LOS ~ NURSE + NURSEstar, data=data_hosp)
summary(reg_hosp_piece)

mutate(data_hosp, fitted = fitted(reg_hosp_piece)) %>%
  ggplot(., aes(y=LOS, x=NURSE)) + geom_point() + 
  geom_line(aes(y = fitted), color = "red", lwd=1.5) + theme_bw()


# Add a smoother
# Default is weighted least squares
# f is a smoothing parameter (proportion), larger values give more smoothness

smoother <- lowess(data_hosp$NURSE, data_hosp$LOS, f=0.2)            # Try different f values 
plot(data_hosp$NURSE, data_hosp$LOS, pch=16,
     xlab="Nurse", ylab="LOS", main="Scatter Plot with Smoother")
lines(smoother, lwd=2, col=2)









