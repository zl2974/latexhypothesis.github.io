################################################################
#                   Biostatistical Methods I                   #
#               Inferences for Two-Sample Proportions          #
#                      Author: Cody Chiuzan                    #
################################################################

rm(list=ls())

# Prob mass functions and check of normality approximation
# Example Air Pollution (see slides)
# n1=n2=200; X1=43; X2=81 (Yes - bothered by air pollution)

par(mfrow = c(1, 2)) 
plot(0:200, dbinom(0:200,200,0.215),type='h', ylim=c(0,0.20),
     xlab='X', main ='Bin(200,0.215)', ylab='P(X)', lwd=3, cex.lab=1.5, cex.axis=2, cex.main=2) 

plot(0:200, dbinom(0:200,200,0.405),type='h', ylim=c(0,0.20),
     xlab='X', main ='Bin(200,0.405)', ylab='P(X)', lwd=3, cex.lab=1.5, cex.axis=2, cex.main=2) 



# Z-test for two proportions (Normal Approximation)
# No continuity correction

twosample_prop <- prop.test(c(43, 81), n = c(200, 200), correct = F)
twosample_prop

# Results give the chi-squared test statistic, but you can extract the z-test statistic

sqrt(twosample_prop$statistic)

# Z-test for two proportions (Normal Approximation)
# Continuity correction

twosample_prop1 <- prop.test(c(43, 81), n = c(200, 200), correct = T)
twosample_prop1

# Results give the chi-squared test statistic, but you can extract the z-test statistic

sqrt(twosample_prop1$statistic)

# Results with and without continuity correction are very similar with the same decision of rej H0