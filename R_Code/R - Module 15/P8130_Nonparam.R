
#################################################################################
#                           Biostatistical Methods I                            #
#                             Non-parametric Tests                              #
#                             Author: Cody Chiuzan                              #
#################################################################################

rm(list=ls())


### Checking normality
# Example of marine biomass ratio taken from 32 marine organisms.
# Biomass ratio is used to test the protection effect of biological reserves used to conserve marine species.
# It is the total mass of all marine plants and animals per unit area of reserve divided by the same quantity in the unprotected control. 
# If the biomass ratio equals one, than the reserve has no protection had no effect.

bio_ratio <-c(1.34, 1.96, 2.49, 1.27, 1.19, 1.15, 1.29, 1.05, 1.10, 1.21, 1.31,
              1.26, 1.38, 1.49, 1.84, 1.84, 3.06, 2.65, 4.25, 3.35, 2.55, 1.72,
              1.52, 1.49, 1.67, 1.78, 1.71, 1.88, 0.83, 1.16, 1.31, 1.40)


# Histogram - notice the severe right skew
hist(bio_ratio, xlab="Biomass Ratio", freq=T, col=2)

# Create a quantile-quantile plot (QQplot)
qqnorm(bio_ratio, col=2, pch=19, cex=1.5)

# Add a straight line which passes through the first and third quartiles.
qqline(bio_ratio, col = 1,lwd=2,lty=2)

# The QQplot also indicates severe departues from normality

# Perform Shapiro-Wilk test
shapiro.test(bio_ratio)
# W test statistic: 0.81751, P<0.0001, reject the null, again evidence towards non-normality


##################################################################################
#                         Non-parametric Sign test                               #
##################################################################################
# Uses the signs of the differences, but not the magnitudes            
# H0: the median of the diff distribution is zero 

install.packages("BSDA")
library(BSDA)

# Testing the effect of linoleic acid supplementation on systolic blood pressure. 
# A group of 17 adults with a diet high in linoleic acid were assessed at baseline and 4 weeks later.

base_SBP<-c(119.67, 100, 123.56, 109.89, 96.22, 133.33, 115.78, 126.39, 122.78, 117.44, 
            111.33, 117.33, 120.67, 131.67, 92.39, 134.44, 108.67)
post_SBP<-c(117.33, 98.78, 123.83, 107.67, 95.67, 128.89, 113.22, 121.56, 126.33, 110.39,
            107, 108.44, 117, 126.89, 93.06, 126.67, 108.67)



SIGN.test(base_SBP, post_SBP, md=0)

# Test statistic: s=13, p-value=0.02127
# Reject H0; true median difference is not equal to 0

###################################################################################
#                 Non-parametric Wilcoxon-Signed Rank test                        #
###################################################################################
# Uses the magnitudes of the differences            
# Gives exact p-values for <50 samples, no ties and no zeros
# Otherwise, gives the normal approximation

# Calculate the differences
diff_SBP<-base_SBP-post_SBP

hist(diff_SBP)

shapiro.test(diff_SBP)

qqnorm(diff_SBP, col=2, pch=19, cex=1.5)
qqline(diff_SBP, col = 1,lwd=2,lty=2)


wilcox.test(diff_SBP)
#OR
wilcox.test(base_SBP,post_SBP, paired=T)         # default is paired=F=> Wilcoxon-Rank Sum


# Wilcoxon signed rank test with continuity correction
# Test statistics: V = 124, p-value = 0.004107
# The test statistic T+ is 124, exactly what we got in the slides (sum of the ranks for positive values)
# Reject the null hypothesis, there is a sigificant difference in SBP values at baseline vs follow-up.

# t.test(base_SBP,post_SBP, paired=T)


######################################################################################
#             Non-parametric Wilcoxon-Rank Sum test: two-independent groups          #
######################################################################################

# Comparing the length of hospital stay for patients with the same diagnosis admitted at two different hospitals.

hosp1<-c(21,10,32,60,8,44,29,5,13,26,33)
hosp2<-c(86,27,10,68,87,76,125,60,35,73,96,44,238)


par(mfrow=c(2,2))
hist(hosp1, xlab="Length of stay (days)", freq=T, main="Hospital1")
hist(hosp2, xlab="Length of stay (days)", freq=T, main="Hospital2")
qqnorm(hosp1, col=2, pch=19, cex=1.5)
qqline(hosp1, col = 1,lwd=2,lty=2)

qqnorm(hosp2, col=2, pch=19, cex=1.5)
qqline(hosp2, col = 1,lwd=2,lty=2)

 
wilcox.test(hosp1, hosp2, mu=0)

# Notice that the test statistic T1 is different from what we got in the slides (83.5)
# Test statistic: W = 17.5, p-value = 0.001925

# Why is that?
# For Wilcoxon Rank Sum test only, R is calculating the statistic without n1(n1+1)/2 term. 
# There is a note in the function documentation: 
# "The literature is not unanimous about the definitions of the Wilcoxon rank sum and Mann-Whitney tests. 
# The two most common definitions correspond to the sum of the ranks of the first sample with the minimum value subtracted or not: 
# R subtracts and S-PLUS does not, giving a value which is larger by m(m+1)/2 for a first sample of size m."

# If we want to get the same value of T1 per our formula in the slides, we need to add the n1(n1+1)/2 term. 

res<-wilcox.test(hosp1, hosp2, mu=0)

res$statistic <- res$statistic + 11*(11+1)/2

res            # W=83.5 - yeay! 

# Same p-value as before, so we can use the original command.



# t.test(hosp1, hosp2, mu=0)




