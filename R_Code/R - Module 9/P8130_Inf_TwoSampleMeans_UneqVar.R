
################################################################
#                    Biostatistical Methods I                  #
#            Statistical Inference: Two-Sample Means           #
#                     Author: Cody Chiuzan                     #
################################################################

rm(list=ls())

# Two-Sample Independent t-Test: Unequal Variances

# Eye Example: Dominant and Recessive Genotypes

# Dominant: X_bar=0.91, s=0.15, n=66
# Recessive: X_bar=0.34, s=0.23, n=34

# Test the equality of variances
F_stats<-0.23^2/0.15^2
F_stats

F_crit<-qf(.975, df1=33, df2=65) 
F_crit   # 1.770

# Compare the F statistic (F_test) to the critical value
# Because F_test > F_crit, we reject H0 and conclude
# that the pop. variances are significantly different.

# Use two-sample t-test with unequal variances

# Calculate the pooled degrees of freedom

d1<-((0.23^2/34)+(0.15^2/66))^2/((0.23^2/34)^2/33)+((0.15^2/66)^2/65)
d1

# Round down d1 to the nearest integer: d2=49

t_stats<-(0.34-0.91)/sqrt((0.23^2/34)+(0.15^2/66))
t_stats

# Compare t_stats to the critical value: t with d2 df

t_crit <- qt(0.975,49) 
t_crit    # 2.010

# |t-stats|> 2.10, reject the null and conclude that
# there is a sig difference between the mean levels the two groups.

# Compute the p-value: t_stats<0, so the p-value is twice area to the left of a t distr. with 49 df

p.val<-2*pt(t_stats,49) # p.val<.0001, reject H0.
p.val
















# Test for equality of the means:
F_stats <- 
t_stats<-(16-25)/(10/sqrt(40))
t_stats

# Compare the test statistics with the critical value, alpha=0.05

qt(0.975,39) # 2.26

# Compute the p-value: t_stats<0, so the p-value is twice area to the left of a t distr. with 39 df

p.val<-2*pt(t_stats,39) # p.val<.0001, reject H0.