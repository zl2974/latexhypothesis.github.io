
################################################################
#                     Biostatistical Methods I                 #
#            Statistical Inference: Two-Sample Means           #
#           Author: Cody Chiuzan; Date: Sept 23, 2019          #
################################################################

rm(list=ls())

###########################################################################
#   Conduct a two-sample paired t-test to assess the effect of a new diet #
###########################################################################

weight_before<-c(201,231,221,260,228,237,326,235,240,267,284,201)
weight_after<-c(200,236,216,233,224,216,296,195,207,247,210,209)
weight_diff<-weight_after-weight_before
sd_diff<-sd(weight_diff)


test_weight<-mean(weight_diff)/(sd_diff/sqrt(length(weight_diff)))

# Use the t.test() built-in function
# What alternative are you testing?
                          
t.test(weight_after, weight_before, paired=T, alternative="less")

# R output

# data:  weight_after and weight_before
# t = -3.0201, df = 11, p-value = 0.005827
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:  -Inf -8.174729
# sample estimates: mean of the differences -20.16667 

# Reject the null and conclude that the mean LDL levels are significantly lower after the diet.


###########################################################################################################
#   Conduct a two-sample independent t-test to assess the differences in BMD b/w the OC and non-OC groups #
###########################################################################################################

# Oral contraceptive example
# Testing equality of variances for two independent samples
# drawn from two underlying normal distributions.

# Sample 1: s1=0.16, n1=10, x1_bar=1.08
# Sample 2: s2=0.14, n2=10, x2_bar=1.00

F_test<-0.16^2/0.14^2

F_crit<-qf(.975, df1=9, df2=9) 

# Compare the F statistic (F_test) to the critical value
# Fcrit: F with 9 dfs in numerator and 9 dfs in denominator
# Because F_test < F_crit, we fail to reject and conclude
# that the pop. variances are not significantly different.

# Use two-sample t-test with equal variances.
std_pooled<-sqrt(((0.16^2*9)+(0.14^2*9))/18)

t_stats<-(1.08-1.00)/(std_pooled*sqrt((1/10)+(1/10)))

# Compare t_stats to the critical value: t with 18 df

qt(0.975,18) # 2.10

# t-stats=1.19 < 2.10, fail to reject the null and conclude that
# there is not a sig difference between the mean BMD levels of the two groups.

# 95% CI is your practice!


################################################################
#               Two-Sample independent t-test                  #
################################################################

# Effect of caffeine on muscle metabolism.
# 15 men were randomly selected to take a capsule containing pure caffeine one hour before the test.
# The other group of 20 men received a placebo capsule. 
# During each exercise the subject's respiratory exchange ratio (RER) was measured.
# The question of interest to the experimenter was whether, on average, caffeine consumption has an effect on RER.
# The two samples came from two underlying normal distributions: N(94.2,5.6), N(105.5,8.1) and are independent.


# Ideally, you should generate data using past info (here I made it up).

set.seed(6)
caff<-rnorm(15, 94.2, 5.6)
placebo<-rnorm(20, 105.5, 8.1)

# Test equality of variances: use R function var.test()

var.test(placebo, caff, alternative = "two.sided")

#F = 3.5768, num df = 19, denom df = 14, p-value = 0.01881             # Reject the null, evidence that variances are not equal.


res<-t.test(caff, placebo, var.equal=FALSE, paired=FALSE)               # var.equal=FALSE is the default, so no need to specifically write it.
res

# Look at the complete list of results.
names(res)

# t = -4.5834, df = 30.125, p-value = 7.472e-05                        # Reject the null and conclude that the means RER are sig diff b/w the two groups.

# 95% CI of the difference: (-17.639668,-6.766628)                     # We could safely say that the mean RER is sig. lower in the caffeine group (Why?)
