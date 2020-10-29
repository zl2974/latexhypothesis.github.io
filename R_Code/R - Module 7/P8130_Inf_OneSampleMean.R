
################################################################
#                    Biostatistical Methods I                  #
#            Statistical Inference: One-Sample Mean            #
#                     Author: Cody Chiuzan                     #
################################################################

rm(list=ls())


############################################################
#               Sample mean distributions: CLT             #
############################################################

# Draw 1000 samples of size 10 from an underlying exponential distribution with parameter lambda=0.3
# Calculate their means/var and draw a histogram to vizualize the sample means distribution

set.seed(2)
sample_means_exp1 = rep(NA, 1000)

for(i in 1:1000){
  sample_means_exp1[i] = mean(rexp(10,0.3))
}

# sample_means_exp

# Calculate the means and the variances of all samples
mean(sample_means_exp1)                # compare to true Mean = 1/lambda
var(sample_means_exp1)                 # compare to true Var=1/lambda^2

#Histogram
hist(sample_means_exp1, main = "Samples of Size N=10 from Exp(0.3)", xlab = "Sample Means", prob = T)
lines(density(sample_means_exp1), col = "darkblue", lwd = 2)

# Draw 1000 samples of size 50 from an underlying exponential distribution with parameter lambda=0.3
# Calculate their means/var and draw a histogram to vizualize the sample means distribution

set.seed(2)
sample_means_exp2 = rep(NA, 1000)

for(i in 1:1000){
  sample_means_exp2[i] = mean(rexp(50,0.3))
}

# Calculate the means and the variances of all samples 
mean(sample_means_exp2)                # compare to true Mean = 1/lambda
var(sample_means_exp2)                 # compare to true Var=1/lambda^2

#Histogram
hist(sample_means_exp2, main = "Samples of Size N=50 from Exp(0.3)", xlab = "Sample Means", prob = T)
lines(density(sample_means_exp2), col = "darkblue", lwd = 2)



# Construct a 95% CI for the population mean with n=10, X_bar=175, and known sigma=15
# Sigma represents the pooulation standard deviation
# 1-(alpha/2)=1-(0.05/2)=0.975

LCLz95<-175 - qnorm(0.975) * 15/sqrt(10)
UCLz95<-175 + qnorm(0.975) * 15/sqrt(10)
CLz95<-c(LCLz95, UCLz95)
CLz95

# What if we want a 99% CI?
LCLz99<-175-qnorm(0.995)* 15/sqrt(10)
UCLz99<-175 + qnorm(0.995) * 15/sqrt(10)
CLz99<-c(LCLz99, UCLz99)
CLz99


# Construct a 95% CI for the population mean with n=10 => df=10-1=9, X_bar=175, and known s=15
# s represents the sample standard deviation

LCLt95<-175 - qt(0.975, df=9) * 15/sqrt(10)
UCLt95<-175 + qt(0.975, df=9) * 15/sqrt(10)
CLt95<-c(LCLt95, UCLt95)
CLt95


# Construct a 95% CI for the population variance with known s=15
# s represents the sample standard deviation

LCL_var95 <- 9*(15^2)/qchisq(0.975, 9)
UCL_var95 <- 9*(15^2)/qchisq(0.025, 9)
CL_var95<-c(LCL_var95, UCL_var95)
CL_var95


# Hypothesis Test: Infarct size example
# Test if the mean infract size is different from 25
# X_bar=16, s=10, N=40

t_stats<-(16-25)/(10/sqrt(40))
t_stats

# Compare the test statistics with the critical value, alpha=0.05

qt(0.975,39) # 2.02

# Compute the p-value: t_stats<0, so the p-value is twice area to the left of a t distr. with 39 df

p.val<-2*pt(t_stats,39) # p.val<.0001, reject H0.


# Remember the low_birth data

low_birth_all <- read.csv("lowbwt_ALL.csv")

# Let's test if the true mean is different than 3000g
# One-sample t-test, two-tailed

t.test(low_birth_all$bwt, alternative='two.sided', mu=3000)


# Output from R

# One Sample t-test

# t = -1.0437, df = 188, p-value = 0.298 ----> Fail to reject H0.
# alternative hypothesis: true mean is not equal to 3000
# 95 percent confidence interval: 2840.049 3049.264
# sample estimates: mean of x is 2944.656 


# Let's test if the true mean is less than 3000g
# One-sample t-test, one-tailed

t.test(low_birth_all$bwt, alternative='less', mu=3000)

# Output from R
# t = -1.0437, df = 188, p-value = 0.149 ----> Fail to reject H0.
# alternative hypothesis: true mean is less than 3000
# 95 percent confidence interval: -Inf 3032.312  (One-sided confidence interval)
# sample estimates: mean of x is 2944.656 


