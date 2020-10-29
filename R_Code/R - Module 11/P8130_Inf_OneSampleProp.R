################################################################
#                   Biostatistical Methods I                   #
#               Inferences for One-Sample Proportions          #
#                      Author: Cody Chiuzan                    #
################################################################

rm(list=ls())


# Normal Approximation: Observe the shape of different Binomial distributions with varying n and p.

#1
par(mfrow = c(1, 2)) 
plot(0:50, dbinom(0:50,10,0.5),type='h', ylim=c(0,0.50),
     xlab='X', main ='Bin(10,0.5)', ylab='P(X)', lwd=3, cex.lab=1.5, cex.axis=2, cex.main=2) 

plot(0:50, dbinom(0:50,30,0.5),type='h', ylim=c(0,0.50),
     xlab='X', main ='Bin(30,0.5)', ylab='P(X)', lwd=3, cex.lab=1.5, cex.axis=2, cex.main=2) 

#2
par(mfrow = c(1, 2)) 
plot(0:50, dbinom(0:50,10,0.10),type='h', ylim=c(0,0.50),
     xlab='X', main ='Bin(10,0.1)', ylab='P(X)', lwd=3, cex.lab=1.5, cex.axis=2, cex.main=2) 

plot(0:50, dbinom(0:50,50,0.10),type='h', ylim=c(0,0.50),
     xlab='X', main ='Bin(50,0.1)', ylab='P(X)', lwd=3, cex.lab=1.5, cex.axis=2, cex.main=2) 

#3
par(mfrow = c(1, 2)) 
plot(0:50, dbinom(0:50,10,0.02),type='h', ylim=c(0,0.8),
     xlab='X', main ='Bin(10,0.02)', ylab='P(X)', lwd=3, cex.lab=1.5, cex.axis=2, cex.main=2) 

plot(0:50, dbinom(0:50,50,0.02),type='h', ylim=c(0,0.8),
     xlab='X', main ='Bin(50,0.02)', ylab='P(X)', lwd=3, cex.lab=1.5, cex.axis=2, cex.main=2) 

#4
par(mfrow = c(1, 2)) 
plot(0:50, dbinom(0:50,10,0.95),type='h', ylim=c(0,0.8),
     xlab='X', main ='Bin(10,0.95)', ylab='P(X)', lwd=3, cex.lab=1.5, cex.axis=2, cex.main=2) 

plot(0:50, dbinom(0:50,50,0.95),type='h', ylim=c(0,0.8),
     xlab='X', main ='Bin(50,0.95)', ylab='P(X)', lwd=3, cex.lab=1.5, cex.axis=2, cex.main=2) 

################################################################
# In a survey of 300 randomly selected drivers, 125 claimed that
# they regularly wear seat belts. Can we conclude from these data 
# that the population proportion who regularly wear seat belts is 0.50?
# Perform a hypothesis test and 
# Construct a 95% confidence interval for the true population proportion. 
################################################################

# p_hat=125/300
# p0=0.50


# Prop.test performs a chi-squared test and not a z-test.
prop.test(125,300, p=0.5)


# Create your own function to perform a one-sample proportion test 
# and create a 100(1-alpha) CI using the Normal Approximation

one.proptest_norm <- function(x, n, p=NULL, conf.level=0.95, alternative="less") {
# x the number of 'cases' in the sample
# n the total sample size
# p is the hypothesized value
  
  z.stat <- NULL
  cint <- NULL
  p.val <- NULL
  phat <- x/n
  qhat <- 1 - phat
 
  if(length(p) > 0) { 
    q <- 1-p
    SE.phat <- sqrt((p*q)/n) 
    z.stat <- (phat - p)/SE.phat
     
     p.val <- pnorm(z.stat)
    
    if(alternative=="two.sided") {
       p.val <- p.val * 2}
    
    if(alternative=="greater") {
      p.val <- 1 - p.val
    }
  } else {
    
# Construct a confidence interval   
    SE.phat <- sqrt((phat*qhat)/n)
  }
  cint <- phat + c(-1*((qnorm(((1 - conf.level)/2) + conf.level))*SE.phat),
                      ((qnorm(((1 - conf.level)/2) + conf.level))*SE.phat))
  
  return(list(estimate=phat, z.stat=z.stat, p.val=p.val, cint=cint))
}

# In our example: 

one.proptest_norm(125,300, 0.5, alternative="two.sided") 

# P-hat estimate
#0.417

# Z-statistic: z.stat
# -2.886

#$p.val
#0.004

# 95 % CI: (0.360, 0.473)


####################################################################
#       Perform an Exact test, no normal approximation             #
#          This function uses Clopper-Pearson method               #
####################################################################

binom.test(125, 300, p = 0.5, alternative = "two.sided", conf.level = 0.95)

# 95% Exact CI: (0.360, 0.474)

# Exact p-value: 0.004








