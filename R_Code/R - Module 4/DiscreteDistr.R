
################################################################
#                   Biostatistical Methods I                   #
#   Discrete probability distributions: Binomial and Poisson   #
#                     Author: Cody Chiuzan                     #
################################################################


# Probability Distribution Chart

# Probabilities that 0, 1, 2, or all 3 patients will respond to medication

x<-c(0.01,0.08, 0.27, 0.64)
barplot(x, col="blue", main="Probability Distribution", xlab="Number of patients with low LDL", ylab="Probability", names.arg=c(0,1,2,3),ylim=c(0,1))

######################################
#       Binomial Distribution        #
######################################

# Calculate binomial probabilites
# n=20 households, p=0.06 prob. of developing asthma
# Calculate prob of having exaclty 5 infants with asthma
dbinom(5, 20, 0.06)

# Try n=100
dbinom(5, 100, 0.06)


# Calculate prob of having at least 10 infants with asthma: P(X >= 10)= 1-P(X<=9)
# pbinom() gives the cumulative probabilities: P(X<=9)
1-pbinom(9, 20, 0.06)


# Graph binomial distribution
# Create your own function with parameters/arguments: n and p

bin_graph<-function(n,p){
  x<-dbinom(0:n, size=n, prob=p)
  barplot(x, names.arg=0:n, main=sprintf(paste('bin.dist.funct.',n,p,sep=':')))
}

# Call the function for n=20 and p=0.06
bin_graph(20,0.06)

######################################
#       Poisson Distribution        #
######################################

# Calculate Poisson probabilities

# Lambda = 10 calls/hour 
# Calculate prob of having exactly 4 calls in the next hour: P(X=4)
dpois(4, 10)

# Calculate prob of receving mostly 8 calls in one hour: P(X<=8)
# ppois() gives the cumulative probabilities

ppois(8,10)


# Observe the shape of different Poission distributions with varying lambdas
par(mfrow = c(1, 3)) 

# Parameter lambda=3
plot(0:20, dpois(0:20,3),type='h', ylim=c(0,0.25),
     xlab='X', main ='Poi(3)', ylab='P(X)', lwd=3, cex.lab=1.5, cex.axis=2, cex.main=2) 

# Parameter lambda=5
plot(0:20, dpois(0:20,5),type='h', ylim=c(0,0.25),
     xlab='X', main ='Poi(5)', ylab='P(X)', lwd=3, cex.lab=1.5, cex.axis=2, cex.main=2) 

# Parameter lambda=10
plot(0:20, dpois(0:20,10),type='h',ylim=c(0,0.25),
     xlab='X', main ='Poi(10)', ylab='P(X)', lwd=3, cex.lab=1.5, cex.axis=2, cex.main=2) 

# Exercise
# A rare birth defect occurs with probability 0.0001. 
# Assuming that 4,000 babies are born at a large hospital within a year, calculate the probability of having at least 10 babies with a birth defect.
# Compute this probability using both Poisson and Binomial formulae and comment on the results.

# Binomial
1-pbinom(4000, 9, 0.0001)

# Poisson
1-ppois(9,0.4)

# Both probabilites are approximately 0.
