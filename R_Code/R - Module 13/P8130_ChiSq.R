#################################################################
#                      Biostatistical Methods I                 #
#          Contingency Tables: Tests for Categorical Data       #
#                          Author: Cody Chiuzan                 #
#################################################################

rm(list=ls())

################################################################
#                      Chi-Squared Test                        #
################################################################

# Marijuana usage among colleg students
# Chi-squared test for homogeneity

drug_data<-matrix(c(57,50,43,57,58,20,56,45,24,45,22,33), nrow=4,ncol=3,byrow=T,
                  dimnames=list(c("freshman","sophomore","junior","senior"), 
                                c("experimental","casual","modheavy")))

drug_data

chisq.test(drug_data)

# X-squared = 19.369, df = 6, p-value = 0.003584
# Critical value: qchisq(0.95,6) = 12.59

# We reject the null hypothesis and conclude that the proportions of marijuana usage are different among classes


###
# Association b/w pelvic inflammatory disease and ectopic pregnancy
# Chi-squared test for independence

preg_data<-matrix(c(28,6,251,273), nrow=2, ncol=2, byrow=T,
                  dimnames=list(c("PID","No PID"), 
                                c("Ect Preg", "No Ect Preg")))

                  
preg_data                  
                  
chisq.test(preg_data)  
# Get the expected values: chisq.test(preg_data)$expected        

                  
# X-squared with Yates' correction
# X-squared=13.81, df=1, p-value=0.0002
# Critical value: qchisq(0.95,1) = 3.84
                  
# We reject the null and conclude that there is sufficient evidence that PID and ectopic pregnancy are associated.


###
# What if we have raw data? How do we perform a chi-squared test?
# Use R data 'quine' (library MASS) to compare gender distribution between ethnicities.

library(MASS)
data(quine)
names(quine)

# Create a 2x2 table
table(quine$Sex, quine$Eth)

# Compute row percentages
prop.table(table(quine$Sex, quine$Eth), 1)

# Chi-squared without continuity correction.
chisq.test(table(quine$Sex, quine$Eth), correct=F)

# X-squared = 0.004, df = 1, p-value = 0.949. Not a significant difference.

# Chi-squared with continuity correction.
chisq.test(table(quine$Sex, quine$Eth), correct=T)

# X-squared ~ 0, df = 1, p-value = 1. Not a significant difference.




