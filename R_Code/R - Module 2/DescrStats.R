
################################################################
#                  Biostatistical Methods I                    #
#                   Descriptive Statistics                     #
#                    Author: Cody Chiuzan                      #
################################################################



# Clean all objects from the current workspace (R memory) 
rm(list=ls())


# Library 'arsenal' is used for descriptive statistics tables
# Library 'dplyr' has nice functions for data manipulation, also mutate()
# Library 'ggplot2' is used for graphing

install.packages(c('arsenal', 'dplyr','ggplot2'))

library(arsenal)
library(dplyr)
library(ggplot2)

#########################################################################
#                          Import Data                                  #
#########################################################################

# Set working directory

setwd("~/Desktop/DescriptiveStats")
low_birth_all <- read.csv("lowbwt_ALL.csv")

names(low_birth_all)
head(low_birth_all)

dim(low_birth_all)
summary(low_birth_all)
str(low_birth_all)

# Check for missing values
anyNA(low_birth_all)

filter(low_birth_all, is.na(age))

# Some details about the data: 189 births info were collected at a medical center.
# The dataset contains the following 10 variables:
# low: indicator of birth weight less than 2.5kg
# age: mother's age in years
# lwt: mother's weight in pounds at last menstrual period
# race: mothers race ("white", "black", "other")
# smoke: smoking status during pregnancy (yes/no)
# ht: history of hypertension (yes/no)
# ui: presence of uterine irritability (yes/no)
# ftv: physician visit during the first trimester (yes/no)
# ptl: previous premature labor (yes/no)
# bwt: birth weight in grams


#########################################################################
#         Descriptive Statistics: Continuous Variables                  #
#########################################################################


mean(low_birth_all$age)                               # Mean
median(low_birth_all$age)                             # Median
sd(low_birth_all$age)                                 # Standard Deviation
quantile(low_birth_all$age)                           # Min, 25ht, 50th, 75th, Max
quantile(low_birth_all$age, c(0.10,0.30,0.60))        # Tertiles

# A more condensed way to obtain summary statistics
summary(low_birth_all$age)

# Summary statistics for each level of another categorical variable
mean<-tapply(low_birth_all$bwt, low_birth_all$race, mean)
sd <- tapply(low_birth_all$bwt, low_birth_all$race, sd)
med <-tapply(low_birth_all$bwt, low_birth_all$race, median)
min <-tapply(low_birth_all$bwt, low_birth_all$race, min)
max <-tapply(low_birth_all$bwt, low_birth_all$race, max)
cbind(mean, sd, med, min, max)

# Use function tableby() from library 'arsenal' to create a summary table (called Table 1 in publications)
# Use continuous and categorical variables


# First table - not ideal
tab1 <- tableby( ~ age + bwt + smoke, data=low_birth_all)
summary(tab1)


# Change variable names/labels
my_labels <- list(age = "Age(yrs)", bwt = "Birthweight(g)", smoke="Smoker", race="Race")

# Clean the output
my_controls <- tableby.control(
               total = T,
               test=F,  # No test p-values yet
               numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),
               cat.stats = c("countpct", "Nmiss2"),
               stats.labels = list(
               meansd = "Mean (SD)",
               medianq1q3 = "Median (Q1, Q3)",
               range = "Min - Max",
               Nmiss2 = "Missing",
               countpct = "N (%)"))

# Make 'smoke' a factor to show N (%)
birth_df<-low_birth_all %>% 
          mutate(smoke=factor(smoke, labels=c("No","Yes"))) # Start labeling with 0 (increasing order)

# Second table
tab2 <- tableby( ~ age + bwt + smoke, data=birth_df, control=my_controls)
summary(tab2, title = "Descriptive Statistics: Lowbirth Data", labelTranslations = my_labels, text=T)

# Tabulation by race categories
tab3 <- tableby( race ~ age + bwt + smoke, data=birth_df, control=my_controls)
summary(tab3, title = "Descriptive Statistics: Lowbirth Data", labelTranslations = my_labels, text=T)



#########################################################################
#         Descriptive Statistics: Categorical Variables                 #
#########################################################################

tbl <- table(low_birth_all$smoke, low_birth_all$race)	        # Two-way table
tbl
prop.table(tbl, 1)                        		                # Row proportions
prop.table(tbl, 2)                                            # Column proportions

# 3-way cross-tabulation
xtabs(~race+smoke+ht, data=low_birth_all)


#########################################################################
#                        Data Vizualization                             #
#########################################################################


#######################
#       Histogram     #
#######################

# Arguments used for ggplot()
# data: dataset used to plot the graph 
# mapping (aes) Control the x and y-axis 
# geometric object: The type of plot you want to show. The most common objects are:
# Point: geom_point()
# Bar: geom_bar()
# Line: geom_line()
# Histogram: geom_histogram()



# Choose the number of bars/breaks to use for the histogram
# First look at the min, max of birthweight to make sure you capture all values

min(low_birth_all$bwt)
# 709
max(low_birth_all$bwt)
# 4990


ggplot(low_birth_all, aes(bwt)) +
  geom_histogram(col="black", fill="pink", breaks=seq(500,5000,300)) +                      # Split the range 500-5000 by 200 (less bins than the default above)
  geom_vline(aes(xintercept=mean(bwt, na.rm=T)), color="red", linetype="dashed", size=2) +  # Add a vertical line for mean bwt, ignore NA values for mean
  labs(title="Histogram of low birthweight") +                                              # Re-name the title
  labs(x="Birthweight(g)", y="Count")                                                       # Re-name the axes

# Histogram with density instead of counts on y-axis
ggplot(low_birth_all, aes(bwt)) +
  geom_histogram(col="black", fill="pink", breaks=seq(500,5000,300), aes(y=..density..)) +                      # Split the range 500-5000 by 200 (less bins than the default above)
  geom_vline(aes(xintercept=mean(bwt, na.rm=T)), color="red", linetype="dashed", size=2) +  # Add a vertical line for mean bwt, ignore NA values for mean
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Histogram of low birthweight") +                                              # Re-name the title
  labs(x="Birthweight(g)", y="Density")  

# Overlaid histograms
# Histogram of birthweight 'bwt' by smoking status 'smoke'

ggplot(low_birth_all, aes(x=bwt, fill=factor(smoke))) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity", col="black", breaks=seq(500,5000,300)) +
  geom_vline(aes(xintercept=mean(bwt, na.rm=T)), color="red", size=1.5) +                   # Add a vertical line for mean bwt, ignore NA values for mean
  labs(title="Histogram of low birthweight") +                                             
  labs(x="Birthweight(g)", y="Count") + 
  scale_fill_discrete(name = "Smoking Status", labels = c("No", "Yes"))                      # Edit legend title and labels


#####################            
#      Boxplot      #
#####################

# Create only one boxplot

ggplot(low_birth_all, aes(x=bwt, y=bwt)) +
  geom_boxplot(col="black", fill="pink",
               outlier.colour="black", outlier.shape=16,                               # Enhance the outliers
               outlier.size=2, notch=FALSE) +
  labs(title="Boxplot low birthweight") +                                              # Re-name the title
  labs(x="Birthweight(g)", y="Count")                                                  # Re-name the axes


# A grouped boxplot

ggplot(low_birth_all, aes(x = race, y = bwt, fill = factor(smoke))) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  scale_fill_manual(values = c("blue", "red"),                                        # Change colors
                    labels = expression("Non-Smokers", "Smokers")) +                  # Edit legend labels       
  xlab("") +
  ylab("Birthweight (g)") +
  theme(legend.position = "top", legend.title = element_blank())


