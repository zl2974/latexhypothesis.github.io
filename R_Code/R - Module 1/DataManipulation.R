
################################################################
#                     Biostatistical Methods I                 #
#            Quick R Tutorial on Data Manipulation             #
#                     Author: Cody Chiuzan                     #
################################################################


#################################################################
#                                                               #
#                 Data Manipulation in R                        #
#                                                               #
#################################################################


# Clean all objects from the current workspace (R memory) 
rm(list=ls())


# Check the list of existing R datasets from package 'datasets
data()


# Load R data 'esoph'
data(esoph, package="datasets")


#########################################################################
#                          Import Data                                  #
#########################################################################

# Read CSV files: Low and Normal Birthweights
# You need to change the path to the floder where you saved the data sets

low_birth<-read.csv("O:\\Projects\\BERD_EDU\\MiniCourses\\Rcourse\\lowbwt_Low.csv")
norm_birth<-read.csv("O:\\Projects\\BERD_EDU\\MiniCourses\\Rcourse\\lowbwt_Normal.csv")


#########################################################################
#                 Examine Data Attributes                               #
#########################################################################


# Variable names
names(low_birth)    

# Data dimension: rows x columns; here: 59 rows and 3 columns
dim(low_birth)

# Number of rows
nrow(low_birth)
ncol(low_birth)

# Head and Tail observations
head(low_birth)
tail(low_birth)

# Check for missing values
anyNA(low_birth)

# Examine the classes of each column
str(low_birth)

# Tabulate variable smoke
table(low_birth$smoke)


#########################################################################
#               Data Manipulation: library 'dplyr'                      #
#########################################################################

# Install and load library "dyplyr"
install.packages("dplyr")
library(dplyr)

# Select only column/variable age
select(low_birth, age)

# Select only rows 1:5 of the data
slice(low_birth, 1:5)              # Not the best function, but it works

# Keep only rows where 'age' is less than 20 
filter(low_birth, age < 20)

# Select rows that contain missing data
filter(low_birth, is.na(age))
# In this data there are no NAs

# Remove column age 
dplyr::select(low_birth, -age)

# Filter rows: select all 25+ yrs old, smokers
filter(low_birth, age > 25 & smoke=="1") 


# Ordering data by variable/column 'id'
low_birth <- arrange(low_birth, id)
head(low_birth)

# Arrange in descending order
low_birth <- arrange(low_birth, desc(id))
head(low_birth)

# Order by multiple columns/variables
low_birth <- dplyr::arrange(low_birth, smoke, desc(age))
head(low_birth)


#Rename variable 'smoke' to 'Smoking_Status' and 
# save this in a new data frame: low_birth_temp

low_birth_temp <- rename(low_birth, Smoking_Status = smoke)
head(low_birth_temp)

# Take the log of 'age'
low_birth_temp <- mutate(low_birth, log_age = log(age))
head(low_birth_temp)


# Centering the data by subtracting the mean from variable 'age'
low_birth_temp <- mutate(low_birth, center_age = age - mean(age, na.rm=TRUE))
head(low_birth_temp)

# Use IF-ELSE function to create new age categories
# Cat 1: Age < 25; Cat 2: 25 < Age < 30. Cat 3: Age > 30
low_birth_temp$new_age <- ifelse(low_birth$age < 25 , 1,
                            ifelse((low_birth$age >=25 & low_birth$age < 30), 2, 3))

tail(low_birth_temp)                                                    
                    
#########################################################################
#               Combine and Merge Different Data Sets                   #
#########################################################################      

# Combine by row

combo_row <- rbind(low_birth, norm_birth)

dim(low_birth)
dim(norm_birth)
dim(combo_row)

# Can also combine by column using function cbind()

#########################################################################
#                  Merging data sets: Application                       #
#########################################################################

# A study was conducted to identify risk factors for low infant birth weight using data from 189 live births
# at Bay State Medical Center in Massachusetts. Low birthweight was defined as a <2500grams.

# Combine the low birthweight-babies (lowbwt_LOW.csv) with the normal birthweight babies (lowbwt_Normal.csv).
# This is the combo_row data from above

# Merge the combo_row data with data on # of visits (lowbwt_ADMIN.csv).
# id  = ID number of infant
# visits = number of physician visits during 1st trimester = 0 if none; 1 if one; 2 if two or more


admin_birth<-read.csv("O:\\Projects\\BERD_EDU\\MiniCourses\\Rcourse\\lowbwt_Admin.csv")

birth_final <- merge(combo_row,admin_birth, by="id")

names(birth_final)
head(birth_final)


# Export merged data to a CSV file called "BWT_stats.csv"

write.csv(birth_final, file="O://Projects//BERD_EDU//MiniCourses//Rcourse//BWT_Stats.csv")

