#############################HR Analytics Case study#############################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
#################################################################################

######################### Business Understanding: START ######################### 

# A large company named XYZ, employs around 4000 employees. 
# However, every year, around 15% of its employees leave the company and need to be replaced.
# The management believes that this level of attrition (employees leaving, either on their own 
# or because they got fired) is bad for the company, because of the following reasons -
# The former employees' projects get delayed, which makes it difficult to meet timelines, resulting in a reputation loss among consumers and partners
# A sizeable department has to be maintained, for the purposes of recruiting new talent
# More often than not, the new employees have to be trained for the job and/or given time to acclimatise themselves to the company

# They want to know what changes they should make to their workplace, in order to get most of their employees to stay. 
# Also, they want to know which of these variables is most important and needs to be addressed right away.

# AIM 
# model the probability of attrition using a logistic regression. 

#Available Data
# 1. General Data
# 2. Employee Survey data
# 3. Manager Survey data
# 4. Employee In time
# 5. Employee Out Time

########################## Business Understanding: END ########################## 

#################################################################################



########################### Data Understanding: START ###########################

# Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("ggplot2")
#install.packages("cowplot")
#install.packages("GGally")
#install.packages("caTools")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("scales")
#install.packages("lubridate")
#install.packages("ROCR")
#install.packages("corrplot")
#install.packages("reshape2")

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(GGally)
library(caTools)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)
library(ROCR)
library(corrplot)
library(reshape2)

options("scipen"=100, "digits"=4)

#setting the working directory
#setwd("C:/PGDDA/HR Analytics Case Study")
list.files()

##########Load the files to get the master file

# Loading 5 csv files
general_data<- read.csv("general_data.csv", stringsAsFactors = F)
employee_survey_data<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey_data<- read.csv("manager_survey_data.csv", stringsAsFactors = F)

in_time<- read.csv("in_time.csv", stringsAsFactors = F)
out_time<- read.csv("out_time.csv", stringsAsFactors = F)


str(general_data)    # 4410 obs. of  24 variables including the target variable
str(employee_survey_data) # 4410 obs. of  4 variables
str(manager_survey_data) # 4410 obs. of  3 variables
str(in_time) #4410 obs. of  262 variables
str(out_time) #4410 obs. of  262 variables

# Collate the data together in one single file
length(unique(general_data$EmployeeID))    # 4410, confirming EmployeeID is key 
length(unique(employee_survey_data$EmployeeID)) # 4410, confirming EmployeeID is key
length(unique(manager_survey_data$EmployeeID)) # 4410, confirming EmployeeID is key

length(unique(in_time$X))    # 4410, confirming X is key 
length(unique(out_time$X))    # 4410, confirming X is key 


setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID, in_time$X) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID, out_time$X) # Identical EmployeeID across these datasets


#merge the general_data, employee_survey_data, manager_survey_data files to form employee dataframe. 
employee<- merge(general_data,employee_survey_data, by="EmployeeID", all = F) #27 variables
employee<- merge(employee,manager_survey_data, by="EmployeeID", all = F) #29 variables

View(employee) #master file

str(employee)

############################ Data Understanding: END ############################


########################## Data Preparation & EDA:START ######################### 

########## Basic data cleaning
#####1. check the duplicates in employee

count(unique(employee)) #no duplicates found
length(unique(employee$EmployeeID))

count(unique(in_time)) #no duplicates found
length(unique(in_time$X))

count(unique(out_time)) #no duplicates found
length(unique(out_time$X))



#####2. Removing the columns with single data

#find the columns that have only a single value in the entire column and remove those columns

which(sapply(employee, function(x) length(unique(x))==1))#gives EmployeeCount, Over18, StandardHours

employee<- employee[, !sapply(employee, function(x) length(unique(x))==1)]#remove all columns having single value. Hence 26 columns


##########
#####3. in_time and out_time related validations

## In_time and Out_time represents the first and the last punch for an employee in a day. 
## There is no data for Saturdays and sundays 
## the data for both in_time and out time shows lots of NA's with some dates containing only NA's, we can assume these to be holidays
## the other NA cells can be considered leaves if the cell is NA in both the files else it should be treated as missing data 
## also assuming that the first column in both the files is the unique employee id. 

## Step 1: Find no of columns which have only NA data for both in_time and out_time
sum(sapply(in_time, function(x) all(is.na((x))))) #gives 12
sum(sapply(out_time, function(x) all(is.na((x))))) #gives 12

#thus there are 12 days which have only NA values. We need to check if these are for the same dates in both files. 
which(sapply(out_time, function(x) all(is.na((x)))))
which(sapply(in_time, function(x) all(is.na((x)))))

#shows its same columns in both files hence we can safely assume these to be holidays. 
#removing them from the data frame. the finale no of columns will be 262-12=250
in_time<- in_time[, !sapply(in_time, function(x) all(is.na((x))))]
out_time<- out_time[, !sapply(out_time, function(x) all(is.na((x))))]

str(in_time)#4410 obs. of  250 variables
str(out_time)#4410 obs. of  250 variables


## Step 2: Find the number of leaves that an employee has taken and add it to the employee data frame
#a. check if the na's match in in_time and out_time
which(rowSums(is.na(in_time)) != rowSums(is.na(out_time))) #integer(0) implies both the files have na's in the same field

#b. make a dataframe with employee number and leaves by counting the NA's in each row and change their column names
employee_Leaves<-as.data.frame(in_time$X)
employee_Leaves$Leaves<-rowSums(is.na(in_time))
colnames(employee_Leaves)<-c("EmployeeID", "Leaves")

#c. merge it with employee data based on the EmployeeID column
employee<- merge(employee,employee_Leaves, by="EmployeeID", all = F) #27 columns


## Step 3: Find the total in-office time per employee by converting the relevant columns to date time and then finding a difference between them

#replace all NA with -1
#in_time[is.na(in_time)]<--1
#out_time[is.na(out_time)]<--1
#commenting this out... will handle NA's later. 

#convert the columns into datetime format
in_time[,2:250]<-lapply(in_time[ , 2:250], as_datetime)
out_time[,2:250]<-lapply(out_time[ , 2:250], as_datetime)

#####Alternative approach
#in_time1<-in_time[,2:250]
#out_time1<-out_time[,2:250]

#in_time1<-lapply(in_time1,function(x){strptime(x, "%Y-%m-%d %H:%M")})
#out_time1<-lapply(out_time1,function(x){strptime(x, "%Y-%m-%d %H:%M")})

#in_time[,2:250]<-as.data.frame(in_time1)
#out_time[,2:250]<-as.data.frame(out_time1)

#confirms that the columns have been successfully converted into datetime
str(in_time)
str(out_time)

#find difference
#the difference thus found is in hrs only and NOT in hrs:min format
working_hrs<-as.data.frame(in_time$X)
working_hrs[,2:250]<-out_time[,2:250] - in_time[,2:250]
#View(working_hrs)
working_hrs<-lapply(working_hrs,function(x){round(as.numeric(x), digits = 2)})##round off to 2 decimal places after 0. 

working_hrs<-as.data.frame(working_hrs)

#All the NA's are for the leaves taken so the working hours can be safely set to 0.
#However while computing average hours worked per employee, we will have to account for these leaves else it will lower the average
working_hrs[is.na(working_hrs)]<-0

str(working_hrs)
#View(working_hrs)


## Step 4: Find the total working hours per year by aggregating the difference calculated above and merging them with employee dayaframe

#change the name of the first column to employee id. 
colnames(working_hrs)[which(names(working_hrs) == "in_time$X")] <- "EmployeeID"

TotalWorkingHrs<-as.data.frame(working_hrs[,1])
TotalWorkingHrs[2]<-data.frame(rowSums(working_hrs[,2:250]))
colnames(TotalWorkingHrs) <- c("EmployeeID", "TotalWorkingHrs")

employee<- merge(employee,TotalWorkingHrs, by="EmployeeID", all = F)#28 columns


## Step 5: Find the average working hours per employee using the formula (TotalWorkingHours/(total no of days - Leaves)) and merge it with the employee dataframe

#total no of coulmns in in_time and out_time is 250 including the employee id column. Hence total no of working days = 249
#to find the average time we need to remove the count of employee leaves from 249 else the average will be lower than it actually is.

AvgTime <- function(TotalWorkingHrs,Leaves){
  result <- (TotalWorkingHrs/(249-Leaves))
  return(result)
}

#add this as a column to employee data
employee$AvgTime<-AvgTime(employee$TotalWorkingHrs, employee$Leaves)#29 columns


## Step 6: Divide the employees into Overworked/Underworked/Normal categories based on average no of hours worked
## AvgTime < 7 -> UnderWorked
## 7 < AvgTime <= 9, "Normal"))
## AvgTime > 9 -> OverWorked
employee$WorkingCondition = ifelse(employee$AvgTime < 7, "UnderWorked", ifelse(employee$AvgTime > 9, "OverWorked", "Normal"))
##highly correlated with AvgTime so not needed for model building, hence will use this only for EDA and not for the model


##########

#####4. Treating invalid values/ missing data/ NA's

# any variable having more than 15% of data points missing is not eligible for imputation 
# hence it makes sense to compute and drop those variables

missing_values <- employee %>%
  summarise_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')

missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
  geom_bar(stat = 'identity',fill='red') +
  coord_flip()

# Conclusion: as missing values are less than 15% hence not removing any values outright. 

# Hence these NA values will have to be treated using an alternate approach. 
# Find the variables containing NA values
sapply(employee, function(x) sum(is.na(x)))
#shows NA's in NumCompaniesWorked, TotalWorkingYears, EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance 


# 3 of these columns are from manager survey data
#View(subset(employee, is.na( EnvironmentSatisfaction))) #shows 25 NA's
#View(subset(employee, is.na( JobSatisfaction))) #shows 20 NA's
#View(subset(employee, is.na(WorkLifeBalance))) #shows 38 NA's
subset(employee, is.na( EnvironmentSatisfaction)) #shows 25 NA's
subset(employee, is.na( JobSatisfaction)) #shows 20 NA's
subset(employee, is.na(WorkLifeBalance)) #shows 38 NA's


summary(as.factor(subset(employee, is.na(WorkLifeBalance))$Attrition))#No 34 Yes 4
summary(as.factor(subset(employee, is.na(JobSatisfaction))$Attrition))#No 19 Yes 1
summary(as.factor(subset(employee, is.na(EnvironmentSatisfaction))$Attrition))#No 20 Yes 5
## thus we have 10 Yes and 73 NO = 12% attrition
##in this case we should definitely impute the values...  because if we remove these values, 
##we are making the data biased as the overall attrition is 16%  


# view of the subset shows that no employess has more than 1 missing value 
# so if we remove all the NA values then we will end up removing 100+ rows which is not advisable

# Thus we need to determine what value can be used to replace the NA values with. 
# we can try replacing the missing values with average of the other two employee survey values as there is no case where 2 columns are NA's for same employee
# also for most employees, there is not a vast variation in all the three columns as seen by the mean and median below

summary(factor(employee$JobSatisfaction))
#   1    2    3    4 NA's 
# 860  840 1323 1367   20 
summary(factor(employee$EnvironmentSatisfaction))
#   1    2    3    4 NA's 
# 845  856 1350 1334   25 
summary(factor(employee$WorkLifeBalance))
#   1    2    3    4 NA's 
# 239 1019 2660  454   38 

summary(employee$EnvironmentSatisfaction)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.000   2.000   3.000   2.724   4.000   4.000      25 
summary(employee$WorkLifeBalance)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  1.000   2.000   3.000   2.761   3.000   4.000      38 
summary(employee$JobSatisfaction)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.000   2.000   3.000   2.728   4.000   4.000      20 


##### Replacing the NA with the average of the other two columns originally part of employee survey data and rounding off the number

employee$EnvironmentSatisfaction<-ifelse(is.na(employee$EnvironmentSatisfaction ), round((employee$WorkLifeBalance + employee$JobSatisfaction)/2), employee$EnvironmentSatisfaction )

employee$WorkLifeBalance<-ifelse(is.na(employee$WorkLifeBalance ), round((employee$EnvironmentSatisfaction + employee$JobSatisfaction)/2), employee$WorkLifeBalance )

employee$JobSatisfaction<-ifelse(is.na(employee$JobSatisfaction ), round((employee$WorkLifeBalance + employee$EnvironmentSatisfaction)/2), employee$JobSatisfaction )

## summary command shows there is not much change after NA values have been replaced. 
summary(employee$EnvironmentSatisfaction)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   2.000   3.000   2.725   4.000   4.000 
summary(employee$WorkLifeBalance)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   2.000   3.000   2.761   3.000   4.000 
summary(employee$JobSatisfaction)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   2.000   3.000   2.728   4.000   4.000 

## Looks like it is correct approach as there is not much variation in the mean/ median values. 


##### Treatment of NA values in NumCompaniesWorked and TotalWorkingYears

#View(subset(employee, is.na(NumCompaniesWorked)))
subset(employee, is.na(NumCompaniesWorked))
     
# if the TotalWorkingYears is the same as YearsAtCompany then NumCompaniesWorked (which represents number of previous companies worked in) is 0
# found 5 such employees
employee$NumCompaniesWorked[which(is.na(employee$NumCompaniesWorked))]<-
  ifelse((employee$TotalWorkingYears[which(is.na(employee$NumCompaniesWorked))] == employee$YearsAtCompany[which(is.na(employee$NumCompaniesWorked))]), 0, NA)

# if the difference between TotalWorkingYears and YearsAtCompany is 1 then assuming that the person has worked in 1 previous company
# thusNumCompaniesWorked (which represents number of previous companies worked in) is 1
#found 4 such employees
employee$NumCompaniesWorked[which(is.na(employee$NumCompaniesWorked))]<-
  ifelse(((employee$TotalWorkingYears[which(is.na(employee$NumCompaniesWorked))] - employee$YearsAtCompany[which(is.na(employee$NumCompaniesWorked))]) == 1), 1, NA)


#View(subset(employee, is.na(TotalWorkingYears)))
subset(employee, is.na(TotalWorkingYears))


# if NumCompaniesWorked is 0, then all of employees experience is in the current company
# finding such employees and replacing the TotalWorkingYears with YearsAtCompany
# 2 such employees
employee$TotalWorkingYears[which(is.na(employee$TotalWorkingYears))]<-
  ifelse((employee$NumCompaniesWorked[which(is.na(employee$TotalWorkingYears))] == 0), employee$YearsAtCompany[which(is.na(employee$TotalWorkingYears))], NA)

length(which(is.na(employee$NumCompaniesWorked))) #10
length(which(is.na(employee$TotalWorkingYears))) #7

##### the total of these columns is (10+7)/4400 = 0.38% of the total data, hence we will remove these from the dataset instead of finding some other way of replacing them
employee <- employee[!is.na(employee$NumCompaniesWorked),]
employee <- employee[!is.na(employee$TotalWorkingYears),]

##total no of rows remaining = 4410-17 = 4393
str(employee)


#####5. Conversion of categorical variables into factors

##### First check if some categorical variable is masked as continuous variable
summary(as.factor(employee$StockOptionLevel)) #looks like it is categorical
#0    1    2    3 
#1893 1788  474  255 

summary(as.factor(employee$NumCompaniesWorked))
#0    1    2    3    4    5    6    7    8    9 NA's 
#586 1558  438  474  415  187  208  222  147  156   19 

summary(as.factor(employee$TrainingTimesLastYear))
#0    1    2    3    4    5    6 
#162  213 1641 1473  369  357  195 
 

employee$Attrition<-as.factor(employee$Attrition)
employee$BusinessTravel<-as.factor(employee$BusinessTravel)
employee$Department<-as.factor(employee$Department)
employee$EducationField<-as.factor(employee$EducationField)
employee$Gender<-as.factor(employee$Gender)
employee$JobRole<-as.factor(employee$JobRole)
employee$MaritalStatus<-as.factor(employee$MaritalStatus)
employee$Education<-as.factor(employee$Education)
employee$JobLevel<-as.factor(employee$JobLevel)
employee$StockOptionLevel<-as.factor(employee$StockOptionLevel) # summary(as.factor(employee$StockOptionLevel)) reveals only 4 levels
employee$EnvironmentSatisfaction<-as.factor(employee$EnvironmentSatisfaction)
employee$JobSatisfaction<-as.factor(employee$JobSatisfaction)
employee$WorkLifeBalance<-as.factor(employee$WorkLifeBalance)
employee$JobInvolvement<-as.factor(employee$JobInvolvement)
employee$PerformanceRating<-as.factor(employee$PerformanceRating)
employee$WorkingCondition<-as.factor(employee$WorkingCondition)

str(employee)



#####6. Find and treat outliers

boxplot(employee$Age, main="Age") #no outliers
boxplot(employee$DistanceFromHome, main="DistanceFromHome")#no outliers

boxplot(employee$MonthlyIncome, main="MonthlyIncome")#has outliers
quantile(employee$MonthlyIncome, seq(0,1,0.01))


boxplot(employee$NumCompaniesWorked, main="NumCompaniesWorked")#has 1 outliers - 9
quantile(employee$NumCompaniesWorked, seq(0,1,0.01))

boxplot(employee$PercentSalaryHike, main="PercentSalaryHike")#no outliers

boxplot(employee$TotalWorkingYears, main="TotalWorkingYears")#has outliers
quantile(employee$TotalWorkingYears, seq(0,1,0.01))

boxplot(employee$TrainingTimesLastYear, main="TrainingTimesLastYear")#has outliers - 5 and 6
quantile(employee$TrainingTimesLastYear, seq(0,1,0.01))

boxplot(employee$YearsAtCompany, main="YearsAtCompany")#has outliers
quantile(employee$YearsAtCompany, seq(0,1,0.01))

boxplot(employee$YearsSinceLastPromotion, main="YearsSinceLastPromotion")#has outliers
quantile(employee$YearsSinceLastPromotion, seq(0,1,0.01))

boxplot(employee$YearsWithCurrManager, main="YearsWithCurrManager")#has outliers
quantile(employee$YearsWithCurrManager, seq(0,1,0.01))

boxplot(employee$Leaves, main="Leaves")#no outliers

boxplot(employee$TotalWorkingHrs, main="TotalWorkingHrs")#has outliers
quantile(employee$TotalWorkingHrs, seq(0,1,0.01))

boxplot(employee$AvgTime, main="AvgTime")#has outliers
quantile(employee$AvgTime, seq(0,1,0.01))


##########Outlier Treatment
treat_upper_outliers <- function(var_name, value){
  quantiles <- quantile( var_name, value)
  var_name[var_name > quantiles]<- quantiles
  return(var_name)
}
treat_lower_outliers <- function(var_name, value){
  quantiles <- quantile( var_name, value)
  var_name[var_name < quantiles] <- quantiles
  return(var_name)
}

##### Based on the quantiles and box plot above, we treated each variable seperately, rather than taking outlier to be the data that has value > 3*SD

employee$MonthlyIncome<-treat_upper_outliers(employee$MonthlyIncome, 0.90)

employee$NumCompaniesWorked<-treat_upper_outliers(employee$NumCompaniesWorked, 0.95)

employee$TotalWorkingYears<-treat_upper_outliers(employee$TotalWorkingYears, 0.95)

employee$TrainingTimesLastYear<-treat_upper_outliers(employee$TrainingTimesLastYear, 0.95)
employee$TrainingTimesLastYear<-treat_lower_outliers(employee$TrainingTimesLastYear, 0.05)

employee$YearsAtCompany<-treat_upper_outliers(employee$YearsAtCompany, 0.92)

employee$YearsSinceLastPromotion<-treat_upper_outliers(employee$YearsSinceLastPromotion, 0.90)

employee$YearsWithCurrManager<-treat_upper_outliers(employee$YearsWithCurrManager, 0.97)

employee$TotalWorkingHrs<-treat_upper_outliers(employee$TotalWorkingHrs, 0.97)



#####7. Change the factors variables to names
# will make it easier to read and interpret both in graphs and tables.

summary(employee$Education) #to find out the levels
levels(employee$Education) <- c('Below College', 'College', 'Bachelor', 'Master', 'Doctor')
summary(employee$Education) #to verify that only level names have changed

summary(employee$EnvironmentSatisfaction) #to find out the levels
levels(employee$EnvironmentSatisfaction) <- c('Low', 'Medium', 'High', 'Very High')
summary(employee$EnvironmentSatisfaction) #to verify that only level names have changed

summary(employee$JobSatisfaction) #to find out the levels
levels(employee$JobSatisfaction) <- c('Low', 'Medium', 'High', 'Very High')
summary(employee$JobSatisfaction) #to verify that only level names have changed

summary(employee$WorkLifeBalance) #to find out the levels
levels(employee$WorkLifeBalance) <- c('Bad', 'Good', 'Better', 'Best')
summary(employee$WorkLifeBalance) #to verify that only level names have changed

summary(employee$JobInvolvement) #to find out the levels
levels(employee$JobInvolvement) <- c('Low', 'Medium', 'High', 'Very High')
summary(employee$JobInvolvement) #to verify that only level names have changed

summary(employee$PerformanceRating) #to find out the levels
levels(employee$PerformanceRating) <- c('Excellent', 'Outstanding')
summary(employee$PerformanceRating) #to verify that only level names have changed

str(employee)

#####


#####8. EDA 
# univariate for categorical vars
univariate_categorical <- function(dataset,var,var_name){
  dataset %>% ggplot(aes(x = as.factor(var), fill=as.factor(var))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + 
    scale_y_continuous(labels = percent) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") +
    labs(title = var_name, y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank()
    ) 
}

# bivariate for categorical vars
categorical_bivariate <- function(dataset, var, var_name){
  dataset %>% ggplot(aes(x = as.factor(var), fill=Attrition)) +
    geom_bar(aes(y = (..count..)/sum(..count..)), position = "identity") +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + 
    scale_y_continuous(labels = percent) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") +
    labs(title = var_name, y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank()
    )
}

# bivariate for continuous vars
continuous_bivariate <- function(dataset, var, var_name, binCount){
  dataset %>% ggplot(aes(x = var, fill=Attrition)) +
    geom_histogram(aes(y = (..count..)/sum(..count..)), bins = binCount ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = var_name, x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank()
    )
}


univariate_categorical(employee,employee$Attrition,"Employee Attrition")
univariate_categorical(employee,employee$BusinessTravel,"Employee Business Travel")
univariate_categorical(employee,employee$Department,"Employee Department")
univariate_categorical(employee,employee$Education,"Employee Education")
univariate_categorical(employee,employee$EducationField,"Employee Education Field")
univariate_categorical(employee,employee$Gender,"Employee Gender")
univariate_categorical(employee,employee$JobLevel,"Employee JobLevel")
univariate_categorical(employee,employee$JobRole,"Employee JobRole")
univariate_categorical(employee,employee$MaritalStatus,"Employee Marital Status")
univariate_categorical(employee,employee$EnvironmentSatisfaction,"Employee Environment Satisfaction")
univariate_categorical(employee,employee$JobSatisfaction,"Employee Job Satisfaction")
univariate_categorical(employee,employee$WorkLifeBalance,"Employee Work Life Balance")
univariate_categorical(employee,employee$JobInvolvement,"Employee JobInvolvement")
univariate_categorical(employee,employee$PerformanceRating,"Employee PerformanceRating")
univariate_categorical(employee,employee$StockOptionLevel,"Employee StockOptionLevel")
univariate_categorical(employee,employee$WorkingCondition,"Employee WorkingCondition")


categorical_bivariate(employee,employee$BusinessTravel,"Employee Business Travel")
categorical_bivariate(employee,employee$Department,"Employee Department")
categorical_bivariate(employee,employee$Education,"Employee Education")
categorical_bivariate(employee,employee$EducationField,"Employee Education Field")
categorical_bivariate(employee,employee$Gender,"Employee Gender")
categorical_bivariate(employee,employee$JobLevel,"Employee JobLevel")
categorical_bivariate(employee,employee$JobRole,"Employee JobRole")
categorical_bivariate(employee,employee$MaritalStatus,"Employee Marital Status")
categorical_bivariate(employee,employee$EnvironmentSatisfaction,"Employee Environment Satisfaction")
categorical_bivariate(employee,employee$JobSatisfaction,"Employee Job Satisfaction")
categorical_bivariate(employee,employee$WorkLifeBalance,"Employee Work Life Balance")
categorical_bivariate(employee,employee$JobInvolvement,"Employee JobInvolvement")
categorical_bivariate(employee,employee$PerformanceRating,"Employee PerformanceRating")
categorical_bivariate(employee, employee$StockOptionLevel, "Employee StockOptionLevel")
categorical_bivariate(employee,employee$WorkingCondition,"Employee WorkingCondition")


continuous_bivariate(employee, employee$Age, "Employee Age", 30)
continuous_bivariate(employee, employee$TotalWorkingYears, "Employee TotalWorkingYears",30)
continuous_bivariate(employee, employee$AvgTime, "Employee AvgTime", 30)
continuous_bivariate(employee, employee$MonthlyIncome, "Employee MonthlyIncome", 30)
continuous_bivariate(employee, employee$TotalWorkingHrs, "Employee TotalWorkingHrs", 30)
continuous_bivariate(employee, employee$DistanceFromHome, "Employee DistanceFromHome", 10)
continuous_bivariate(employee, employee$PercentSalaryHike, "Employee PercentSalaryHike", 10)
continuous_bivariate(employee, employee$YearsAtCompany, "Employee YearsAtCompany", 10)
continuous_bivariate(employee, employee$YearsWithCurrManager, "Employee YearsWithCurrManager", 10)
continuous_bivariate(employee, employee$Leaves, "Employee Leaves", 10)
continuous_bivariate(employee, employee$NumCompaniesWorked, "Employee NumCompaniesWorked", 5)
continuous_bivariate(employee, employee$TrainingTimesLastYear, "Employee TrainingTimesLastYear",5)
continuous_bivariate(employee, employee$YearsSinceLastPromotion, "Employee YearsSinceLastPromotion",5)




# Barcharts for categorical features with stacked employee information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = 0.5), legend.position="none")


#####JOB Attributes
plot_grid(ggplot(employee, aes(x=DistanceFromHome,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=JobLevel,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1, 
          ggplot(employee, aes(x=JobRole,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=Department,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          align = "h") 

#####PERSONAL Attributes
plot_grid(ggplot(employee, aes(x=Education,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=EducationField,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=Gender,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=Age,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          align = "h") 

#####PERFORMANCE Attributes
plot_grid(ggplot(employee, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=PerformanceRating,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=WorkingCondition,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          align = "h") 

#####Historical Attributes
plot_grid(ggplot(employee, aes(x=NumCompaniesWorked,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=PercentSalaryHike,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=TotalWorkingYears,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=TrainingTimesLastYear,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=YearsAtCompany,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=YearsSinceLastPromotion,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=YearsWithCurrManager,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee, aes(x=Leaves,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          align = "h") 


# Boxplots of numeric variables separated by Attrition

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
                    axis.text.x = element_text(angle = 90), legend.position="none")

box_theme_y1<- theme(legend.position="none", axis.text.x = element_text(angle = 90))

plot_grid(ggplot(employee, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.9)+ coord_flip() + box_theme_y1,
          ggplot(employee, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.9)+ coord_flip() + box_theme_y,
          ggplot(employee, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.9)+ coord_flip() + box_theme_y,
          ggplot(employee, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.9)+ coord_flip() + box_theme_y1,
          ggplot(employee, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.9)+ coord_flip() + box_theme_y,
          ggplot(employee, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.9)+ coord_flip() + box_theme_y,
          ggplot(employee, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.9)+ coord_flip() + box_theme_y1,
          ggplot(employee, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.9)+ coord_flip() + box_theme_y,
          ggplot(employee, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.9)+ coord_flip() + box_theme_y,
          ggplot(employee, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.9)+ coord_flip() + box_theme_y1,
          ggplot(employee, aes(x=Attrition,y=Leaves, fill=Attrition))+ geom_boxplot(width=0.9)+ coord_flip() + box_theme_y,
          ggplot(employee, aes(x=Attrition,y=TotalWorkingHrs, fill=Attrition))+ geom_boxplot(width=0.9)+ coord_flip() + box_theme_y,
          ggplot(employee, aes(x=Attrition,y=AvgTime, fill=Attrition))+ geom_boxplot(width=0.9)+ coord_flip() + box_theme_y1,
          align = "v",nrow = 5)

##### variables that impact attrition if we treat outliers - 
#Age, NumCompaniesWorked, TotalWorkingYears, YearsAtCompany, YearsWithCurrManager, Leaves, TotalWorkingHrs, AvgTime 



#####correlation matrix to vizualize the correlations

getNumericColumns<-function(t){
  tn = sapply(t,function(x){is.numeric(x)})
  return(names(tn)[which(tn)])
}

#Create a Correlation heatmap
cormat<-cor(employee[,getNumericColumns(employee)])
melted_cormat<-melt(cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "green", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

## Corrplot for all numeric columns. 
corrplot(cor(employee[getNumericColumns(employee)],use="na.or.complete"))

########################### Data Preparation & EDA:END ########################## 




########################## Feature standardisation:START ######################## 


# Normalising continuous features 
##### Age, DistanceFromHome, MonthlyIncome, NumCompaniesWorked, PercentSalaryHike, TotalWorkingYears      
##### TrainingTimesLastYear, YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrManager, Leaves, TotalWorkingHrs, AvgTime           


employee$Age<-scale(employee$Age) # scale used: mean  36.92, sd  9.133
employee$DistanceFromHome<-scale(employee$DistanceFromHome) # scale used: mean 9.193, sd 8.105
employee$MonthlyIncome<-scale(employee$MonthlyIncome) # scale used: mean 61135, sd 38120
employee$NumCompaniesWorked<-scale(employee$NumCompaniesWorked) # scale used: mean 2.65, sd 2.41
employee$PercentSalaryHike<-scale(employee$PercentSalaryHike) # scale used: mean 15.21, sd 3.659
employee$TotalWorkingYears<-scale(employee$TotalWorkingYears) # scale used: mean 11.1, sd 7.25
employee$TrainingTimesLastYear<-scale(employee$TrainingTimesLastYear) # scale used: mean 2.79, sd 1.12
employee$YearsAtCompany<-scale(employee$YearsAtCompany)  # scale used: mean 6.54, sd 4.81
employee$YearsSinceLastPromotion<-scale(employee$YearsSinceLastPromotion) # scale used: mean 1.89, sd 2.4
employee$YearsWithCurrManager<-scale(employee$YearsWithCurrManager) # scale used: mean 4.07, sd 3.41
employee$Leaves<-scale(employee$Leaves) # scale used: mean 12.73, sd 5.504
employee$TotalWorkingHrs<-scale(employee$TotalWorkingHrs) # scale used: mean 1819, sd 327
employee$AvgTime<-scale(employee$AvgTime) # scale used: mean 7.701, sd 1.34

str(employee)

########################## Feature standardisation:END ########################## 




########################## Categorical Data Treatment:START ########################## 


# convert factors with 2 levels to numerical variables Attrition, Gender, PerformanceRating
summary(employee$Attrition)
levels(employee$Attrition)<-c(0,1)#No is 0, Yes is 1
employee$Attrition<- as.numeric(levels(employee$Attrition))[employee$Attrition]

summary(employee$Gender)
levels(employee$Gender)<-c(0,1)#Female is 0, Male is 1
employee$Gender<- as.numeric(levels(employee$Gender))[employee$Gender]

summary(employee$PerformanceRating)#3 is 0, 4 is 1
levels(employee$PerformanceRating)<-c(0,1)
employee$PerformanceRating<- as.numeric(levels(employee$PerformanceRating))[employee$PerformanceRating]

#find the attrition
attrition<-sum(employee$Attrition)/nrow(employee)
attrition #0.1607 =>16.07%


#####Creating dummy variables for categorical variables having more than 2 factors. 
# BusinessTravel, Department, Education, EducationField, JobLevel, JobRole,  MaritalStatus, 
#StockOptionLevel, EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance, JobInvolvement


# Create the dummy variables for categorical variables
dummy_1 <- data.frame(model.matrix(~ BusinessTravel + Department + Education + EducationField  + JobLevel + JobRole 
                                   + MaritalStatus + StockOptionLevel + EnvironmentSatisfaction + JobSatisfaction 
                                   + WorkLifeBalance +JobInvolvement, data=employee))[, -1]

#View(dummy_1)

discard_var <- c("EmployeeID","BusinessTravel","Department","Education","EducationField","JobLevel",
                 "JobRole","MaritalStatus","StockOptionLevel","EnvironmentSatisfaction",
                 "JobSatisfaction","WorkLifeBalance","JobInvolvement", "WorkingCondition")

# Combine the dummy variables and the numeric columns of employee dataset, 
#in a new dataset called EmployeeFinal
EmployeeFinal <- cbind(employee[,!(colnames(employee) %in% discard_var)], dummy_1)
View(EmployeeFinal)

########################## Categorical Data Treatment:END ########################## 



####################### Data Split into Test and Train:START #######################

# splitting the data between train and test
set.seed(100)

indices = sample.split(EmployeeFinal$Attrition, SplitRatio = 0.7)

train = EmployeeFinal[indices,]

test = EmployeeFinal[!(indices),]

######################## Data Split into Test and Train:END ########################



############################ Logistic Regression:START #############################

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 
#AIC: 2133


# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)
vif(model_2)

#glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
#      TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
#      TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
#      DepartmentResearch...Development + DepartmentSales + 
#      EducationBachelor + EducationMaster + EducationDoctor + 
#      EducationFieldMarketing + EducationFieldTechnical.Degree + 
#      JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
#      JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
#      EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
#      EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
#      JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
#      WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
#      family = "binomial", data = train)

#AIC: 2098

##################################################
 
##All the variables that have vif of more than 4 are highly significant, hence removing the ones with low significance
#EducationFieldMarketing           -0.3492     0.2234   -1.56              0.11798    

model_3<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
               DepartmentResearch...Development + DepartmentSales + 
               EducationBachelor + EducationMaster + EducationDoctor + 
               EducationFieldTechnical.Degree + 
               JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
               JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
               EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
               EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
               JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
               WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
               family = "binomial", data = train)

summary(model_3)
vif(model_3)
#AIC: 2098



##################################################

##All the variables that have vif of more than 4 are highly significant, hence removing the ones with low significance
#EducationFieldTechnical.Degree    -0.3218     0.2286   -1.41              0.15912    

model_4<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
               DepartmentResearch...Development + DepartmentSales + 
               EducationBachelor + EducationMaster + EducationDoctor + 
               JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
               JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
               EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
               EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
               JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
               WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
             family = "binomial", data = train)

summary(model_4)
vif(model_4)
#AIC: 2098


##################################################

##All the variables that have vif of more than 4 are highly significant, hence removing the ones with low significance
#EducationMaster                   -0.2650     0.1504   -1.76              0.07801 .  

model_5<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
               DepartmentResearch...Development + DepartmentSales + 
               EducationBachelor + EducationDoctor + 
               JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
               JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
               EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
               EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
               JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
               WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
             family = "binomial", data = train)

summary(model_5)
vif(model_5)
#AIC: 2100


##################################################

##All the variables that have vif of more than 4 are highly significant, hence removing the ones with low significance
#EducationBachelor                 -0.1384     0.1203   -1.15              0.24984    

model_6<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
               DepartmentResearch...Development + DepartmentSales + EducationDoctor + 
               JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
               JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
               EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
               EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
               JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
               WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
             family = "binomial", data = train)

summary(model_6)
vif(model_6)
#AIC: 2099



##################################################

##All the variables that have vif of more than 4 are highly significant, hence removing the ones with low significance
#EducationDoctor                   -0.4526     0.3303   -1.37              0.17060    

model_7<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
               DepartmentResearch...Development + DepartmentSales + 
               JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
               JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
               EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
               EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
               JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
               WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
             family = "binomial", data = train)

summary(model_7)
vif(model_7)
#AIC: 2099


##################################################

##All the variables that have vif of more than 4 are highly significant, hence removing the ones with low significance
#JobRoleResearch.Director           0.4576     0.2355    1.94              0.05201 .  

model_8<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
               DepartmentResearch...Development + DepartmentSales + 
               JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
               JobRoleSales.Executive + MaritalStatusSingle + 
               EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
               EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
               JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
               WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
             family = "binomial", data = train)

summary(model_8)
vif(model_8)
#AIC: 2100


##################################################

##All the variables that have vif of more than 4 are highly significant, hence removing the ones with low significance
#JobLevel2                          0.2372     0.1173    2.02              0.04313 *  

model_9<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
               DepartmentResearch...Development + DepartmentSales + 
               JobRoleManager + JobRoleManufacturing.Director + 
               JobRoleSales.Executive + MaritalStatusSingle + 
               EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
               EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
               JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
               WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
             family = "binomial", data = train)

summary(model_9)
vif(model_9)
#AIC: 2103


##################################################

##All the variables that have vif of more than 4 are highly significant, hence removing the ones with low significance
#JobRoleSales.Executive             0.2681     0.1365    1.96              0.04945 *  
  
model_10<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
               DepartmentResearch...Development + DepartmentSales + 
               JobRoleManager + JobRoleManufacturing.Director + MaritalStatusSingle + 
               EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
               EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
               JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
               WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
             family = "binomial", data = train)

summary(model_10)
vif(model_10)
#AIC: 2104


##################################################

##All the variables that have vif of more than 4 are highly significant, hence removing the ones with low significance
#JobInvolvementHigh                -0.2318     0.1156   -2.01              0.04495 *  

model_11<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                DepartmentResearch...Development + DepartmentSales + 
                JobRoleManager + JobRoleManufacturing.Director + MaritalStatusSingle + 
                EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                WorkLifeBalanceBetter + WorkLifeBalanceBest, 
              family = "binomial", data = train)

summary(model_11)
vif(model_11)
#AIC: 2106


##################################################

##All the variables that have vif of more than 4 are highly significant, hence removing the ones with low significance
#JobSatisfactionHigh               -0.3459     0.1546   -2.24              0.02524 *  

model_12<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                DepartmentResearch...Development + DepartmentSales + 
                JobRoleManager + JobRoleManufacturing.Director + MaritalStatusSingle + 
                EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                JobSatisfactionVery.High + WorkLifeBalanceGood + 
                WorkLifeBalanceBetter + WorkLifeBalanceBest, 
              family = "binomial", data = train)

summary(model_12)
vif(model_12)
#AIC: 2109


##################################################

##All the variables that have vif of more than 4 are highly significant, hence removing the ones with low significance
#JobSatisfactionMedium             -0.2590     0.1485   -1.74              0.08119 .  

model_13<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                DepartmentResearch...Development + DepartmentSales + 
                JobRoleManager + JobRoleManufacturing.Director + MaritalStatusSingle + 
                EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                EnvironmentSatisfactionVery.High + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                WorkLifeBalanceBetter + WorkLifeBalanceBest, 
              family = "binomial", data = train)

summary(model_13)
vif(model_13)
#AIC: 2110


##################################################

##All the variables that have vif of more than 4 are highly significant, hence removing the ones with low significance
#TrainingTimesLastYear               -0.1344     0.0575   -2.34              0.01939 *  

model_14<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                DepartmentResearch...Development + DepartmentSales + 
                JobRoleManager + JobRoleManufacturing.Director + MaritalStatusSingle + 
                EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                EnvironmentSatisfactionVery.High + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                WorkLifeBalanceBetter + WorkLifeBalanceBest, 
              family = "binomial", data = train)

summary(model_14)
vif(model_14)
#AIC: 2114


##################################################

##All the variables that have vif of more than 4 are highly significant, hence removing the ones with low significance
#JobRoleManager                    -0.7242     0.2405   -3.01              0.00260 ** 

model_15<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                DepartmentResearch...Development + DepartmentSales + 
                JobRoleManufacturing.Director + MaritalStatusSingle + 
                EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                EnvironmentSatisfactionVery.High + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                WorkLifeBalanceBetter + WorkLifeBalanceBest, 
              family = "binomial", data = train)

summary(model_15)
vif(model_15)
#AIC: 2122


##################################################

##Now all the variables are highly significant, hence removing the ones with max p value and high VIF
#WorkLifeBalanceBest               -0.8568     0.2604   -3.29              0.00100 ***

model_16<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                DepartmentResearch...Development + DepartmentSales + 
                JobRoleManufacturing.Director + MaritalStatusSingle + 
                EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                EnvironmentSatisfactionVery.High + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                WorkLifeBalanceBetter, 
              family = "binomial", data = train)

summary(model_16)
vif(model_16)
#AIC: 2131

##################################################

##All the variables that have vif of more than 4 are highly significant, hence removing the ones with low significance
#WorkLifeBalanceGood               -0.2833     0.1724   -1.64              0.10027    

model_17<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                DepartmentResearch...Development + DepartmentSales + 
                JobRoleManufacturing.Director + MaritalStatusSingle + 
                EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                EnvironmentSatisfactionVery.High + JobSatisfactionVery.High + 
                WorkLifeBalanceBetter, 
              family = "binomial", data = train)

summary(model_17)
vif(model_17)
#AIC: 2131


##################################################


##All the variables that have vif of more than 4 are highly significant, hence removing the ones with low significance
#WorkLifeBalanceBetter             -0.2817     0.1140   -2.47              0.01346 *  

model_18<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                DepartmentResearch...Development + DepartmentSales + 
                JobRoleManufacturing.Director + MaritalStatusSingle + 
                EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                EnvironmentSatisfactionVery.High + JobSatisfactionVery.High, 
              family = "binomial", data = train)

summary(model_18)
vif(model_18)
#AIC: 2135


##################################################


##Now all the variables are highly significant, hence removing the ones with max p value and high VIF
#BusinessTravelTravel_Rarely        0.8210     0.2446    3.36              0.00079 ***

model_19<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + 
                DepartmentResearch...Development + DepartmentSales + 
                JobRoleManufacturing.Director + MaritalStatusSingle + 
                EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                EnvironmentSatisfactionVery.High + JobSatisfactionVery.High, 
              family = "binomial", data = train)

summary(model_19)
vif(model_19)
#AIC: 2147


##################################################


##Now all the variables are highly significant, hence removing the ones with max p value and high VIF
#removing JobRoleManufacturing.Director gives a higher AIC value hence not removing it. 
#DepartmentSales                   -1.0607     0.2397   -4.43   0.0000096049421241 ***

model_20<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + 
                DepartmentResearch...Development + 
                JobRoleManufacturing.Director + MaritalStatusSingle + 
                EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                EnvironmentSatisfactionVery.High + JobSatisfactionVery.High, 
              family = "binomial", data = train)

summary(model_20)
vif(model_20)
#AIC: 2163


##################################################


##removing the ones with max p value
#DepartmentResearch...Development  -0.1070     0.1158   -0.92                 0.36    

model_21<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + 
                JobRoleManufacturing.Director + MaritalStatusSingle + 
                EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                EnvironmentSatisfactionVery.High + JobSatisfactionVery.High, 
              family = "binomial", data = train)

summary(model_21)
vif(model_21)
#AIC: 2162


##################################################

##At this point all the variables are highly significant
##From the correlation plot we drew previously, we know that there is a high correlation between Age and TotalWorkingYears
#removing Age as this is of relatively lower significance wrt TotalWorkingYears
#Age                                 -0.3430     0.0810   -4.23    0.000022991512932 ***

model_22<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + 
                JobRoleManufacturing.Director + MaritalStatusSingle + 
                EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                EnvironmentSatisfactionVery.High + JobSatisfactionVery.High, 
              family = "binomial", data = train)

summary(model_22)
vif(model_22)
#AIC: 2179


##################################################

##At this point all the variables are highly significant
##From the correlation plot we drew previously, we know that there is a high correlation between YearsSinceLastPromotion and YearsWithCurrManager
#removing YearsWithCurrManager as this is of relatively lower significance
#YearsWithCurrManager                -0.4574     0.0884   -5.17    0.000000230079892 ***
  
model_23<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + 
                JobRoleManufacturing.Director + MaritalStatusSingle + 
                EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                EnvironmentSatisfactionVery.High + JobSatisfactionVery.High, 
              family = "binomial", data = train)

summary(model_23)
vif(model_23)
#AIC: 2204


##################################################

##At this point all the variables are highly significant.
##trying to remove the element with least significance
#JobRoleManufacturing.Director     -0.9033     0.2277   -3.97       0.000072503230 ***

model_24<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + MaritalStatusSingle + 
                EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                EnvironmentSatisfactionVery.High + JobSatisfactionVery.High, 
              family = "binomial", data = train)

summary(model_24)
vif(model_24)
#AIC: 2221


##################################################

##At this point all the variables are highly significant.
##trying to remove the element with least significance
#EnvironmentSatisfactionMedium     -0.8430     0.1675   -5.03       0.000000480769 ***

model_25<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + MaritalStatusSingle + 
                EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + JobSatisfactionVery.High, 
              family = "binomial", data = train)

summary(model_25)
vif(model_25)
#AIC: 2245

  
##################################################

##At this point all the variables are highly significant.
##trying to remove the element with least significance
#EnvironmentSatisfactionHigh       -0.5194     0.1312   -3.96       0.000075026364 ***
  
model_26<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + MaritalStatusSingle + 
                EnvironmentSatisfactionVery.High + JobSatisfactionVery.High, 
              family = "binomial", data = train)

summary(model_26)
vif(model_26)
#AIC: 2259 


##################################################

##Remove the element with least significance
#EnvironmentSatisfactionVery.High  -0.3857     0.1214   -3.18               0.0015 ** 
  
model_27<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                TotalWorkingHrs + BusinessTravelTravel_Frequently + MaritalStatusSingle + JobSatisfactionVery.High, 
              family = "binomial", data = train)

summary(model_27)
vif(model_27)
#AIC: 2268 


##At this point again all the variables are highly significant
#Beyond model_23 Number of Fisher Scoring iterations: 5


##################################################
# With 9 significant variables in the model

final_model<- model_25

##################################################

#We ran the code below to see evaluate which model is better out of the 8 models
#We found the cutoff, Accuracy, Sensitivity, Specificity, AUC and KS Statistics for all these models
#as tabulated below and we concluded that model 25 is the optimal model
#though model with NA Removal had better numbers we concluded that NA imputation was required
#Hence the final model is model_25

#Characteristics	|model 21	|model 22	|model 23	|model 24	|model 25	|model 26	|model 27	|Model -NA Removal
#cutoff		        |0.1555		|0.1639		|0.1658		|0.1647		|0.1605		|0.1672		|0.1672		|0.1616
#Accuracy	        |0.6882		|0.6897		|0.6912		|0.692		|0.6927		|0.6904		|0.6859		|0.7303
#Sensitivity      |0.6887		|0.6887		|0.6934		|0.6887		|0.6934		|0.6887		|0.6887		|0.7416
#Specificity	    |0.6881		|0.6899		|0.6908		|0.6926		|0.6926		|0.6908		|0.6854		|0.7281
#AUC		          |0.6884		|0.6893		|0.6934		|0.6906		|0.693		|0.6897		|0.687		|0.7349						
#KS Statistics			        |0.3786		|0.3869		|0.3813		|0.386		|0.3795		|0.374		|0.4697


############################# Logistic Regression:END ##############################



############################# Model Evaluation:START ###############################

### Test Data ####

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", newdata = test[, -2])
summary(test_pred)

test$prob <- test_pred
#View(test)

test_actual_attrition <- factor(ifelse(test$Attrition ==1,"Yes","No"))

# probability cutoff of 50%.
test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
table(test_actual_attrition,test_pred_attrition)##Too many YES predicted as NO

# probability cutoff of 40%.
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
table(test_actual_attrition,test_pred_attrition)##Better but still too many YES predicted as NO

# probability cutoff of 20%.
test_pred_attrition <- factor(ifelse(test_pred >= 0.20, "Yes", "No"))
table(test_actual_attrition,test_pred_attrition)##More YES correctly predicted than wrongly predicted

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf #Accuracy : 0.744, Sensitivity : 0.623, Specificity : 0.768

#At this point the sensitivity of the data is much higher but we still need to find the optimal cutoff point
#Find the Optimal probalility cutoff and the Accuracy, Sensitivity and Specificity at that point

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

summary(test_pred)
# Creating cutoff values from 0.0022 to 0.7690 for plotting and initiallizing a matrix of 100 X 3.

s = seq(.01,.85,length=1000)

##Create a blank matrix to store data returned from the function
OUT = matrix(0,1000,3)
for(i in 1:1000)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.1,.35,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.002)]#0.1605


# Let's choose a cutoff value of 0.1605 for final model

#test_cutoff_attrition <- factor(ifelse(test_pred >=0.1605, "Yes", "No"))
test_cutoff_attrition <- factor(ifelse(test_pred >=cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
conf_final

#          Reference
#Prediction  No Yes
#        No  766  65
#       Yes  340 147

Accuracy <- conf_final$overall[1]
Sensitivity <- conf_final$byClass[1]
Specificity <- conf_final$byClass[2]

Accuracy #0.6927
Sensitivity #0.6934
Specificity #0.6926
cutoff #0.1605

##################### KS -statistics #####################

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

plot(performance_measures_test,main="ROC Curve")
auc<-performance(pred_object_test,measure="auc")
auc#0.693

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)#0.386


##################### Lift & Gain Chart #####################

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
attrition_decile

plot(attrition_decile,main="Lift & Gain Chart")


############################## Model Evaluation:END ################################