############################# HR Solution ###################

################################################################

##Steps followed for the analysis include the following:

#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation

################################################################

### Business Understanding:

# Based on the business and data understanding, the purpose of the analysis is to find an apt
# solution to curb attrition. This is supposed to be a data based analysis where the general 
# employee data, employee asessment data, managers asessment data, in and out office time
# for an year is provided.

## AIM:
# The aim is to automate the process of predicting if an employee will attrition based on the
# previous attrition data and surrounding information based on an automated model. 

################################################################


##Setting work directory
#setwd("E:/Anup/Study/DataScience_IIITB/Course 3/Predective Analytics 1/Case Study/PA-I_Case_Study_HR_Analytics")


library("MASS")
library("car")
library("caTools")
library("ggplot2")

################ 1. Load the data sets #####################################
empdf                   <- read.csv("general_data.csv", stringsAsFactors=FALSE)
empsurvey               <- read.csv("employee_survey_data.csv", stringsAsFactors=FALSE)
mgrsurvey               <- read.csv("manager_survey_data.csv" , stringsAsFactors=FALSE )
in_time                 <- read.csv("in_time.csv" , stringsAsFactors=FALSE)
out_time                <- read.csv("out_time.csv", stringsAsFactors=FALSE)


#Rename the first field of the in_time and out_time data set as Employee Id
colnames(in_time)[1] <-"EmployeeID"
colnames(out_time)[1]<-"EmployeeID"
############################################################################


##Undestanding data - based on Job Role, Education field and Department to understand HR misplacement of profiles
ggplot(empdf, aes(JobRole, fill = EducationField, label= EducationField) ) +  geom_bar(position = "stack") + facet_grid(Department ~ .)


##Undestanding data - based on Job Role, Education field and whether attrition employee to understand whether attrition is related to specific job role or department
ggplot(empdf, aes(JobRole, fill = EducationField, label= EducationField) ) +  geom_bar(position = "stack") + facet_grid(Attrition ~ .)

##Undestanding data - based on Attrition and YearsSinceLastPromotion
ggplot(empdf, aes(YearsSinceLastPromotion, fill = Attrition, label= Attrition) ) +  geom_bar(position = "stack")

##Undestanding data - based on Attrition and Age
ggplot(empdf, aes(Age, fill = Attrition, label= Attrition) ) +  geom_bar(position = "stack")

##Undestanding data - based on Attrition and TotalWorkingYears
ggplot(empdf, aes(TotalWorkingYears, fill = Attrition, label= Attrition) ) +  geom_bar(position = "stack")

##Undestanding data - based on Attrition and YearsAtCompany
ggplot(empdf, aes(YearsAtCompany, fill = Attrition, label= Attrition) ) +  geom_bar(position = "stack")


####################################### Data Preparation   ####################################### 

###################### General_data #######################################
#1. Employee count has only value as 1 , remove it
#2. Over18 is always "Y" , remove it
#3. Standard Hours is always "8", remove it .
#4. Put Employee_ID as first column for readability and so we can merge with other data as required easily later.
#5. Merge general_data with employee and manager survey data 
#5. Handle NA values.
#6. Converting to dummy variables




################## Merge employee and manager survey data ################## 
##### Check if the row numbers are not duplicated and then merge the both
setdiff(empdf$EmployeeID, empsurvey$EmployeeID)
setdiff(empdf$EmployeeID, mgrsurvey$EmployeeID)
setdiff(empdf$EmployeeID, in_time$EmployeeID)
setdiff(empdf$EmployeeID, out_time$EmployeeID)

empdf <- empdf[order(empdf$EmployeeID),]
in_time<-in_time[order(empdf$EmployeeID),]
out_time<-out_time[order(empdf$EmployeeID),]

empdf <- merge(empdf, empsurvey, by ="EmployeeID", all = F)
empdf <- merge(empdf, mgrsurvey, by ="EmployeeID", all = F)


##################### Derive average working hours and leaves from in_time and out_time and ########################

in_time[]               <- lapply(in_time, function(x) as.POSIXct(strptime(x, format = "%Y-%m-%d %H:%M:%S")))
out_time[]              <- lapply(out_time, function(x) as.POSIXct(strptime(x, format = "%Y-%m-%d %H:%M:%S")))
employee_hrs_df         <- as.data.frame(out_time - in_time)
employee_hrs_df         <- as.data.frame(lapply(employee_hrs_df, as.numeric))
employee_hrs_df$meanTime<- apply(employee_hrs_df, MARGIN = 1, function(x) { mean(x,na.rm = TRUE)})
employee_hrs_df$sumNA   <- apply(employee_hrs_df, MARGIN = 1, function(x) sum(is.na(x)))
empdf$meanWorkingTime   <- employee_hrs_df$meanTime
empdf$meanWorkingTime   <- sapply(empdf$meanWorkingTime,round,2)
empdf$totalLeaves       <- employee_hrs_df$sumNA




################## Remove columns that are no more required ###################################
dropCols <- c("EmployeeCount", "Over18","StandardHours","EmployeeID")
empdf    <- empdf[ , !(names(empdf) %in% dropCols)]

################## Outlier Treatment  #################################
## We have only 2% of total rows that contain NA values.  Also fields like JobStatisfaction, EnvironmentSatisfaction,
## Worklifebalance, can't be deduced and setting to mean/median might not make much sense as it's individually opinion.
## Hence, we remove the 2% data values , ie rows having NA in any of the columns.

empdf <- na.omit(empdf)



### Confirm once again that all NA's are removed now
##getNACols(empdf)



################## Identify factor variables in merged dataset ################################

# Identify factor variables

empdf$Attrition = as.factor(empdf$Attrition)
empdf$BusinessTravel = as.factor(empdf$BusinessTravel)
empdf$Department = as.factor(empdf$Department)
empdf$Education = as.factor(empdf$Education)
empdf$EducationField = as.factor(empdf$EducationField)
empdf$Gender    = as.factor(empdf$Gender)
empdf$JobLevel  =as.factor(empdf$JobLevel)
empdf$JobRole = as.factor(empdf$JobRole)
empdf$MaritalStatus = as.factor(empdf$MaritalStatus)
empdf$StockOptionLevel = as.factor(empdf$StockOptionLevel)
empdf$EnvironmentSatisfaction = as.factor(empdf$EnvironmentSatisfaction)
empdf$JobSatisfaction = as.factor(empdf$JobSatisfaction)
empdf$WorkLifeBalance = as.factor(empdf$WorkLifeBalance)
empdf$JobInvolvement = as.factor(empdf$JobInvolvement)
empdf$PerformanceRating = as.factor(empdf$PerformanceRating)



################################################################
# Feature standardisation

# Normalising continuous features 
empdf$DistanceFromHome=scale(empdf$DistanceFromHome)
empdf$MonthlyIncome=scale(empdf$MonthlyIncome)
empdf$NumCompaniesWorked=scale(empdf$NumCompaniesWorked)
empdf$PercentSalaryHike=scale(empdf$PercentSalaryHike)
empdf$TotalWorkingYears=scale(empdf$TotalWorkingYears)
empdf$TrainingTimesLastYear=scale(empdf$TrainingTimesLastYear)
empdf$YearsAtCompany=scale(empdf$YearsAtCompany)
empdf$YearsSinceLastPromotion=scale(empdf$YearsSinceLastPromotion)
empdf$YearsWithCurrManager=scale(empdf$YearsWithCurrManager)
empdf$meanWorkingTime=scale(empdf$meanWorkingTime)
empdf$totalLeaves = scale(empdf$totalLeaves)


##########  Convert Categorical variables to Numeric values ###############



#####  A. Converting Categorical variables with 2 levels  #####

# convert Attrition variable to numeric with levels and store the numeric values in the same variable

summary(factor(empdf$Attrition))
levels(empdf$Attrition)<-c(0,1)
empdf$Attrition<- as.numeric(levels(empdf$Attrition))[empdf$Attrition]

# convert Gender variable to numeric with levels and store the numeric values in the same variable

summary(factor(empdf$Gender))
levels(empdf$Gender)<-c(0,1)
empdf$Gender<- as.numeric(levels(empdf$Gender))[empdf$Gender]

#####  B. Converting Categorical variables with more than 2 levels  #####

#Converting "BusinessTravel" into dummies
summary(factor(empdf$BusinessTravel))
dummy_BusinessTravel <- data.frame(model.matrix( ~BusinessTravel, data = empdf))
dummy_BusinessTravel <- dummy_BusinessTravel[,-1]
empdf <- cbind(empdf[,-which(colnames(empdf) == 'BusinessTravel')], dummy_BusinessTravel)


#Converting "Department" into dummies
summary(factor(empdf$Department))
dummy_Department <- data.frame(model.matrix( ~Department, data = empdf))
dummy_Department <- dummy_Department[,-1]
empdf <- cbind(empdf[,-which(colnames(empdf) == 'Department')], dummy_Department)

#Converting "Education" into dummies
summary(factor(empdf$Education))
dummy_Education <- data.frame(model.matrix( ~Education, data = empdf))
dummy_Education <- dummy_Education[,-1]
empdf <- cbind(empdf[,-which(colnames(empdf) == 'Education')], dummy_Education)

#Converting "EducationField" into dummies
summary(factor(empdf$EducationField))
dummy_EducationField <- data.frame(model.matrix( ~EducationField, data = empdf))
dummy_EducationField <- dummy_EducationField[,-1]
empdf <- cbind(empdf[,-which(colnames(empdf) == 'EducationField')], dummy_EducationField)

#Converting "JobLevel" into dummies
summary(factor(empdf$JobLevel))
dummy_JobLevel <- data.frame(model.matrix( ~JobLevel, data = empdf))
dummy_JobLevel <- dummy_JobLevel[,-1]
empdf <- cbind(empdf[,-which(colnames(empdf) == 'JobLevel')], dummy_JobLevel)


#Converting "JobRole" into dummies
summary(factor(empdf$JobRole))
dummy_JobRole <- data.frame(model.matrix( ~JobRole, data = empdf))
dummy_JobRole <- dummy_JobRole[,-1]
empdf <- cbind(empdf[,-which(colnames(empdf) == 'JobRole')], dummy_JobRole)


#Converting "StockOptionLevel" into dummies
summary(factor(empdf$StockOptionLevel))
dummy_StockOptionLevel <- data.frame(model.matrix( ~StockOptionLevel, data = empdf))
dummy_StockOptionLevel <- dummy_StockOptionLevel[,-1]
empdf <- cbind(empdf[,-which(colnames(empdf) == 'StockOptionLevel')], dummy_StockOptionLevel)

#Converting "MaritalStatus" into dummies
summary(factor(empdf$MaritalStatus))
dummy_MaritalStatus <- data.frame(model.matrix( ~MaritalStatus, data = empdf))
dummy_MaritalStatus <- dummy_MaritalStatus[,-1]
empdf <- cbind(empdf[,-which(colnames(empdf) == 'MaritalStatus')], dummy_MaritalStatus)

#Converting "EnviornmentSatisfaction" into dummies
summary(factor(empdf$EnvironmentSatisfaction))
dummy_EnvironmentSatisfaction <- data.frame(model.matrix( ~EnvironmentSatisfaction, data = empdf))
dummy_EnvironmentSatisfaction <- dummy_EnvironmentSatisfaction[,-1]
empdf <- cbind(empdf[,-which(colnames(empdf) == 'EnvironmentSatisfaction')], dummy_EnvironmentSatisfaction)

#Converting "JobSatisfaction" into dummies
summary(factor(empdf$JobSatisfaction))
dummy_JobSatisfaction <- data.frame(model.matrix( ~JobSatisfaction, data = empdf))
dummy_JobSatisfaction <- dummy_JobSatisfaction[,-1]
empdf <- cbind(empdf[,-which(colnames(empdf) == 'JobSatisfaction')], dummy_JobSatisfaction)

#Converting "WorkLifeBalance" into dummies
summary(factor(empdf$WorkLifeBalance))
dummy_WorkLifeBalance <- data.frame(model.matrix( ~WorkLifeBalance, data = empdf))
dummy_WorkLifeBalance <- dummy_WorkLifeBalance[,-1]
empdf <- cbind(empdf[,-which(colnames(empdf) == 'WorkLifeBalance')], dummy_WorkLifeBalance)

#Converting "JobInvolvement" into dummies
summary(factor(empdf$JobInvolvement))
dummy_JobInvolvement <- data.frame(model.matrix( ~JobInvolvement, data = empdf))
dummy_JobInvolvement <- dummy_JobInvolvement[,-1]
empdf <- cbind(empdf[,-which(colnames(empdf) == 'JobInvolvement')], dummy_JobInvolvement)


#Converting "PerformanceRating" into dummies
summary(factor(empdf$PerformanceRating))
dummy_PerformanceRating <- data.frame(model.matrix( ~PerformanceRating, data = empdf))
dummy_PerformanceRating <- dummy_PerformanceRating[,-1]
empdf <- cbind(empdf[,-which(colnames(empdf) == 'PerformanceRating')], dummy_PerformanceRating)



########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(empdf$Attrition, SplitRatio = 0.7)

train = empdf[indices,]

test = empdf[!(indices),]



########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)
# Removing multicollinearity through VIF check
vif(model_2)



model_3 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                Education5 + EducationFieldLife.Sciences + EducationFieldMarketing + 
                EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree + 
                JobLevel2 + JobLevel5 + JobRoleHuman.Resources + JobRoleManager + 
                JobRoleManufacturing.Director + JobRoleResearch.Director + 
                JobRoleSales.Executive + StockOptionLevel1 + MaritalStatusMarried + 
                MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + JobInvolvement3,data = train, family = "binomial")

summary(model_3)
vif(model_3)

#removing JobLevel5 with p-value = 0.01 > 0.05
model_4 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                Education5 + EducationFieldLife.Sciences + EducationFieldMarketing + 
                EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree + 
                JobLevel2  + JobRoleHuman.Resources + JobRoleManager + 
                JobRoleManufacturing.Director + JobRoleResearch.Director + 
                JobRoleSales.Executive + StockOptionLevel1 + MaritalStatusMarried + 
                MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + JobInvolvement3,data = train, family = "binomial")

summary(model_4)
vif(model_4)


# removing MaritalStatusMarried with p-value 0.093 and vif more than 2 ie 2.117486
model_5 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                Education5 + EducationFieldLife.Sciences + EducationFieldMarketing + 
                EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree + 
                JobLevel2  + JobRoleHuman.Resources + JobRoleManager + 
                JobRoleManufacturing.Director + JobRoleResearch.Director + 
                JobRoleSales.Executive + StockOptionLevel1  + 
                MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + JobInvolvement3,data = train, family = "binomial")

summary(model_5)
vif(model_5)



# removing Education5 with high p-value 0.092285 
model_6 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely  
                 + EducationFieldLife.Sciences + EducationFieldMarketing + 
                EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree + 
                JobLevel2  + JobRoleHuman.Resources + JobRoleManager + 
                JobRoleManufacturing.Director + JobRoleResearch.Director + 
                JobRoleSales.Executive + StockOptionLevel1  + 
                MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + JobInvolvement3,data = train, family = "binomial")

summary(model_6)
vif(model_6)


#removing JobRoleHuman.Resources with p-value 0.093
model_7 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely  
              + EducationFieldLife.Sciences + EducationFieldMarketing + 
                EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree + 
                JobLevel2  + JobRoleManager + 
                JobRoleManufacturing.Director + JobRoleResearch.Director + 
                JobRoleSales.Executive + StockOptionLevel1  + 
                MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + JobInvolvement3,data = train, family = "binomial")

summary(model_7)
vif(model_7)


#since pvalue is all less than 0.05, we remove the varible which has high vif ,ie EducationFieldLife.Sciences with vif 9.1
model_8 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely  
               + EducationFieldMarketing + 
                EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree + 
                JobLevel2  + JobRoleManager + 
                JobRoleManufacturing.Director + JobRoleResearch.Director + 
                JobRoleSales.Executive + StockOptionLevel1  + 
                MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + JobInvolvement3,data = train, family = "binomial")

summary(model_8)
vif(model_8)


#removing  EducationFieldMedical since p-value is high at 0.196748
model_9 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely  
                + EducationFieldMarketing + 
                + EducationFieldOther + EducationFieldTechnical.Degree + 
                JobLevel2  + JobRoleManager + 
                JobRoleManufacturing.Director + JobRoleResearch.Director + 
                JobRoleSales.Executive + StockOptionLevel1  + 
                MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + JobInvolvement3,data = train, family = "binomial")

summary(model_9)
vif(model_9)


#removing EducationFieldMarketing with low significance with p-value of 0.320610 
model_10 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely  
                + EducationFieldOther + EducationFieldTechnical.Degree + 
                JobLevel2  + JobRoleManager + 
                JobRoleManufacturing.Director + JobRoleResearch.Director + 
                JobRoleSales.Executive + StockOptionLevel1  + 
                MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + JobInvolvement3,data = train, family = "binomial")

summary(model_10)
vif(model_10)

#removing EducationFieldOther with low significance of 0.3322s
model_11 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely  
                 + EducationFieldTechnical.Degree + 
                 JobLevel2  + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Executive + StockOptionLevel1  + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 + JobInvolvement3,data = train, family = "binomial")

summary(model_11)
vif(model_11)



#removing EducationFieldTechnical.Degree  with low significance 0.157945
model_12 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely  +
                 JobLevel2  + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Executive + StockOptionLevel1  + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 + JobInvolvement3,data = train, family = "binomial")

summary(model_12)
vif(model_12)



#removing StockOptionLevel1 with p-value 0.043319
model_13 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely  +
                 JobLevel2  + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Executive   + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 + JobInvolvement3,data = train, family = "binomial")

summary(model_13)
vif(model_13)



#removing JobRoleManager with p-value 0.033364
model_14 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely  +
                 JobLevel2 + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Executive   + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 + JobInvolvement3,data = train, family = "binomial")

summary(model_14)
vif(model_14)



#removing JobLevel2 with p-value 0.014916s
model_15 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely  +
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Executive   + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 + JobInvolvement3,data = train, family = "binomial")

summary(model_15)
vif(model_15)


#removing JobInvolvement3 with p-value 0.011s
model_16 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely  +
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Executive   + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 ,data = train, family = "binomial")

summary(model_16)
vif(model_16)


#removing BusinessTravelTravel_Rarely with vif 3.5
model_17 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                 BusinessTravelTravel_Frequently   +
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Executive   + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 ,data = train, family = "binomial")

summary(model_17)
vif(model_17)



#removing JobRoleSales.Executive with p-value 0.005291
model_18 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                 BusinessTravelTravel_Frequently   +
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 ,data = train, family = "binomial")

summary(model_18)
vif(model_18)



#removing JobRoleResearch.Director with p-value 0.009539
model_19 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                 BusinessTravelTravel_Frequently   +
                 JobRoleManufacturing.Director  + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 ,data = train, family = "binomial")

summary(model_19)
vif(model_19)


#removing WorkLifeBalance4 with p-value 0.000115
model_20 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                 BusinessTravelTravel_Frequently   +
                 JobRoleManufacturing.Director  + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3
                  ,data = train, family = "binomial")

summary(model_20)
vif(model_20)


#removing WorkLifeBalance2 with p-value 0.025016
model_21 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                 BusinessTravelTravel_Frequently   +
                 JobRoleManufacturing.Director  + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4  + WorkLifeBalance3
               ,data = train, family = "binomial")

summary(model_21)
vif(model_21)


#removing TotalWorkingYears with vif  2.510420 >2
model_22 = glm(formula = Attrition ~ Age + NumCompaniesWorked  + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + meanWorkingTime + 
                 BusinessTravelTravel_Frequently   +
                 JobRoleManufacturing.Director  + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance3
               ,data = train, family = "binomial")

summary(model_22)
vif(model_22)


final_model<- model_22

##Final Model :model_22

### Model Evaluation

### Test Data ####

test_pred = predict(final_model, type = "response",
                    newdata = test[,-2])

summary(test_pred)
test$prob <- test_pred
View(test)


# Let's use the probability cutoff of 50%.

test_pred_attrition   <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition ==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)
#install.packages("caret")
library(e1071)
library(caret)

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf


###### Finding the optimal probalility cutoff value
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

# Creating cutoff values from 0.0006504 to 0.84582 for plotting and initiallizing a matrix of 100 X 3

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.30,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.18 for final model

test_cutoff_attr <- factor(ifelse(test_pred >=0.185, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attr, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
##################################################################################################
### Test Data - KS -statistic  ######

test_cutoff_attr      <- ifelse(test_cutoff_attr=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

install.packages("ROCR")
library(ROCR)

#Check it on testing  data
pred_object_test<- prediction(test_cutoff_attr, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

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

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)



