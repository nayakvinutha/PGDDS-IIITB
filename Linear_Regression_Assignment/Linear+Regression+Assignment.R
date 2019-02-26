######################### Linear Regression Assignment ###############################


# Install the required packages 
# install.packages("MASS")        - Required for stepAIC
# install.packages("car")         - Required for vif
# install.packages("DataCombine") - Used for Find and Replace of misspelt names

# Load the libaries as they are installed now
library("DataCombine")
library("MASS")
library("car")

#Load the car price csv file 
df<- read.csv("CarPrice_Assignment.csv")

#check the structure of data frame
str(df$fueltype)


#################### Data Cleaning and Preparation ###################################



# Split the carName and use only the car company name
# Model is not required to be used as mentioned in the problem statement
df$CarName <- sapply(strsplit(as.character(df$CarName)," ",fixed=TRUE),"[", 1)

##### a. Remove Duplicated and NA Rows #####

# Check for duplicated rows/columns, No duplicated rows were found
df[which(duplicated(df)),]

#check for NA  values , No NA values were found
names(df)[sapply(df, anyNA)]  


##### b. Correct the Mispelt Names  #####
# Create replacements data frame for mispelled carNames
# We list all the misspelt names in "From" and Corresponding corrections in "To" list
Corrected <- data.frame(from = c("maxda", "porcshce","toyouta","vokswagen","vw","Nissan"), to = c("mazda", "porsche","toyota","volkswagen","volkswagen", "nissan"))

# Replace wrong carNames with the correct one
df <- FindReplace(data = df, Var = "CarName", replaceData = Corrected,
                      from = "from", to = "to", exact = FALSE)


##### c. Treat outliers #####

## Outlier values can have  impact on the model, hence we treat the outliers.
## There are different ways to treat outliers like,
## 1. Imputation ( Replacing with mean/median/mode/values)
## 2. Removing outliers
## 3. Replacing with NA's
## 4. Capping etc.
## I chose to use capping.
## We check for outliers using the boxplot, and if the value is more than the 1.5 times more
## than the Inter Quartile range(IQR), we replace as follows:
## Values less than lowerlimit with 5th percentile value
## Values more than higher limit with the 95th pecentile value
## Create a function and call it on each numerical variable to treat it. 


treatOutlier <- function(val){
  outlier_vals <- boxplot.stats(val)$out                    ## Checks for existence of outliers
  if(length(outlier_vals) > 0) {
     qnt  <- quantile( val, probs=c(.25, .75), na.rm = T)   ## Value at 25th and 75th percentile
     caps <- quantile( val, probs=c(.05, .95), na.rm = T)   ## Value at  5th and 95th percentile
     H <- 1.5 * IQR(val, na.rm = T)                         ## 1.5 times IQR  , say buffer
     val[val < (qnt[1] - H)] <- caps[1]                     ## Replace values less than Lower limit ( ie. Val at 25%ile - buffer ) with  5th%ile value
     val[val > (qnt[2] + H)] <- caps[2]                     ## Replace values more than Upper limit ( ie. Val at 75%ile + buffer ) with 95th%ile value
  } 
}


# Fix outliers from numeric data fields of cars dataset
df$wheelbase        <- treatOutlier(df$wheelbase)
df$carlength        <- treatOutlier(df$carlength)
df$carwidth         <- treatOutlier(df$carwidth)
df$carheight        <- treatOutlier(df$carheight)
df$curbweight       <- treatOutlier(df$curbweight)
df$boreratio        <- treatOutlier(df$boreratio)
df$enginesize       <- treatOutlier(df$enginesize)
df$stroke           <- treatOutlier(df$stroke)
df$compressionratio <- treatOutlier(df$compressionratio)
df$horsepower       <- treatOutlier(df$horsepower)
df$peakrpm          <- treatOutlier(df$peakrpm)
df$citympg          <- treatOutlier(df$citympg)
df$highwaympg       <- treatOutlier(df$highwaympg) 




##########  Convert Categorical variables to Numeric values ###############



#####  A. Converting Categorical variables with 2 levels  #####

# convert fueltype variable to numeric with levels and store the numeric values in the same variable

summary(factor(df$fueltype))
levels(df$fueltype)<-c(0,1)
df$fueltype<- as.numeric(levels(df$fueltype))[df$fueltype]

# convert aspiration variable to numeric with levels and store the numeric values in the same variable
levels(df$aspiration)<-c(0,1)
df$aspiration<- as.numeric(levels(df$aspiration))[df$aspiration]

# convert doornumber variable to numeric with levels and store the numeric values in the same variable
levels(df$doornumber)<-c(0,1)
df$doornumber<- as.numeric(levels(df$doornumber))[df$doornumber]

# convert enginelocation variable to numeric with levels and store the numeric values in the same variable
levels(df$enginelocation)<-c(0,1)
df$enginelocation<- as.numeric(levels(df$enginelocation))[df$enginelocation]



#####  B. Converting Categorical variables with more than 2 levels  #####

#Converting "CarName" into dummies
summary(factor(df$CarName))
dummy_carname <- data.frame(model.matrix( ~CarName, data = df))
dummy_carname <- dummy_carname[,-1]
df <- cbind(df[,-which(colnames(df) == 'CarName')], dummy_carname)


#Converting "carbody" into dummies
summary(factor(df$carbody))
dummy_carbody <- data.frame(model.matrix( ~carbody, data = df))
dummy_carbody <- dummy_carbody[,-1]
df <- cbind(df[,-which(colnames(df) == 'carbody')], dummy_carbody)




#Converting "cylindernumber" into dummies
summary(factor(df$cylindernumber))
dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = df))
dummy_cylindernumber <- dummy_cylindernumber[,-1]
df <- cbind(df[,-which(colnames(df) == 'cylindernumber')], dummy_cylindernumber)


#Converting "fuelsystem" into dummies
summary(factor(df$cylindernumber))
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = df))
dummy_fuelsystem <- dummy_fuelsystem[,-1]
df <- cbind(df[,-which(colnames(df) == 'fuelsystem')], dummy_fuelsystem)

#Converting "enginetype" into dummies
summary(factor(df$enginetype))
dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = df))
dummy_enginetype <- dummy_enginetype[,-1]
df <- cbind(df[,-which(colnames(df) == 'enginetype')], dummy_enginetype)

#Converting "drivewheel" into dummies
summary(factor(df$drivewheel))
dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = df))
dummy_drivewheel <- dummy_drivewheel[,-1]
df <- cbind(df[,-which(colnames(df) == 'drivewheel')], dummy_drivewheel)





########################### Model Building ###############################

#set the seed to 100
set.seed(100)

#CAR_ID doesn't contribute to anything, hence we remove the column.
df <- df[,-which(colnames(df)=="car_ID")]

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(df), 0.7*nrow(df))

# generate the train data set
train = df[trainindices,]

#store the rest into test data set
test = df[-trainindices,]

#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(price~.,data=train)


# Check the summary of model. 
summary(model_1)


#  Create Step AIC model
step <- stepAIC(model_1, direction = "both")

# Create model_2 with the variables removed as denoted by StepAIC output
model_2 <- lm(price ~ fueltype + aspiration + doornumber + enginelocation + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemitsubishi + 
                CarNamenissan + CarNameplymouth + CarNamerenault + CarNamesaab + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumbertwo + fuelsystem2bbl + fuelsystemmfi + fuelsystemmpfi + 
                fuelsystemspdi + drivewheelrwd, data=train)


summary(model_2)
vif(model_2)

#fueltype         aspiration         doornumber     enginelocation         CarNamebmw       CarNamebuick 
#4.856221           2.366118           1.459954           1.590835           1.403017           3.251204 
#CarNamechevrolet       CarNamedodge       CarNameisuzu      CarNamejaguar       CarNamemazda  CarNamemitsubishi 
#1.635946           1.908737           1.205702           1.401749           2.972608           2.641385 
#CarNamenissan    CarNameplymouth     CarNamerenault        CarNamesaab      CarNamesubaru      CarNametoyota 
#2.587814           2.079628           1.319723           1.790390           1.761503           2.959148 
#CarNamevolkswagen       CarNamevolvo cylindernumberfive cylindernumberfour  cylindernumbersix  cylindernumbertwo 
#1.987772           1.728174           4.042513          16.256987          10.613587           5.262657 
#fuelsystem2bbl      fuelsystemmfi     fuelsystemmpfi     fuelsystemspdi      drivewheelrwd 
#8.627910           1.622095           9.595185           3.466158           4.340061 


# As thumb rule, we first remove variable with high p-value. Here "CarNameSaab"
model_3 <- lm(price ~ fueltype + aspiration + doornumber + enginelocation + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemitsubishi + 
                CarNamenissan + CarNameplymouth + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumbertwo + fuelsystem2bbl + fuelsystemmfi + fuelsystemmpfi + 
                fuelsystemspdi + drivewheelrwd, data=train)

summary(model_3)
vif(model_3)


## We observe that now the "drivewheelrwd" has  high vif -3.453238 andp-val = 0.233395 > 0.05, hence remove and rebuild

model_4 <- lm(price ~ fueltype + aspiration + doornumber + enginelocation + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemitsubishi + 
                CarNamenissan + CarNameplymouth + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumbertwo + fuelsystem2bbl + fuelsystemmfi + fuelsystemmpfi + 
                fuelsystemspdi , data=train)

summary(model_4)
vif(model_4)

## now since door number has higher p-value of 0.09 we reomve that variable

model_5 <- lm(price ~ fueltype + aspiration  + enginelocation + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemitsubishi + 
                CarNamenissan + CarNameplymouth + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumbertwo + fuelsystem2bbl + fuelsystemmfi + fuelsystemmpfi + 
                fuelsystemspdi , data=train)

summary(model_5)
vif(model_5)

## Now mostly all variables are statistically significant. However "fuelsystemspdi" variable has high VIF but less significant , 0.01 hence remove the same.

model_6 <- lm(price ~ fueltype + aspiration  + enginelocation + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemitsubishi + 
                CarNamenissan + CarNameplymouth + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumbertwo + fuelsystem2bbl + fuelsystemmfi + fuelsystemmpfi  , data=train)


summary(model_6)
vif(model_6)

##"CarNameisuzu" is insignificant with p value 0.09> 0.05 .Removing the same.

model_7 <- lm(price ~ fueltype + aspiration  + enginelocation + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNamejaguar + CarNamemazda + CarNamemitsubishi + 
                CarNamenissan + CarNameplymouth + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumbertwo + fuelsystem2bbl + fuelsystemmfi + fuelsystemmpfi  , data=train)
summary(model_7)
vif(model_7)

##"carNamemazda" has relatively higher VIF > 2 and p value  0.014236, remove it.
model_8 <- lm(price ~ fueltype + aspiration  + enginelocation + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNamejaguar + CarNamemitsubishi + 
                CarNamenissan + CarNameplymouth + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumbertwo + fuelsystem2bbl + fuelsystemmfi + fuelsystemmpfi  , data=train)
summary(model_8)
vif(model_8)

##"fuelsystem2bbl"  still has  high VIF > 2 (3.569) and p value  0.068842, remove that
model_9 <- lm(price ~ fueltype + aspiration  + enginelocation + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNamejaguar + CarNamemitsubishi + 
                CarNamenissan + CarNameplymouth + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumbertwo  + fuelsystemmfi + fuelsystemmpfi  , data=train)
summary(model_9)
vif(model_9)

## "CarNamemitsubishi" has relatively high p value  0.090222, remove that
model_10 <- lm(price ~ fueltype + aspiration  + enginelocation + 
                 CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                 CarNamejaguar + 
                 CarNamenissan + CarNameplymouth + CarNamerenault + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumbertwo  + fuelsystemmfi + fuelsystemmpfi  , data=train)
summary(model_10)
vif(model_10)


##"fuelsystemmfi"  has relatively high p value  0.0783, remove that
model_11 <- lm(price ~ fueltype + aspiration  + enginelocation + 
                 CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                 CarNamejaguar + 
                 CarNamenissan + CarNameplymouth + CarNamerenault + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumbertwo  + fuelsystemmpfi  , data=train)
summary(model_11)
vif(model_11)

##"carNameNissan"  has relatively high p value  0.18618, removing it.
model_12 <- lm(price ~ fueltype + aspiration  + enginelocation + 
                 CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                 CarNamejaguar + CarNameplymouth + CarNamerenault + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumbertwo  + fuelsystemmpfi  , data=train)
summary(model_12)
vif(model_12)

## Remove "CarNameplymouth"  with relatively high p value  0.204
model_13 <- lm(price ~ fueltype + aspiration  + enginelocation + 
                 CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                 CarNamejaguar + CarNamerenault + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumbertwo  + fuelsystemmpfi  , data=train)
summary(model_13)
vif(model_13)


##Remove "CarNamedodge"  has relatively high p value 0.0810
model_14 <- lm(price ~ fueltype + aspiration  + enginelocation + 
                 CarNamebmw + CarNamebuick + CarNamechevrolet  + 
                 CarNamejaguar + CarNamerenault + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumbertwo  + fuelsystemmpfi  , data=train)
summary(model_14)
vif(model_14)

##Remove "CarNamerenault"  with high p value  0.110879
model_15 <- lm(price ~ fueltype + aspiration  + enginelocation + 
                CarNamebmw + CarNamebuick + CarNamechevrolet  + CarNamejaguar  + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumbertwo  + fuelsystemmpfi  , data=train)
summary(model_15)
vif(model_15)

##Remove "CarNamesubaru"  with p-value 0.074404
model_16 <- lm(price ~ fueltype + aspiration  + enginelocation + 
                 CarNamebmw + CarNamebuick + CarNamechevrolet  + CarNamejaguar  + 
                 CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumbertwo  + fuelsystemmpfi  , data=train)
summary(model_16)
vif(model_16)



## Remove "carNametoyota"  with p-value- 0.00417. Others seem better. 
model_17 <- lm(price ~ fueltype + aspiration  + enginelocation + 
                 CarNamebmw + CarNamebuick + CarNamechevrolet  + CarNamejaguar  + 
                 CarNamevolkswagen + CarNamevolvo + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumbertwo  + fuelsystemmpfi  , data=train)
summary(model_17)
vif(model_17)

##Remove "CarNamevolkswagen" with high-p value  0.00417
model_18 <- lm(price ~ fueltype + aspiration  + enginelocation + 
                 CarNamebmw + CarNamebuick + CarNamechevrolet  + CarNamejaguar + CarNamevolvo + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumbertwo  + fuelsystemmpfi  , data=train)
summary(model_18)
vif(model_18)

##"fueltype" has high p valued 0.0289 ,removing it .
model_18_1 <-  lm(price ~ aspiration  + enginelocation + 
                    CarNamebmw + CarNamebuick + CarNamechevrolet  + CarNamejaguar + CarNamevolvo + 
                    cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                    cylindernumbertwo  + fuelsystemmpfi  , data=train)
summary(model_18_1)
vif(model_18_1)

##Now  all variable ase significant, We now remove "cylindernumberfour" with score of 13.7, i.e high VIF, will signify
## Multicollinearity.
model_19 <- lm(price ~ aspiration  + enginelocation + 
                 CarNamebmw + CarNamebuick + CarNamechevrolet  + CarNamejaguar + CarNamevolvo + 
                 cylindernumberfive + cylindernumbersix + 
                 cylindernumbertwo  + fuelsystemmpfi  , data=train)
summary(model_19)
vif(model_19)

##We see that now "cylindernumbefive" becomes insignifincant with p value 0.841435 > 0.05, we remove it.
model_20 <- lm(price ~ aspiration  + enginelocation + 
                             CarNamebmw + CarNamebuick + CarNamechevrolet  + CarNamejaguar + CarNamevolvo + 
                             + cylindernumbersix + 
                             cylindernumbertwo  + fuelsystemmpfi  , data=train)
summary(model_20)
vif(model_20)

##CarNamechevrolet has high p value 0.456119  > 0.05 , remove it .
model_21 <- lm(price ~ aspiration  + enginelocation + 
                 CarNamebmw + CarNamebuick   + CarNamejaguar + CarNamevolvo + 
                 + cylindernumbersix + 
                 cylindernumbertwo  + fuelsystemmpfi  , data=train)
summary(model_21)
vif(model_21)

##cylindernumbertwo can or cannot be removed , though significant , since p value 0.02  is not "***"  further refine
model_22 <- lm(price ~ aspiration  + enginelocation + 
                 CarNamebmw + CarNamebuick   + CarNamejaguar + CarNamevolvo + 
                 + cylindernumbersix + fuelsystemmpfi  , data=train)
summary(model_22)
vif(model_22)

##carnamevolvo try removal 0.015041 , since it is not "***"
model_23 <- lm(price ~ aspiration  + enginelocation + 
                 CarNamebmw + CarNamebuick   + CarNamejaguar + 
                 + cylindernumbersix + fuelsystemmpfi  , data=train)
summary(model_23)
vif(model_23)

##Now since all the variables are significant and VIF is less than 2 we stop here . model_23 is the final model


# Predict the prices in the testing dataset using model_23
Predict_1 <- predict(model_23,test)
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2


##################### Driving factors as per the final model #########################

#1.CarNamebuick
#2.aspiration
#3.CarNamebmw 
#4.CarNamejaguar 
#5.fuelsystemmpfi
#6.enginelocation
#7.cylindernumbersix 

##################### Driving factors as per the final model #########################

