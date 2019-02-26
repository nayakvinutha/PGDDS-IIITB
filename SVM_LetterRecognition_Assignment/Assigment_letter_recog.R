###################   Loading required libraries   #####################################
library("caret")
library("kernlab")
library("dplyr")
library("readr")
library("ggplot2")
library("gridExtra")
library("e1071")

###################        Load the data sets          #####################################
## Please note that the column names are missing from the data set, hence we set      ##
## header = F while reading the same                                                  ##

df_train <- read.csv("mnist_train.csv", stringsAsFactors = F, header = F)
test     <- read.csv("mnist_test.csv" , stringsAsFactors = F, header = F) 




################### Data preparation and Understanding  #####################################

#### Understand the data dimensions ####
dim(df_train)

#### We have 60000 data points with 785 features. Considering first column as the digit, ####
#### we have 784 features per point.                                                     ####


#### 1. Check for NA's #####
which(sapply(df_train, function(x)sum(is.na(x))) > 0)
which(sapply(test    , function(x)sum(is.na(x))) > 0)


#### 2. Check for duplicated rows in test and train data  #####
ifelse( sum(duplicated(df_train)>0) , print("Duplicates exist ") , print("No Duplicates in train data") )
ifelse( sum(duplicated(test )>0)    , print("Duplicates exist ") , print("No Duplicates in test data" ) )


#### 3. Considering the data set is vast we only take 15% of the data as mentioned ####
set.seed(100)
indices        <- sample(1:nrow(df_train), 0.10*nrow(df_train))
train          <- df_train[indices,]



#### 4. Scale the filtered data #### 

## Get the column numbers where the sum of column values is not 0, which means that
## All values are not 0's. And then scale the rest columns
## We exclude the first column, as it's label which we don't want to scale.

colnum <- which(colSums(train) !=0 )
train[,colnum[-1]]= sapply(train[,colnum[-1]],function(x)scale(x))

colnum <- which(colSums(test) != 0)
test[,colnum[-1]]= sapply(test[,colnum[-1]],function(x)scale(x))


#### 5. Naming the fist column for easy readability and usage ######
names(train)[1] <- "label"
names(test)[1]  <- "label"


train$label <- factor(train$label)
test$label  <- factor(test$label)



#### After above tasks, data is now perpared and ready for model building and evaluation #####
#### "train" ->  contains the training data for model building                           #####
#### "test " ->  contains the test data for using for evaluation (use in prediction)     #####




###################      Model building and Evaluation      #####################################


##############  1.  Building Linear Model  #########################
Model_linear  <- ksvm(label ~ ., data = train, scale = FALSE, kernel = "vanilladot")

##### Predict on the test data and display confusion matrix #####
Eval_linear   <- predict(Model_linear, test)
confusionMatrix(Eval_linear, test$label) 

##### Observations ##### 
# Overall Statistics

# Accuracy                 : 91.37 %
# Specificities quite high : Around 98-99%
# Sensitivities also good  : Above 84% 

# Below are Statistics by Class:

# Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity            0.9745   0.9841   0.8924   0.8911   0.9450   0.8812   0.9280   0.9173   0.8368   0.8751
# Specificity            0.9922   0.9938   0.9887   0.9848   0.9876   0.9876   0.9948   0.9931   0.9893   0.9923
# Pos Pred Value         0.9317   0.9531   0.9012   0.8679   0.8923   0.8743   0.9498   0.9383   0.8936   0.9275
# Neg Pred Value         0.9972   0.9980   0.9876   0.9877   0.9940   0.9884   0.9924   0.9906   0.9825   0.9861
# Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
# Detection Rate         0.0955   0.1117   0.0921   0.0900   0.0928   0.0786   0.0889   0.0943   0.0815   0.0883
# Detection Prevalence   0.1025   0.1172   0.1022   0.1037   0.1040   0.0899   0.0936   0.1005   0.0912   0.0952
# Balanced Accuracy      0.9834   0.9890   0.9406   0.9379   0.9663   0.9344   0.9614   0.9552   0.9130   0.9337



####  Hyperparameter tuning and Cross Validation            #####
####  We will use the train function from caret package to perform crossvalidation
####  1. Set C to (0.005, 0.001, 0.1,0.5,1,2) [ expand.grid(C=c(0.005, 0.001, 0.1,0.5,1,2))]
####  2. Perform 5 fold cross validation      [ trainControl(method="cv", number=5)        ]

set.seed(100)
fit.svm <- train(label~., data=train, method="svmLinear", metric="Accuracy", 
                 tuneGrid= expand.grid(C=c(0.005, 0.001, 0.1,0.5,1,2)), trControl=trainControl(method="cv", number=5))
                 
# Printing cross validation result
print(fit.svm)

# Plotting "fit.svm" results
plot(fit.svm)

# Valdiating the model after cross validation on test data
Eval_linear_crossvalidated <- predict(fit.svm, test)
confusionMatrix(Eval_linear_crossvalidated, test$label)


##### Observations ##### 
# Overall Statistics

# Accuracy                 : 91.55%
# Specificities quite high : Around 98-99%
# Sensitivities also good  : Above 85% 

# Below are Statistics by Class:
   

#Statistics by Class:

# Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity            0.9745   0.9841   0.8915   0.8911   0.9450   0.8800   0.9280   0.9222   0.8511   0.8761
# Specificity            0.9924   0.9935   0.9890   0.9861   0.9877   0.9874   0.9948   0.9936   0.9893   0.9925
# Pos Pred Value         0.9326   0.9506   0.9028   0.8780   0.8932   0.8722   0.9498   0.9433   0.8952   0.9295
# Neg Pred Value         0.9972   0.9980   0.9875   0.9877   0.9940   0.9882   0.9924   0.9911   0.9840   0.9862
# Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
# Detection Rate         0.0955   0.1117   0.0920   0.0900   0.0928   0.0785   0.0889   0.0948   0.0829   0.0884
# Detection Prevalence   0.1024   0.1175   0.1019   0.1025   0.1039   0.0900   0.0936   0.1005   0.0926   0.0951
# Balanced Accuracy      0.9834   0.9888   0.9402   0.9386   0.9664   0.9337   0.9614   0.9579   0.9202   0.9343


#############################################################################################################




##############  2.  Polynomial Model  #########################

Model_poly <- ksvm(label~ ., data = train, scale = FALSE, kernel = "polydot")
Eval_poly  <- predict(Model_linear, test)

#confusion matrix - Polynomial Kernel
confusionMatrix(Eval_poly,test$label)


##### Observations ##### 
# Overall Statistics

# Accuracy                 : 91.37%
# Specificities quite high : Around 98-99%
# Sensitivities also good  : Above 83% 

# Statistics by Class:
  
#                        Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity            0.9745   0.9841   0.8924   0.8911   0.9450   0.8812   0.9280   0.9173   0.8368   0.8751
# Specificity            0.9922   0.9938   0.9887   0.9848   0.9876   0.9876   0.9948   0.9931   0.9893   0.9923
# Pos Pred Value         0.9317   0.9531   0.9012   0.8679   0.8923   0.8743   0.9498   0.9383   0.8936   0.9275
# Neg Pred Value         0.9972   0.9980   0.9876   0.9877   0.9940   0.9884   0.9924   0.9906   0.9825   0.9861
# Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
# Detection Rate         0.0955   0.1117   0.0921   0.0900   0.0928   0.0786   0.0889   0.0943   0.0815   0.0883
# Detection Prevalence   0.1025   0.1172   0.1022   0.1037   0.1040   0.0899   0.0936   0.1005   0.0912   0.0952
# Balanced Accuracy      0.9834   0.9890   0.9406   0.9379   0.9663   0.9344   0.9614   0.9552   0.9130   0.9337


####  Hyperparameter tuning and Cross Validation            #####
####  1. Set C to ( 0.01,0.1,0.5,1,2,5)  [  expand.grid(C=c(0.01,0.1,0.5,1,2)) ]
####  2. Perform 2 fold cross validation [ trainControl(method="cv", number=2) ]


set.seed(100)
grid <- expand.grid(C=c(0.01,0.1,0.5,1,2),degree = c(1, 2, 3, 4, 5), 
                    scale = c(-100, -10, -1, 1, 10, 100))

fit.poly <- train(label~., data=train, method="svmPoly", metric="Accuracy", 
                 tuneGrid= grid, trControl=trainControl(method="cv", number=2))
# Printing cross validation result
print(fit.poly)
plot(fit.poly)

# Valdiating the model after cross validation on test data

evaluate_poly_crossvalidated<- predict(fit.poly, test)
confusionMatrix(evaluate_poly_crossvalidated, test$label)


## Observations 
## After cross validation The final values used for the model were degree = 3, scale = 1 and C = 0.01.

# Overall Statistics

# Accuracy                 : 94.93%
# Specificities quite high : Around 98-99%
# Sensitivities also good  : Improved to above 92% 

#Statistics by Class:
  
#                       Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity            0.9847   0.9868   0.9399   0.9446   0.9654   0.9305   0.9384   0.9193   0.9487   0.9296
# Specificity            0.9968   0.9983   0.9946   0.9945   0.9917   0.9941   0.9973   0.9968   0.9869   0.9927
# Pos Pred Value         0.9708   0.9868   0.9528   0.9511   0.9267   0.9389   0.9740   0.9702   0.8868   0.9343
# Neg Pred Value         0.9983   0.9983   0.9931   0.9938   0.9962   0.9932   0.9935   0.9908   0.9944   0.9921
# Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
# Detection Rate         0.0965   0.1120   0.0970   0.0954   0.0948   0.0830   0.0899   0.0945   0.0924   0.0938
# Detection Prevalence   0.0994   0.1135   0.1018   0.1003   0.1023   0.0884   0.0923   0.0974   0.1042   0.1004
# Balanced Accuracy      0.9907   0.9925   0.9673   0.9696   0.9785   0.9623   0.9679   0.9580   0.9678   0.9611


#############################################################################################################




##############  3.  RBF  Model  #########################

#Using RBF Kernel
Model_RBF <- ksvm(label~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$label)

##### Observations ##### 
# Overall Statistics

# Accuracy                   : 93.13%
# Specificities quite high   : Around 99%
# Sensitivities is also good : Above 88% 

# Statistics by Class:

#                       Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity            0.9724   0.9815   0.9254   0.9277   0.9389   0.9148   0.9405   0.9241   0.8973   0.8831
# Specificity            0.9951   0.9963   0.9901   0.9928   0.9918   0.9917   0.9954   0.9857   0.9924   0.9925
# Pos Pred Value         0.9559   0.9712   0.9148   0.9351   0.9257   0.9148   0.9555   0.8813   0.9268   0.9301
# Neg Pred Value         0.9970   0.9976   0.9914   0.9919   0.9933   0.9917   0.9937   0.9913   0.9890   0.9869
# Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
# Detection Rate         0.0953   0.1114   0.0955   0.0937   0.0922   0.0816   0.0901   0.0950   0.0874   0.0891
# Detection Prevalence   0.0997   0.1147   0.1044   0.1002   0.0996   0.0892   0.0943   0.1078   0.0943   0.0958
# Balanced Accuracy      0.9838   0.9889   0.9577   0.9602   0.9653   0.9532   0.9679   0.9549   0.9448   0.9378


####  Hyperparameter tuning and Cross Validation            #####
####  1. Set C to ( 0.01,0.1,0.5,1,2,5)     [ expand.grid(C=c(0.1,0.5,1,2))        ]
####  2. Set sigma values to c(0.025, 0.05) [ .sigma=c(0.025, 0.05, 0.01, 0.1, 0.5)]
####  3. Perform 5 fold cross validation    [ trainControl(method="cv", number=5)  ]

set.seed(100)
grid_RBF <- expand.grid(.sigma=c(0.025, 0.05, 0.01, 0.1, 0.5), .C=c(0.1,0.5,1,2,3) )

# Performing 5-fold cross validation
fit.rbf <- train(label~., data=train, method="svmRadial", metric="Accuracy", 
                     tuneGrid=grid_RBF, trControl= trainControl(method="cv", number=5))

# Printing cross validation result
print(fit.rbf)

# Plotting "fit.svm" results
plot(fit.rbf)

# Valdiating the model after cross validation on test data

evaluate_rbf_crossvalidated<- predict(fit.rbf, test)
confusionMatrix(evaluate_rbf_crossvalidated, test$label)

##### Observations ##### 
## After cross validation The final values used for the model were sigma = 0.01 and C = 2.
# Overall Statistics

# Accuracy                   : 76.36%
# Specificities quite high   : Except for Class 2 which is at 76%, it's around  99% for otheres
# Sensitivities is low       : Except for Class 1 and 2 which is at 95 and 98% , rest are around 70% on an average.

# Statistics by Class:
  
#                       Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity            0.7367   0.9595   0.9835   0.6683   0.7444   0.6839   0.6701   0.6790   0.7115   0.7542
# Specificity            0.9990   0.9990   0.7683   0.9963   0.9946   0.9964   0.9986   0.9953   0.9945   0.9947
# Pos Pred Value         0.9877   0.9918   0.3282   0.9534   0.9372   0.9487   0.9802   0.9432   0.9327   0.9407
# Neg Pred Value         0.9722   0.9948   0.9975   0.9639   0.9728   0.9699   0.9662   0.9644   0.9696   0.9730
# Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
# Detection Rate         0.0722   0.1089   0.1015   0.0675   0.0731   0.0610   0.0642   0.0698   0.0693   0.0761
# Detection Prevalence   0.0731   0.1098   0.3093   0.0708   0.0780   0.0643   0.0655   0.0740   0.0743   0.0809
# Balanced Accuracy      0.8679   0.9792   0.8759   0.8323   0.8695   0.8401   0.8344   0.8372   0.8530   0.8744
> 

