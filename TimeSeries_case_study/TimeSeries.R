#Install the required libraries ###

library(dplyr)
library(forecast)
library(tseries)
require(graphics)


###################################################################################
#                Data Prepration and EDA
###################################################################################
###################################################################################



#Read the input global superstore data
global_ss <- read.csv("Global Superstore.csv", header = T, sep = ',')


#Remove the unwanted columns 
global_ss<- global_ss[,c('Order.Date','Segment','Market','Sales','Quantity','Profit')]


#check if there are any NA's in any column- None
sapply(global_ss, function(x) sum(is.na(x)))

#check if there are 7 factor levels as mentioned else we need to handle the mis-spelled,wrong words etc
str(as.factor(global_ss$Market))

#check if there are 3 factor levels as mentioned else we need to handle the mis-spelled,wrong words etc
str(as.factor(global_ss$Segment))

#Convert Order.Date to date format
global_ss$Order.Date <- as.Date(global_ss$Order.Date, format = "%d-%m-%Y")

#Remove the date and keep only month and year
global_ss$Order.Date <- format(as.Date(global_ss$Order.Date),"%Y-%m")

#Further grouped monthly for the 21 segments monthwise and match against Excel
#Group the data market wise and segment wise and then aggregare Sales, Quantity & Profit month wise 

tot_21_segments<- global_ss%>%group_by(Segment,Market,global_ss$Order.Date)%>%summarise(Sum_Of_Sales=sum(Sales),Sum_Of_Quantity=sum(Quantity),Sum_Of_Profits=sum(Profit))%>% ungroup()


## Based on segment and market, for each of the 21 group, find the coefficient of variation
segments_cv <- tot_21_segments %>% group_by(Segment, Market) %>% summarise(mean_Profit=mean(Sum_Of_Profits),  sdev=sd(Sum_Of_Profits), cv= sdev/mean_Profit, count=n())


##Arrange the summarized data and find the first two most profitable and consistently profitable segments
top_2_segments <- segments_cv[order(segments_cv$cv),][1:2,]

#View the top 2 segments
View(top_2_segments)

## Looking at the summary, we see that Consumer Segment has lowest CV with EU and APAC values as 0.624 and 0.6321
EU_seg   <- select((tot_21_segments [tot_21_segments $Segment == top_2_segments[1,]$Segment & tot_21_segments$Market == top_2_segments[1,]$Market,]), -1, -2)
APAC_seg <- select(tot_21_segments[tot_21_segments$Segment == top_2_segments[2,]$Segment & tot_21_segments$Market == top_2_segments[2,]$Market,], -1, -2)


###################################################################################
#                               Model Building
###################################################################################



################ Define some of the helper functions  ###########################

###### 1.  Convert the Time to number of months ######
colnames(EU_seg)[1]   <- "Date"
colnames(APAC_seg)[1] <- "Date"


### Convert the date to the number of months starting from 2011
simplify <- function(x){
  print(x)
  splitstr = strsplit(x,"-")
  year     = as.numeric(splitstr[[1]][1])
  month    = as.numeric(splitstr[[1]][2])
  Time     = abs(2011-year)*12 + month
  return(Time)
}

EU_seg$Time <- sapply(EU_seg$Date, function(x) simplify(x))
EU_seg <- EU_seg[,c(5,2,3,4,1)]

APAC_seg$Time <- sapply(APAC_seg$Date, function(x) simplify(x))
APAC_seg <- APAC_seg[,c(5,2,3,4,1)]



###### 2. Given local predictable series, find armafit using ACF/PACF plots  ######
verifyArmaFit <- function(local_pred){
  
  plot(local_pred, col='blue', type = "l")
  acf(local_pred)                       ### Generate ACF plot 
  acf(local_pred, type="partial")       ### Generate PACF plot 
  armafit <- auto.arima(local_pred)     ### Fit auto arima model
  tsdiag(armafit)
  print(paste0("ARMA FIT is", armafit))
  return(armafit)
  
}

###### 3. Given local predictable series, verify stationarity using   ######
###### a. Dicky Fueller Test
###### b. KPSS  Statistic

validateStationarity <- function(tSeries, armafit){
  
  #We'll check if the residual series is white noise
  residualSeries  <- tSeries-fitted(armafit)
  print(adf.test(residualSeries,alternative = "stationary"))
  print(kpss.test(residualSeries))
  
}

###### 4. Given test data and model, use MAPE to evaluate model  ######
predictAndVerifyModel  <- function(outData, model){
  
  testDataResult <- predict(model,data.frame(Time=outData$Time))
  #Comparing predicted values against  the actual values, using MAPE
  MAPE_class_dec <- accuracy(testDataResult,outData[,2])[5]
  print(paste0("Mape is ", MAPE_class_dec))
  return(testDataResult)
  
}



#####5. Plot dual series #############
plotDualSeries <- function(tseriesOrg, tseriesNew){
  plot (tseriesOrg, col = "black")
  lines(tseriesNew, col = "red")
}


###################################################################################

####### First forecast for Consumer_EU Segment ####################
#### 1.  Convert the given Data Frame into Time series ############

sales_EU_df           <- as.data.frame(select(EU_seg, 1, 2))       ### Select only Time (in months from 2011) and Sales Data for model building
colnames(sales_EU_df) <- c("Time","Sales")                         ### Rename the column names  to "Time" and "Sales"
sales_EU_outData      <- sales_EU_df[43:48,]                       ### Input set
sales_EU_df           <- sales_EU_df[-43:-48,]                     ### Test Data (As mentioned taking last 6 months as test data )
sales_EU_ts           <- ts(sales_EU_df$Sales)


#### 2.  We build two models          ############
#### a. Classical Decomposition       ############
#### b.  ARIMA                        ############

######### a. Classical Decomposition   ##################################
######### We first smoothen the series so we can make out the trend #####

w <-2
smoothedseries <- stats::filter(sales_EU_ts, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}


#Smoothing right end of the time series
n    <- length(sales_EU_ts)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

plotDualSeries(sales_EU_ts,smoothedseries)

## Convert the smoothed series to data frame, for fitting linear regression model ####
smoothdf           <-  as.data.frame(cbind(sales_EU_df$Time, as.vector(smoothedseries)) )
colnames(smoothdf) <-  c("Time","Sales")

#Now, let's fit a model and lets see how it fairs ###########
sales_EU_model1    <- lm(Sales ~ sin(0.5*Time) * poly(Time,3) + cos(0.5*Time) * poly(Time,3)
                  + Time, data = smoothdf)
summary(sales_EU_model1)

#Make prediction 
sales_EU_pred <- predict(sales_EU_model1, Time=sales_EU_df$Time)
summary(sales_EU_pred)

lines(sales_EU_df$Time, sales_EU_pred, col='blue', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series


sales_EU_local_pred          <- sales_EU_ts-sales_EU_pred
autoarimaClassic             <- verifyArmaFit(sales_EU_local_pred)    ### ARIMA Fit gives ARIMA(0,0,0)
validateStationarity(sales_EU_local_pred, autoarimaClassic)           ### Based on P-values of DF(0.01) and KPSS(0.1), we conclude that series is stationary
sales_EU_fcast_classic_decom <- predictAndVerifyModel(sales_EU_outData, sales_EU_model1)
sales_EU_classic_decom_pred  <- c(ts(sales_EU_pred),c(ts(sales_EU_fcast_classic_decom)))
plotDualSeries(sales_EU_ts,sales_EU_classic_decom_pred)




######### b. ARIMA FIT   ##################################


sales_EU_autoarimaDiff <- auto.arima(sales_EU_ts)                        ### ARIMA Fit gives ARIMA(2,1,0)
sales_EU_autoarimaDiff
tsdiag(sales_EU_autoarimaDiff)
plotDualSeries(sales_EU_autoarimaDiff$x, fitted(sales_EU_autoarimaDiff))
validateStationarity(sales_EU_ts, sales_EU_autoarimaDiff)                ### Based on P-values of DF(0.01) and KPSS(0.1), we conclude that series is stationary


#Model evaluation using MAPE

sales_EU_fcast_auto_arima <- predict(sales_EU_autoarimaDiff, n.ahead = 6)
sales_EU_MAPE_auto_arima  <- accuracy(sales_EU_fcast_auto_arima$pred,sales_EU_outData[,2])[5]
sales_EU_MAPE_auto_arima


###  Plot  prediction along with original to find the fit visually ###
sales_EU_auto_arima_pred <- c(fitted(sales_EU_autoarimaDiff),ts(sales_EU_fcast_auto_arima$pred))
plotDualSeries(sales_EU_ts, sales_EU_auto_arima_pred)



###################################################################################
#                      Model Summary - EU - SALES                                 #
###################################################################################
#   Method                 Classical Decomposition            ARIMA               #
#---------------------------------------------------------------------------------#
#   MAPE                   28.27                              28.92               #
#---------------------------------------------------------------------------------#
#   Evaluation :  Based on the MAPE values are close, but we do observe that      #
#   Classical decomposition gives a ARIMA(0,0,0) model . We chose to use classical#
#   approach for EU .                                                             #
###################################################################################





#########  Predict Qunatity            ##################################
######### a. Classical Decomposition   ##################################
######### We first smoothen the series so we can make out the trend #####

qty_EU_df           <- as.data.frame(select(EU_seg, 1, 3))       ### Select only Time (in months from 2011) and Quantity Data for model building
colnames(qty_EU_df) <- c("Time","Quantity")                      ### Rename the column names  to "Time" and "Quantity"
qty_EU_outData      <- qty_EU_df[43:48,]                         ### Input set
qty_EU_df           <- qty_EU_df[-43:-48,]                       ### Test Data (As mentioned taking last 6 months as test data )
qty_EU_ts           <- ts(qty_EU_df$Quantity)

#### 2.  We build two models          ############
#### a. Classical Decomposition       ############
#### b.  ARIMA                        ############

######### a. Classical Decomposition   ##################################
######### We first smoothen the series so we can make out the trend #####


w <-1
smoothedseries <- stats::filter(qty_EU_ts, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}


#Smoothing right end of the time series
n    <- length(qty_EU_ts)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

plotDualSeries(qty_EU_ts,smoothedseries)

## Convert the smoothed series to data frame, for fitting linear regression model ####
smoothdf           <-  as.data.frame(cbind(qty_EU_df$Time, as.vector(smoothedseries)) )
colnames(smoothdf) <-  c("Time","Quantity")

#Now, let's fit a model and lets see how it fairs ###########
qty_EU_model1      <- lm(Quantity ~ sin(0.5*Time) * poly(Time,3) + cos(0.5*Time) * poly(Time,3) + Time, data = smoothdf)
summary(qty_EU_model1)

#Make predition 
qty_EU_pred_model1 <- predict(qty_EU_model1, data.frame(Time=qty_EU_df$Time))
summary(qty_EU_pred_model1)
lines(qty_EU_df$Time, qty_EU_pred_model1, col='blue', lwd=2)



#Now, let's look at the locally predictable series
#We will model it as an ARMA series
qty_EU_local_pred          <- qty_EU_ts-qty_EU_pred_model1
qty_EU_autoarimaClassic    <- verifyArmaFit(qty_EU_local_pred)     ### ARIMA Fit gives ARIMA(2,0,0)
validateStationarity(qty_EU_local_pred, qty_EU_autoarimaClassic)   ### Based on P-values of DF(0.01) and KPSS(0.1), we conclude that series is staionary
qty_EU_fcast_classic_decom <- predictAndVerifyModel(qty_EU_outData, qty_EU_model1)

### Plot  prediction along with original to find the fit visually ###
qty_EU_classic_decom_pred  <- c(ts(qty_EU_pred_model1),c(ts(qty_EU_fcast_classic_decom)))
plotDualSeries(qty_EU_ts,qty_EU_classic_decom_pred)





######### b. ARIMA FIT   ##################################


qty_EU_autoarimaDiff <- auto.arima(qty_EU_ts)               ### ARIMA Fit gives ARIMA(2,1,0)
qty_EU_autoarimaDiff
tsdiag(qty_EU_autoarimaDiff)
plotDualSeries(qty_EU_autoarimaDiff$x, fitted(qty_EU_autoarimaDiff))
validateStationarity(qty_EU_ts, qty_EU_autoarimaDiff)       ### Based on P-values of DF(0.045) and KPSS(0.1), we conclude that series is staionary


#Model evaluation using MAPE

qty_EU_fcast_auto_arima <- predict(qty_EU_autoarimaDiff, n.ahead = 6)
qty_EU_MAPE_auto_arima  <- accuracy(qty_EU_fcast_auto_arima$pred,qty_EU_outData[,2])[5]
qty_EU_MAPE_auto_arima


### Plot  prediction along with original to find the fit visually ###
qty_EU_auto_arima_pred <- c(fitted(qty_EU_autoarimaDiff),ts(qty_EU_fcast_auto_arima$pred))
plotDualSeries(qty_EU_ts, qty_EU_auto_arima_pred)



###################################################################################
#                      Model Summary - EU - Quantity                              #
###################################################################################
#   Method                 Classical Decomposition            ARIMA               #
#---------------------------------------------------------------------------------#
#   MAPE                   30.39                              30.13               #
#---------------------------------------------------------------------------------#
#   Evaluation :  Through both methods we see that the series is stationary.      #
#   Based on the MAPE values are close, We chose to use ARIMA Fit                 #
#   approach for EU .                                                             #
###################################################################################

##########################End Consumer Segment EU ##########################################





########################## Begin Consumer Segment APAC #########################################################

####### First forecast for Consumer_APAC Segment ####################
#### 1.  Convert the given Data Frame into Time series ############

sales_APAC_df           <- as.data.frame(select(APAC_seg, 1, 2)) ### Select only Time (in months from 2011) and Sales Data for model building
colnames(sales_APAC_df) <- c("Time","Sales")                     ### Rename the columns to "Time" and "Sales"
sales_APAC_outData      <- sales_APAC_df[43:48,]                 ### Input set
sales_APAC_df           <- sales_APAC_df[-43:-48,]               ### Test Data (As mentioned taking last 6 months as test data )
sales_APAC_ts           <- ts(sales_APAC_df$Sales)



#### 2.  We build two models          ############
#### a. Classical Decomposition       ############
#### b.  ARIMA                        ############

######### a. Classical Decomposition   ##################################
######### We first smoothen the series so we can make out the trend #####

w <-2
smoothedseries <- stats::filter(sales_APAC_ts, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}


#Smoothing right end of the time series
n    <- length(sales_APAC_ts)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}


plotDualSeries(sales_APAC_ts,smoothedseries)

## Convert the smoothed series to data frame, for fitting linear regression model ####
smoothdf           <-  as.data.frame(cbind(sales_APAC_df$Time, as.vector(smoothedseries)) )
colnames(smoothdf) <-  c("Time","Sales")

#Now, let's fit a model and lets see how it fairs ###########
sales_APAC_model1      <- lm(Sales ~ sin(0.5*Time) * poly(Time,3) + cos(0.5*Time) * poly(Time,3)
                           + Time, data = smoothdf)
summary(sales_APAC_model1)

#Make prediction 
sales_APAC_pred <- predict(sales_APAC_model1, Time=sales_APAC_df$Time)
summary(sales_APAC_pred)
lines(sales_APAC_df$Time, sales_APAC_pred, col='blue', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
sales_APAC_local_pred         <- sales_APAC_ts-sales_APAC_pred
autoarimaClassic              <- verifyArmaFit(sales_APAC_local_pred) ### ARIMA Fit gives ARIMA(0,0,0)
validateStationarity(sales_APAC_local_pred, autoarimaClassic)         ### Based on P-values of DF(0.01) and KPSS(0.1), we conclude that series is staionary
sales_APAC_fcast_classic_decom <- predictAndVerifyModel(sales_APAC_outData, sales_APAC_model1)

### Plot  prediction along with original to find the fit visually ###
sales_APAC_classic_decom_pred  <- c(ts(sales_APAC_pred),c(ts(sales_APAC_fcast_classic_decom)))
plotDualSeries(sales_APAC_ts,sales_APAC_classic_decom_pred)




######### b. ARIMA FIT   ##################################


sales_APAC_autoarimaDiff <- auto.arima(sales_APAC_ts)                       ### ARIMA Fit gives ARIMA(0,1,1)
sales_APAC_autoarimaDiff
tsdiag(sales_APAC_autoarimaDiff)
plotDualSeries(sales_APAC_autoarimaDiff$x, fitted(sales_APAC_autoarimaDiff))
validateStationarity(sales_APAC_ts, sales_APAC_autoarimaDiff)               ### Based on P-values of DF(0.01) and KPSS(0.1), we conclude that series is staionary

sales_APAC_fcast_auto_arima <- predict(sales_APAC_autoarimaDiff, n.ahead = 6)
sales_APAC_MAPE_auto_arima  <- accuracy(sales_APAC_fcast_auto_arima$pred,sales_APAC_outData[,2])[5]
sales_APAC_MAPE_auto_arima

### Plot  prediction along with original to find the fit visually ###
sales_APAC_auto_arima_pred <- c(fitted(sales_APAC_autoarimaDiff),ts(sales_APAC_fcast_auto_arima$pred))
plotDualSeries(sales_APAC_ts, sales_APAC_auto_arima_pred)


###################################################################################
#                      Model Summary - APAC - Sales                               #
###################################################################################
#   Method                 Classical Decomposition            ARIMA               #
#---------------------------------------------------------------------------------#
#   MAPE                   23.86                              27.68               #
#---------------------------------------------------------------------------------#
#   Evaluation :  Through both methods we see that the series is stationary.      #
#   ARMA fit after classical decomposition is ARMA(0,0,0)                         #
#   Based on the MAPE values are close, We chose to use ARIMA Fit                 #
#   approach for EU .                                                             #
###################################################################################




#########  Predict Qunatity            ##################################
######### a. Classical Decomposition   ##################################
######### We first smoothen the series so we can make out the trend #####

qty_APAC_df  <-as.data.frame(select(APAC_seg, 1, 3))       ### Select only Time (in months from 2011) and Quantity Data for model building
colnames(qty_APAC_df) <- c("Time","Quantity")              ### Rename the columns to "Time" and "Quantity"
qty_APAC_outData  <- qty_APAC_df[43:48,]                   ### Input set
qty_APAC_df  <- qty_APAC_df[-43:-48,]                      ### Test Data (As mentioned taking last 6 months as test data )
qty_APAC_ts <- ts(qty_APAC_df$Quantity)
plot(qty_APAC_ts)



#### 2.  We build two models          ############
#### a. Classical Decomposition       ############
#### b.  ARIMA                        ############

######### a. Classical Decomposition   ##################################
######### We first smoothen the series so we can make out the trend #####


w <-3
smoothedseries <- stats::filter(qty_APAC_ts, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}


#Smoothing right end of the time series
n    <- length(qty_APAC_ts)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

plotDualSeries(qty_APAC_ts,smoothedseries)

## Convert the smoothed series to data frame, for fitting linear regression model ####
smoothdf           <-  as.data.frame(cbind(qty_APAC_df$Time, as.vector(smoothedseries)) )
colnames(smoothdf) <-  c("Time","Quantity")

#Now, let's fit a model and lets see how it fairs ###########
qty_APAC_model1      <- lm(Quantity ~ sin(0.5*Time) * poly(Time,3) + cos(0.5*Time) * poly(Time,3)
                         + Time, data = smoothdf)
summary(qty_APAC_model1)

#Make prediction 
qty_APAC_pred_model1 <- predict(qty_APAC_model1, data.frame(Time=qty_APAC_df$Time))
summary(qty_APAC_pred_model1)
lines(qty_APAC_df$Time, qty_APAC_pred_model1, col='red', lwd=2)


#Now, let's look at the locally predictable series
#We will model it as an ARMA series
qty_APAC_local_pred          <- qty_APAC_ts-qty_APAC_pred_model1
qty_APAC_autoarimaClassic    <- verifyArmaFit(qty_APAC_local_pred)   ### ARIMA Fit gives ARIMA(0,0,0)
validateStationarity(qty_APAC_local_pred, qty_APAC_autoarimaClassic) ### Based on P-values of DF(0.01) and KPSS(0.1), we conclude that series is staionary
qty_APAC_fcast_classic_decom <- predictAndVerifyModel(qty_APAC_outData, qty_APAC_model1)

### Plot  prediction along with original to find the fit visually ###
qty_APAC_classic_decom_pred  <- c(ts(qty_APAC_pred_model1),c(ts(qty_APAC_fcast_classic_decom)))
plotDualSeries(qty_APAC_ts,qty_APAC_classic_decom_pred)




######### b. ARIMA FIT   ##################################


qty_APAC_autoarimaDiff <- auto.arima(qty_APAC_ts)
qty_APAC_autoarimaDiff                                            ### ARIMA Fit gives ARIMA(0,1,0)
tsdiag(qty_APAC_autoarimaDiff)
plotDualSeries(qty_APAC_autoarimaDiff$x, fitted(qty_APAC_autoarimaDiff))
validateStationarity(qty_APAC_ts, qty_APAC_autoarimaDiff)         ### Based on P-values of DF(0.01) and KPSS(0.1), we conclude that series is staionary


qty_APAC_fcast_auto_arima <- predict(qty_APAC_autoarimaDiff, n.ahead = 6)
qty_APAC_MAPE_auto_arima  <- accuracy(qty_APAC_fcast_auto_arima$pred,qty_APAC_outData[,2])[5]
qty_APAC_MAPE_auto_arima


### Plot  prediction along with original to find the fit visually ###
qty_APAC_auto_arima_pred <- c(fitted(qty_APAC_autoarimaDiff),ts(qty_APAC_fcast_auto_arima$pred))
plotDualSeries(qty_APAC_ts, qty_APAC_auto_arima_pred)



###################################################################################
#                      Model Summary - APAC - Quantity                            #
###################################################################################
#   Method                 Classical Decomposition            ARIMA               #
#---------------------------------------------------------------------------------#
#   MAPE                   30.49                              26.24               #
#---------------------------------------------------------------------------------#
#   Evaluation :  Through both methods we see that the series is stationary.      #
#   Based on the MAPE values are close, We chose to use ARIMA Fit                 #
#   approach for APAC quantity .                                                  #
###################################################################################
