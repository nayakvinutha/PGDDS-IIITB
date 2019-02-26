## Install lubridate package for handling date and time ##
## install.packages("lubridate")


## Load required packages ##
library(lubridate)        # Date and time formatting #
library(ggplot2)          # Plotting #
library(dplyr)            # For grouping operations #
library(tidyr)            # For grouping operations #


## Load the data file and understand each variable, it's type etc #####
df <- read.csv("Uber Request Data.csv")
str(df)


###################################### Data cleaning ######################################################

### Remove Unecessary columns ###
#  1.Request ID is not required ##
df$Request.id     <- NULL


### Handle missing values  ###
#  1. Check columns having missing values. 
#  2. Driver.id has NA values corresponding to Status="No Cars available" so, leave it as it is. 
#  3. Drop  timestamp is NA corresponding to "No cars available", so leave it as it is .
sapply(df, function(x)any(is.na(x)))


### Convert values to standard format ###
#   1. Convert date values to standard format for df$Request.timestamp Using function from lubridate package 
#   2. We have already verified there is no "NA" values in request timestamp. Verify there are no "NA" values    
#      after standardization to confirm we have not erred anywhere. 

prev = sum(is.na(df$Request.timestamp))

df$Request.timestamp  <- parse_date_time(df$Request.timestamp , tz="Asia/Calcutta" ,c('%d-%m-%Y HMS','%d-%m-%Y HM', '%d/%m/%Y HM','%d/%m/%Y HMS'))

if( sum(is.na(df$Request.timestamp)) == prev ){
    print("Successfully modified the timestamp format") 
}else {
    print("Error during conversion of timestamp format")
}


prev = sum(is.na(df$Drop.timestamp))
df$Drop.timestamp    <- parse_date_time(df$Drop.timestamp ,tz="Asia/Calcutta" , c('%d-%m-%Y HMS','%d-%m-%Y HM', '%d/%m/%Y HM','%d/%m/%Y HMS'))
if( sum(is.na(df$Drop.timestamp)) == prev ){
  print("Successfully modified the timestamp format") 
}else {
  print("Error during conversion of timestamp format")
}


 
### Derive new variables ###
#   1. Extract hour so we can use it to find the spread of requests on hourly basis.
df$Request_hour       <- hour(df$Request.timestamp)

#   2. Elapsed time.  Basically, we dont have continuos data of drivers for entire day, hence finding 
#    "IDLE" or "WAIT" time is not possible. We could find elapsed time ( Drop time - request ), to see 
#     if there is any impact.
df$Elapsed_time      <- difftime(df$Drop.timestamp,df$Request.timestamp)
mean_elapsed_time    <- mean(df$Elapsed_time, na.rm=TRUE)
median_elapsed_time  <- median(df$Elapsed_time, na.rm=TRUE)
paste("Mean   Elapsed time : " , mean_elapsed_time)
paste("Median Elapsed time : " , median_elapsed_time)

# Since mean and median are same, we don't have any outliers as such. 
# Also we leave NA as it is, because if we mark it to 0, it is like adding outliers and which can have significant impact 
# on the mean considering it constitues around 58% .




################################### Data Analysis ##########################################################


##########  Plot 1: Spread of Number of requests along the day based on the pickup point ########## 
# Type : Histogram - as we plot "frequencies" of requests per day. 

ggplot(df, aes(Request_hour, fill=Pickup.point))  +                                          # fill - to differentiate pick up point         
labs(x="Hour of day",y="Number of Pickup request",title="Spread of Requests in a day") +     # labs - Add title and label x,y for clarity
scale_x_continuous("Hour of day",breaks=c(0:24))  +                                          # breaks(1-24) - to clearly label requests per hour in a day 
scale_fill_manual(values=c("#6e82b7", "#f492a5")) +                                          # To improve coloring with custom colors
geom_histogram(stat="count",show.legend = TRUE,binwidth = 1)   +                             # histogram, that counts number of requests( stat = count)
stat_bin(binwidth =1,geom="text",size=2.5,aes(label=..count.., group=Pickup.point, y=(..count..),vjust=1)) # To show number of requests to get realistic view



           

##### Analysis ######
## 1. Based on time of the day and  Pickup point there is variation in pickup requests
## 2. Demand  from "City"      is more during the daytime from   5am - 10am
## 3. Demand  from "Airports"  is more during evening/night time 5pm - 10pm







##########  Plot 2: How the above requests are handled at each hour in time based on the pickup point                          ########## 
##########  i,e, how is the supply ( Status) for the  demand (request). Does Pick up time have an impact on the request status ########## 



# Type - Line graph, we want to see how data fairs( trend) for each status, hence the choice.


# We saw there are peak points at different time based on pick up points, so we analyse for each pick up point 
# with two line graphs. 

# A.  Pickup point = Airport 

ggplot(df[df$Pickup.point == "Airport",],aes(x = Request_hour, color = Status))+                            
geom_line(stat = "count", size=1) +                                        # Aggregate on number of requests per hour, size for line thickness
labs(x="Hour of day",y="Number of Pickup requests",title="Status of Pick-up Requests from Airport in a day")+  # labels and title for clarity
scale_x_continuous("Hour of day",breaks=c(0:24))                                                               #  scale - for hourly breakup


##### Analysis ######
# 1. We saw in Plot 1 that there is increased Demand at 5pm to 10pm .
# 2. At exactly same hour range, we see "no cars available".
# 3. Cancellations are not too high, and Trip completed seems uniform.
# Based on above, there is clearly " Demand - Supply gap". 
# 4. During night around 10 to 4, Although cancellation are very minimum, number of No cars is more than the trips, so there is slight demand supply gap there as well.


# B.  Pickup point = City 

ggplot(df[df$Pickup.point == "City",],aes(x = Request_hour, color = Status))+ geom_line(stat = 'count',size=1)+
labs(x="Hour of day",y="Number of Pickup requests",title="Status of Pick-up Requests from City in a day")+
scale_x_continuous("Hour of day",breaks=c(0:24))  

##### Analysis ######
#1. We saw in Plot 1 that there is increased Demand at 5am to 10am .
#2. At the same time, we see Cancellations are high between 5am - 10am.
#3. Based on above, there is clearly " Demand - Supply gap". 
#4. During night around 10 to 4, Although cancellation are less, number of No cars is more than the trips, so there is slight demand supply gap there as well.



## Demand - Supply gap occurs when 
# Pickup Airport - 5pm - 10pm  |  Reason -> No cars available
# Pickup City    - 5am - 10am  |  Reason -> Too many cancellations




## Now that we know  where the demand-supply gap occurs , lets derive another metric to classify peak and non-peak hours.
#  5 am to 10am - Early morning , 10 to 4 - afternoon , 5 - 10 late evening, 10 - 4 , night
df$TimeofDay <- sapply( (df$Request_hour), function(x) {
  if(x >= 5 && x <=10 ) {
    return ("Early morning")
  }else if(x >=17 && x <= 22 )
  {
    return("Late evening")
  }
  else if(x >=23 || x <= 5 )
  { return ("Night")}
  else
    return("Afternoon")
})

##########  Plot 4 : Find the success ratio  during peak hour ( df$Timeofday) ########## 

#  Type : Bar graph.  As we have Specific four areas where % is to be plotted.
#  Aesthetic : Change color of histogram - blue - geom_col(fill)
#              Added label with rounding of the % to 2 digits geom_text(label)
#              Text label as with earlier graphs ( labs)
#  Based on Pickup point and TimeofDay,  find out the % of success percentage at different points in a day
df$TimeofDay <- factor(df$TimeofDay, levels = c("Early morning","Afternoon","Late evening","Night"))


# A. Overall success % of trips 
df_city     <- df %>% group_by(Status,TimeofDay)%>%  summarise (Number_of_Requests = n()) %>%ungroup(df_city)
df_mut_city <- df_city%>% group_by(TimeofDay)%>%mutate(freq = Number_of_Requests/sum(Number_of_Requests)*100)
ggplot(df_mut_city[df_mut_city$Status=="Trip Completed",],aes(x = TimeofDay,y=freq)) + 
geom_col(fill="steelblue") +
geom_text(aes(label= sprintf("%0.2f", round(freq, digits = 2))), vjust=-0.25,position = position_dodge(0), size=3)+
ggtitle("Success % for overall trips in a given Time of Day") +
labs(x= "Time Of Day",y="% of trips serve ") 



# B. Success % of trip with Pickup Airport 
df_city     <- df[df$Pickup.point=="Airport",]%>% group_by(Status,TimeofDay)%>%  summarise (Number_of_Requests = n()) %>%ungroup(df_city)
df_mut_city <- df_city%>% group_by(TimeofDay)%>%mutate(freq = Number_of_Requests/sum(Number_of_Requests)*100)
ggplot(df_mut_city[df_mut_city$Status=="Trip Completed",],aes(x = TimeofDay,y=freq)) + 
geom_col(fill="steelblue") +
geom_text(aes(label= sprintf("%0.2f", round(freq, digits = 2))), vjust=-0.25,position = position_dodge(0), size=3)+
ggtitle("Success ratio for overall trips in a given Time of Day - For Pick up Airport") +
labs(x= "Time Of Day",y="Success ratio of requests ")




# C. Success % of trip with Pickup City 
df_city     <- df[df$Pickup.point=="City",]%>% group_by(Status,TimeofDay)%>%  summarise (Number_of_Requests = n()) %>%ungroup(df_city)
df_mut_city <- df_city%>% group_by(TimeofDay)%>%mutate(freq = Number_of_Requests/sum(Number_of_Requests)*100)
ggplot(df_mut_city[df_mut_city$Status=="Trip Completed",],aes(x = TimeofDay,y=freq)) + 
  geom_col(fill="steelblue") +
  geom_text(aes(label= sprintf("%0.2f", round(freq, digits = 2))), vjust=-0.25,position = position_dodge(0), size=3)+
  ggtitle("Success ratio for overall trips in a given Time of Day - For Pick up City") +
  labs(x= "Time Of Day",y="Success ratio of requests")


##### Analysis ######
#1. On an average, success ratio for trips is high - 86% (11am - 4pm) and least -35% (5pm - 10pm)
#2. Further splitting on the Pickup point:
#3. City    - Early Morning (10am - 4pm)  - 29%
#4. Airport - Late  Evening (4pm  - 10pm) - 22%
#Hence, these two areas are hit the maximum in terms of Demand Supply 


##########  Plot 4 : Find the impact of elapsed time (Travel time) ratio  during peak hour ( df$Timeofday) ########## 

df_temp <- df %>% group_by(Pickup.point,Request_hour) %>% summarise(Mean_elapsed = mean(as.numeric(Elapsed_time),na.rm=TRUE))
ggplot(df_temp,aes(x = Request_hour,y = Mean_elapsed,na.rm=TRUE,color = Pickup.point))+ 
  geom_line()+
  scale_x_continuous("Hour of day",breaks=c(0:24))  

## Pattern shows during peak hours there is an increased trend for elapsed time which might be reason for driver cancellations




##########  Plot 5 : Find the success ratio per driver, ie. number of trips completed/(Trips completed + Cancelled) ########## 

  
  df_t <- df[df$Status=="Cancelled" | df$Status=="Trip Completed",] %>% group_by(Pickup.point,Status,Driver.id) %>% summarise(Total = n())%>%ungroup(df_t) %>% complete(Pickup.point,Status,Driver.id,fill = list(Total = 0))
  df_t <- df_t %>% group_by(Pickup.point,Driver.id)%>% mutate (Status,percent = Total/sum(Total))


# A. Percentage of Trips completed when Pick up is City
  ggplot(data =  df_t[(df_t$Status == "Trip Completed") & (df_t$Pickup.point=="City"),], aes(Driver.id,percent))+
  labs(x="Driver Id ",y="Percentage of Trips Completed from City",title="Percentage of Trips Completed from City per driver")+  # labels and title for clarity
  geom_line()+geom_point()+geom_smooth()
  
  
##### Analysis ######
# 1. We see that when pick up is City, we see high number of Driver cancellations.
# 2. Based on above graph, we can identify drivers who never cancel and provide incentive to keep them motivated, as well as motivate other drivers
# to reduced the number of cancellations by other drivers as well.
# 3. Also we see few drivers have less than 0.25% cancellation and some as low as 0. We can put some penalty when cancellation rate is too high, so as to
# increase availability and bridge the demand-supply gap






