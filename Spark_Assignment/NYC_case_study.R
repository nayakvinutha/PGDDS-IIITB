#################### 1.   Setting environment ##################################

# Load SparkR
spark_path <- '/usr/local/spark'

if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}

library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

sparkR.session(master = "yarn", sparkConfig = list(spark.driver.memory = "1g"))

# Before executing any hive-sql query from RStudio, you need to add a jar file in RStudio 
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")


df_2015 <- read.df("hdfs:///common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2015.csv", source = "csv", 
                   inferSchema = "true", header = "true")

df_2016 <- read.df("hdfs:///common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2016.csv", source = "csv", 
                   inferSchema = "true", header = "true")

df_2017 <- read.df("hdfs:///common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv", source = "csv", 
                   inferSchema = "true", header = "true")


################################################################################ 






################### 2. Basic data cleaning ########################################
### 1. Remove duplicate rows 
df_2015      <- dropDuplicates(df_2015)
df_2016      <- dropDuplicates(df_2016)
df_2017      <- dropDuplicates(df_2017)


## Group by Summons Number to find duplicate summons number. 
## We see there is one "Summons Number" that repeats, but rows are not duplicate
## Considering it's not stated in the data and information provided that the number has to be unique, we have retained the row.

dftemp  <- SparkR::summarize(groupBy(df_2015, df_2015$`Summons Number`), count = n(df_2015$`Summons Number`))   
head(SparkR::filter(dftemp, dftemp$count >1)) 
head(SparkR:: filter(df_2015, df_2015$`Summons Number`== 1368159308))






################# 3. Examing the Data #############################################
# 1. Find the total number of tickets for each year.

#Since we have removed all duplicate rows, we simply use nrow
total_entry_2015<- nrow(df_2015)
total_entry_2015
#[1] 10951257 
total_entry_2016<- nrow(df_2016)
total_entry_2016
#[1] 10626899
total_entry_2017<- nrow(df_2017)
total_entry_2017
#[1] 10803028


##################### Comparison of total number of tickets across year #####################


#Create a data frame with three years and the total number of tickets
df_three_years <- data.frame( x=c(2015,2016,2017), y = c(total_entry_2015,total_entry_2016,total_entry_2017))

#Initialize the ggplot librarby
library(ggplot2)

#Plot a bar graph comparing the tickets across the three years.
plot <- ggplot(df_three_years, aes(x =df_three_years$x, y = df_three_years$y,label=y,fill=x)) +
  geom_bar(stat = "identity") +
  xlab("Year") + ylab("Total number of tickets")+ scale_y_continuous(labels = function(l) {trans = l / 1000}) +
  labs(x = "Year ", y = "Total number of tickets")+
  geom_text(nudge_y = 1,color="white",size = 3, hjust = 0.5,vjust=1.5)+ theme_bw() + theme(legend.position = "none") 

#View the plot 
plot
################################################################################




#2. Find out the number of unique states from where the cars that got parking tickets came from. 

#####################  2015 #################################################
### Count frequency of tickets per Registration State and arrange in decreasing order of number of tickets per each state
createOrReplaceTempView(df_2015,"dfv2015")
df_by_state_2015  <-  SparkR::sql("SELECT `Registration State`, count(*) as  frequency_of_tickets from dfv2015
                                  group by `Registration State`
                                  order by  frequency_of_tickets desc")
showDF(df_by_state_2015)

### Registration state Code with maximum entry for parking tickets raised.
stateCodeMax2015  <-  first(df_by_state_2015)$`Registration State`       
stateCodeMax2015

### Select State code having numeric entry and store it in stateCodeNum
dftemp_2015       <-  SparkR::sql("SELECT `Registration State` from dfv2015 where `Registration State` rlike '\\[0-9]\\[0-9]'")
stateCodeNum2015  <-  first(dftemp_2015)$ `Registration State`
stateCodeNum2015


### Replace entries with "stateCodeNum" ( numeric entry ) with "stateCodeMax" (state with maximum entry for tickets)
### Since count has modified now, summarize and arrange now and check again
df_by_state_2015  <- withColumn(df_2015, "Registration State", ifelse(df_2015$`Registration State` == stateCodeNum2015,stateCodeMax2015,df_2015$`Registration State`))
createOrReplaceTempView(df_by_state_2015, "dfv_state_2015")
df_by_state_2015  <-  SparkR::sql("SELECT `Registration State`, count(*) as  frequency_of_tickets from dfv_state_2015
                                  group by `Registration State`
                                  order by  frequency_of_tickets desc")
showDF(df_by_state_2015)


#####################  2016 #################################################
### Count frequency of tickets per Registration State and arrange in decreasing order of number of tickets per each state
createOrReplaceTempView(df_2016,"dfv2016")
df_by_state_2016  <-  SparkR::sql("SELECT `Registration State`, count(*) as  frequency_of_tickets from dfv2016
                                  group by `Registration State`
                                  order by  frequency_of_tickets desc")
showDF(df_by_state_2016)

### Registration state Code with maximum entry for parking tickets raised.
stateCodeMax2016  <-  first(df_by_state_2016)$`Registration State`       
stateCodeMax2016

### Select State code having numeric entry and store it in stateCodeNum
dftemp_2016       <-  SparkR::sql("SELECT `Registration State` from dfv2016 where `Registration State` rlike '\\[0-9]\\[0-9]'")
stateCodeNum2016  <-  first(dftemp_2016)$ `Registration State`
stateCodeNum2016


### Replace entries with "stateCodeNum" ( numeric entry ) with "stateCodeMax" (state with maximum entry for tickets)
### Since count has modified now, summarize and arrange now and check again
df_by_state_2016  <- withColumn(df_2016, "Registration State", ifelse(df_2016$`Registration State` == stateCodeNum2016,stateCodeMax2016,df_2016$`Registration State`))
createOrReplaceTempView(df_by_state_2016, "dfv_state_2016")
df_by_state_2016  <-  SparkR::sql("SELECT `Registration State`, count(*) as  frequency_of_tickets from dfv_state_2016
                                  group by `Registration State`
                                  order by  frequency_of_tickets desc")
showDF(df_by_state_2016)



#####################  2017 #################################################
### Count frequency of tickets per Registration State and arrange in decreasing order of number of tickets per each state
createOrReplaceTempView(df_2017,"dfv2017")
df_by_state_2017 <-  SparkR::sql("SELECT `Registration State`, count(*) as  frequency_of_tickets from dfv2017
                                 group by `Registration State`
                                 order by  frequency_of_tickets desc")
showDF(df_by_state_2017)

### Registration state Code with maximum entry for parking tickets raised.
stateCodeMax2017  <-  first(df_by_state_2017)$`Registration State`       
stateCodeMax2017

### Select State code having numeric entry and store it in stateCodeNum
dftemp_2017       <-  SparkR::sql("SELECT `Registration State` from dfv2017 where `Registration State` rlike '\\[0-9]\\[0-9]'")
stateCodeNum2017  <-  first(dftemp_2017)$ `Registration State`
stateCodeNum2017


### Replace entries with "stateCodeNum" ( numeric entry ) with "stateCodeMax" (state with maximum entry for tickets)
### Since count has modified now, summarize and arrange now and check again
df_by_state_2017  <- withColumn(df_2017, "Registration State", ifelse(df_2017$`Registration State` == stateCodeNum2017,stateCodeMax2017,df_2017$`Registration State`))
createOrReplaceTempView(df_by_state_2017, "dfv_state_2017")
df_by_state_2017  <-  SparkR::sql("SELECT `Registration State`, count(*) as  frequency_of_tickets from dfv_state_2017
                                  group by `Registration State`
                                  order by  frequency_of_tickets desc")
showDF(df_by_state_2017)


#################################################################################################################



#3.Some parking tickets don't have the address for violation location on them, which is a cause for concern. Write a query to check the number of such tickets.

#We assume that we are using 'Violation Location' Column for this task.
###### 2015
df_2015_no_violation_location <- SparkR::filter(df_2015, isNull(df_2015$`Violation Location`))
nrow(df_2015_no_violation_location)

###### 2016
df_2016_no_violation_location <- SparkR::filter(df_2016, isNull(df_2016$`Violation Location`))
nrow(df_2016_no_violation_location)

###### 2017
df_2017_no_violation_location <- SparkR::filter(df_2017, isNull(df_2017$`Violation Location`))
nrow(df_2017_no_violation_location)





################# 4. Aggregation Tasks #############################################

# 1. How often does each violation code occur? Display the frequency of the top five violation codes.

#########  Find the Violation Code and the frequency of Violation for each year and then list the top 5 
#########  2015 
df_2015_violation_freq <-SparkR::summarize(groupBy(df_2015, df_2015$`Violation code`), count = n(df_2015$`Violation Code`))
df_2015_violation_freq<- arrange(df_2015_violation_freq,"count",decreasing=TRUE)
head(df_2015_violation_freq,5)

#########  2016
df_2016_violation_freq <-SparkR::summarize(groupBy(df_2016, df_2016$`Violation code`), count = n(df_2016$`Violation Code`))
df_2016_violation_freq<- arrange(df_2016_violation_freq,"count",decreasing=TRUE)
head(df_2016_violation_freq,5)

#########  2017
df_2017_violation_freq <-SparkR::summarize(groupBy(df_2017, df_2017$`Violation code`), count = n(df_2017$`Violation Code`))
df_2017_violation_freq<- arrange(df_2017_violation_freq,"count",decreasing=TRUE)
head(df_2017_violation_freq,5)
############################################################################################################################






#2.How often does each 'vehicle body type' get a parking ticket? How about the 'vehicle make'? (Hint: find the top 5 for both)

#########  Similar as above, but we find the frequency of tickets for vehicle make .
#########  2015
df_2015_vehicle_body <-SparkR::summarize(groupBy(df_2015, df_2015$`Vehicle Body Type`), count = n(df_2015$`Vehicle Body Type`))
df_2015_vehicle_body<- arrange(df_2015_vehicle_body,"count",decreasing=TRUE)
head(df_2015_vehicle_body,5) 


#########  2016
df_2016_vehicle_make <-SparkR::summarize(groupBy(df_2016, df_2016$`Vehicle Make`), count = n(df_2016$`Vehicle Make`))
df_2016_vehicle_make<- arrange(df_2016_vehicle_make,"count",decreasing=TRUE)
head(df_2016_vehicle_make,5)

#########  2017
df_2017_vehicle_make <-SparkR::summarize(groupBy(df_2017, df_2017$`Vehicle Make`), count = n(df_2017$`Vehicle Make`))
df_2017_vehicle_make<- arrange(df_2017_vehicle_make,"count",decreasing=TRUE)
head(df_2017_vehicle_make,5)
############################################################################################################################




#3. Find the (5 highest) frequency of tickets for each of the following:

############# 2015
# a. Violation Precinct 
df_2015_violation_prec <-SparkR::summarize(groupBy(df_2015, df_2015$`Violation Precinct`), count = n(df_2015$`Violation Precinct`))
head(arrange(filter(df_2015_violation_prec,df_2015_violation_prec$`Violation Precinct` != 0),"count",decreasing=TRUE),5)

#b.  Issuer Precinct
df_2015_issuer_prec <-SparkR::summarize(groupBy(df_2015, df_2015$`Issuer Precinct`), count = n(df_2015$`Issuer Precinct`))
head(arrange(filter(df_2015_issuer_prec,df_2015_issuer_prec$`Issuer Precinct` != 0),"count",decreasing=TRUE), 5)



############# 2016
# a. Violation Precinct 
df_2016_violation_prec <-SparkR::summarize(groupBy(df_2016, df_2016$`Violation Precinct`), count = n(df_2016$`Violation Precinct`))
head(arrange(filter(df_2016_violation_prec,df_2016_violation_prec$`Violation Precinct` != 0),"count",decreasing=TRUE),5)


#b.  Issuer Precinct
df_2016_issuer_prec <-SparkR::summarize(groupBy(df_2016, df_2016$`Issuer Precinct`), count = n(df_2016$`Issuer Precinct`))
head(arrange(filter(df_2016_issuer_prec,df_2016_issuer_prec$`Issuer Precinct` != 0),"count",decreasing=TRUE), 5)



############# 2017
# a. Violation Precinct 
df_2017_violation_prec <-SparkR::summarize(groupBy(df_2017, df_2017$`Violation Precinct`), count = n(df_2017$`Violation Precinct`))
head(arrange(filter(df_2017_violation_prec,df_2017_violation_prec$`Violation Precinct` != 0),"count",decreasing=TRUE),5)

#b.  Issuer Precinct
df_2017_issuer_prec <-SparkR::summarize(groupBy(df_2017, df_2017$`Issuer Precinct`), count = n(df_2017$`Issuer Precinct`))
head(arrange(filter(df_2017_issuer_prec,df_2017_issuer_prec$`Issuer Precinct` != 0),"count",decreasing=TRUE), 5)
############################################################################################################################




#4. Find the violation code frequency across three precincts which have issued the most number of tickets - do these precinct zones
#have an exceptionally high frequency of certain violation codes? Are these codes common across precincts?

### We have found the top 3 precincts which have most number of tickets in previous question ie. ( Question no: 3 above)
### We see that those precicts are 19, 18 and 14 for 2015. We use the same for solving this query. 
############# 2015
createOrReplaceTempView(df_2015, "dfv_2015")
df_viloation_code_top_precincts <-
  SparkR::sql("select  `Violation Precinct`,`Violation Code`, count(*) as frequency from dfv_2015
              where `Violation Precinct` = 19
              group by  `Violation Precinct` ,`Violation Code` 
              union all
              Select  `Violation Precinct` ,`Violation Code`, count(*) as frequency from dfv_2015
              where `Violation Precinct` = 18
              group by  `Violation Precinct` ,`Violation Code` 
              union all
              select  `Violation Precinct` ,`Violation Code`, count(*) as frequency from dfv_2015
              where `Violation Precinct` = 14
              group by  `Violation Precinct` ,`Violation Code`
              order by  `Violation Precinct`, frequency desc ")

createOrReplaceTempView(df_viloation_code_top_precincts,"df_viloation_code_top_precincts_2015")
df_viloation_code_top_precincts <- SparkR::sql("select  `Violation Precinct` ,`Violation Code`, frequency,   ROW_NUMBER() OVER (PARTITION BY  `Violation Precinct` order by  `Violation Precinct` asc, frequency desc) as rank
                                               from df_viloation_code_top_precincts_2015
                                               having rank <=5 ")
showDF(df_viloation_code_top_precincts)
head(df_viloation_code_top_precincts,15)

############# 2016
createOrReplaceTempView(df_2016, "dfv_2016")
df_viloation_code_top_precincts <-
  SparkR::sql("select  `Violation Precinct`,`Violation Code`, count(*) as frequency from dfv_2016
              where `Violation Precinct` = 19
              group by  `Violation Precinct` ,`Violation Code` 
              union all
              Select  `Violation Precinct` ,`Violation Code`, count(*) as frequency from dfv_2016
              where `Violation Precinct` = 18
              group by  `Violation Precinct` ,`Violation Code` 
              union all
              select  `Violation Precinct` ,`Violation Code`, count(*) as frequency from dfv_2016
              where `Violation Precinct` = 14
              group by  `Violation Precinct` ,`Violation Code`
              order by  `Violation Precinct`, frequency desc ")

createOrReplaceTempView(df_viloation_code_top_precincts,"df_viloation_code_top_precincts_2016")
df_viloation_code_top_precincts <- SparkR::sql("select  `Violation Precinct` ,`Violation Code`, frequency,   ROW_NUMBER() OVER (PARTITION BY  `Violation Precinct` order by  `Violation Precinct` asc, frequency desc) as rank
                                               from df_viloation_code_top_precincts_2016
                                               having rank <=5 ")
showDF(df_viloation_code_top_precincts)
head(df_viloation_code_top_precincts,15)

############# 2017
createOrReplaceTempView(df_2017, "dfv_2017")
df_viloation_code_top_precincts <-
  SparkR::sql("select  `Violation Precinct`,`Violation Code`, count(*) as frequency from dfv_2017
              where `Violation Precinct` = 19
              group by  `Violation Precinct` ,`Violation Code` 
              union all
              Select  `Violation Precinct` ,`Violation Code`, count(*) as frequency from dfv_2017
              where `Violation Precinct` = 14
              group by  `Violation Precinct` ,`Violation Code` 
              union all
              select  `Violation Precinct` ,`Violation Code`, count(*) as frequency from dfv_2017
              where `Violation Precinct` = 1
              group by  `Violation Precinct` ,`Violation Code`
              order by  `Violation Precinct`, frequency desc ")

createOrReplaceTempView(df_viloation_code_top_precincts,"df_viloation_code_top_precincts_2017")
df_viloation_code_top_precincts <- SparkR::sql("select  `Violation Precinct` ,`Violation Code`, frequency,   ROW_NUMBER() OVER (PARTITION BY  `Violation Precinct` order by  `Violation Precinct` asc, frequency desc) as rank
                                               from df_viloation_code_top_precincts_2017
                                               having rank <=5 ")
showDF(df_viloation_code_top_precincts)
head(df_viloation_code_top_precincts,15)
############################################################################################################################

#5.You’d want to find out the properties of parking violations across different times of the day:


############# 2015

#Find a way to deal with missing values, if any.
#Hint: Check for the null values using 'isNull' under the SQL. Also, to remove the null values, check the 'dropna' command in the API documentation.

# Filter and included only the non null values for this analysis
df_2015_violation_time <- SparkR::filter(df_2015, isNotNull(df_2015$`Violation Time`))


#The Violation Time field is specified in a strange format. Find a way to make this into a time attribute that you can use to divide into groups.
#Divide 24 hours into six equal discrete bins of time. The intervals you choose are at your discretion. For each of these groups, find the three most
#commonly occurring violations.
#Hint: Use the CASE-WHEN in SQL view to segregate into bins. 

# Split the Violation Time column Ex: 0945A to be interpreted as 9:45 AM
# Hence, we split the string into hour as 09 and ignore the minutes
# We use the last column 'A' or 'P' to convert time into 24 hour format
# We add 00 to the time if the last column is A( refering to AM)
# We add 12 to the time if the last column is P( refering to PM)

#Consider the hour column
df_2015_violation_time$hour<- substr(df_2015_violation_time$`Violation Time`,1,2)

#EXtract the AM or PM information
df_2015_violation_time$AMPM <- substr(df_2015_violation_time$`Violation Time`,5,5)

#Convert AM to 0 and PM to 12
df_2015_violation_time$AMPM <- regexp_replace(df_2015_violation_time$AMPM,'A','0')
df_2015_violation_time$AMPM <- regexp_replace(df_2015_violation_time$AMPM,'P','12')

#Convert time to 24 hour format
df_2015_violation_time$hour <- df_2015_violation_time$hour+df_2015_violation_time$AMPM
createOrReplaceTempView(df_2015_violation_time, "dfv_2015")

#Create 6 different bins for time of hour and group the data
#Primary Group - Time Of Day , Secondary Group - Violation Code
#Arrange in the descending order of total violation records per time of day per violation code

bins <- sql("SELECT `Violation Code`,count(`Violation Code`)as `Total Violation`, \
            CASE  WHEN ( hour >=0 and hour <= 3 ) THEN 1\
            WHEN (hour > 3  and hour <= 7) THEN 2\
            WHEN (hour > 8  and hour <= 12) THEN 3\
            WHEN (hour > 12  and hour <= 16) THEN 4\
            WHEN (hour > 16 and hour <= 20) THEN 5\
            ELSE 6 END  as timeOfDay FROM dfv_2015
            group by timeOfDay,`Violation Code`
            order by timeOfDay ASC ,`Total Violation` DESC ")

head(bins)
createOrReplaceTempView(bins, "dfv_2015_Total_Violation")

#For finding the most commonly occurring violations, a similar approach can be used as mention in the hint for question 4.
#Find the top 3 violation codes for each time of the day

df_top_3_violation_for_each_time_of_day_2015 <- sql("(select * from dfv_2015_Total_Violation where timeOfDay = 1 order by `Total Violation` DESC limit 3)
                                                    union all
                                                    (select * from dfv_2015_Total_Violation where timeOfDay = 2 order by `Total Violation` DESC limit 3)
                                                    union all
                                                    (select * from dfv_2015_Total_Violation where timeOfDay = 3 order by `Total Violation` DESC limit 3)
                                                    union all
                                                    (select * from dfv_2015_Total_Violation where timeOfDay = 4 order by `Total Violation` DESC limit 3)
                                                    union all
                                                    (select * from dfv_2015_Total_Violation where timeOfDay = 5 order by `Total Violation` DESC limit 3)
                                                    union all
                                                    (select * from dfv_2015_Total_Violation where timeOfDay = 6 order by `Total Violation` DESC limit 3)")


df_top_3_violation_for_each_time_of_day_2015$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2015$timeOfDay,"1",'Midnight')
df_top_3_violation_for_each_time_of_day_2015$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2015$timeOfDay,"2",'Earlymorning')
df_top_3_violation_for_each_time_of_day_2015$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2015$timeOfDay,"3",'Morning')
df_top_3_violation_for_each_time_of_day_2015$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2015$timeOfDay,"4",'Afternoon')
df_top_3_violation_for_each_time_of_day_2015$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2015$timeOfDay,"5",'Evening')
df_top_3_violation_for_each_time_of_day_2015$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2015$timeOfDay,"6",'Night')

head(df_top_3_violation_for_each_time_of_day_2015)

#Find time of the day for top three violation codes

bins <- sql("SELECT distinct `Violation Code`,count(`Violation Code`)as `Total Violation`, \
            CASE  WHEN ( hour >=0 and hour <= 3 ) THEN 1\
            WHEN (hour > 3  and hour <= 7) THEN 2\
            WHEN (hour > 8  and hour <= 12) THEN 3\
            WHEN (hour > 12  and hour <= 16) THEN 4\
            WHEN (hour > 16 and hour <= 20) THEN 5\
            ELSE 6 END  as timeOfDay FROM dfv_2015
            group by `Violation Code`,timeOfDay
            order by `Total Violation` DESC Limit 3 ")

bins$timeOfDay <- regexp_replace(bins$timeOfDay,"1",'Midnight')
bins$timeOfDay <- regexp_replace(bins$timeOfDay,"2",'Earlymorning')
bins$timeOfDay <- regexp_replace(bins$timeOfDay,"3",'Morning')
bins$timeOfDay <- regexp_replace(bins$timeOfDay,"4",'Afternoon')
bins$timeOfDay <- regexp_replace(bins$timeOfDay,"5",'Evening')
bins$timeOfDay <- regexp_replace(bins$timeOfDay,"6",'Night')

showDF(bins)

############# 2016

#Find a way to deal with missing values, if any.
#Hint: Check for the null values using 'isNull' under the SQL. Also, to remove the null values, check the 'dropna' command in the API documentation.

# Filter and included only the non null values for this analysis
df_2016_violation_time <- SparkR::filter(df_2016, isNotNull(df_2016$`Violation Time`))


#The Violation Time field is specified in a strange format. Find a way to make this into a time attribute that you can use to divide into groups.
#Divide 24 hours into six equal discrete bins of time. The intervals you choose are at your discretion. For each of these groups, find the three most
#commonly occurring violations.
#Hint: Use the CASE-WHEN in SQL view to segregate into bins. 

# Split the Violation Time column Ex: 0945A to be interpreted as 9:45 AM
# Hence, we split the string into hour as 09 and ignore the minutes
# We use the last column 'A' or 'P' to convert time into 24 hour format
# We add 00 to the time if the last column is A( refering to AM)
# We add 12 to the time if the last column is P( refering to PM)

#Consider the hour column
df_2016_violation_time$hour<- substr(df_2016_violation_time$`Violation Time`,1,2)

#EXtract the AM or PM information
df_2016_violation_time$AMPM <- substr(df_2016_violation_time$`Violation Time`,5,5)

#Convert AM to 0 and PM to 12
df_2016_violation_time$AMPM <- regexp_replace(df_2016_violation_time$AMPM,'A','0')
df_2016_violation_time$AMPM <- regexp_replace(df_2016_violation_time$AMPM,'P','12')

#Convert time to 24 hour format
df_2016_violation_time$hour <- df_2016_violation_time$hour+df_2016_violation_time$AMPM
createOrReplaceTempView(df_2016_violation_time, "dfv_2016")

#Create 6 different bins for time of hour and group the data
#Primary Group - Time Of Day , Secondary Group - Violation Code
#Arrange in the descending order of total violation records per time of day per violation code

bins <- sql("SELECT `Violation Code`,count(`Violation Code`)as `Total Violation`, \
            CASE  WHEN ( hour >=0 and hour <= 3 ) THEN 1\
            WHEN (hour > 3  and hour <= 7) THEN 2\
            WHEN (hour > 8  and hour <= 12) THEN 3\
            WHEN (hour > 12  and hour <= 16) THEN 4\
            WHEN (hour > 16 and hour <= 20) THEN 5\
            ELSE 6 END  as timeOfDay FROM dfv_2016
            group by timeOfDay,`Violation Code`
            order by timeOfDay ASC ,`Total Violation` DESC ")

head(bins)
createOrReplaceTempView(bins, "dfv_2016_Total_Violation")

#For finding the most commonly occurring violations, a similar approach can be used as mention in the hint for question 4.
#Find the top 3 violation codes for each time of the day

df_top_3_violation_for_each_time_of_day_2016 <- sql("(select * from dfv_2016_Total_Violation where timeOfDay = 1 order by `Total Violation` DESC limit 3)
                                                    union all
                                                    (select * from dfv_2016_Total_Violation where timeOfDay = 2 order by `Total Violation` DESC limit 3)
                                                    union all
                                                    (select * from dfv_2016_Total_Violation where timeOfDay = 3 order by `Total Violation` DESC limit 3)
                                                    union all
                                                    (select * from dfv_2016_Total_Violation where timeOfDay = 4 order by `Total Violation` DESC limit 3)
                                                    union all
                                                    (select * from dfv_2016_Total_Violation where timeOfDay = 5 order by `Total Violation` DESC limit 3)
                                                    union all
                                                    (select * from dfv_2015_Total_Violation where timeOfDay = 6 order by `Total Violation` DESC limit 3)")


df_top_3_violation_for_each_time_of_day_2016$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2016$timeOfDay,"1",'Midnight')
df_top_3_violation_for_each_time_of_day_2016$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2016$timeOfDay,"2",'Earlymorning')
df_top_3_violation_for_each_time_of_day_2016$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2016$timeOfDay,"3",'Morning')
df_top_3_violation_for_each_time_of_day_2016$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2016$timeOfDay,"4",'Afternoon')
df_top_3_violation_for_each_time_of_day_2016$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2016$timeOfDay,"5",'Evening')
df_top_3_violation_for_each_time_of_day_2016$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2016$timeOfDay,"6",'Night')

head(df_top_3_violation_for_each_time_of_day_2016)

#Find time of the day for top three violation codes

bins <- sql("SELECT distinct `Violation Code`,count(`Violation Code`)as `Total Violation`, \
            CASE  WHEN ( hour >=0 and hour <= 3 ) THEN 1\
            WHEN (hour > 3  and hour <= 7) THEN 2\
            WHEN (hour > 8  and hour <= 12) THEN 3\
            WHEN (hour > 12  and hour <= 16) THEN 4\
            WHEN (hour > 16 and hour <= 20) THEN 5\
            ELSE 6 END  as timeOfDay FROM dfv_2016
            group by `Violation Code`,timeOfDay
            order by `Total Violation` DESC Limit 3 ")

bins$timeOfDay <- regexp_replace(bins$timeOfDay,"1",'Midnight')
bins$timeOfDay <- regexp_replace(bins$timeOfDay,"2",'Earlymorning')
bins$timeOfDay <- regexp_replace(bins$timeOfDay,"3",'Morning')
bins$timeOfDay <- regexp_replace(bins$timeOfDay,"4",'Afternoon')
bins$timeOfDay <- regexp_replace(bins$timeOfDay,"5",'Evening')
bins$timeOfDay <- regexp_replace(bins$timeOfDay,"6",'Night')

showDF(bins)

############# 2017

#Find a way to deal with missing values, if any.
#Hint: Check for the null values using 'isNull' under the SQL. Also, to remove the null values, check the 'dropna' command in the API documentation.

# Filter and included only the non null values for this analysis
df_2017_violation_time <- SparkR::filter(df_2017, isNotNull(df_2017$`Violation Time`))


#The Violation Time field is specified in a strange format. Find a way to make this into a time attribute that you can use to divide into groups.
#Divide 24 hours into six equal discrete bins of time. The intervals you choose are at your discretion. For each of these groups, find the three most
#commonly occurring violations.
#Hint: Use the CASE-WHEN in SQL view to segregate into bins. 

# Split the Violation Time column Ex: 0945A to be interpreted as 9:45 AM
# Hence, we split the string into hour as 09 and ignore the minutes
# We use the last column 'A' or 'P' to convert time into 24 hour format
# We add 00 to the time if the last column is A( refering to AM)
# We add 12 to the time if the last column is P( refering to PM)

#Consider the hour column
df_2017_violation_time$hour<- substr(df_2017_violation_time$`Violation Time`,1,2)

#EXtract the AM or PM information
df_2017_violation_time$AMPM <- substr(df_2017_violation_time$`Violation Time`,5,5)

#Convert AM to 0 and PM to 12
df_2017_violation_time$AMPM <- regexp_replace(df_2017_violation_time$AMPM,'A','0')
df_2017_violation_time$AMPM <- regexp_replace(df_2017_violation_time$AMPM,'P','12')

#Convert time to 24 hour format
df_2017_violation_time$hour <- df_2017_violation_time$hour+df_2017_violation_time$AMPM
createOrReplaceTempView(df_2017_violation_time, "dfv_2017")

#Create 6 different bins for time of hour and group the data
#Primary Group - Time Of Day , Secondary Group - Violation Code
#Arrange in the descending order of total violation records per time of day per violation code

bins <- sql("SELECT `Violation Code`,count(`Violation Code`)as `Total Violation`, \
            CASE  WHEN ( hour >=0 and hour <= 3 ) THEN 1\
            WHEN (hour > 3  and hour <= 7) THEN 2\
            WHEN (hour > 8  and hour <= 12) THEN 3\
            WHEN (hour > 12  and hour <= 16) THEN 4\
            WHEN (hour > 16 and hour <= 20) THEN 5\
            ELSE 6 END  as timeOfDay FROM dfv_2017
            group by timeOfDay,`Violation Code`
            order by timeOfDay ASC ,`Total Violation` DESC ")

head(bins)
createOrReplaceTempView(bins, "dfv_2017_Total_Violation")

#For finding the most commonly occurring violations, a similar approach can be used as mention in the hint for question 4.
#Find the top 3 violation codes for each time of the day

df_top_3_violation_for_each_time_of_day_2017 <- sql("(select * from dfv_2017_Total_Violation where timeOfDay = 1 order by `Total Violation` DESC limit 3)
                                                    union all
                                                    (select * from dfv_2017_Total_Violation where timeOfDay = 2 order by `Total Violation` DESC limit 3)
                                                    union all
                                                    (select * from dfv_2017_Total_Violation where timeOfDay = 3 order by `Total Violation` DESC limit 3)
                                                    union all
                                                    (select * from dfv_2017_Total_Violation where timeOfDay = 4 order by `Total Violation` DESC limit 3)
                                                    union all
                                                    (select * from dfv_2017_Total_Violation where timeOfDay = 5 order by `Total Violation` DESC limit 3)
                                                    union all
                                                    (select * from dfv_2015_Total_Violation where timeOfDay = 6 order by `Total Violation` DESC limit 3)")


df_top_3_violation_for_each_time_of_day_2017$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2017$timeOfDay,"1",'Midnight')
df_top_3_violation_for_each_time_of_day_2017$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2017$timeOfDay,"2",'Earlymorning')
df_top_3_violation_for_each_time_of_day_2017$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2017$timeOfDay,"3",'Morning')
df_top_3_violation_for_each_time_of_day_2017$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2017$timeOfDay,"4",'Afternoon')
df_top_3_violation_for_each_time_of_day_2017$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2017$timeOfDay,"5",'Evening')
df_top_3_violation_for_each_time_of_day_2017$timeOfDay <- regexp_replace(df_top_3_violation_for_each_time_of_day_2017$timeOfDay,"6",'Night')

head(df_top_3_violation_for_each_time_of_day_2017)

#Find time of the day for top three violation codes

bins <- sql("SELECT distinct `Violation Code`,count(`Violation Code`)as `Total Violation`, \
            CASE  WHEN ( hour >=0 and hour <= 3 ) THEN 1\
            WHEN (hour > 3  and hour <= 7) THEN 2\
            WHEN (hour > 8  and hour <= 12) THEN 3\
            WHEN (hour > 12  and hour <= 16) THEN 4\
            WHEN (hour > 16 and hour <= 20) THEN 5\
            ELSE 6 END  as timeOfDay FROM dfv_2017
            group by `Violation Code`,timeOfDay
            order by `Total Violation` DESC Limit 3 ")

bins$timeOfDay <- regexp_replace(bins$timeOfDay,"1",'Midnight')
bins$timeOfDay <- regexp_replace(bins$timeOfDay,"2",'Earlymorning')
bins$timeOfDay <- regexp_replace(bins$timeOfDay,"3",'Morning')
bins$timeOfDay <- regexp_replace(bins$timeOfDay,"4",'Afternoon')
bins$timeOfDay <- regexp_replace(bins$timeOfDay,"5",'Evening')
bins$timeOfDay <- regexp_replace(bins$timeOfDay,"6",'Night')

showDF(bins)

############################################################################################################################

#6.First, divide the year into some number of seasons, and 
## 1. Find frequencies of tickets for each season. (Hint: Use Issue Date to segregate into seasons)
## 2. Then, find the three most common violations for each of these seasons.

############# 2015
df_2015$`Issue Month`  <- month(SparkR::to_date(df_2015$`Issue Date`,format ="MM/dd/yyyy"))

createOrReplaceTempView(df_2015, "dfv2015")
df_segregated_seasons_2015 <- SparkR::sql("SELECT 
                                          CASE  WHEN(`Issue Month`>=4 and `Issue Month` <= 6 ) THEN 1 \
                                          WHEN ( `Issue Month` >=7 and `Issue Month` <= 9) THEN 2 \
                                          WHEN ( `Issue Month` >=10 and `Issue Month` <= 12 ) THEN 3 \
                                          ELSE  4 END  as season, `Violation Code`,count(`Violation Code`)as `Total Violation`  FROM dfv2015
                                          group by season,`Violation Code`
                                          order by season asc, `Total Violation` desc")

createOrReplaceTempView(df_segregated_seasons_2015, "binv2015")

df_top_violation_perseason_2015 <- SparkR::sql("select season, `Violation Code`,`Total Violation`,  ROW_NUMBER() OVER (PARTITION BY season order by season asc,`Total Violation` desc) as rank
                                               from binv2015
                                               having rank <=3")

cast(df_top_violation_perseason_2015$season, "string")
df_top_violation_perseason_2015$season <- regexp_replace(df_top_violation_perseason_2015$season,"1",'Summer')
df_top_violation_perseason_2015$season <- regexp_replace(df_top_violation_perseason_2015$season,"2",'Rainy')
df_top_violation_perseason_2015$season <- regexp_replace(df_top_violation_perseason_2015$season,"3",'Autumn')
df_top_violation_perseason_2015$season <- regexp_replace(df_top_violation_perseason_2015$season,"4",'Winter')

showDF(df_top_violation_perseason_2015)


############# 2016
df_2016$`Issue Month`  <- month(SparkR::to_date(df_2016$`Issue Date`,format ="MM/dd/yyyy"))

createOrReplaceTempView(df_2016, "dfv2016")
df_segregated_seasons_2016 <- SparkR::sql("SELECT 
                                          CASE  WHEN(`Issue Month`>=4 and `Issue Month` <= 6 ) THEN 1 \
                                          WHEN ( `Issue Month` >=7 and `Issue Month` <= 9) THEN 2 \
                                          WHEN ( `Issue Month` >=10 and `Issue Month` <= 12 ) THEN 3 \
                                          ELSE  4 END  as season, `Violation Code`,count(`Violation Code`)as `Total Violation`  FROM dfv2016
                                          group by season,`Violation Code`
                                          order by season asc, `Total Violation` desc")

#Then, find the three most common violations for each of these seasons.
#(Hint: A similar approach can be used as mention in the hint for question 4.)

createOrReplaceTempView(df_segregated_seasons_2016, "binv2016")

df_top_violation_perseason_2016 <- SparkR::sql("select season, `Violation Code`,`Total Violation`,  ROW_NUMBER() OVER (PARTITION BY season order by season asc,`Total Violation` desc) as rank
                                               from binv2016
                                               having rank <=3")


cast(df_top_violation_perseason_2016$season, "string")
df_top_violation_perseason_2016$season <- regexp_replace(df_top_violation_perseason_2016$season,"1",'Summer')
df_top_violation_perseason_2016$season <- regexp_replace(df_top_violation_perseason_2016$season,"2",'Rainy')
df_top_violation_perseason_2016$season <- regexp_replace(df_top_violation_perseason_2016$season,"3",'Autumn')
df_top_violation_perseason_2016$season <- regexp_replace(df_top_violation_perseason_2016$season,"4",'Winter')

showDF(df_top_violation_perseason_2016)



############# 2017
df_2017$`Issue Month`  <- month(SparkR::to_date(df_2017$`Issue Date`,format ="MM/dd/yyyy"))

createOrReplaceTempView(df_2017, "dfv2017")
df_segregated_seasons_2017 <- SparkR::sql("SELECT 
                                          CASE  WHEN(`Issue Month`>=4 and `Issue Month` <= 6 ) THEN 1 \
                                          WHEN ( `Issue Month` >=7 and `Issue Month` <= 9) THEN 2 \
                                          WHEN ( `Issue Month` >=10 and `Issue Month` <= 12 ) THEN 3 \
                                          ELSE  4 END  as season, `Violation Code`,count(`Violation Code`)as `Total Violation`  FROM dfv2017
                                          group by season,`Violation Code`
                                          order by season asc, `Total Violation` desc")

#Then, find the three most common violations for each of these seasons.
#(Hint: A similar approach can be used as mention in the hint for question 4.)

createOrReplaceTempView(df_segregated_seasons_2017, "binv2017")

df_top_violation_perseason_2017 <- SparkR::sql("select season, `Violation Code`,`Total Violation`,  ROW_NUMBER() OVER (PARTITION BY season order by season asc,`Total Violation` desc) as rank
                                               from binv2017
                                               having rank <=3")

cast(df_top_violation_perseason_2017$season, "string")
df_top_violation_perseason_2017$season <- regexp_replace(df_top_violation_perseason_2017$season,"1",'Summer')
df_top_violation_perseason_2017$season <- regexp_replace(df_top_violation_perseason_2017$season,"2",'Rainy')
df_top_violation_perseason_2017$season <- regexp_replace(df_top_violation_perseason_2017$season,"3",'Autumn')
df_top_violation_perseason_2017$season <- regexp_replace(df_top_violation_perseason_2017$season,"4",'Winter')

showDF(df_top_violation_perseason_2017)


############################################################################################################################                

#7.The fines collected from all the parking violation constitute a revenue source for the NYC police department. Let’s take an example of estimating that for the 
#three most commonly occurring codes.

############# 2015
createOrReplaceTempView(df_2015, "dfv2015") 

#Find total occurrences of the three most common violation codes            
df_fine <- SparkR::sql("SELECT `Violation Code`,count(*)as `Total Violation` \
                       from dfv2015
                       group by `Violation Code`
                       order by `Total Violation` DESC
                       Limit 3")

df_fine <- data.frame(head(df_fine))
head(df_fine)

#Then, visit the website:
#http://www1.nyc.gov/site/finance/vehicles/services-violation-codes.page
#It lists the fines associated with different violation codes. They’re divided into two categories, one for the highest-density locations of the city, the other for the rest of the city. #For simplicity, take an average of the two.

df_fee_r <- data.frame(Fee=c(55,50,115))
df_fine <- cbind(df_fine,df_fee_r)
df_fine$`Total_Fine` <- df_fine$`Total.Violation` * df_fine$`Fee`
head(df_fine)

#Using this information, find the total amount collected for the three violation codes with maximum tickets. State the code which
# has the highest total collection.
head(df_fine[order(-df_fine$Total_Fine),],1)

############# 2016
createOrReplaceTempView(df_2016, "dfv2016") 

#Find total occurrences of the three most common violation codes            
df_fine <- SparkR::sql("SELECT `Violation Code`,count(*)as `Total Violation` \
                       from dfv2016
                       group by `Violation Code`
                       order by `Total Violation` DESC
                       Limit 3")

df_fine <- data.frame(head(df_fine))
head(df_fine)

#Then, visit the website:
#http://www1.nyc.gov/site/finance/vehicles/services-violation-codes.page
#It lists the fines associated with different violation codes. They’re divided into two categories, one for the highest-density locations of the city, the other for the rest of the city. #For simplicity, take an average of the two.

df_fee_r <- data.frame(Fee=c(55,50,50))
df_fine <- cbind(df_fine,df_fee_r)
df_fine$`Total_Fine` <- df_fine$`Total.Violation` * df_fine$`Fee`
head(df_fine)

#Using this information, find the total amount collected for the three violation codes with maximum tickets. State the code which
# has the highest total collection.
head(df_fine[order(-df_fine$Total_Fine),],1)

############# 2017
createOrReplaceTempView(df_2017, "dfv2017") 

#Find total occurrences of the three most common violation codes            
df_fine <- SparkR::sql("SELECT `Violation Code`,count(*)as `Total Violation` \
                        from dfv2017
                        group by `Violation Code`
                        order by `Total Violation` DESC
                        Limit 3")

df_fine <- data.frame(head(df_fine))
head(df_fine)

#Then, visit the website:
#http://www1.nyc.gov/site/finance/vehicles/services-violation-codes.page
#It lists the fines associated with different violation codes. They’re divided into two categories, one for the highest-density locations of the city, the other for the rest of the city. #For simplicity, take an average of the two.

df_fee_r <- data.frame(Fee=c(55,50,50))
df_fine <- cbind(df_fine,df_fee_r)
df_fine$`Total_Fine` <- df_fine$`Total.Violation` * df_fine$`Fee`
head(df_fine)

#Using this information, find the total amount collected for the three violation codes with maximum tickets. State the code which
# has the highest total collection.
head(df_fine[order(-df_fine$Total_Fine),],1)


############################################################################################################################ 
