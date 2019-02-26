# Install
#install.packages("dplyr")
#install.packages("tidyr")

#Load Deployer library to use functions available in the same.
library("dplyr")
library(tidyr)


############################## Checkpoint 1  #################################################


#Load the companies and rounds data (provided on the previous page) into two data frames
#and name them companies and rounds2 respectively.


companies <- read.delim("companies.txt", header = TRUE, sep = "\t",na.strings=c(""," ","NA"),stringsAsFactors = FALSE)
rounds2   <- read.csv("rounds2.csv",header=TRUE)




### We see two companies having same permalink but in different case, hence assuming companies with same permalink but different
### cases to be same companies, we first convert everything to a common case, as unique/distinct functions differentiates based on case.
rounds2$company_permalink <-  tolower(rounds2$company_permalink)



#How many unique companies are present in rounds2?
### Unique returns a vector while distinct returns df, and we need vector for the match() function used next, hence we chose unique
rounds2_distinct          <-  unique(rounds2$company_permalink)    
length(rounds2_distinct)      # Find number of entries


#How many unique companies are present in companies?
companies$permalink <- tolower(companies$permalink)
companies_distinct  <- distinct(companies,permalink)    
nrow(companies_distinct)

#In the companies data frame, which column can be used as the unique key for each company? Write the name of the column
#permalink


#Are there any companies in the rounds2 file which are not present in companies? Answer yes or no:
result <- match(rounds2_distinct, companies$permalink)
print(anyNA(result))


#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. 
#Name the merged frame master_frame. How many observations are present in master_frame?

#permalink is unique ID in companies file. Also we have seen that all companies in round2 are present in companies.Hence
#following merge is safe and the number of rows should be same as that in round2
master_frame <- merge(rounds2, companies, by.x="company_permalink", by.y="permalink")
nrow(master_frame)






############################## Checkpoint 2  #################################################

#Calculate the average investment amount for each of the four funding types (venture, angel, seed, and private equity) 
#and report the answers in Table 2.1


df_with_angel_investment    <- master_frame[master_frame$funding_round_type =="angel",]
df_with_seed_investment     <- master_frame[master_frame$funding_round_type =="seed",]
df_with_venture_investment  <- master_frame[master_frame$funding_round_type =="venture",]
df_with_equity_investment   <- master_frame[master_frame$funding_round_type =="private_equity",]



mean_venture  <- mean(df_with_venture_investment$raised_amount_usd,na.rm = TRUE)
mean_angel    <- mean(df_with_angel_investment$raised_amount_usd,na.rm = TRUE)
mean_seed     <- mean(df_with_seed_investment$raised_amount_usd,na.rm = TRUE) 
mean_equity   <- mean(df_with_equity_investment$raised_amount_usd,na.rm = TRUE)

#Based on the average investment amount calculated above, which investment type do you think is the most suitable for 
#Spark Funds?
#venture

############################## Checkpoint 3  #################################################

#Spark Funds wants to see the top nine countries which have received the highest total funding
#(across ALL sectors for the chosen investment type)
#For the chosen investment type, make a data frame named top9 with the top nine countries 
#(based on the total investment amount each country has received)


#For few companies, the country code is not mentioned, so we have marked such rows as NA while importing.
#So we get "ONLY" those rows where country code is specified and group by Country Code.
df_with_venture_investment <- df_with_venture_investment[!is.na(df_with_venture_investment$country_code),]
df_with_venture_investment <- group_by(df_with_venture_investment, country_code)
top9 <- (summarize(df_with_venture_investment, raised_amount_usd=sum(raised_amount_usd, na.rm = TRUE)))
top9 <- top9[order(top9$raised_amount_usd, decreasing = TRUE),]
top9 <- top9[1:9,]




#>Identify the top three English-speaking countries in the data frame top9.
#USA
#UK
#India




############################## Checkpoint 4  #################################################


#Extract the primary sector of each category list from the category_list column
#Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors 
#(Note that 'Others' is also considered one of the main sectors)


sector  <- read.csv("mapping.csv",header=TRUE)

#We use gather function to convert wide to long format.
newdata <- gather(sector,main_sector,category_data,c(2:10))

#We now only want those rows where category_data is 1, which means that is the main sector for the given primary sector.
newdata <- newdata[newdata$category_data == 1,c(1:2),]

#For the main sector in companies in the format A|B|C|D, we need to treat A as primary sector, so we rename it accordingly
#So that helps us for the merge function used next.
df_with_venture_investment$category_list<-sapply(strsplit(df_with_venture_investment$category_list,"|",fixed=TRUE),"[", 1)
df_with_venture_investment <- merge(df_with_venture_investment, newdata, by ="category_list", all.x = TRUE)






############################## Checkpoint 5  #################################################

#Now, the aim is to find out the most heavily invested main sectors in each of the three countries 
#(for funding type FT and investments range of 5-15 M USD).Create three separate data frames D1, D2 
#and D3 for each of the three  countries containing the observations of funding type FT falling 
#within the 5-15 million
#USD range. The three data frames should contain:
#All the columns of the master_frame along with the primary sector and the main sector
#The total number (or count) of investments for each main sector in a separate column
#The total amount invested in each main sector in a separate column


#Below command returns all the rows where investment is between 5-15 millon for all the countries.
#We then select each country selectively and create 3 data frames namely D1,D2,D3.
df_with_venture_investment <- subset(df_with_venture_investment,raised_amount_usd>=5000000 & raised_amount_usd <=15000000)
D1 <- df_with_venture_investment[df_with_venture_investment$country_code == "USA",]
D2 <- df_with_venture_investment[df_with_venture_investment$country_code == "GBR",]
D3 <- df_with_venture_investment[df_with_venture_investment$country_code == "IND",]

#We remove all rows where main sector values are NA's because we want to get investment trends in 8 primary sectors.
D1 <- D1[!is.na(D1$main_sector),]
D2 <- D2[!is.na(D2$main_sector),]
D3 <- D3[!is.na(D3$main_sector),]


D1       <- group_by(D1,main_sector)
count1   <- summarize(D1, nrow = n(),total_invest=sum(raised_amount_usd,na.rm = TRUE))
count1   <- count1[order(count1$nrow,decreasing = TRUE),]

D2       <- group_by(D2,main_sector)
count2   <- summarize(D2, nrow = n(),total_invest=sum(raised_amount_usd,na.rm = TRUE))
count2   <- count2[order(count2$nrow,decreasing = TRUE),]

D3       <- group_by(D3,main_sector)
count3   <- summarize(D3, nrow = n(),total_invest=sum(raised_amount_usd,na.rm = TRUE))
count3   <- count3[order(count3$nrow,decreasing = TRUE),]

################################################################################################
##############################Get table values for 5.1##########################################
################################################################################################
# 1. Total number of investments (count)
# 2. Total amount of investment (USD) 
# 3. Top sector (based on count of investments)
# 4. Second-best sector (based on count of investments)
# 5. Third-best sector (based on count of investments)
# 6. Number of investments in the top sector (refer to point 3)
# 7. Number of investments in the second-best sector (refer to point 4)
# 8. Number of investments in the third-best sector (refer to point 5)
# 9. For the top sector count-wise (point 3), which company received the highest investment?
# 10.For the second-best sector count-wise (point 4), which company received the highest investment?
#####################################################################################################

#   cat(sprintf("<set name=\"%s\" value=\"%f\" ></set>\n", df$timeStamp, df$Price))
        print("                                                  |       USA     |    UK      | INDIA ")
print(sprintf("Total number of investments (count)               |       %s      |     %s     | %s  |",sum(count1$nrow),sum(count2$nrow),sum(count3$nrow)))
print(sprintf("Total amount of investment (USD)                  |       %s      |     %s     | %s  |",sum(count1$total_invest),sum(count2$total_invest),sum(count3$total_invest)))
print(sprintf("Top sector (based on count of investments)        |       %s      |     %s     | %s  |",count1[1,1],count2[1,1],count3[1,1]))
print(sprintf("Second-best sector (based on count of investments)|       %s      |     %s     | %s  |",count1[2,1],count2[2,1],count3[2,1]))
print(sprintf("Third-best sector (based on count of investments) |       %s      |     %s     | %s  |",count1[3,1],count2[3,1],count3[3,1]))
print(sprintf("No of investments in the top sector(point 3)      |       %s      |     %s     | %s  |",count1[1,2],count2[1,2],count3[1,2]))
print(sprintf("No of investments in the second sector(point 4)   |       %s      |     %s     | %s  |",count1[2,2],count2[2,2],count3[2,2]))
print(sprintf("No of investments in the third sector (point 5)   |       %s      |     %s     | %s  |",count1[3,2],count2[3,2],count3[3,2]))

invisible(ungroup(D1))
invisible(ungroup(D2))
invisible(ungroup(D3))

US_invest_top <- group_by(D1[which(D1$main_sector ==  as.character(count1[1,1])),],company_permalink)
US_invest_top <- summarize(US_invest_top, total_invest_per_company = sum(raised_amount_usd,na.rm = TRUE))
US_invest_top <- US_invest_top[order(US_invest_top$total_invest_per_company,decreasing = TRUE),]
US_top        <- companies[which(companies$permalink == as.character(US_invest_top[1,1])),]
US_top_name   <- US_top$name[1]

US_invest_second <- group_by(D1[which(D1$main_sector ==  as.character(count1[2,1])),],company_permalink)
US_invest_second <- summarize(US_invest_second, total_invest_per_company = sum(raised_amount_usd,na.rm = TRUE))
US_invest_second <- US_invest_second[order(US_invest_second$total_invest_per_company,decreasing = TRUE),]
US_second        <- companies[which(companies$permalink == as.character(US_invest_second[1,1])),]
US_second_name   <- US_second$name[1]


UK_invest_top <- group_by(D2[which(D2$main_sector ==  as.character(count2[1,1])),],company_permalink)
UK_invest_top <- summarize(UK_invest_top, total_invest_per_company = sum(raised_amount_usd,na.rm = TRUE))
UK_invest_top <- UK_invest_top[order(UK_invest_top$total_invest_per_company,decreasing = TRUE),]
Uk_top        <- companies[which(companies$permalink == as.character(UK_invest_top[1,1])),]
Uk_top_name   <- Uk_top$name[1]

UK_invest_second <- group_by(D2[which(D2$main_sector ==  as.character(count2[2,1])),],company_permalink)
UK_invest_second <- summarize(UK_invest_second, total_invest_per_company = sum(raised_amount_usd,na.rm = TRUE))
UK_invest_second <- UK_invest_second[order(UK_invest_second$total_invest_per_company,decreasing = TRUE),]
Uk_second        <- companies[which(companies$permalink == as.character(UK_invest_second[1,1])),]
Uk_second_name   <- Uk_second$name[1]


IND_invest_top <- group_by(D3[which(D3$main_sector ==  as.character(count3[1,1])),],company_permalink)
IND_invest_top <- summarize(IND_invest_top, nrows=n(),total_invest_per_company = sum(raised_amount_usd,na.rm = TRUE))
IND_invest_top <- IND_invest_top[order(IND_invest_top$total_invest_per_company,decreasing = TRUE),]
IND_top        <- companies[which(companies$permalink == as.character(IND_invest_top[1,1])),]
IND_top_name   <- IND_top$name[1]

IND_invest_second <- group_by(D3[which(D3$main_sector ==  as.character(count3[2,1])),],company_permalink)
IND_invest_second <- summarize(IND_invest_second, total_invest_per_company = sum(raised_amount_usd,na.rm = TRUE))
IND_invest_second <- IND_invest_second[order(IND_invest_second$total_invest_per_company,decreasing = TRUE),]
IND_second        <- companies[which(companies$permalink == as.character(IND_invest_second[1,1])),]
IND_second_name   <- IND_second$name[1]


print(sprintf("For the top sector    the company received the highest investment?   |       %s      |     %s     | %s  |",US_top_name,Uk_top_name,IND_top_name))
print(sprintf("For the second sector the company received the highest investment?   |       %s      |     %s     | %s  |",US_second_name ,Uk_second_name,IND_second_name))
