######     EDA Case Study  #######



## Load required packages ##
library(lubridate)        # Date and time formatting #
library(ggplot2)          # Plotting #
library(dplyr)            # For grouping operations #
library(tidyr)            # For grouping operations #
library(ggthemes)



## Load the data set ##
df <- read.csv("loan.csv", na.strings=c("n/a","NA"))



###################################### Data cleaning ######################################################

### 1. Remove unnecessary columns ### 

##columns were removed basis on following criteria ##
# 1.unique values or keys (Eg: id,member_id)
# 2.Future variables (Eg:last_paymnt_date)
# 3.Single valued variables(Ex:application_type)
# 4.Missing Values(Ex:num_actv_bc_tl)
# 5.High % of NA values
# 6.Delete duplicate columns(Ex:total_pymnt,total_pymnt_inv)
# 7.Irrelevant columns(Ex: url)
######################################################

df <- df[,c("loan_amnt", "term", "int_rate","installment","grade","emp_length","home_ownership","annual_inc", "issue_d","loan_status","purpose", "dti","delinq_2yrs", "earliest_cr_line", "inq_last_6mths", "mths_since_last_delinq","open_acc","pub_rec","revol_util", "total_rec_prncp","total_rec_int","addr_state")]

#write.csv(df, "Loan_cleaned.csv")
str(df)

## Find all column names with NA Values ##
names(df)[sapply(df, anyNA)]             #   emp_length and mths_since_last_delinq, leave it as it is.



## Format values to proper format       ##
df$term       <- sapply(df$term,       function(x) gsub("months","", as.character(x)))  # Remove "months" everywhere as we want to evaluate it as number
df$int_rate   <- sapply(df$int_rate,   function(x) gsub("%","", as.character(x)))
df$revol_util <- sapply(df$int_rate,   function(x) gsub("%","", as.character(x)))
df$emp_length <- sapply(df$emp_length, function(x) gsub("years*", "",x))
df$emp_length <- sapply(df$emp_length, function(x) gsub("< 1", "0",x))
df$emp_length <- sapply(df$emp_length, function(x) gsub("\\+", "",x))




## Split the date to month and year
df$issue_d             <- parse_date_time(df$issue_d ,tz="Asia/Calcutta" , c('%b-%y'))
df$issue_month         <- month(df$issue_d)
df$issue_year          <- year(df$issue_d)

df$earliest_cr_line    <- parse_date_time(df$earliest_cr_line ,tz="Asia/Calcutta" , c('%b-%y'))
df$early_cr_line_month <- month(df$earliest_cr_line)
df$early_cr_line_year  <- year(df$earliest_cr_line)

df$issue_d             <- NULL
df$earliest_cr_line    <- NULL

df$percent_of_income_as_installment <- df$installment/(df$annual_inc/12)*100



##################################PLOT BEGINS ######################################################

##------------------------------------------------------------------------------------------------
##       Plot1 : Pattern Analysis of loans across years
##------------------------------------------------------------------------------------------------


## 1. Find what is the % of loans that are defaulting overall ##

df_by_loan_status <- df%>%group_by(loan_status)%>%summarise(total=n())%>% mutate (percent = total/sum(total)*100)
ggplot(df_by_loan_status, aes(x=loan_status, y=percent))  +                                                                
  labs(x="Status of loans issued",y="% of loans in each category  ",title="Spread of Loans by status ") +    
  geom_col(fill="steelblue")  +
  geom_text(aes(label= sprintf("%0.2f", round(percent, digits = 2))), vjust=-0.25,position = position_dodge(0), size=3)



# 2. Find the trend of the loans across years##

# For a loan issued in particular  year, what % of loans have been "Fully Paid" to that "Charged Off"

df_loan_by_year <- df %>% group_by(issue_year,loan_status) %>% summarise(Total = n()) %>% ungroup(df_by_loan_year)
df_loan_by_year <- df_loan_by_year %>% group_by(issue_year)%>% mutate (percent = Total/sum(Total)) %>% filter(loan_status == "Charged Off" | loan_status == "Fully Paid")
ggplot(df_loan_by_year, aes(x=loan_status, y=percent))  +                                                                
  labs(x="Loan status (per year)",y="% of loans in each category  ",title="Spread of Loans by status in each year") +    
  geom_col(fill="steelblue")  +
  geom_text(aes(label= sprintf("%0.2f", round(percent, digits = 2))), vjust=-0.25,position = position_dodge(0), size=3)+
  facet_grid(. ~ issue_year)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


########################################### Analysis ###################################################
## 1. We see that across the years, we see around 14.17 % of loans have defaulted. 
## 2. There is no upward trend in loans being charged-off across years.
## 3. As we see similar loan pattern across years, for further analysis we needn't dig into particular year specifically, but rather see the impact of the different variables on the status collectively, that is across all years.
########################################### Analysis ###################################################







##------------------------------------------------------------------------------------------------
##       Plot2 : Purpose  Analysis : 
##-------------------------------------------------------------------------------------------------

# Based on the purpose, we find the patterns of default as follows:
#
# 1. Of the total loans sanctioned,  loans lended for which purpose have defaulted the most
#    ---> This provides, for  which "purpose", there is maximum count of loan charge off's.   

#Filter the data set to analyse on the defaulter profiles

df_charged_off <- df[which(df$loan_status == "Charged Off"),]


ggplot(df_charged_off,aes(x=purpose))+
  geom_bar(stat = "count", position="dodge", fill="steelblue") +
  labs(x = "Purpose of Loan ", y = "Count of Charged off cases",title="Spread of  'Charged-off '  loans v/s 'Purpose'") +facet_grid(issue_year~ .)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#
# 2. Of the total loans issued, which "Purpose" has high charged-off/Total Sanctioned ratio.
#     ---> This provides of the loans taken, in which "Purpose",  borrowers are more likely to default.
#

df_t <- df %>% group_by(purpose,loan_status) %>% summarise(Total = n()) %>% ungroup(df_t)
df_t <- df_t %>% group_by(purpose)%>% mutate (percent = Total/sum(Total)*100)
df_t <- df_t[which(df_t$loan_status == "Charged Off"),]
ggplot(df_t, aes(x=purpose, y=percent))  +                                                                
  labs(x="Purpose  for Loan",y="% Loans defaulted (ie.count of loan charged-off/ Total numberof loan sanctioned)  ",title="Spread of Loan status for each Purpose") +
  geom_col(fill="steelblue")  +
  geom_text(aes(label= sprintf("%0.2f", round(percent, digits = 2))), vjust=-0.25,position = position_dodge(0), size=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#
# 3. Of the total loans issued, we find for which "Purpose", the sum of loan amount 
#    defaulted is maximum
#    --> This provides out of charged-off loans, money lent  for which "Purpose" is leading to major loss of money for the bank

df_by_total_loss <- df_charged_off %>% group_by(purpose) %>% summarise(Total = n(), Sum_of_loans = sum(loan_amnt)) %>% ungroup(df_t)
df_by_total_loss <- df_by_total_loss %>% mutate (percent = Sum_of_loans/sum(Sum_of_loans)*100)
ggplot(df_by_total_loss, aes(x=purpose, y=percent))  +                                                                
  labs(x="Purpose  for Loan",y=" %  of Total charged-off amount",title="Spread of % of amount(in Rs) lost in different \"Purpose\"") +
  geom_col(fill="steelblue")  +
  geom_text(aes(label= sprintf("%0.2f", round(percent, digits = 2))), vjust=-0.25,position = position_dodge(0), size=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


########################################### Analysis ###################################################
## 1. Over the years, we see number of charged-off loans, that were  taken for "debt_consolidation" purpose are consistently and significantly higher.
## 2. Over the years, we also see a consistent pattern of defaults for specific purposes.
## 3. Of the loans issued, we see that "small buisness" has high  (Toal number of loans charged-off/Total number of  loans sanctioned ratio)
## 4. Apart from above two, if we analyse the money lost in each category, 
##         >> Money lost in "debt-consolidation" is considerably high
##         >> While credit_card doesn't seems as category where more loans are defaulted or more people have default, it is certainly a category where more money is lost. Hence, we need to consider the same.
##         >> We also see that the third category where money lost is maximum is "small_business"
## Based on patterns provided by the 3 plots above, Top three categories where money lost is maximum is :
##        1. Debt_consolidation
##        2. Credit_card
##        3. Small_business
########################################################################################################




##------------------------------------------------------------------------------------------------
##       Plot3 : Grade  Analysis :Impact of Grade on defaulting loan categories
##------------------------------------------------------------------------------------------------


## Grade wise spread of defaulters for Top 3 defaulting categories

df_by_grade <- df_charged_off %>% filter(purpose == "debt_consolidation"  | purpose == "small_business" | purpose == "credit_card")
df_by_grade <- df_by_grade %>% group_by(purpose,grade) %>% summarise(Total = n())
df_by_grade <- df_by_grade %>% mutate (percent = (Total/sum(Total))*100)
ggplot(df_by_grade, aes(x=purpose, y=Total,fill = grade,c,position="fill"))  +                                             
  labs(x="Purpose of Loan",y="Proportion of Loans defaulted by different \"Grades\" ",title="Gradewise Spread of Charged Off loans ") +    
  geom_col(position = "fill") +
  geom_text(aes(label= Total), vjust=1,size=3,position = "fill")
  

##  For a given "Purpose", we find what % of total loan sanctioned for each grade has been charged-off
##  ie. ( Total number of loans "charged-off" for a given grade)/( Total number of loans sanctioned for a given grade)

df_t1 <- df %>% filter(purpose == "debt_consolidation"  | purpose == "small_business" | purpose == "credit_card")
df_by_grade_and_status <- df_t1 %>% group_by(purpose,grade,loan_status) %>% summarise(total=n()) %>% mutate(percent_of_total=total/sum(total)*100)
df_by_grade_and_status <- df_by_grade_and_status[which(df_by_grade_and_status$loan_status == "Charged Off"),]
ggplot(df_by_grade_and_status,aes(x=purpose,y=percent_of_total,fill=grade))+
  geom_col(position="dodge") +
  labs(x = "Purpose of Loans ", y = "% of defaults in each Grade(relative to total loan issued)",title="Percentage of loans defaulted in each grade ") +
  geom_text(aes(label= sprintf("%0.2f", round(percent_of_total, digits = 2))), vjust=-0.5,position = position_dodge(width = 1),size=3)
  



########################################### Analysis ###################################################
# 1. We see that in the Top 3 defaulting categories, the maximum number of  defaults are from Grades
#    B,C,D. This is also the case given that most number of loans sanctioned also are to those grades.
# 2. However, if we check the ratio of Toal number of loans charged-off:Total number of  loans sanctioned, we see that 
#    E,F,G grades actually default more. 
# 3. Hence, there is a trade off between the two. 
# To avoid risk,  Banks should   reduce the number of loans given to grades E,F,G as they are more likely to default
#######################################################################################################





  

##------------------------------------------------------------------------------------------------
##       Plot4 : Employment Length  Analysis :Impact of Employment Length on Credit Loss Risk
##------------------------------------------------------------------------------------------------
df_emp_length_only<-df_charged_off[which(!is.na(df_charged_off$emp_length)),]%>%group_by(emp_length)%>%summarise(total=n())%>%mutate(percent=total/sum(total)*100)

ggplot(df_emp_length_only, aes(x = emp_length,y=percent,fill=emp_length,label=percent)) +
  geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_text(aes(label= sprintf("%0.2f", round(percent, digits = 2))),size = 3, hjust = 0.5,vjust=1.5)+
  labs(x="Employee Length in (Years)",y=" %  of Total Loan Charged off",title="Spread of Charged off Loan across Employee Length") 

########################################### Analysis ###################################################
#1. We see that employee length more than 10 years of experience account for maximum number of charged off loans
#2. Employee length as low as 0,2,3 account for next largest defaulters.
#3. So employees with low or very high experience have higher risk of defaulting
#######################################################################################################



##------------------------------------------------------------------------------------------------
##       Plot5 : Income  Analysis :Impact of Interest Rate and Salary on Defaulters
##------------------------------------------------------------------------------------------------

#1. Income Analysis v/s Loan Status: spread of annual income against against loan status
df_box<- df%>%group_by(status=df$loan_status)%>%summarise(med=median(annual_inc))

ggplot(df, aes(x = interaction(loan_status), y = annual_inc)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+scale_y_log10(breaks = scales::pretty_breaks(n = 5))+
  geom_text(data = df_box, aes(x = status, y = med, label = med), 
            size = 3, vjust = 1.5)+ labs(x="Loan Status",y=" Annual Income",title="Spread of annual income across each loan status") 




#1. Income Analysis v/s Grade : spread of annual income against against each grade for defaulters

df_by_grade<- df_charged_off%>%group_by(status=grade)%>%summarise(med=median(annual_inc))

ggplot(df_charged_off, aes(x = grade, y = annual_inc)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+scale_y_log10(breaks = scales::pretty_breaks(n = 5))+
  geom_text(data = df_by_grade, aes(x = status, y = med, label = med), 
            size = 3, vjust = 1.5)+ labs(x="Grade",y=" Annual Income",title="Spread of annual Income across each grade") 

#
#2. Interest Rate v/s Grade : 

df_charged_off$int_rate<- as.numeric(df_charged_off$int_rate)

ggplot(df_charged_off,aes(x=int_rate,fill=grade,position="dodge"))+
  geom_bar(stat = "count",width = 0.1) +
  labs(x="Interest Rate",y=" Count of Charged off Loans",title="Spread of charged off loans across each grade v/s interest rate") 

########################################### Analysis ###################################################
#1.The average income of full paid group is higher
#2.Fully paid group has lot of outliers above the third quartile range
#3.Interest Rate is directly proportional to Salary.
#4.A with lower average salary have defaulted for lower interest rates
#5.B,C have defaulted at slightly high interest rates,with their mean salary simialar,the overlap on 
#  interest rates are also high
#4.E,F,G who have average salary higher have defaulted only at highter interest rates.
########################################### Analysis ###################################################


##------------------------------------------------------------------------------------------------
##       Plot6 : Impact of DTI
##------------------------------------------------------------------------------------------------

ggplot(df_charged_off,aes(x=dti,color=grade))+
  geom_density() +
  labs(x = "DTI ", y = "Density",title="Distribution of DTI for various grades in Charged off cases")
########################################### Analysis ###################################################
#1.The distribution for various grades is slightly right skewed
#2.most of the charged off cases have dtis between 10-20
#####################################################################################################
