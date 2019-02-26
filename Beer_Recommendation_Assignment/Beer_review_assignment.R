  

###  Load the required library as we are building recommender systems ####
library(recommenderlab)
library(dplyr)
library(ggplot2)



####################################### 1. Data Preparation ###################################################
### Read the data 
df <- read.csv("beer_data.csv")                # 475984 - original number of rows 

########## 1.Data cleaning ########## 

#a. Check for any NA's in any row or column and remove it 
df <- na.omit(df)                              # No NA's found


#b. Check for duplicates
df <- df[-which(duplicated(df)),]              # 475404 - 580 duplicate rows removed
  

#c. Remove rows with
#   Empty profile names 
#   Empty beerID's if any
#   Incorrects Ratings as in 0
df <- df[-which(df$review_profilename ==""),]  # 475304 - 100 empty profile names removed
length(which(df$beer_beerid == ''))            # No invalid beer ID's
length(which(df$review_overall == ''|df$review_overall == 0))
df<- df[df$review_overall != 0,]              # 6 rows with rating 0 removed.       




#d. Remove rows where same user has given two reviews to same beer_id
df_new <-  df %>% group_by(beer_beerid, review_profilename)%>% summarize( number_of_reviews =n()) %>% filter(number_of_reviews>1)  

## 840 ppl reviewed more than once for the same beer. We treat them as invalid as it is ideally wrong and remove all those faulty
## reviews along with the original ones. 
## So we have 838 ppl giving 2 reviews and 2 ppl giving 3 reviews for the same beer. So it comes to 
## 838 * 2  - 2*3  = 1676 + 6 = 1682 faulty/invalid reviews. 

df     <- df[- which(duplicated(df[c('beer_beerid', 'review_profilename')]) | duplicated(df[c('beer_beerid', 'review_profilename')], fromLast = TRUE)),]
                                               ### 473622 - 1682 invalid reviews removed

 
##########  2. Filtering Data  ########## 

####  1. Choose only those beers that have at least N number of reviews


####  To chose the value of N, for which we do the following :
####  a. Group the data by beer_id and arrange in descending order on number of ratings
####  b. Create a table to find  the number of reviews, and beers corresponding to it, how much % of beers have less rating and 
####     how much % of data is constituted by it.
####  c. Plot the grouped data to find the spread
####  d. Find the statistical data of the spread and analyze

df_beer_rating          <- df %>% group_by(beer_beerid) %>% summarize( number_of_reviews =n()) %>% arrange(number_of_reviews)
df_beer_rating          <- ungroup(df_beer_rating)
df_beer_rating_overall  <- df_beer_rating %>% group_by(number_of_reviews) %>% summarize(number_of_beers=n()) %>% mutate(ratio = (number_of_beers/sum(number_of_beers))*100) %>% mutate(cumfreq = cumsum(ratio)) %>% mutate( ratio_of_overall_data = (number_of_reviews*number_of_beers*100)/473622)%>% mutate(cumfreq_ration_overall = cumsum(ratio_of_overall_data)) 

  qplot(df_beer_rating_overall$number_of_reviews)
summary(df_beer_rating_overall$number_of_reviews)

#### Analysis
### 1. We see that Median for spread of reviews for each beer is at 2 and Mean is quite low at 11.8  and even 3rd quartile comes at 5 while the maximum is at 980
### 2. This indicates that more than 50% of the beers have only very few reviews
### 3. As to say accurately, we have 94% beers having number of reviews 50 or less, however that only constitutes only 38% of data.
### 4. Hence we chose a value of N to 99



## Convert in to data frame and re-order the columns
beer_df      <- as.data.frame(df %>% group_by(beer_beerid) %>% mutate(count=n())%>%filter(count > 99)%>%ungroup())
beer_df      <- beer_df[,c(2,1,3)]

#reconfirm there are no NA values in the final dataset used for data exploration
which(is.na(beer_df))

# Finally Convert dataframe to realRatingMatrix
r <- as(beer_df, "realRatingMatrix")


# Check the class type and confirm it's in the format we are expecting
class(r)

# Check other information
range(getRatings(r))
dimnames(r)
head(rowCounts(r))
head(colCounts(r))
head(rowMeans(r))
##############################################################################################################







############################################ 2.  Data Exploration ###########################################

#1.Determine how similar the first ten users are with each other and visualise it
similar_users <- similarity(r[1:10, ],
                            method = "cosine",
                            which  = "users")
#Similarity matrix
as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")
 
# We see that
# User 1 has similarity with Users with 4 and 9
# User 4 has similarity with Users with 7 and 9
# We see similarity of 7 and 9 are greater 
# To summarize, 1 ,4, 7 and 9 are similar.




#2.Compute and visualise the similarity between the first 10 beers

similar_items <- similarity(r[,1:10 ],
                            method = "cosine",
                            which = "items")
as.matrix(similar_items)
image(as.matrix(similar_items), main = "Item similarity")

# We see that first 10 beers are similar to each other.



#3.What are the unique values of ratings?
df_unique_ratingVal<- beer_df %>% group_by(review_overall) %>% summarise(count=n())
# There are totally 9 ratings ie.
# 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5
# Rating 1 and 1.5 being very less frequently used and 5.0 being heavily used.




######################## Visualise the rating values and notice ################################

### Looking at the spread of ratings, following is what we have :
qplot(getRatings(r), binwidth = 1, 
      main = "Histogram of ratings", xlab = "Rating")

summary(getRatings(r)) # Skewed to the right   mean shows 3.915
avg_rating  <- sum(beer_df$review_overall)/nrow(beer_df) ## gives same as above.



###### 1. The average beer ratings ###### 

avg_beer_ratings <- beer_df %>% group_by(beer_beerid)%>% summarize(avg_rating=mean(review_overall))
ggplot(avg_beer_ratings, aes(beer_beerid,avg_rating))  +
       ggtitle("Spread of average rating per Beer ")   +
       labs(x="Beer ID",y="Average rating ")           +
       geom_line() + geom_smooth()    

qplot(colMeans(r),xlab = "Mean rating per beer", ylab = "Count",main = " Histogram of average beer rating " )

summary(avg_beer_ratings)
######  Average beer ratings comes to 3.862



###### 2. The average user ratings    ###### 

avg_user_ratings <- beer_df %>% group_by(review_profilename)%>% summarize(avg_rating=mean(review_overall))
summary(avg_user_ratings)

## As we have very large number of users, we only see the average spread using qplot.
qplot(rowMeans(r),xlab = "Mean rating given by each User", ylab = "Count",main = " Histogram of average user rating " )
######  Average user ratings comes to 3.969


###### 3. The average number of ratings given to the beers  ###### 

avg_number_beer_ratings <- beer_df %>% group_by(beer_beerid)%>% summarize(number_of_ratings=n())
ggplot(avg_number_beer_ratings, aes(beer_beerid,number_of_ratings))  +
  ggtitle("Spread of number of  ratings per beer ")   +
  labs(x="Beer ID ",y="Number ofratings ")           +
  geom_line() + geom_smooth()  

qplot(colCounts(r),xlab = "Number of ratings given to each Beer", ylab = "Count",main = " Histogram of average number of beer rating " )

summary(avg_number_beer_ratings)

######  Average number of  ratings given to Beers is 217.7


###### 4. The average number of ratings given by the users  ###### 
avg_number_user_ratings <- beer_df %>% group_by(review_profilename)%>% summarize(number_of_ratings=n())

## As we have very large number of users, we only see the average spread using qplot.
qplot(rowCounts(r),xlab = "Number of ratings given by each User", ylab = "Count",main = " Histogram of average number of User ratings " )

summary(avg_number_user_ratings)

#### On an average, each user has rated 12.26 movies.


####################################################################################################




###################### Recommendation models ###################### 

#1. Divide your data into training and testing datasets
#   Experiment with 'split' and 'cross-validation' evaluation schemes


#Method 1
#Divide data into  test and train, and evaluate using "Split Method"
splitScheme   <- evaluationScheme(r, method = "split", train = .7, k = 1, given = -1, goodRating = 4)

#Method 2
#Evaluate using 5-fold cross-validation method.
crossValScheme <- evaluationScheme(r, method = "cross-validation", k = 5, given = -1, goodRating = 4)

#2.Build IBCF and UBCF models
algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)


# Run algorithms with both the schemes, predict next n beers
splitResults    <- evaluate(splitScheme   , algorithms, n=c(1, 3, 5, 10, 15, 20))
crossValResults <- evaluate(crossValScheme, algorithms, n=c(1, 3, 5, 10, 15, 20))


#3.Compare the performance of the two models and suggest the one that should be deployed
#  Plot the ROC curves for UBCF and IBCF and compare them
plot(  splitResults   , annotate = 1:4, legend="topleft")
plot(  crossValResults, annotate = 1:4, legend="topleft")

#Analysis :
#1.We see that that both schemes fair nearly simlar.
#2.Cross validation scheme shows some improvement in TPR. 
#3.UBCF fairs better when comparing UBCF and IBCF models for both schemes.
#3.The TPR for UBCF increases as number of predicted beers increases
#4.The TPR for IBCF does not increase much ,while the value even for 20 is below 0.01
#Hence, suggestion is  UBCF to be deployed



#4.Give the names of the top 5 beers that you would recommend to the users "cokes", "genog" & "giblet"

#Build recommender using UBCF
Rec.model=Recommender(r,method="UBCF")

#recommended top 5 items for user "cokes", "genog" & "giblet"
recommended.items.cokes  <- predict(Rec.model, r["cokes",] , n=5)
recommended.items.genog  <- predict(Rec.model, r["genog",] , n=5)
recommended.items.giblet <- predict(Rec.model, r["giblet",], n=5)

# to display them
as(recommended.items.cokes , "list")
as(recommended.items.genog , "list")
as(recommended.items.giblet, "list")

#### Recommendation for user :
#### Cokes  :  4083, 7971, 22227, 34420, 2137
#### Genog  :  1641, 6076, 57908, 1005,  47026
#### Giblet :   141, 7971, 459,   34420, 73  

