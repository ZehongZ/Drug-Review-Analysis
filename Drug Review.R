##Project Goals##
#Classification: predict the patient's condition based on the review
#Regression: Predict the rating of the drug based on the review
#Sentiment Analysis
#Data Visualization

##Import dataset##
library(data.table)
myd=data.table::fread("drugsComTrain_raw.csv",header = T)

##Explore dataset##
head(myd)
dim(myd)#161297*7
names(myd)#uniqueID, drugName, condition, review, rating, date, usefulCount(Number of users who found review useful)
str(myd)#uniqueID(int), drugName(chr),condition(chr),review(chr),rating(int),date(chr),usefulcount(int)
summary(myd)
range(myd$date)

##Explore Rating##
library(ggplot2)
library(gcookbook)
summary(myd$rating)
hist(myd$rating,xlab = "Rating",main="Histogram of Rating")#U shape.Lowest and Highest rating have the most count
ggplot(myd, aes(x=rating))+geom_bar(colour="black",fill="blue")
ggplot(myd, aes(x=uniqueID, y=rating))+geom_bar(fill="black",stat='identity')
boxplot(myd$rating,main="Boxplot of Rating")#No outliers
highRating=subset(myd, rating>5)#Subset rating >5
ggplot(highRating,aes(x=rating))+geom_bar(fill="blue")
ggplot(highRating,aes(x=drugName, y=rating))+geom_bar(fill="blue",stat='identity')
perfectRating=subset(myd, rating==10)
length(unique(perfectRating$drugName))#2698 drugs got perfect rating
length(unique(perfectRating$drugName))/length(unique(myd$drugName))#78.5% of all drugs products

##Explore UsefulCount##
summary(myd$usefulCount)
hist(myd$usefulCount,xlab="Useful Count",main="Histogram of Useful Count")#Long right tails
ggplot(myd, aes(x=usefulCount))+geom_bar(fill="red")
boxplot(myd$usefulCount)#There are outliers
ggplot(myd, aes(x=uniqueID, y=usefulCount))+geom_bar(stat="identity")
highUsefulCount=subset(myd,usefulCount>33)
ggplot(highUsefulCount,aes(x=usefulCount))+geom_bar(fill="blue")
maxReview=subset(myd, usefulCount==1291)
head(maxReview)#The drugs which got most review both have perfect ratings

##Explore usefulCount and ratings##
plot(myd$rating,myd$usefulCount)#Significantly large amounts of  reviews have positive effect on ratings
cor(myd$rating,myd$usefulCount)#Correlation only 0.2341854

#Explore usefulCount and Time
ggplot(myd, aes(x=myd$date,y=myd$usefulCount))+geom_bar(stat="identity")
useful<-ts(myd$usefulCount, start=c(2008,4),frequency = 12)
plot(useful)#No significant trend with time changes

#Explore Ratings and Time
ratingTrend<-ts(myd$rating,start=c(2008,4),frequency = 12)
plot(ratingTrend)
ggplot(myd, aes(x=myd$date,y=myd$rating))+geom_bar(stat='identity')#No significant trent with time changes

##Condition analysis##
condition=myd$condition
head(condition)
#Transform into tidy form
library(dplyr)
condition_df<-data_frame(text=condition)
condition_df
library(tidytext)
condition_df%>%
  unnest_tokens(word, text)
data("stop_words")
condition_df<-condition_df%>%
  anti_join(stop_words)#No common variables
#Count most common words
condition_df%>%
  count(text, sort=TRUE)#Birth Control has been mentioned the most
library(ggplot2)
condition_df%>%
  count(text, sort = TRUE)%>%
  filter(n>1000)%>%
  mutate(text=reorder(text,n))%>%
  ggplot(aes(text,n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()


#Review analysis
library(dplyr)
review=myd$review
#Remove "\"
gsub('\"',"",review,fixed = TRUE)
#Transform dataset
review_df=data_frame(text=review)
head(review)
#Analyze text data in tidytext
library(tidytext)
review_df%>%
  unnest_tokens(word, text)
data("stop_words")
review_df<-review_df%>%
  anti_join(stop_words)#No common variables
#Count most common words
review_df%>%
  count(text, sort=TRUE)
library(ggplot2)
review_df%>%
  count(text, sort = TRUE)%>%
  filter(n>10)%>%
  mutate(text=reorder(text,n))%>%
  ggplot(aes(text,n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
#Word Cloud for most common words
library(wordcloud)
review_df%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word)%>%
  with(wordcloud(word,n,max.words = 100))

#Sentiments Analysis
library(tidytext)
data("sentiments")
#get_sentiments to get specific setiment lexicons without the columns that are not used in that lexicon
afinn<-get_sentiments("afinn")#Word & Score
bing<-get_sentiments("bing")#Word & Sentiment
nrc<-get_sentiments("nrc")#Word & Sentiment
head(afinn)
head(bing)
head(nrc)
#Plot the sentiment scores
review_df%>%
  unnest_tokens(word,text)%>%
  inner_join(afinn)%>%
  summarise(sentiment=sum(score))%>%
  mutate(method="AFINN")#-117778 Negative scores
#See the numbers of positive and negative words in these lexicons
get_sentiments("nrc")%>%
  filter(sentiment %in% c("positive","negative"))%>%
  count(sentiment)#Negative sentiments: 3324, Positive sentiments:2312
get_sentiments("bing")%>%
  count(sentiment)#Negative sentiments: 4782, Positive sentiments:2006
#Most Common Positive and Negative Words
bing_word_counts<-review_df%>%
  unnest_tokens(word, text)%>%
  inner_join(get_sentiments("bing"))%>%
  count(word, sentiment, sort=TRUE)%>%
  ungroup()
bing_word_counts
#Visualize results
library(ggplot2)
bing_word_counts%>%
  group_by(sentiment)%>%
  top_n(10)%>%
  ungroup()%>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n, fill=sentiment))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment, scales="free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()
#Word Clouds for most common and negative and positive words
library(reshape2)
review_df%>%
  unnest_tokens(word, text)%>%
  inner_join(bing)%>%
  count(word, sentiment, sort=TRUE)%>%
  acast(word~sentiment, value.var = "n", fill=0)%>%
  comparison.cloud(colors=c("gray20","gray80"),max.words = 100)

#Find the drug has the highest proportion of negative words
myd2=myd[,c("drugName","review")]
head(myd2)
myd2_df=data_frame(text=myd2)
bingnegative<-bing%>%
  filter(sentiment=="negative")
wordcounts<-myd2_df%>%
  group_by("drugName")%>%
  summarize(words=n())
wordcounts

wordcounts
myd2_df%>%
  semi_join(bingnegative)%>%
  group_by("drugName")%>%
  summarize(negativewords=n())%>%
  left_join(wordcounts, by=c("drugName"))%>%
  mutate(ratio=negativewords/words)%>%
  top_n(1)%>%
  ungroup()
#No Common Variables 

#Classification

#Regression