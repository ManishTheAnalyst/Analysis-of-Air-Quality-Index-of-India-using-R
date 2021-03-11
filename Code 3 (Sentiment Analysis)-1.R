# sentiment analysis using nrc

library(tidytext)
library(stringr)
library(RColorBrewer)
library(wordcloud)
library(syuzhet)
library(sentimentr)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(SentimentAnalysis)
sentiments
get_sentiments("bing")
get_sentiments("nrc")
get_sentiments("afinn")
text <- readLines(file.choose()) 
View(text)
review<-as.character(text)

#using nrc to setermine sentiment analysis
get_sentiments("nrc")
get_nrc_sentiment('fear')
get_nrc_sentiment('anger')
get_nrc_sentiment('negative')
get_nrc_sentiment('true')
s<-get_nrc_sentiment(review)
review_sentiment<-cbind(text)
barplot(colSums(s),col = rainbow(10),ylab='Count',main="Sentiment Analysis for Odd-Even Rule, Delhi")
pie(colSums(s),col = rainbow(10),ylab='Count',main="Sentiment Analysis for AQI, India")
str_split(text,pattern = "\\s+")
textbag<-str_split(text,pattern = "\\s+")

#word and frequency count 
class(textbag)
textbag
words<-c(textbag)
words
count(text)

#positive and negative sentiment analysis
ab<-read.csv("Sheet1.csv")
View(ab)
ggplot(ab,aes(x=keywords,color=Sentiments))+geom_bar()+
  theme(axis.text.x = element_text(angle = 90))

