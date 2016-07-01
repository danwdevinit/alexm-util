#install.packages("streamR")
#install.packages("ROAuth")
library(ROAuth)
library(streamR)

#sets your working directory
setwd('D:/Documents/')

auth <- readLines("twitter_authR.txt")

#create your OAuth credential
credential <- OAuthFactory$new(consumerKey=auth[1],
                               consumerSecret=auth[2],
                               requestURL='https://api.twitter.com/oauth/request_token',
                               accessURL='https://api.twitter.com/oauth/access_token',
                               authURL='https://api.twitter.com/oauth/authorize')

#authentication process
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
credential$handshake(cainfo="cacert.pem")

#function to actually scrape Twitter
filterStream( file.name="tweets_test.json",
              track="#United", tweets=1000, oauth=credential, timeout=120, lang='en' )


#Parses the tweets
tweet_df <- parseTweets(tweets='tweets_test.json')

#using the Twitter dataframe
tweet_df$created_at
tweet_df$text


plot(tweet_df$friends_count, tweet_df$followers_count) #plots scatterplot
cor(tweet_df$friends_count, tweet_df$followers_count) #returns the correlation coefficient