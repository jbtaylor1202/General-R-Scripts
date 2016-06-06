library(twitteR)

#http://thinktostart.com/twitter-authentification-with-r/
#See https://apps.twitter.com/

consumerKey = #ADD KEY HERE
consumerSecret = #ADD SECRET HERE
accessToken = #ADD TOKEN HERE
accessSecret = #ADD SECRET HERE

options(httr_oauth_cache=TRUE) # skip question appearing on console
setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret, 
                    access_token = accessToken, access_secret = accessSecret)

hashTag<-#ADD HASHTAG HERE
tweets<-searchTwitter(hashTag,n = 500)
tweetDF<-twListToDF(tweets)

currentDate<-Sys.Date()
csvFileName <- paste("tweets ",currentDate,".csv",sep="") 

write.csv(x = tweetDF,file = csvFileName)


