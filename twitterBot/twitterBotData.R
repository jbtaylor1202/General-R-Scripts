tweetMaster<-file.choose()
tweetData<-read.csv(tweetMaster)

library(dplyr)
library(tidyr)

topTweeter<-tweetData%>%
  group_by(screenName)%>%
  summarise(count=n())%>%
  arrange(desc(count))
write.csv(x = topTweeter,file = "topTweeter.csv")

topRetweeter<-tweetData%>%
  group_by(screenName)%>%
  summarise(totalretweets=sum(retweetCount))%>%
  arrange(desc(totalretweets))
write.csv(x = topRetweeter,file = "topRetweeter.csv")





tweetByDayDF<-tweetData

tweetByDay<-tweetByDayDF%>%
  separate(created, into = c("tweetDate", "time"), sep = " ")%>%
  group_by(tweetDate)%>%
  summarise(dayCount=n())%>%
  arrange(tweetDate)
write.csv(x = tweetByDay,file = "tweetByDay.csv")

library(ggplot2)
plotData<-read.csv("tweetByDay.csv")
plotData<-select(plotData,tweetDate,dayCount)
ggplot(data = plotData, aes(x = tweetDate, y=dayCount))+geom_bar(stat = "identity")


tweetByDayTWEETnotRETWEET<-tweetByDayDF%>%
  separate(created, into = c("tweetDate", "time"), sep = " ")%>%
  filter(isRetweet=='TRUE')%>%
  group_by(tweetDate)%>%
  summarise(dayCount=n())%>%
  arrange(tweetDate)
write.csv(x = tweetByDayTWEETnotRETWEET,file = "tweetNOTRETWEETByDay.csv")

library(ggplot2)
plotData<-read.csv("tweetNOTRETWEETByDay.csv")
plotData<-select(plotData,tweetDate,dayCount)
ggplot(data = plotData, aes(x = tweetDate, y=dayCount))+geom_bar(stat = "identity")

  