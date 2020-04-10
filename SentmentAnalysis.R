#This Project is developed By: Shruti Basarkar
library("NLP")
library("twitteR")
library("syuzhet")
library("tm")
library("twitteR")
library("ROAuth")
library("ggplot2")
library("tm")
library("wordcloud")

#please enter your access tokens and key details to connect twitter account to R, in order to extract the required tweets.
consumer_key <- ''
consumer_secret <- ''
access_token <- ''
access_secret <- ''
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets_corona <- searchTwitter("#coronavirus", n=1500,lang = "en")
tweets_flu <- searchTwitter("#flu", n=1500,lang = "en")

corona_tweets <- twListToDF(tweets_corona)
flu_tweets <- twListToDF(tweets_flu)

View(corona_tweets)

#preprocessing
corona_text<- corona_tweets$text
flu_text<- flu_tweets$text
corona_text<- tolower(corona_text)
flu_text<- tolower(flu_text)
corona_text <- gsub("rt", "", corona_text)
flu_text <- gsub("rt", "", flu_text)
corona_text <- gsub("@\\w+", "", corona_text)
flu_text <- gsub("@\\w+", "", flu_text)
corona_text <- gsub("[[:punct:]]", "", corona_text)
flu_text <- gsub("[[:punct:]]", "", flu_text)
corona_text <- gsub("http\\w+", "", corona_text)
flu_text <- gsub("http\\w+", "", flu_text)
corona_text <- gsub("[ |\t]{2,}", "", corona_text)
flu_text <- gsub("[ |\t]{2,}", "", flu_text)
corona_text <- gsub("^ ", "", corona_text)
flu_text <- gsub("^ ", "", flu_text)
corona_text <- gsub(" $", "", corona_text)
flu_text <- gsub(" $", "", flu_text)

#create corpus
corona_tweets.text.corpus <- Corpus(VectorSource(corona_text))
flu_tweets.text.corpus <- Corpus(VectorSource(flu_text))

# removing stop words
corona_tweets.text.corpus <- tm_map(corona_tweets.text.corpus, function(x)removeWords(x,stopwords()))
wordcloud(corona_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

flu_tweets.text.corpus <- tm_map(flu_tweets.text.corpus, function(x)removeWords(x,stopwords()))
wordcloud(flu_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

#getting emotions using in-built function 
mysentiment_corona<-get_nrc_sentiment((corona_text))
mysentiment_flu<-get_nrc_sentiment((flu_text))

#calculationg total score for each sentiment
Sentimentscores_corona<-data.frame(colSums(mysentiment_corona[,]))
Sentimentscores_flu<-data.frame(colSums(mysentiment_flu[,]))

names(Sentimentscores_corona)<-"Score"
Sentimentscores_corona<-cbind("sentiment"=rownames(Sentimentscores_corona),Sentimentscores_corona)
rownames(Sentimentscores_corona)<-NULL

names(Sentimentscores_flu)<-"Score"
Sentimentscores_flu<-cbind("sentiment"=rownames(Sentimentscores_flu),Sentimentscores_flu)
rownames(Sentimentscores_flu)<-NULL

#plotting the sentiments with scores

ggplot(data=Sentimentscores_corona, aes(x=sentiment,y=Score)) + 
  geom_point(aes(color = sentiment, size = Score), alpha = 0.8) +
  scale_color_manual(values = c("#FFA500", "#FF4500", "#FFFF00","#00BFFF","#191970","#FF0000","#32CD32","#FF1493", "#9400D3","#008080")) +
  scale_size(range = c(0.9, 18))+ggtitle("Sentiments of people behind the tweets on #coronavirus")

ggplot(data=Sentimentscores_flu,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on #flu")

