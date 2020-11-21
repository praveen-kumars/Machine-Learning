#installing the required packages

install.packages('twitteR')
library(twitteR)
install.packages('RCurl')
library(RCurl)
install.packages('ROAuth')
library(ROAuth)
install.packages('ggplot2')
library(ggplot2)
install.packages('SnowballC')
library(SnowballC)

#Installing the rest all libraries
install.packages("stringr")
library(stringr)
install.packages('wordcloud')
library(wordcloud)
install.packages('tm')
library('tm')
library(dplyr)
library(RColorBrewer)
install.packages("httr", repos = "http://cran.us.r-project.org")
library('httr')
install.packages("syuzhet")
library('syuzhet')

API_Key<-"iLUoS99gZ8OaDuiOqxAxlBFf6"
API_Secret<-"VVYavuOjKwm4E5KP2NBnQpDGi94BgQuP3SKmvBoml2rBcTqxwS"
Access_Token<-"2901540738-PYjphQaw5NTJmSt25XuYKBaB0JNUN9fma2JJOuj"
Access_Token_Secret<-"UIab1Lp3ylvtKNPUi4bcODqfIs1BNq6JwBAT1CABQK68u"

#Accessing my account
setup_twitter_oauth(API_Key,API_Secret,Access_Token,Access_Token_Secret)

#Extracting tweets from the official handle of Congress

Congress_tweets2 = searchTwitter("@INCIndia", since="2019-05-15",until="2019-05-21",n=5000, lang = "en")

# Store the tweets into dataframe
Congress_tweets2.df = twListToDF(Congress_tweets2)
head(Congress_tweets2.df)

# Clean the tweets
Congress_tweets2.df$text=gsub("&amp", "", Congress_tweets2.df$text)
Congress_tweets2.df$text = gsub("&amp", "", Congress_tweets2.df$text)
Congress_tweets2.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", Congress_tweets2.df$text)
Congress_tweets2.df$text = gsub("@\\w+", "", Congress_tweets2.df$text)
Congress_tweets2.df$text = gsub("[[:punct:]]", "", Congress_tweets2.df$text)
Congress_tweets2.df$text = gsub("[[:digit:]]", "", Congress_tweets2.df$text)
Congress_tweets2.df$text = gsub("http\\w+", "", Congress_tweets2.df$text)
Congress_tweets2.df$text = gsub("[ \t]{2,}", "", Congress_tweets2.df$text)
Congress_tweets2.df$text = gsub("^\\s+|\\s+$", "", Congress_tweets2.df$text)

Congress_tweets2.df$text <- iconv(Congress_tweets2.df$text, "UTF-8", "ASCII", sub="")

#Emotion score of each tweet
emotions <- get_nrc_sentiment(Congress_tweets2.df$text)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

# Visualize the emotions from sentiments
install.packages('plotly')
library(plotly)
p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion for Congress")
#api_create(p,filename="Sentimentanalysis")
p

# Create comparison word cloud data
wordcloud_tweet = c(
  paste(Congress_tweets2.df$text[emotions$anger > 0], collapse=" "),
  paste(Congress_tweets2.df$text[emotions$anticipation > 0], collapse=" "),
  paste(Congress_tweets2.df$text[emotions$disgust > 0], collapse=" "),
  paste(Congress_tweets2.df$text[emotions$fear > 0], collapse=" "),
  paste(Congress_tweets2.df$text[emotions$joy > 0], collapse=" "),
  paste(Congress_tweets2.df$text[emotions$sadness > 0], collapse=" "),
  paste(Congress_tweets2.df$text[emotions$surprise > 0], collapse=" "),
  paste(Congress_tweets2.df$text[emotions$trust > 0], collapse=" ")
)

# create corpus
corpus = Corpus(VectorSource(wordcloud_tweet))

# remove punctuation, convert every word in lower case and remove stop words

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
corpus = tm_map(corpus, stemDocument)

# create document term matrix

tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)
tdmnew <- tdm[nchar(rownames(tdm)) < 11,]

# column name binding
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdmnew) <- colnames(tdm)
comparison.cloud(tdmnew, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(1, 0.4),rot.per=0.2)


