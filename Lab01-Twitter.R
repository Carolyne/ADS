#setwd("//nas-csdm.rgu.ac.uk/csdm-H/Students/17/1712664/Carolynes Documents/Advanced Data Science")
require(twitteR)
library(twitteR)
require(httr)
library(httr)


# These variables are set in the file Lab01-Twitter-Keys.R which has been ignored in Git.
# api_key
# api_secret
# access_token
# access_token_secret

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

thisAccount <- "R_Programming"

timeline <- userTimeline(thisAccount, n = 3000)
df <- twListToDF(timeline)

head(df,10)

df[1:10,1]

dfSorted <- df[order(-df$retweetCount),]
dfSorted[1,c(1,5,12)]

# 1.4 Visualising Results
require(tm)
require(wordcloud)
library(wordcloud)
library(RColorBrewer, quietly = TRUE)

# Text processing
corp <- Corpus(VectorSource(df$text))

corp <- tm_map(corp, content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))

# Remove punctuation & stopwords
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, function(x)removeWords(x,c(stopwords("english"),"will")))

tdm <- TermDocumentMatrix(corp)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

d$word <- gsub("~", " ", d$word)

filename <- paste(thisAccount, ".png", sep="")
png(filename)

wordcloud(word = d$word, freq = d$freq, min.freq = 3,
          max.words=2000, random.order=FALSE, rot.per=0.2,
          colors = brewer.pal(8, "Dark2"))

dev.off() # uncomment to save to a file

unsortedFile <- paste("data/",thisAccount, "-unsorted.csv", sep="")
sortedFile <- paste("data/",thisAccount, "-sorted.csv", sep="")

write.csv(df, unsortedFile)
write.csv(dfSorted, sortedFile)

fav = favorites(thisAccount, n=100)

dff <- twListToDF(fav)

names(dff)

library(dplyr)
aggData <- aggregate(favoriteCount ~ screenName, 
                     data = dff,
                     FUN=sum)

aggDataSorted <- aggData[order(-aggData$favoriteCount),]
aggDataSorted

