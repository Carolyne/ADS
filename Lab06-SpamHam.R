# Lab 06 - Text Processing & Classification

# Dataset from : http://www.dt.fee.unicamp.br/~tiago/smsspamcollection/
setwd("~/RGU/Semester 02/Advanced Data Science/ADS-Labs")

df <- read.table(
  "data/SMSSpamCollection.txt",
  fill = TRUE,
  header = FALSE,
  quote = "",
  sep = "\t",
  encoding = "UTF-8"
)
# change the df names
names(df) <- c('Label', 'SMS')


nrow(df)
ncol(df)

table(df$Label)

str(df)

library(tm)
dfCorpus <- Corpus(VectorSource(df$SMS))


print(dfCorpus)

# Load necessary libraries
library(tm)
library(SnowballC)
# Lets first build a function to build the text corpus

buildCorpus <- function(someText) {
  # build a corpus, and specify the source to be character vectors
  myCorpus <- Corpus(VectorSource(someText))
  
  # I had to add this line to make the code work
  # For windows, it may not be an issue
  myCorpus <- tm_map(myCorpus,
                     content_transformer(function(x)
                       iconv(x, to = 'UTF-8',
                             sub = 'byte')))
  myCorpus <-
    tm_map(myCorpus, content_transformer(tolower))
  # remove punctuation
  myCorpus <- tm_map(myCorpus, removePunctuation)
  # remove numbers
  myCorpus <- tm_map(myCorpus, removeNumbers)
  # remove URLs
  removeURL <- function(x)
    
    gsub("http[[:alnum:]]*", "", x)
  
  ### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE)
  myCorpus <-
    tm_map(myCorpus, content_transformer(removeURL))
  
  # add two extra stop words: 'available' and 'via'
  # myStopwords <- c(stopwords("english"), "RT","rt")
  # remove "RT from stopwords
  # myStopwords <- setdiff(myStopwords, c("RT","rt"))
  # remove stopwords from corpus
  myCorpus <- tm_map(myCorpus, removeWords, stopwords())
  #
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  # Return the text corpus
  return(myCorpus)
}



# To build the corpus and make it ready,
# call the build corpus function and stem the document
myCorpus <- buildCorpus(df$SMS)


for (i in 1:5){
cat("[", i, "]", myCorpus[[i]]$content, "nn")
}


df[1:5, "SMS"]



# 3.2 Tokenize Text

dtm <- DocumentTermMatrix(myCorpus)

dim(dtm)

head(dimnames(dtm)$Terms)

# 3.3 Preparing Data for Classification

training <- dtm[1:4169,]
testing <- dtm[4170:5574,]

trainLabels <- df[1:4169,]$Label
testLabels <- df [4170:5574,]$Label
prop.table(table(trainLabels))

prop.table(table(testLabels))


trainDF <- dfCorpus[1:4169]
testeDF <- dfCorpus[4170:5574]

library(wordcloud)
wordcloud(trainDF, min.freq = 40, random.order = FALSE)
wordcloud(testeDF, min.freq = 40, random.order = FALSE)


spam <- subset(df, Label == "spam")
ham <- subset(df, Label == "ham")
wordcloud(spam$SMS, max.words = 40, scale = c(3, .5))
wordcloud(ham$SMS, max.words = 40, scale = c(3, .5))

# 3.4 Features Preparation

freqTerms <- findFreqTerms(training, 5)

reducedTrain <- training[, freqTerms]
reducedTest <- testing[, freqTerms]

dim(training)
dim(reducedTrain)
dim(testing)
dim(reducedTest)

convert_counts <- function(x)
{
x <- ifelse(x > 0, 1, 0)
x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
return(x)
}
reducedTrainU <-
  apply(reducedTrain, MARGIN = 2, FUN = convert_counts)
reducedTestU <- apply(reducedTest, MARGIN = 2, FUN = convert_counts)

x <- as.matrix((reducedTrain[100:110, 31:40]))
x

x <- as.data.frame(reducedTrainU[100:110, 31:40])
x

# 3.5 Building and Evaluating a Classifier

library(e1071)

bayesModel <- naiveBayes(reducedTrainU, trainLabels)

predictions <- predict(bayesModel, reducedTestU)

table(predictions, testLabels)

library(gmodels)
CrossTable(predictions, testLabels, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual') )
