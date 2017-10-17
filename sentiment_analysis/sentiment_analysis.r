####Sentiment analysis####################

#Install packages
install.packages("Rstem",
                 repos = "http://www.omegahat.org/R", type="source")
install.packages("devtools")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
install.packages("wordcloud", dependencies = TRUE)
install.packages("plyr", dependencies = TRUE)
install.packages("stringr", dependencies = TRUE)

#Set working directory##
#setwd("C:/Users/Madhu/Desktop/data_science/sentiment_analysis")

#Read files
amazon_revdf <- read.csv("amazon_reviews.csv")
imdb_revdf <- read.csv("imdb_reviews.csv")
yelp_revdf <- read.csv("yelp_reviews.csv")

nrow(amazon_revdf)


library(devtools)
require(sentiment)
ls("package:sentiment")
library(sentiment)
library(Rstem)
library(wordcloud)

catch.error = function(x)
{
  # let us create a missing value for test purpose
  y = NA
  # Try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}

cleanTexts<- function(Text){
  # Clean the Text for sentiment analysis
  # remove html links, which are not required for sentiment analysis
  Text = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", Text)
  # First we will remove reText entities from  the stored Texts (text)
  Text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", Text)
  # Then remove all "#Hashtag"
  Text = gsub("#\\w+", " ", Text)
  # Then remove all "@people"
  Text = gsub("@\\w+", " ", Text)
  # Then remove all the punctuation
  Text = gsub("[[:punct:]]", " ", Text)
  # Then remove numbers, we need only text for analytics
  Text = gsub("[[:digit:]]", " ", Text)
  # finally, we remove unnecessary spaces (white spaces, tabs etc)
  Text = gsub("[ \t]{2,}", " ", Text)
  Text = gsub("^\\s+|\\s+$", "", Text)
  # Next we'll convert all the word in lower case.
  Text = catch.error(Text)
  Text
}

cleanTextsAndRemoveNAs<- function(Texts) {
  TextsCleaned = sapply(Texts, cleanTexts)
  # Remove the "NA" Texts from this Text list
  TextsCleaned = TextsCleaned[!is.na(TextsCleaned)]
  names(TextsCleaned) = NULL
  # Remove the repetitive Texts from this Text list
  #TextsCleaned = unique(TextsCleaned)
  TextsCleaned
}

#Cleaning text
amazon_rev_cleaned = cleanTextsAndRemoveNAs(amazon_revdf[,1])
length(amazon_rev_cleaned)

imdb_rev_cleaned = cleanTextsAndRemoveNAs(imdb_revdf[,1])
length(imdb_rev_cleaned)

yelp_rev_cleaned = cleanTextsAndRemoveNAs(yelp_revdf[,1])
length(yelp_rev_cleaned)
##############################################################################
getwd()

setwd("C:/Users/Madhu/Desktop/data_science/sentiment_analysis/opinion-lexicon-English")


opinion.lexicon.pos =
  scan('positive-words.txt',
       what='character', comment.char=';')

opinion.lexicon.neg =
  scan('negative-words.txt',
       what='character', comment.char=';')

head(opinion.lexicon.neg)

head(opinion.lexicon.pos)

pos.words = c(opinion.lexicon.pos,'10/10')
neg.words = c(opinion.lexicon.neg,'wait',
              'waiting', 'wtf', 'cancellation')

getSentimentScore = function(sentences, words.positive,
                             words.negative, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences,
                 function(sentence, words.positive, words.negative) {
                   # Let first remove the Digit, Punctuation character and Control characters:
                   sentence = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '',
                                                           gsub('\\d+', '', sentence)))
                   # Then lets convert all to lower sentence case:
                   sentence = tolower(sentence)
                   # Now lets split each sentence by the space delimiter
                   words = unlist(str_split(sentence, '\\s+'))
                   # Get the boolean match of each words with the positive & negative opinion-lexicon
                   pos.matches = !is.na(match(words, words.positive))
                   neg.matches = !is.na(match(words, words.negative))
                   # Now get the score as total positive sentiment minus the total negatives
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, words.positive, words.negative, .progress=.progress )
  # Return a data frame with respective sentence and the score
  return(data.frame(text=sentences, score=scores))
}

##Calling the function to get sentiment score
amazonResult=getSentimentScore(amazon_rev_cleaned,pos.words,neg.words)
imdbResult=getSentimentScore(imdb_rev_cleaned, pos.words, neg.words)
yelpResult= getSentimentScore(yelp_rev_cleaned, pos.words, neg.words)

##Combining the result
amazonResult = cbind(amazon_revdf,amazonResult)
imdbResult= cbind(imdb_revdf, imdbResult)
yelpResult= cbind(yelp_revdf, yelpResult)

##writing the output to csv files
setwd("C:/Users/Madhu/Desktop/data_science/sentiment_analysis")
write.csv(amazonResult,"amazon_sentiment_score.csv")
write.csv(imdbResult, "imdb_sentiment_score.csv")
write.csv(yelpResult, "yelp_sentiment_score.csv")

#Plotting sentiment score
hist(amazonResult$score)
table1 <- table(amazonResult$score)
table1
lbls <- paste(names(table1), "\n", table1, sep="")
pie(table1, labels = lbls, col = rainbow(length(lbls)), radius = 0.9 ,main="Pie Chart of Amazon Scores\n (with sample sizes)")
barplot(table1,main="Sentiment Score for Amazon", xlab="Score")
mean(amazonResult$score)
sd(amazonResult$score)

hist(imdbResult$score)
mean(imdbResult$score)
sd(imdbResult$score)

hist(yelpResult$score)
mean(yelpResult$score)
sd(yelpResult$score)
# classify_emotion function returns an object of class data frame
#with seven columns (anger, disgust, fear, joy, sadness, surprise, 
  # best_fit) and one row for each document:

amazon_revdf_class_emotion = classify_emotion(amazon_rev_cleaned,algorithm="bayes", prior=1.0)
imdb_revdf_class_emotion = classify_emotion(imdb_rev_cleaned,algorithm="bayes", prior=1.0)
yelp_revdf_class_emotion = classify_emotion(yelp_rev_cleaned,algorithm="bayes", prior=1.0)

head(amazon_revdf_class_emotion, 20)

#fetch emotion category best_fit for our analysis purposes
amazon_emotion = amazon_revdf_class_emotion[,7]
imdb_emotion = imdb_revdf_class_emotion[,7]
yelp_emotion = yelp_revdf_class_emotion[,7]

#substituting "NA" with "unknown"
amazon_emotion[is.na(amazon_emotion)]= "unknown"
imdb_emotion[is.na(imdb_emotion)]= "unknown"
yelp_emotion[is.na(yelp_emotion)] = "unknown"

amazon_emotion

#classify_polarity returns the overall log likelihood positive/negative

amazon_revdf_class_polarity  =classify_polarity(amazon_rev_cleaned,algorithm = "bayes")
imdb_revdf_class_polarity = classify_polarity(imdb_rev_cleaned, algorithm = "bayes")
yelp_revdf_class_polarity = classify_polarity(yelp_rev_cleaned, algorithm = "bayes")

head(amazon_revdf_class_polarity,20)

# fetch polarity category best_fit for our analysis purpose

amazon_polarity = amazon_revdf_class_polarity[,4]
imdb_polarity = imdb_revdf_class_polarity[,4]
yelp_polarity = yelp_revdf_class_polarity[,4]

#Now creating a data frame with the above results
AmazonSentimentDataFrame = data.frame(text=amazon_rev_cleaned,
emotion=amazon_emotion, polarity=amazon_polarity, stringsAsFactors=FALSE)

ImdbSentimentDataFrame = data.frame(text=imdb_rev_cleaned,
emotion=imdb_emotion, polarity=imdb_polarity, stringsAsFactors = FALSE)

YelpSentimentDataFrame = data.frame(text=yelp_rev_cleaned, 
emotion=yelp_emotion, polarity= yelp_polarity, stringsAsFactors = FALSE)

setwd("C:/Users/Madhu/Desktop/data_science/sentiment_analysis")

write.csv(cbind(AmazonSentimentDataFrame, amazon_revdf[,2]), "Amazon_sentiment_final.csv")
write.csv(cbind(ImdbSentimentDataFrame, imdb_revdf[,2]), "Imdb_sentiment_final.csv")
write.csv(cbind(YelpSentimentDataFrame, yelp_revdf[,2]), "Yelp_sentiment_final.csv")

# rearrange data inside the frame by sorting it
AmazonSentimentDataFrame =within(AmazonSentimentDataFrame, emotion <-
                                   factor(emotion, levels=names(sort(table(emotion),
                                                                     decreasing=TRUE))))

ImdbSentimentDataFrame =within(ImdbSentimentDataFrame, emotion <-
                                   factor(emotion, levels=names(sort(table(emotion),
                                                                     decreasing=TRUE))))

YelpSentimentDataFrame =within(YelpSentimentDataFrame, emotion <-
                                   factor(emotion, levels=names(sort(table(emotion),
                                                                     decreasing=TRUE))))


#Creating a function for plotting sentiments
plotSentiments1<- function (sentiment_dataframe,title) {
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=emotion)) +
    geom_bar(aes(y=..count.., fill=emotion)) +
    scale_fill_brewer(palette="Dark2") +
    ggtitle(title) +
    theme(legend.position='right') + ylab('Number of Texts') +
    xlab('Emotion Categories')
}

table(AmazonSentimentDataFrame$emotion)
table(ImdbSentimentDataFrame$emotion)
table(YelpSentimentDataFrame$emotion)

#Calling function
plotSentiments1(AmazonSentimentDataFrame,'Sentiment analysis of reviews on Amazon')

plotSentiments1(ImdbSentimentDataFrame, 'Sentiment analysis of reviews on Imdb')

plotSentiments1(YelpSentimentDataFrame, 'Sentiment analysis of reviews on Yelp')


# Similarly we will plot distribution of polarity in the texts
plotSentiments2 <- function (sentiment_dataframe,title) {
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="RdGy") +
    ggtitle(title) +
    theme(legend.position='right') + ylab('Number of Texts') +
    xlab('Polarity Categories')
}

table(AmazonSentimentDataFrame$polarity)
table(ImdbSentimentDataFrame$polarity)
table(YelpSentimentDataFrame$polarity)

#Calling function
plotSentiments2(AmazonSentimentDataFrame,'Polarity analysis of reviews on Amazon')

plotSentiments2(ImdbSentimentDataFrame, 'Polarity analysis of reviews on Imdb')

plotSentiments2(YelpSentimentDataFrame, 'Polarity analysis of reviews on Yelp')

install.packages("tm",dependencies = TRUE)
library(tm)

##Function to remove stop words or custom words

removeCustomeWords <- function (TextCleaned) {
  for(i in 1:length(TextCleaned)){
    TextCleaned[i] <- removeWords(TextCleaned[i],
                                     c(stopwords("english"), "care", "guys", "can", "dis", "didn",
                                       "guy" ,"booked", "plz"))  }
  return(TextCleaned)
}

##Function to form a wordcloud

getWordCloud <- function
(sentiment_dataframe, TextCleaned, Emotion) {
  emos = levels(factor(sentiment_dataframe$emotion))
  n_emos = length(emos)
  emo.docs = rep("", n_emos)
  TextCleaned = removeCustomeWords(TextCleaned)
  for (i in 1:n_emos){
    emo.docs[i] = paste(TextCleaned[Emotion ==
                                        emos[i]], collapse=" ")
  }
  corpus = Corpus(VectorSource(emo.docs))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  colnames(tdm) = emos
  require(wordcloud)
  suppressWarnings(comparison.cloud(tdm, colors =
                                      brewer.pal(n_emos, "Dark2"), scale = c(3,.5), random.order =
                                      FALSE, title.size = 1.5))
}

getWordCloud(AmazonSentimentDataFrame, as.matrix(amazon_revdf$Comment),amazon_emotion)

getWordCloud(ImdbSentimentDataFrame, as.matrix(imdb_revdf$Comment),imdb_emotion)

getWordCloud(YelpSentimentDataFrame, as.matrix(yelp_revdf$Comment),yelp_emotion)


####################################################################################
