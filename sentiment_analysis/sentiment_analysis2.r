install.packages("caret", dependencies=TRUE)
library(caret)
library(e1071)
library(RTextTools)

setwd("C:/Users/Madhu/Desktop/data_science/sentiment_analysis")
amazon_revdf <- read.csv("amazon_reviews.csv")
imdb_revdf <- read.csv("imdb_reviews.csv")
yelp_revdf <- read.csv("yelp_reviews.csv")
nrow(amazon_revdf)
amazon_train <- createDataPartition(y= amazon_revdf$Verdict, p= .80, list= FALSE)
imdb_train <- createDataPartition(y=imdb_revdf$Verdict, p= .80, list= FALSE)
yelp_train <- createDataPartition(y=yelp_revdf$Verdict, p= .80, list= FALSE)

Training.amazon <- create_matrix(amazon_revdf[amazon_train,1], language="english", 
                                 removeStopwords=FALSE, removeNumbers=TRUE, 
                                 stemWords=FALSE)
Training.imdb <- create_matrix(imdb_revdf[imdb_train,1 ],language="english", 
                               removeStopwords=FALSE, removeNumbers=TRUE, 
                               stemWords=FALSE)
Training.yelp <- create_matrix(yelp_revdf[yelp_train,1 ], language="english", 
                               removeStopwords=FALSE, removeNumbers=TRUE, 
                               stemWords=FALSE)
class(Training.amazon)

Test.amazon <- create_matrix(amazon_revdf[-amazon_train,1 ], language="english", 
                             removeStopwords=FALSE, removeNumbers=TRUE, 
                             stemWords=FALSE)
Test.imdb <- create_matrix(imdb_revdf[-imdb_train,1 ], language="english", 
                           removeStopwords=FALSE, removeNumbers=TRUE, 
                           stemWords=FALSE)
Test.yelp <- create_matrix(yelp_revdf[-yelp_train,1 ],language="english", 
                           removeStopwords=FALSE, removeNumbers=TRUE, 
                           stemWords=FALSE)

nrow(Test.amazon)
Test.amazon

#create model
amazon_model <- naiveBayes(as.matrix(Training.amazon),as.factor(amazon_revdf[amazon_train,2]))
imdb_model <- naiveBayes(as.matrix(Training.imdb), as.factor(imdb_revdf[imdb_train,2]))
yelp_model <- naiveBayes(as.matrix(Training.yelp), as.factor(yelp_revdf[yelp_train,2]))

#test the model 
test_amazon_model <- predict(amazon_model,as.matrix(Test.amazon))
test_imdb_model <- predict(imdb_model,as.matrix(Test.imdb))
test_yelp_model <- predict(yelp_model, as.matrix(Test.yelp))

#confustion matrix and accuracy
table(test_amazon_model,amazon_revdf[-amazon_train,2])
recall_accuracy(test_amazon_model,amazon_revdf[-amazon_train,2])

table(test_imdb_model, imdb_revdf[-imdb_train,2])
recall_accuracy(test_imdb_model, imdb_revdf[-imdb_train,2])

table(test_yelp_model, yelp_revdf[-yelp_train,2])
recall_accuracy(test_yelp_model, yelp_revdf[-yelp_train,2])

write.csv(cbind(amazon_revdf[-amazon_train,], as.data.frame(test_amazon_model)),"amazon_naive.csv")

############################################################################

# build the data to specify response variable, training set, testing set.


amazon_all <- rbind(amazon_revdf[amazon_train,],amazon_revdf[-amazon_train,])

amazon_matrix <- create_matrix(amazon_all[,1], language="english", 
                                removeStopwords=FALSE, removeNumbers=TRUE, 
                                stemWords=FALSE)
nrow(amazon_matrix)

amazon_container = create_container(amazon_matrix,as.factor(amazon_all[,2]),
                             trainSize=1:800,testSize=801:1000,virgin=FALSE)

amazon_models = train_models(amazon_container,
                             algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))

results = classify_models(amazon_container, amazon_models)

write.csv(results,"amazon_ml_results.csv")

# accuracy table
table(results[,"FORESTS_LABEL"],as.factor(amazon_all[801:1000, 2]))
table(results[,"MAXENTROPY_LABEL"],as.factor(amazon_all[801:1000, 2]))

# recall accuracy
recall_accuracy(results[,"FORESTS_LABEL"],as.factor(amazon_all[801:1000, 2]))
recall_accuracy(results[,"MAXENTROPY_LABEL"],as.factor(amazon_all[801:1000, 2]))
recall_accuracy(results[,"TREE_LABEL"],as.factor(amazon_all[801:1000, 2]))
recall_accuracy(results[,"BAGGING_LABEL"],as.factor(amazon_all[801:1000, 2]))
recall_accuracy(results[,"SVM_LABEL"],as.factor(amazon_all[801:1000, 2]))

# model summary
analytics = create_analytics(amazon_container, results)
summary(analytics)
head(analytics@document_summary)
analytics@ensemble_summary

