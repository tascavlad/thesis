# This R script was used to perform all data management and analysis tasks related to the completion of my master thesis in June 2019.
# Author: Vlad Tasca

library('tidyverse')
library('tm')
library('SnowballC')
library('MLmetrics')
library('quanteda')

twcs <- read_csv('twcs.csv')
ae <- read_csv('amazonenglish.csv', guess_max = 58796)
ae <- as.data.frame(ae)
head(ae)
glimpse(ae)
ae$type <- as.factor(ae$type)
levels(ae$type)

ae<- ae %>% 
  filter(type != 9) #remove unused rows
ae <- ae %>% 
  filter(type != 0) #remove filtered out rows
ae <- ae %>% 
  filter(type != 4) #remove suggestions


colnames(twcs)
glimpse(twcs)

inbounds <- twcs %>% 
  filter(inbound == 'True')

colnames(inbounds)
glimpse(inbounds)

first_inbounds <- inbounds %>% 
  filter(is.na(in_response_to_tweet_id))

# start using TM
newdata <- first_inbounds$text
newdata <- ae$text_x
mydata <- VCorpus(VectorSource(newdata))

##   preprocessing

## remove brand handle

mydata <- tm_map(mydata, removeWords, 'AmazonHelp')

#lowercase
mydata <- tm_map(mydata, content_transformer(tolower))

#remove emoji
mydata<-tm_map(mydata, content_transformer(gsub), pattern="\\W",replace=" ")

#remove URL
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
mydata <- tm_map(mydata, content_transformer(removeURL)
)
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
mydata <- tm_map(mydata, content_transformer(removeNumPunct))

# remove stopwords
mydata <- tm_map(mydata, removeWords, stopwords("english"))

# remove extra whitespace
mydata <- tm_map(mydata, stripWhitespace)
# Remove numbers
mydata <- tm_map(mydata, removeNumbers)
# Remove punctuations
mydata <- tm_map(mydata, removePunctuation)

inspect(mydata[[314]])

# stem them 
mydata <- tm_map(mydata, stemDocument)

# create document term matrix
dtm <- DocumentTermMatrix(mydata)

inspect(dtm)

# building document term matrix

dtm_sparse <- removeSparseTerms(dtm, 0.996)

inspect(dtm_sparse)

dtm_dm <- as.data.frame(as.matrix(dtm_sparse)) # count matrix



dtm_df <- as.matrix((dtm_dm > 0) + 0) # binary instance matrix

dtm_df <- as.data.frame(dtm_df)

dtm_df <- cbind(dtm_df, ae$type) # append label column from original dataset
names(dtm_df)[names(dtm_df) == "ae$type"] <- "type"

dtm_df$type <- factor(dtm_df$type)

table(dtm_df$type)

tdm_df$type
library(plyr)

dtm_df$type <- revalue(dtm_df$type,
        c('1' = 'complaint',
          '2' = 'question',
          '3' = 'request',
          '5' = 'compliment'))

dtm_dm$type <- dtm_df$type

# word counts and such
freq <- sort(colSums(as.matrix(dtm_df[,1:498])), decreasing=TRUE)   
head(freq, 100)
wf <- data.frame(word=names(freq), freq = freq)
head(wf)
wf$type <- dtm_df$type

# most commonly ocurring words
ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x = 'Words', y = 'Frequency')

### most commonly ocurring words per category

# COMPLAINT
dtm_df_complaint <- dtm_df %>% 
  filter(type == 'complaint')
freq_complaint <- sort(colSums(as.matrix(dtm_df_complaint[,1:498])), decreasing=TRUE)   
wf_complaint <- data.frame(word=names(freq_complaint), freq = freq_complaint)
wf_complaint$freq <- (wf_complaint$freq/547)

plot_complaint <- ggplot(subset(wf_complaint, freq>0.12), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = 'identity', fill = 'brown4') +
  theme(axis.text.x=element_text(angle=45, hjust=1, size = 12),
        axis.text.y=element_text(size = 12)) +
  labs(x = 'Words', y = 'Percentage', title = bquote(paste(bold('Complaint'))))

# QUESTION
dtm_df_question <- dtm_df %>% 
  filter(type == 'question')
freq_question <- sort(colSums(as.matrix(dtm_df_question[,1:498])), decreasing=TRUE)   
wf_question <- data.frame(word=names(freq_question), freq = freq_question)
head(wf_question)
wf_question$freq <- (wf_question$freq/287)

plot_question <- ggplot(subset(wf_question, freq>0.09), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue4') +
  theme(axis.text.x=element_text(angle=45, hjust=1, size = 12),
        axis.text.y=element_text(size = 12)) +
  labs(x = 'Words', y = 'Percentage', title = bquote(paste(bold('Question'))))

# REQUEST
dtm_df_request <- dtm_df %>% 
  filter(type == 'request')
freq_request <- sort(colSums(as.matrix(dtm_df_request[,1:498])), decreasing=TRUE)   
wf_request <- data.frame(word=names(freq_request), freq = freq_request)
head(wf_request)
wf_request$freq <- (wf_request$freq/97)

plot_request <- ggplot(subset(wf_request, freq>0.09), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = 'identity', fill = 'purple4') +
  theme(axis.text.x=element_text(angle=45, hjust=1, size = 13),
        axis.text.y=element_text(size = 12)) +
  labs(x = 'Words', y = 'Percentage', title = bquote(paste(bold('Request'))))

# COMPLIMENT
dtm_df_compliment <- dtm_df %>% 
  filter(type == 'compliment')
freq_compliment <- sort(colSums(as.matrix(dtm_df_compliment[,1:498])), decreasing=TRUE)   
wf_compliment <- data.frame(word=names(freq_compliment), freq = freq_compliment)
head(wf_compliment)
wf_compliment$freq <- (wf_compliment$freq/32)


plot_compliment <- ggplot(subset(wf_compliment, freq>0.1), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = 'identity', fill = 'chartreuse4') +
  theme(axis.text.x=element_text(angle=45, hjust=1, size = 13),
        axis.text.y=element_text(size = 12)) +
  labs(x = 'Words', y = 'Percentage', title = bquote(paste(bold("Compliment"))))

library(gridExtra)

grid.arrange(plot_complaint, plot_question, plot_request, plot_compliment)
ggsave('classfreqs.png', height = 4, width = 6)
library(RColorBrewer)
library(wordcloud)
# wordcloud
?element_text
set.seed(10)
wordcloud(names(freq), freq, min.freq=30, max.words = 10)

# actual analysis maybe
library(dplyr)
library(caret)

# data partitions
#set.seed(1996)
#index_train <- createDataPartition(train$helpful, p = 0.7, list = F)
#ctrain <- train[index.train, ]
#ctest <- train[-index.train, ]

# To-do: replace this with caret variant
s <- sample(1:nrow(tdm_df), nrow(tdm_df)*(0.70), replace = FALSE) # random sampling

train <- tdm_df[s,] # training set

test <- tdm_df[-s,] # testing set

# set first seed
set.seed(1996)

train_index <- createDataPartition(dtm_df$type,
                                   p = 0.7, 
                                   list = FALSE)
train <- dtm_df[train_index, ]
test <- dtm_df[-train_index, ]

table(train$type)# class instances in train data
table(test$type)

ctrl <- trainControl(method = "repeatedcv", repeats = 5, 
                     classProbs = TRUE,
                     summaryFunction = multiClassSummary,
                     verboseIter = TRUE)



set.seed(10)
svm.tfidf <- train(train[,c(1:498)], train[,499],
                  method = "svmLinear3", 
                  trControl = ctrl,
                  tuneLength = 10,
                  metric = 'mlogLoss') # train svm

set.seed(10)
knn.tfidf <- train(train[,c(1:498)], train[,499],
                   method = "kknn", 
                   trControl = ctrl,
                   tuneLength = 20,
                   metric = 'mlogLoss') # train knn

set.seed(10)
sda.tfidf <- train(train[,c(1:498)], train[,499],
                   method = "lda", 
                   trControl = ctrl,
                   tuneLength = 10,
                   metric = 'mlogLoss') # train pda

set.seed(10)
xgb.tfidf <- train(train[,c(1:498)], train[,499],
                   method = "xgbTree", 
                   trControl = ctrl,
                   tuneLength = 10,
                   metric = 'mlogLoss') # train xgb

rf.tfidf
svm.tfidf
knn.tfidf
sda.tfidf
xgb.tfidf
nb.tfidf

?make.names
# predictions


svm.preds <- predict(svm.tfidf, newdata = test)
knn.preds <- predict(knn.tfidf, newdata = test)
sda.preds <- predict(sda.tfidf, newdata = test)
xgb.preds <- predict(xgb.tfidf, newdata = test)

table(test$type, xgb.preds)

source('calc_confusion.R')
confusionMatrix(xgb.preds, test$type, mode = 'everything')
confusionMatrix(knn.preds, test$type, mode = 'everything')
confusionMatrix(svm.preds, test$type, mode = 'everything')
confusionMatrix(sda.preds, test$type, mode = 'everything')

preds

table(test$type, svm.preds)
confusionMatrix(pda.preds)
?confusionMatrix
table(test$preds)

tdm_df <- cbind(tdm_df, ae$type)
glimpse(test)

tdm_dm[1:10, ]

ggplot(tdm_dm)

write_csv(test, path = 'testresults.csv')

