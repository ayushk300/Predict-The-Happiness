library('data.table')
library("tm", lib.loc="~/R/win-library/3.3")
library("SnowballC", lib.loc="~/R/win-library/3.3")
library(h2o)
library("caTools" , lib.loc ="~/R/win-library/3.3")
train = fread('train.csv', stringsAsFactors = T)
test = fread('test.csv', stringsAsFactors = T)

test$Is_Response = train[1, 5]
train_test = rbind(train,test)
corpus = VCorpus(VectorSource(train_test$Description))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)

corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)

#convert it into data table
dataset_train_test = as.data.frame(as.matrix(dtm))
train_row <- nrow(train)
test_row <- nrow(test)
dataset = dataset_train_test[1:train_row,]
train_row_fetch = train_row+1
test_dataset = dataset_train_test[train_row_fetch:nrow(dataset_train_test),]

dataset$Is_Response = train$Is_Response
gc(reset=TRUE)
tracemem(dataset)
dataset <- as.data.table(dataset)
tracemem(test_dataset)
test_dataset <- as.data.table(test_dataset)

#remove unwanted things
remove(corpus)
remove(dtm)
remove(dataset_train_test)

dataset$Is_Response = factor(dataset$Is_Response,levels = c('happy', 'not happy'),labels= c(1,0))

set.seed(123)
split = sample.split(dataset$Is_Response, SplitRatio = 0.8)
training_dataset = subset(dataset, split == TRUE)

Cross_validation_dataset = subset(dataset, split == FALSE)
detach("package:caTools", unload=TRUE)
detach("package:tm", unload=TRUE)
detach("package:SnowballC", unload=TRUE)

localH2O <- h2o.init(nthreads = -1)
h2o.init()

dataset.h2o <- as.h2o(training_dataset)
test_dataset.h2o <- as.h2o(test_dataset)
Cross_validation_dataset.h2o <- as.h2o(Cross_validation_dataset)
ncol(test_dataset)
indep <- 1:(ncol(dataset)-1)
dep <- ncol(dataset)

#rforest.model <- h2o.randomForest(y = dep, x = indep, training_frame = dataset.h2o, ntrees =10)
gbm.model <- h2o.gbm(training_frame = dataset.h2o, x = indep , y = dep, 
                     ntrees= 500, max_depth = 8, learn_rate = 0.1,  stopping_rounds = 20, validation_frame = Cross_validation_dataset.h2o)
#airlines.gbm <- h2o.gbm(x = predictors, y = response, training_frame = train, validation_frame = valid, stopping_metric = "AUC", stopping_rounds = stopping_tolerance = 1e-2, seed = 1234)
y_pred <- as.data.frame(h2o.predict(gbm.model,test_dataset.h2o))


#y_pred <- as.data.frame(h2o.predict(rforest.model,test_dataset.h2o))
Cross_validation_dataset.h2o <- Cross_validation_dataset.h2o[,indep]
y_pred_cross_validation <- as.data.frame((h2o.predict(gbm.model,Cross_validation_dataset.h2o)))


sub <- data.table(User_ID = test$User_ID, Is_Response= y_pred$predict)
cross_validation <- data.table(Is_Response = Cross_validation_dataset$Is_Response, Is_Response_predict= y_pred_cross_validation$predict)
#test2 <- data.table(User_ID = test$User_ID, comment = test$Description, Is_Response= y_pred$predict)
#train2 <- data.table(User_ID = train$User_ID, comment = train$Description, Is_Response = train$Is_Response, Is_Response_predict= y_pred_train$predict)
#View(sub)

sub[Is_Response==0, Is_Response:='not_happy']
sub[Is_Response=='not happy', Is_Response:='not_happy']

sub[Is_Response==1, Is_Response:='happy']
#test2[Is_Response==0, Is_Response:='not happy']
#test2[Is_Response==1, Is_Response:='happy']
#train2[Is_Response_predict==0, Is_Response:='not happy']
#train2[Is_Response_predict==1, Is_Response:='happy']

fwrite(sub,file="sub6.csv")
#fwrite(test2,file="testing1.csv")
#fwrite(train2,file="training1.csv")

#precision , recall, accuracy , f1_score
cm = table( cross_validation$Is_Response,y_pred_cross_validation$predict)
#cm = caret::confusionMatrix( y_pred_cross_validation$predict,cross_validation$Is_Response)
true_positive = cm[1,2]
true_negative = cm[2,1]
false_positive = cm[1,1]
false_negative = cm[2,2]
accuracy = (true_negative + true_positive)/(false_positive + false_negative + true_negative + true_positive)
precision = true_positive /(true_positive + false_positive)
recall = true_positive /(true_positive + false_negative)
f1_score = 2*(precision * recall)/(precision + recall)
