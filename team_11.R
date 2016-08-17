### Business Analytics hw5 | MKT Team_11  0.18333
### DO NOT SHARE CODES WITH OTHERS

library(tm)
library(SnowballC)
library(e1071)
library(caret)
library(ggplot2)
library(ROCR)
library(rpart)

accuracy = function(cm){
      return(sum(diag(cm))/sum(cm))  
}

# Question 1: read files
filePos = read.csv(file = 'Tweets_pos.txt', header = F, stringsAsFactors = F)
fileNeg = read.csv(file = 'Tweets_neg.txt', header = F, stringsAsFactors = F)
fileEval = read.csv(file = 'Tweets_eval.txt', header = F, stringsAsFactors = F)

# Question 2: combine all files into one dataframe
fileAll = rbind(filePos, fileNeg, fileEval)

# Question 3: a vector that has the indices of the evaluation tweets
eval = c(301:379)

# Question 4: make a vector class with the labels 
class = c(rep('pos', 200), rep('neg', 100))
class = as.factor(class)

# Question 5: read the data frame with all the tweets into a corpus.
all.reviews.raw = VCorpus(VectorSource(fileAll[,1]))

# Question 6: clean the documents in the corpus
all.reviews = tm_map(all.reviews.raw, content_transformer(tolower))
all.reviews = tm_map(all.reviews, content_transformer(removeNumbers))
all.reviews = tm_map(all.reviews, content_transformer(removePunctuation))
all.reviews = tm_map(all.reviews, removeWords, stopwords("english"))
all.reviews = tm_map(all.reviews, stemDocument)
all.reviews = tm_map(all.reviews, content_transformer(stripWhitespace))

# Question 7: make a DTM with all the documents (pos, neg, and eval, all together)
dtm = DocumentTermMatrix(all.reviews, control = list(weighting = weightTfIdf))

# Question 8: pick a sparsity setting and remove sparse terms from the full dtm

sparsity.setting = c()
sparsity = c()
err = c()
acc = c()
num.terms = c()
j = 0

for (i in seq(from = 0.960, to = 0.975, by = 0.001)) { # I find best sparsity is between 0.96 and 0.975
                                                       # after trying several times.
      j = j + 1
      dtm = DocumentTermMatrix(all.reviews)
      dtm = removeSparseTerms(dtm, i)
      
      sparsity.setting[j] = i
      sparsity[j] = 1 - length(dtm)/(dtm$ncol * dtm$nrow)
      num.terms[j] = dtm$ncol
      
      dtm.df = as.data.frame(as.matrix(dtm))
      dtm.train = dtm.df[-eval,]
      dtm.test = dtm.df[eval,]
      
      model = naiveBayes(dtm.train, class)
      pred = predict(model, dtm.train)
      cm = table(class, pred)
      acc[j] = accuracy(cm)
      err[j] = 1 - acc[j]
      
}

report = data.frame(sparsity.setting, sparsity, num.terms, acc, err)
report
bestSparsity = report[which.min(report$err), 'sparsity.setting']

dtm = DocumentTermMatrix(all.reviews, control = list(weighting = weightTfIdf))
dtm = removeSparseTerms(dtm, bestSparsity)
dtm

# Question 9: split the cleaned dtm into two parts
dtm.df = as.data.frame(as.matrix(dtm))
dtm.train = dtm.df[-eval,]
dtm.evel = dtm.df[eval,]

# Question 10: build the model using naive Bayes,
# consider using cross validation in the Caret package.
train_control = trainControl(method="cv", number=10, repeats=1)  # generally number = 10, 
model = train(dtm.train, class, trControl=train_control, method="nb")

## Here I use all 300 obs to build the model, instead of using 200 to build model and rest to test,
## because I believe such few obs are not enough to build a good model, thus eliminating obs will
## strongly hurt the fitness of the model.

pred = predict(model, dtm.train) # resub accuracy
cm = table(class, pred)
accuracy(cm) 

pred = predict(model, dtm.evel)
result = fileEval
result$pred = pred
View(result)
save(pred, file = 'Prediction-Team11.Rda')
