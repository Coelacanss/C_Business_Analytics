library(AppliedPredictiveModeling)
library(caret)

## Question 1
data(segmentationOriginal)
inTrain <- segmentationOriginal$Case=="Train"
training <- segmentationOriginal[inTrain,]
testing <- segmentationOriginal[-inTrain,]

set.seed(125)
fit <- train(Class ~., method = 'rpart', data= training)
fit$finalModel
plot(fit$finalModel, uniform=TRUE, main="Classification Tree")
text(fit$finalModel, use.n = TRUE, all = TRUE, cex = 0.8)


## Question 3
library(pgmm)
data(olive)
olive = olive[,-1]

fit <- train(Area ~ ., method = "rpart", data = olive)

newdata = as.data.frame(t(colMeans(olive)))
predict(fit, newdata)


## Question 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

set.seed(13234)
modelfit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
                  method = "glm", family = "binomial", data = trainSA)
trainingpredict <- predict(modelfit, trainSA)
testingpredict <- predict(modelfit, testSA)

missClass(trainSA$chd, trainingpredict)
missClass(testSA$chd, testingpredict)


## Question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)
fit <- randomForest(y ~ ., data = vowel.train, importance = FALSE)
order(varImp(fit), decreasing = TRUE)
varImp(fit)


