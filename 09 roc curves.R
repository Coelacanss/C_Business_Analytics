# Use RStudio to load in bm.Rda

#now split the data into testing and training data sets
# we will first randomly select 2/3 of the rows
set.seed(432) # for reproduceable results
train = sample(1:nrow(bm),nrow(bm)*0.667)


# Use the train index set to split the dataset
#  bm.train for building the model
#  bm.test to test the model
bm.train = bm[train,]   # 400 rows
bm.test = bm[-train,]   # the other 200 rows

# now make a function for computing the accuracy
accuracy = function(cm){  # input confusion matrix
  return(sum(diag(cm))/sum(cm))  # accuracy
}

#now build the model - a large tree
# first load package rpart
library(rpart)

#grow a small tree by pre-pruning on cp = 0.05
# parms
#  xval = 10 : cross-sample with 10 folds to determine error rate at each node
#  cp = 0.05  : minimum improvement in complexity parameter for splitting
# smaller minsplit and cp result in larger trees
fit.small = rpart(ira ~ ., 
            data=bm.train, method="class",
            control=rpart.control(xval=10, cp=0.05))
nrow(fit.small$frame)
par(mar = rep(1, 4))
plot(fit.small, uniform=T, branch=0.5, compress=T,
     main="Tree with cp = 0.05 (7 nodes)", margin=0.1)
text(fit.small,  splits=T, all=T, use.n=T, pretty=T, fancy=T, cex=1.2)

# ROC Curves
# make a smaller test set
bm.tests = bm.test[1:20,]
# we have been using class predictions
predict(fit.small, bm.tests, type="class")
# now for probability predictions
ira.pred.small.test = predict(fit.small, bm.tests, type = "prob")
ira.pred.small.test

#load ROCR package
library(ROCR)
ira.pred.small.score = # first of two steps - compute the score
    prediction(ira.pred.small.test[,2],  # the predicted P[Yes]
               bm.tests$ira) # the actual class
# next step is to compute the performance object
ira.pred.perf = performance(ira.pred.small.score, "tpr", "fpr")

# the ROC plot
plot(ira.pred.perf)
abline(0,1)  # abline(intercept, slope) -- plot the diagonal line

# now let us grow the big tree, post prune it and examine its ROC 
# parms
#  xval = 10 : cross-sample with 10 folds to determine error rate at each node
#  minsplit = 2  : min number of observations to attempt split
#  cp = 0  : minimum improvement in complexity parameter for splitting
# smaller minsplit and cp result in larger trees
fit = rpart(ira ~ ., 
            data=bm.train, method="class",
            control=rpart.control(xval=10, minsplit=2, cp=0))
fit.post = prune.rpart(fit, cp=0.00713)
nrow(fit.post$frame)
# the confusion matrix
# test
ira.pred = predict(fit.post, bm.test, type="class")
ira.actual = bm.test[,"ira"]
cm.smallp.test = table(ira.actual, ira.pred)
cm.smallp.test
accuracy(cm.smallp.test)
# FPR = 12/113 = 0.1062
# FNR = 13/87 = 0.1494

# ROC for fit.post
# first get the prob predictions from the tree
ira.pred.test = predict(fit.post, bm.test, type = "prob")
# second create the score object using prediction()
ira.pred.score = prediction(ira.pred.test[,2],bm.test$ira,
                label.ordering=c("NO", "YES")) # order neg pos
# third create the performance() object
ira.pred.perf = performance(ira.pred.score, "tpr", "fpr")

plot(ira.pred.perf, 
     colorize=T, # colorize to show cutoff values
     lwd=4) # make the line 4 times thicker than default
abline(0,1)  # draw a line with intercept=0, slope = 1
abline(h=1) #draw a horizontal line at y = 1
abline(v=0) #draw a vertical line at x = 0
# more plot options: 
# http://www.statmethods.net/advgraphs/parameters.html

# now for cost tradeoff
# the expected profit function from clas: 450 - 450 beta - 950 alpha
ira.cost = performance(ira.pred.score, measure="cost", cost.fn=450, cost.fp=950)
plot(ira.cost)
#seems to be minimized around 0.83
# meaning if Prob[yes] < 0.83 assign to NO else YES
# Let us make predictions using this cutoff rate

#create a vector of predictions based on optimal cutoff value
cutoffs = data.frame(cut = ira.cost@"x.values"[[1]],
                     cost = ira.cost@"y.values"[[1]])
cutoff = cutoffs[which.min(cutoffs$cost), "cut"]
ira.pred.test.cutoff = 
  ifelse(ira.pred.test[,2] < 0.8333,"NO","YES")

#make the confusion matrix using table()
cm = table(bm.test$ira,ira.pred.test.cutoff)
cm

accuracy(cm)

# General evaluation of ROC of classifiers
# Area Under the Curve
# area under the curve (AUC)
ira.auc = performance(ira.pred.score, "auc")
ira.auc@y.values


