# Use RStudio to load in bm.Rda

library(rpart)
# now make a function for computing the accuracy
accuracy = function(cm){  # input confusion matrix
      return(sum(diag(cm))/sum(cm))  # accuracy
}

#now split the data into testing and training data sets
# we will first randomly select 2/3 of the rows
set.seed(432) # for reproduceable results
train = sample(1:nrow(bm),nrow(bm)*0.667)


# Use the train index set to split the dataset
#  bm.train for building the model
#  bm.test to test the model
bm.train = bm[train,]   # 400 rows
bm.test = bm[-train,]   # the other 200 rows

#now build the model - a large tree
# first load package rpart
library(rpart)

#grow the tree
# parms
#  xval = 10 : cross-sample with 10 folds to determine error rate at each node
#  minsplit = 2  : min number of observations to attempt split
#  cp = 0  : minimum improvement in complexity parameter for splitting
# smaller minsplit and cp result in larger trees
fit = rpart(ira ~ ., 
            data=bm.train, method="class",
            control=rpart.control(xval=10, minsplit=2, cp=0))

# object fit$frame has a row for each node in the tree
nrow(fit$frame) # 213 nodes

# plot tree 
plot(fit, # the tree to be plotted
     uniform=T, # uniform spacing of nodes
     branch=0.5, # bent branches
     compress=T, # take less space
     main="A Complex Tree", #title
     margin=0.0) #no extra space

text(fit,  # tree to be embellished
     splits=F, # do not detail split criteria
     all=F, # label only terminal nodes labeled
     use.n=T, # label leaves with observations
     pretty=F, # keep it simple
     cex=0.6) # compress fonts to 60%

# extract the vector of predicted values for IRA for every row
ira.pred = predict(fit, bm.train, type="class")
# extract the actual value of ira for every row
ira.actual = bm.train[,"ira"]

# now make a contingency table of actual versus predicted
# this is called the confusion matrix
# for RESUBSTITUTION
cm = table(ira.actual, ira.pred)
cm

# now use this to get resubstitution accuracy
accuracy(cm)

#now let us use the hold out data in bm.test
# as above, figure out the confusion matrix
ira.pred = predict(fit, bm.test, type="class")
ira.actual = bm.test[,"ira"]
cmtest = table(ira.actual, ira.pred)
cmtest
accuracy(cmtest)

# PRE-PRUNING
# first let us do some pre-pruning
# do not grow such a big tree
fit.small = rpart(ira ~ ., 
            data=bm.train, method="class",
            control=rpart.control(xval=10, minsplit=5, cp=0.0))
nrow(fit.small$frame)
plot(fit.small, uniform=T, branch=0.5, compress=T,
     main="Tree with minsplit = 5, 85 nodes", margin=0.0)
text(fit.small,  splits=F, all=F, use.n=T, pretty=F, cex=0.6)

#compute the confusion matrices and accuracy
# resubstitution
ira.pred = predict(fit.small, bm.train, type="class")
ira.actual = bm.train[,"ira"]
cm.small = table(ira.actual, ira.pred)
cm.small
accuracy(cm.small)
# test set
ira.pred = predict(fit.small, bm.test, type="class")
ira.actual = bm.test[,"ira"]
cm.small.test = table(ira.actual, ira.pred)
cm.small.test
accuracy(cm.small.test)

# Another approach:
# Examine the tradeoff between complexity and error rate
# Complexity Parameter is a very commonly used metric 
# as the tree grows, the CP decreases
plotcp(fit, # tree for which to plot
       upper="size")  # plot size of tree (no. of nodes) on top

#find the CP which provides the lowest error
fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

# It appears that lowest error occurs at CP = 0.008
# We can use that for pre-pruning
fit.small2 = rpart(ira ~ ., 
                  data=bm.train, method="class",
                  control=rpart.control(xval=10, cp=0.00713))
nrow(fit.small2$frame)
plot(fit.small2, uniform=T, branch=0.5, compress=T,
     main="Tree with cp=0.00713, (33 nodes)", margin=0.05)
text(fit.small2,  splits=T, all=F, use.n=T, 
                           pretty=T, fancy=F, cex=0.8)

# check the accuracy
# resubstitution
ira.pred = predict(fit.small2, bm.train, type="class")
ira.actual = bm.train[,"ira"]
cm.small2 = table(ira.actual, ira.pred)
cm.small2
accuracy(cm.small2)

# test
ira.pred = predict(fit.small2, bm.test, type="class")
ira.actual = bm.test[,"ira"]
cm.small2.test = table(ira.actual, ira.pred)
cm.small2.test
accuracy(cm.small2.test)


# POST-PRUNING
#yet another approach - post prune the big tree with prune.rpart
fit.post = prune.rpart(fit, cp=0.00713)
nrow(fit.post$frame)
plot(fit.post, uniform=T, branch=0.5, compress=T,
     main="Tree with Post-Pruning cp = 0.00713 (43 Nodes)", margin=0.05)
text(fit.post,  splits=T, all=F, use.n=T, 
     pretty=T, fancy=F, cex=0.8)

#accuracy of post pruned tree
# resubstitution
ira.pred = predict(fit.post, bm.train, type="class")
ira.actual = bm.train[,"ira"]
cm.smallp = table(ira.actual, ira.pred)
cm.smallp
accuracy(cm.smallp)

# test
ira.pred = predict(fit.post, bm.test, type="class")
ira.actual = bm.test[,"ira"]
cm.smallp.test = table(ira.actual, ira.pred)
cm.smallp.test
accuracy(cm.smallp.test)

cm.smallp = confusion.matrix(fit.post, bm.train)
cm.smallp
accuracy(cm.smallp)
cm.smallp.test = confusion.matrix(fit.post, bm.test)
cm.smallp.test
accuracy(cm.smallp.test)

