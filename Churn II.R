## BA hw4 Churn II  |  MKT Team 11
## DO NOT SHARE CODES WITH OTHERS

library(rpart)
library(ROCR)
load(file.choose())   # load data from "churn.Rda"
accuracy = function(cm) {
      return(sum(diag(cm)) / sum(cm))
}

## Question 1 : create subsets of train data and test data
set.seed(3478)
train <- sample(1:nrow(churn), nrow(churn) * 0.667)
churn.train = churn[train, ]
churn.test = churn[-train, ]

## Question 2
fit.big = rpart(LEAVE ~ ., data = churn.train, method = "class", 
                control = rpart.control(minsplit = 1, cp = 0))
nrow(fit.big$frame)
# error rates
churn.pred.big = predict(fit.big, churn.test, type = "class")
churn.actual.big = churn.test[, "LEAVE"]
confusion.matrix.big = table(churn.actual.big, churn.pred.big)
confusion.matrix.big
fpr.big = table(churn.actual.big, churn.pred.big)[1,2] / 
          sum(table(churn.actual.big, churn.pred.big)[1,])
fnr.big = table(churn.actual.big, churn.pred.big)[2,1] / 
          sum(table(churn.actual.big, churn.pred.big)[2,])
acc.big = accuracy(confusion.matrix.big)

## Question 3
# find the best cp
plotcp(fit.big, upper = "size")
bestCP = fit.big$cptable[which.min(fit.big$cptable[,"xerror"]),"CP"]
fit.best = prune.rpart(fit.big, cp = bestCP)
nnodes = nrow(fit.best$frame)
# plot the tree
plot(fit.best, uniform = T, branch = 0.5, compress = T, 
     main = paste("Pruned Tree, cp =", bestCP, nnodes, "nodes"), margin = 0.02)
text(fit.best, splits = T, all = F, use.n = T, pretty = T, fancy = F, cex = 0.5)
# save the plot
dev.copy(pdf, file="plot.pdf", width = 10)
dev.off()
# error rates
churn.pred.best = predict(fit.best, churn.test, type = "class")
churn.actual.best = churn.test[,"LEAVE"]
confusion.matrix.best = table(churn.actual.best, churn.pred.best)
confusion.matrix.best
fpr.best = table(churn.actual.best, churn.pred.best)[1,2] / 
          sum(table(churn.actual.best, churn.pred.best)[1,])
fnr.best = table(churn.actual.best, churn.pred.best)[2,1] / 
          sum(table(churn.actual.best, churn.pred.best)[2,])
acc.best = accuracy(confusion.matrix.best)

## Question 4
leave.pred.test = predict(fit.best, churn.test, type ="prob")
leave.pred.score = prediction(leave.pred.test[,2], churn.test$LEAVE,
                              label.ordering = c("LEAVE", "STAY"))
leave.pred.perf = performance(leave.pred.score, "tpr", "fpr")

# for observation
plot(leave.pred.perf, colorize = T, lwd = 4)
abline(0,1)
abline(h = 1)
abline(v = 0)

# Profit = 510,000*(1-fpr)*(0.5*600-0.5*400) + 490,000*fnr*600 + 490,000*(1-fnr)*1000
#        = 835,000,000 - 51,000,000*fpr - 196,000,000*fnr
leave.cost = performance(leave.pred.score, measure = 'cost', 
                         cost.fn = 196000000, cost.fp = 51000000)
plot(leave.cost)
leave.cost
cutoffs = data.frame(cut = leave.cost@"x.values"[[1]],
                    cost = leave.cost@"y.values"[[1]])
cutoff = cutoffs[which.min(cutoffs$cost), "cut"]
leave.pred.test.cutoff = ifelse(leave.pred.test[,2] < cutoff, "LEAVE", "STAY")

# error rates
cm.BTPT = table(churn.test$LEAVE, leave.pred.test.cutoff)
cm.BTPT
fpr.BTPT = table(churn.test$LEAVE, leave.pred.test.cutoff)[1,2] / 
      sum(table(churn.test$LEAVE, leave.pred.test.cutoff)[1,])
fnr.BTPT = table(churn.test$LEAVE, leave.pred.test.cutoff)[2,1] / 
      sum(table(churn.test$LEAVE, leave.pred.test.cutoff)[2,])
acc.BTPT = accuracy(cm.BTPT)

## Question 5
ev.big = 835000000 - 51000000*fpr.big - 196000000*fnr.big
ev.best = 835000000 - 51000000*fpr.best - 196000000*fnr.best
ev.BTPT = 835000000 - 51000000*fpr.BTPT - 196000000*fnr.BTPT

result <- data.frame(FPR = c(fpr.big, fpr.best, fpr.BTPT),
                     FNR = c(fnr.big, fnr.best, fnr.BTPT),
                     Accuracy = c(acc.big, acc.best, acc.BTPT),
                     Expected_Value = c(ev.big, ev.best, ev.BTPT),
                     row.names = c("Big Tree", "Pruned Tree", "Best Threshold Pruned Tree"))
View(result)

