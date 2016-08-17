## BA hw3 MKT TEAM 11 
## ***DO NOT SHARE CODES***

library(rpart)

# Step 1 & 2: Reading File and Check Structure
churn <- read.csv("churn.csv", header = TRUE)
str(churn)

# Step 3: Change Colnames
colnames(churn) = toupper(colnames(churn))
colnames(churn)[12] = "LEAVE"

# Step 4: Reorder
churn$COLLEGE = factor(churn$COLLEGE, levels(churn$COLLEGE)[c(2,1)])
churn$REP_SAT = factor(churn$REP_SAT, levels(churn$REP_SAT)[c(5,3,1,2,4)])
churn$REP_USAGE = factor(churn$REP_USAGE, levels(churn$REP_USAGE)[c(5,3,1,2,4)])
churn$REP_CHANGE = factor(churn$REP_CHANGE, levels(churn$REP_CHANGE)[c(3,4,2,5,1)])
lapply(churn, levels)   # for check

# Step 5: Save as .Rda
save(churn, file = "churn.Rda")

# Step 6: Creat Sample
set.seed(3478)
train <- sample(1:nrow(churn),nrow(churn)*0.667)
churn.train = churn[train,]
churn.test = churn[-train,]


# Step 7 & 8: Grow a tree to explain the stay class variable, and display
fit = rpart(LEAVE ~ ., data = churn.train, method="class", 
            control = rpart.control(minsplit = 100))
fit

# Step 9: Explain rows numbered 1, 10, and 3. Which node is the parent node. 
# What was the immediate split that created it? What is the count of stay and 
# leave at this node?
# 
# Row 1:  Among all 13340 observations, 6525 are "leave", accounted for
#         48.9%; the rest are "stay", accounted for 51.1%.
# Row 3:  Among all 4461 observations that house is greater than 60440.5, 1379
#         are "leave", accounted for 30.9%; the rest are "stay", accounted for
#         69.1%.
# Row 10: Among all 1995 observations that house < 604440.5 and overage >= 97.5,
#         774 are "stay", accounted for 38.8%; the rest are "stay", accounted
#         for 61.2%.
# Row 1 is the parent node. It splits the group that house >= 60440.5 and the 
# group that house < 60440.5. At this node, the count of stay is 6815 and the 
# count of leave is 6525.

# Step 10: Plot and label the tree, save as PDF
plot(fit, uniform = T, branch = 1, margin = 0.17)
text(fit, use.n = T, pretty = T, fancy = F)
dev.copy(pdf, file="plot.pdf", width = 10)
dev.off()

# Step 11: Print the confusion matrix for the test data set
churn.pred = predict(fit, churn.test, type = "class")
churn.actual = churn.test[,"LEAVE"]
confusion.matrix = table(churn.actual, churn.pred)
confusion.matrix

# Step 12: Determine the accuracy, error rates, recall, specificity, 
# and precision for this tree and the test data set.
accuracy = (table(churn.actual, churn.pred)[1,1] + 
            table(churn.actual, churn.pred)[2,2]) /
            nrow(churn.test)
FPR = table(churn.actual, churn.pred)[1,2] / 
      sum(table(churn.actual, churn.pred)[1,])
FNR = table(churn.actual, churn.pred)[2,1] / 
      sum(table(churn.actual, churn.pred)[2,])
TPR = table(churn.actual, churn.pred)[2,2] / 
      sum(table(churn.actual, churn.pred)[2,])
TNR = table(churn.actual, churn.pred)[1,1] / 
      sum(table(churn.actual, churn.pred)[1,])
Precision = table(churn.actual, churn.pred)[2,2] / 
            sum(table(churn.actual, churn.pred)[,2])
FPR  # false positive rate
FNR  # false negative rate
TPR  # true positive rate or recall
TNR  # true negative rate or specificity
Precision #precision

