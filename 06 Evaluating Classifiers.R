bm.raw <- read.csv("bank-data.csv")
bm = bm.raw[,c(2:12)]
bm$age = cut(bm$age, breaks = c(18,36,48,57), lables = c("yound", "mid", "old"))
bm$income = cut(bm$income, breaks = c(0, 20000, 30000, Inf), lables = c("low", "mid", "high"))
bm$children = as.factor(bm$children)

# create sample
set.seed(345)
train = sample(1:nrow(bm), nrow(bm)*0.667)
bm.train = bm[train,]
bm.test = bm[-train,]

# plot tree
fit = rpart(ira ~ ., data = bm.train, method = "class", 
            control = rpart.control(xval = 0, minsplit = 5),
            parms = list(split = "information"))

plot(fit, uniform = T, branch = 0.5, main = "Classification",margin = 0.1)
text(fit, use.n = T, pretty = T, cex = 0.6)

# confusion matrix
ira.pred = predict(fit, bm.test, type = "class")
ira.actual = bm.test[,"ira"]
confusion.matrix = table(ira.actual, ira.pred)
confusion.matrix

# Using adabag.package
library(adabag)
fitcv = bagging.cv(ira ~ ., bm, v=10, mfinal = 1, 
                   control = rpart.control(xval = 0, minsplit = 5, split = "information"))
fitcv$confusion



