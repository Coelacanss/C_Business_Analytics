### Topic 05: Introduction to Classification

load(file.choose())  ## choose bm.t.Rda

## Using gmodels
library(gmodels)
with(bm.t, CrossTable(ira))
with(bm.t, CrossTable(age, ira, prop.chisq = F, prop.t = F, prop.c = F, prop.r = T))
with(bm.t, CrossTable(children, ira, prop.chisq = F, prop.t = F, prop.c = F, prop.r = T))
with(bm.t, CrossTable(married, ira, prop.chisq = F, prop.t = F, prop.c = F, prop.r = T))

## Using rpart
library(rpart)
# Grow Tree
fit <- rpart(ira ~ age + married + children, data = bm.t, method = "class", 
             control = rpart.control(xval = 0, minsplit = 3), 
             parms = list(split = "information"))
fit
# Plot Tree
plot(fit, uniform = TRUE, branch = 0.5, compress = T, 
     main = "Classification Tree for Marketing", margin = 0.1)
text(fit, use.n = T, all = T, fancy = TRUE, pretty = T)

