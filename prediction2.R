#set.seed(11111)
set.seed(123)
load("data_prep.RData")
library(caret)
library(data.table)
library(ROCR)
library(OptimalCutpoints)
#str(data_type1_tr)
data_type1_tr = lapply(data_type1_tr, as.numeric)
data_type1_tr = data.frame(data_type1_tr)
data_type1_tr$outcome = as.factor(data_type1_tr$outcome)
X = c(1:dim(data_type1_tr)[1])
samp = sample(X,15000)
data_type1_tr = data_type1_tr[samp,]
inTrain <- createDataPartition(data_type1_tr$outcome, p=0.7, list = FALSE)
tr_set = data_type1_tr[inTrain,]
te_set = data_type1_tr[-inTrain,]
# 
# control <- trainControl(method="repeatedcv", number=10, search="random")
# start = Sys.time()
# rf_random <- train(outcome~., data=tr_set, method="rf", metric="Accuracy", tuneLength=10, trControl=control)
# end= Sys.time()
# elapsed = end-start
# elapsed
# print(rf_random)
# plot(rf_random)

# do prediction on validation set
load("rf_random.RData")
pred_rf = predict(rf_random, te_set)
confmat = table(pred_rf, te_set$outcome)
sens = confmat[1,1]/(confmat[1,1]+confmat[1,2])
spec = confmat[2,2]/(confmat[2,2]+confmat[2,1])

result.pr = predict(rf_random, type="prob", newdata=te_set)[,2]
result.pred = prediction(result.pr, te_set$outcome)
result.perf = performance(result.pred,"tpr","fpr")
result.auc = performance(result.pred, "auc")
ss = performance(result.pred, "sens", "spec")
idx = which.max(ss@x.values[[1]]+ss@y.values[[1]])
ss@x.values[idx]
ss@y.values[idx]
plot(result.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

# now do the prediction on the test set
data_type1_te = lapply(data_type1_te, as.numeric)
data_type1_te = data.frame(data_type1_te)
str(data_type1_te)
predict_te = predict(rf_random, data_type1_te)

# now build model for the rest of the types
data_rest_tr = lapply(data_rest_tr, as.numeric)
data_rest_tr = data.frame(data_rest_tr)
data_rest_tr$outcome = as.factor(data_rest_tr$outcome)
str(data_rest_tr)
set.seed(234)
X = c(1:dim(data_rest_tr)[1])
samp = sample(X,15000)
data_rest_tr = data_rest_tr[samp,]
inTrain <- createDataPartition(data_rest_tr$outcome, p=0.7, list = FALSE)
tr_set = data_rest_tr[inTrain,]
te_set = data_rest_tr[-inTrain,]
control <- trainControl(method="repeatedcv", number=10, search="random")
start = Sys.time()
rf_random2 <- train(outcome~., data=tr_set, method="rf", metric="Accuracy", tuneLength=10, trControl=control)
end= Sys.time()
print(rf_random2)
elapsed = end-start

# do prediction on validation set
pred_rf = predict(rf_random2, te_set)
confmat = table(pred_rf, te_set$outcome)
sens = confmat[1,1]/(confmat[1,1]+confmat[1,2])
spec = confmat[2,2]/(confmat[2,2]+confmat[2,1])

# check ROC
result.pr = predict(rf_random2, type="prob", newdata=te_set)[,2]
result.pred = prediction(result.pr, te_set$outcome)
result.perf = performance(result.pred,"tpr","fpr")
result.auc = performance(result.pred, "auc")
ss = performance(result.pred, "sens", "spec")
idx = which.max(ss@x.values[[1]]+ss@y.values[[1]])
ss@x.values[idx]
ss@y.values[idx]
plot(result.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

# now do the prediction on the test set
data_rest_te = lapply(data_rest_te, as.numeric)
data_rest_te = data.frame(data_rest_te)
str(data_rest_te)
predict_te = predict(rf_random2, data_rest_te)
