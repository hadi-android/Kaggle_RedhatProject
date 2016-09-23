rm(list=ls(all=T))
# load requied libraries --------------------------------------------------
library(data.table)
library(caret)
# load and transform people data ------------------------------------------
ppl <- fread("people.csv")
act_train <- fread("act_train.csv")
data_train <- merge(ppl,act_train,by="people_id")

#prepare training data set
data_train <- data.frame(data_train)
data_train <- data_train[,-c(1,5,42,43)]
data_train = lapply(data_train,as.factor)
data_train = data.frame(data_train)
data_train$char_38 = as.numeric(data_train$char_38)
#data_train = lapply(data_train,as.numeric)
#data_train = data.frame(data_train)
#data_train$outcome = as.factor(data_train$outcome)

data_type1_tr = subset(data_train,data_train$activity_category=="type 1")

ind_type1 = nearZeroVar(data_type1_tr)
data_type1_tr = data_type1_tr[,-ind_type1]

data_rest_tr = subset(data_train, data_train$activity_category!="type 1")
ind_rest = nearZeroVar(data_rest_tr)
data_rest_tr = data_rest_tr[,-ind_rest]

#prepare testing data set
act_test = fread("act_test.csv")
act_test <- merge(ppl,act_test,by="people_id")
act_test = data.frame(act_test)
act_test <- act_test[,-c(1,5,42,43)]
act_test = lapply(act_test,as.factor)
act_test = data.frame(act_test)
act_test$char_38 = as.numeric(act_test$char_38)

data_type1_te = data.frame(subset(act_test, act_test$activity_category=="type 1"))
data_type1_te = data_type1_te[,-ind_type1]

data_rest_te = subset(act_test, act_test$activity_category!="type 1")
data_rest_te = data_rest_te[,-ind_rest]

rm(ppl)
rm(act_test)
rm(data_train)
rm(act_train)