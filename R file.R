rm(list = ls())

#Setting path
setwd("C:/Users/michael/Desktop/edWisor/PROJECT/santander project")
getwd()

#LOADING LIBRARIES
library(data.table)
library(DT)
library(ggplot2)
library(ggthemes)
library(DMwR)
library(caret)
library(randomForest)
library(unbalanced)
library(C50)
library(dummies)
library(MASS)
library(rpart)
library(gbm)
library(ROSE)
library(pct)
library(corrgram)
library(mice)
library(DataCombine)
library(AUC)
library(e1071)

#LOADING both datasets
train = fread('train.csv')
test = fread('test.csv')

#LOOKING AND SUMMARIZING BOTH DATASETS
train[1:6, 1:3]
test[1:6, 1:3]
dim(train)
dim(test)

#################################### EXPLORATARY DATA ANALYSIS #######################################
#MISSING VALUE ANALYSIS
missing_values = train[, lapply(.SD, function(x) sum(!is.na(x))/.N), .SDcols = names(train)] %>% melt(variable.name = 'feature', value.name = 'complete_pct')
setorder(missing_values, complete_pct)
head(missing_values, 10) %>% ggplot(aes(x = reorder(feature, -complete_pct), y = complete_pct)) + geom_bar(stat = "identity", fill = "blue") + coord_flip() + theme_few() + labs(x = 'Variables', y = 'Data completeness')

#DISTRIBUTION OF TARGET VARIABLE
train[!is.na(target), .(count = .N), by = target] %>% ggplot(aes(x = target, y = count, fill = target)) + geom_bar(stat = "identity", position = "identity") + geom_text(aes(label = paste0(sprintf("%0.1f", round(count/sum(count)*100,1)), '%'), vjust = -0.10)) + theme_few() + labs(y = "Customers", x = "Percentage of Transactions made") + theme(legend.position = 'none')



##################################### MODEL SELECTION #################################################

#DIVIDING THE DATA INTO TRAIN AND TEST
set.seed(1234)
train.index = createDataPartition(train$target, p = 0.8, list = FALSE)
train_data = train[train.index]
test_data = train[-train.index]

########################################### LOGISTIC REGRESSION ######################################
#Accuracy = 91.4%
#FNR = 72.7
#Recall = 27
#Precision = 69.9
#F1 score = 38.8

model1 = glm(target ~., data = train_data, family = "binomial") 
summary(model1)
model1_pred = predict(model1, newdata = test_data, type = "response")
model1_pred = ifelse(model1_pred > 0.5, 1, 0)


con_mat1 = table(test_data$target, model1_pred)
#accuracy = TP+TN/Total no. of obs
#precision = TP/(TP+FP)
#recall = TP/(TP+FN)
#F1 score = 2*((Precision*Recall)/(Precision+Recall))

#TP = 1095
#TN = 35488
#FP = 471
#FN = 2946



final = data.table(ID_code = test, target = model1_pred)
head(final, 5)
final[1:7, 1]
final[1:7, 202]

fwrite(final, "./final.csv")

################################################ DISTRIBUTIONS ###############################################
#Distribution of continuous variables
histogram1 = ggplot(data = train, aes(x = var_0)) + ggtitle("Distribution of Var_0") + geom_histogram(bins = 25)
histogram2 = ggplot(data = train, aes(x = var_1)) + ggtitle("Distribution of Var_1") + geom_histogram(bins = 25)
histogram3 = ggplot(data = train, aes(x = var_2)) + ggtitle("Distribution of Var_2") + geom_histogram(bins = 25)
histogram4 = ggplot(data = train, aes(x = var_3)) + ggtitle("Distribution of Var_3") + geom_histogram(bins = 25)
histogram5 = ggplot(data = train, aes(x = var_4)) + ggtitle("Distribution of Var_4") + geom_histogram(bins = 25)
histogram6 = ggplot(data = train, aes(x = var_5)) + ggtitle("Distribution of Var_5") + geom_histogram(bins = 25)
histogram7 = ggplot(data = train, aes(x = var_6)) + ggtitle("Distribution of Var_6") + geom_histogram(bins = 25)
histogram8 = ggplot(data = train, aes(x = var_7)) + ggtitle("Distribution of Var_7") + geom_histogram(bins = 25)
histogram9 = ggplot(data = train, aes(x = var_8)) + ggtitle("Distribution of Var_8") + geom_histogram(bins = 25)
histogram10 = ggplot(data = train, aes(x = var_9)) + ggtitle("Distribution of Var_9") + geom_histogram(bins = 25)
histogram11 = ggplot(data = train, aes(x = var_10)) + ggtitle("Distribution of Var_10") + geom_histogram(bins = 25)


gridExtra::grid.arrange(histogram1, histogram2, histogram3, histogram4, histogram5, histogram6, histogram7, histogram8, histogram9, histogram10, histogram11, ncol = 5)

