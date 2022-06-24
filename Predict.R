#install.packages('rpart.plot')
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(ggplot2)
library(knitr)
library(kableExtra)

#Load Data and Print Structure
Bank_Marketing=read.table(file = file.choose(),sep = ",",header = TRUE)
str(Bank_Marketing)


#Exclude those specified columns 
Bank_Marketing<- Bank_Marketing[,-c(1,19)]

#Transform the output variable into binary of 1 and 0.
Bank_Marketing$y = ifelse(Bank_Marketing$y=='yes',1,0)


# split into training and testing data
set.seed(123)
split = sample.split(Bank_Marketing$y,SplitRatio = 0.70)
training_set = subset(Bank_Marketing, split == TRUE)
test_set = subset(Bank_Marketing, split == FALSE)


# fit the decision tree classification
classifier = rpart(formula = y ~ .,
                   data = training_set, method = "class")

# plot
prp(classifier, type = 2, extra = 104, fallen.leaves = TRUE, main="Decision Tree")

# predict test data by probability
pred.DT = predict(classifier, newdata = test_set[-21], type = 'prob')
pred.DT
# find the threshold for prediction optimization
predictions_DT <- data.frame(y = test_set$y, pred = NA)
predictions_DT$pred <- pred.DT[,2]
predictions_DT$pred
plot_pred_type_distribution(predictions_DT,0.36)
test.eval.DT

# choose the best threshold as 0.36
test.eval.DT = binclass_eval(test_set[, 19], pred.DT[,2] > 0.38)
test.eval.DT$cm

# calculate accuracy, precision, recall and fscore.
acc_DT=test.eval.DT$accuracy
prc_DT=test.eval.DT$precision
recall_DT=test.eval.DT$recall
fscore_DT=test.eval.DT$fscore

cat("Accuracy:  ",   acc_DT,
    "\nPrecision: ", prc_DT,
    "\nRecall:    ", recall_DT,
    "\nFScore:    ", fscore_DT)

# calculate ROC curve
rocr.pred = prediction(predictions = pred.DT[,2], labels = test_set$y)
rocr.perf = performance(rocr.pred, measure = "tpr", x.measure = "fpr")
rocr.auc = as.numeric(performance(rocr.pred, "auc")@y.values)

# print ROC AUC
rocr.auc
# plot ROC curve
plot(rocr.perf,
     lwd = 3, colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1),
     text.adj = c(-0.2, 1.7),
     main = 'ROC Curve')
mtext(paste('Decision Tree - auc : ', round(rocr.auc, 5)))
abline(0, 1, col = "red", lty = 2)

# creating the classifier
classifier.lm = glm(formula = y ~ .,
                    family = binomial,
                    data = training_set)

pred_lm = predict(classifier.lm, type='response', newdata=test_set[-21])

# plot the prediction distribution
predictions_LR <- data.frame(y = test_set$y, pred = NA)
predictions_LR$pred <- pred_lm
plot_pred_type_distribution(predictions_LR,0.30)

# choose the best threshold as 0.30
test.eval.LR = binclass_eval(test_set[, 19], pred_lm > 0.30)

# Making the Confusion Matrix
test.eval.LR$cm

# calculate accuracy, precision, recall and fscore.
acc_LR=test.eval.LR$accuracy
prc_LR=test.eval.LR$precision
recall_LR=test.eval.LR$recall
fscore_LR=test.eval.LR$fscore

# print evaluation
cat("Accuracy:  ",   acc_LR,
    "\nPrecision: ", prc_LR,
    "\nRecall:    ", recall_LR,
    "\nFScore:    ", fscore_LR)

# calculate ROC
rocr.pred.lr = prediction(predictions = pred_lm, labels = test_set$y)
rocr.perf.lr = performance(rocr.pred.lr, measure = "tpr", x.measure = "fpr")
rocr.auc.lr = as.numeric(performance(rocr.pred.lr, "auc")@y.values)

# print ROC AUC
rocr.auc.lr

# plot ROC curve
plot(rocr.perf.lr,
     lwd = 3, colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1),
     text.adj = c(-0.2, 1.7),
     main = 'ROC Curve')
mtext(paste('Logistic Regression - auc : ', round(rocr.auc.lr, 5)))
abline(0, 1, col = "red", lty = 2)


# compare Accuracy and ROC Between the Two Models
compare <- data.frame(Method = c('Decision Tree', 'Logistic Regression'), Accuracy = NA, Precision = NA, Recall = NA, 
                      FScore = NA, 'ROC AUC' = NA)
compare$Accuracy <- c(acc_DT,acc_LR)
compare$ROC.AUC <- c(rocr.auc,rocr.auc.lr)
compare$Precision <- c(prc_DT, prc_LR)
compare$Recall <- c(recall_DT, recall_LR)
compare$FScore <- c(fscore_DT, fscore_LR)

# plot both ROC curves side by side for an easy comparison
par(mfrow=c(1,2))

# plot ROC curve for Decision Tree
plot(rocr.perf,
     lwd = 3, colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1),
     text.adj = c(-0.2, 1.7),
     main = 'ROC Curve')
mtext(paste('Decision Tree - auc : ', round(rocr.auc, 5)))
abline(0, 1, col = "red", lty = 2)

# plot ROC curve for Logistic Regression
plot(rocr.perf.lr,
     lwd = 3, colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1),
     text.adj = c(-0.2, 1.7),
     main = 'ROC Curve')
mtext(paste('Logistic Regression - auc : ', round(rocr.auc.lr, 5)))
abline(0, 1, col = "red", lty = 2)
kable_styling(kable(compare),c("striped","bordered"), full_width = F)