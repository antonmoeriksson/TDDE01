# Lab 2 assignment II
# Anton Mo Eriksson
# anter491
# 2017-12-04

# Import data.
setwd("/home/ame/git/tdde01/TDDE01/lab-2")

require(readxl)
library(readxl)

credit_score = read.csv("creditscoring.csv")

# Divade data into 3 sets.
n = dim(credit_score)[1]
set.seed(12345)
id = sample(1:n, floor(n * 0.5))
train = credit_score[id, ]
test_tmp = credit_score[-id, ]

n_2 = dim(test_tmp)[1]
id_2 = sample(1:n_2, floor(n_2 * 0.5))
validation = test_tmp[id_2, ]
test = test_tmp[-id_2, ]

misclassification_rate_matrix <- function(data, prediction) {
  t = table(data$good_bad, prediction)
  print(t)
  mcr = (1 - (sum(diag(t) / sum(t))))
  return (mcr)
}
# Fir decision tree, using the following measures of impurity
# a. Deviance
# b. Gini index
require(MASS)
library(MASS)

require(e1071)
library(e1071)

require(tree)
library(tree)

require(rpart)
library(rpart)

deviance_fit = tree(good_bad~., data = data.frame(train), split = "deviance")
deviance_predict_train = predict(deviance_fit, newdata = train, type = "class")
deviance_predict_test = predict(deviance_fit, newdata = test, type = "class")

gini_fit = tree(good_bad~., data = data.frame(train), split = "gini")
gini_predict_train = predict(gini_fit, newdata = train, type = "class")
gini_predict_test = predict(gini_fit, newdata = test, type = "class")

misclassification_rate_matrix(test, deviance_predict_test)
misclassification_rate_matrix(train, deviance_predict_train)

# Task II-iii
summary(deviance_fit)

best_fit = deviance_fit

traning_score = c()
validation_score =  c()

for (index in 2:11) {
  prune_tree = prune.tree(best_fit, best = index)
  predict_tree = predict(prune_tree, validation, "tree")
  pred=predict(prune_tree, newdata=validation ,type="tree")
  traning_score[index] = deviance(prune_tree)
  validation_score[index] = deviance(predict_tree)
}

plot(2:11, traning_score[2:11], type = "b", col = "green", ylim = c(250, 600))
points(2:11, validation_score[2:11], type = "b", col = "red")

optimal_tree = prune.tree(best_fit, best = 4)
plot(optimal_tree)
text(optimal_tree)
