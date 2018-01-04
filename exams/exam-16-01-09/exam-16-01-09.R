# Anton Mo Eriksson 
# TDDE01
# 2017-12-31
# Exam 16-01-09

# 1.
setwd("/home/ame/git/tdde01/TDDE01/exams/exam-16-01-09")

require(readxl)
library(readxl)

crx = read.csv("crx.csv")

# Divade data into 3 sets.
n = dim(crx)[1] # n = number of rows
set.seed(12345) 
id = sample(1:n, floor(n * 0.8)) # Procent of a set.
train = crx[id, ]
test = crx[-id, ]

tree_fit = tree(Class ~., data = data.frame(train))
predict_train = predict(tree_fit, newdata = train, type = "class")

plot(tree_fit)
text(tree_fit)

test_new = test[-2,]
train_new = train[-2,]

new_tree_fit = tree(Class ~., data = train_new)
new_predict_train = predict(tree_fit, newdata = train_new, type = "class")

plot(new_tree_fit)
text(new_tree_fit)


cross_validated_fit_tree = cv.tree(tree_fit)
summary(cross_validated_fit_tree)
plot(cross_validated_fit_tree$size,cross_validated_fit_tree$dev, col = "blue", type = "b")
points(log(cross_validated_fit_tree$k),cross_validated_fit_tree$dev, col =" green", type = "b")

plot(y = cross_validated_fit_tree$dev, x = 1:7, type = "b", col = "green", main = "Find best (lowest) score")

prune_tree = prune.tree(tree_fit, best = 3)
plot(prune_tree)
text(prune_tree)
summary(prune_tree)
library(glmnet)
x_train = model.matrix(~ .-1, train[,-16])
model=c()
model=cv.glmnet(x_train,train[16], alpha=1,family="gaussian")
model$lambda.min
plot(model)
coef(model, s="lambda.min")
summary(model)




# 2.
library(mboost)
library(kernlab)
bf <- read.csv2("bodyfatregression.csv")
set.seed(1234567890)

m <- blackboost(Bodyfat_percent ~ Waist_cm+Weight_kg, data=bf)
mstop(m)
cvf <- cv(model.weights(m), type="kfold")
cvm <- cvrisk(m, folds=cvf, grid=1:100)
plot(cvm)
mstop(cvm)


# 3.

# 4.