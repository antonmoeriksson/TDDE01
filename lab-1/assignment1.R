# Anton MO Eriksson
# anter491
# 2017-11-16

require(readxl)
library(readxl)

require(kknn)
library(kknn)

setwd("/home/ame/git/tdde01/TDDE01/lab-1")
set.seed(12345)

# 1.1 Import data.
input_data <- data.matrix(read_excel("spambase.xlsx"))

n = dim(input_data)[1]
set.seed(12345)
id = sample(1:n, floor(n * 0.5))
train = input_data[id, ]
test = input_data[-id, ]

n1=dim(input_data)[1]

p=dim(input_data)[2]
Prob=numeric(n2)

# I-iii
training(train, train, 5)
training(train, test, 5)
# I-iv
training(data.frame(train), train, 1)
training(train, data.frame(test), 1)

# I-iv
kknn_k5 = kknn(
  formula = Spam ~ .,
  train = data.frame(train),
  test = data.frame(test),
  k = 5
)
kknn_k5_fitted = kknn_k5$fitted.values > 0.5
kknn_k5_fitted_table = table(kknn_k5_fitted, test[,p])

print(kknn_k5_fitted_table)

accuracy =sum(diag(kknn_k5_fitted_table)) / sum(kknn_k5_fitted_table)
print(1 - accuracy)

# I-v
ROC_knerest_k5 = ROC(data.frame(test)$Spam, knearest(train, k, test), seq(0.5, 0.95, 0.05))

ROC_kknn_k5 = ROC(data.frame(test)$Spam, kknn_k5$fitted.values, seq(0.5, 0.95, 0.05))

plot(ROC_knerest_k5$FPR, ROC_knerest_k5$TPR,
     xlab = "FPR", ylab = "TRP",
     type = "l",
     col = "red",
     main ="Receiver operating characteristic"
     )
lines(ROC_kknn_k5$FPR, ROC_kknn_k5$TPR, col = "green")
legend(
      "bottomright",
      lty = c(1, 1),
      col = c("green", "red"),
      legend = c("KNEREST", "KKNN")
)

knearest=function(data, k, newdata) {
  n1=dim(data)[1]
  n2=dim(newdata)[1]
  p=dim(data)[2]
  Prob=numeric(n2)
  
  X = as.matrix(data[,-p])
  X = X / matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1)
  
  Y = as.matrix(newdata[, -p])
  Y = Y / matrix(sqrt(rowSums(Y^2)), nrow=n2, ncol=p-1)
  
  Y_tran = t(Y)
  C = X %*% Y_tran
  
  distance = 1 - C
  
  for (index in 1:n2 ) {
    distance_ordered = order(distance[, index], decreasing = FALSE)
    chosen_order = distance_ordered[1:k]
    
    Prob[index] = sum(data[chosen_order, p]) / k

  }
  return(Prob)
}

ROC <- function(Y, Yfit, p) {
  m=length(p)
  TPR=numeric(m)
  FPR=numeric(m)
  for(i in 1:m) {
    t=table(Yfit>p[i], Y)
    TPR[i] = t[2, 2] / sum(t[, 2])
    FPR[i] = t[2, 1] / sum(t[, 1])
  }
  return (list(TPR=TPR,FPR=FPR))
}

training <- function(training_data, test_data, threshold) {
 train_probebility = knearest(training_data, threshold, test_data)
 Y_train = train_probebility > 0.5
 table_trained = table(Y_train, test_data[,p])
 print(table_trained) 
 
 accuracy = sum(diag(table_trained)) / sum(table_trained)
 print(1 - accuracy)
}


