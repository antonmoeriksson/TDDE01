# anter491
# Komp for lab 3 late hand in.
# Neural Networks.

library(MASS)
library(neuralnet)
library(kernlab)

set.seed(1234567890)
data(spam)
email_data = spam

# Writes out the type of all featuers, numeric for all exepct type, which is spam or !spam (factor).
str(email_data)
hist(as.numeric(as.logical(email_data$type)), col = "blue", xlab = "Spam or not spam")

# 1 = not spam
# 2 = spam
email_data$type = as.numeric(email_data$type)

email_type = ifelse(email_data$type == 2 ,yes = TRUE, no = FALSE)
email_data$type = email_type

#head(email_data, 3)
apply(email_data, 2, range)

min_value = as.numeric(apply(email_data, 2, min))
max_value = as.numeric(apply(email_data, 2, max))


new_email_data = as.data.frame(scale(email_data, center = min_value, scale = max_value - min_value))

# Divade data into test and training sets.

n = dim(new_email_data)[1]
id = sample(1:n, floor(n * 0.5))
train = new_email_data[id, ]
validation = new_email_data[-id, ]

mse_validation = c()
mse_traning = c()
layer = c(3,10,3)

varibales = colnames(new_email_data[,-58])
pred_vars = varibales[!varibales%in%"type"]
res = paste(pred_vars, collapse = "+")
eq = as.formula(paste("type ~", res, collapse = "+"))
# Random initialization of the weights in the interval [-1, 1]
start_weight = runif(n = 1000, min = -1, max = 1)


for (i in 1:10) {
  threshold = (i / 100)
  
  nn <- neuralnet(formula = eq, data = train, 
                  hidden =  5, threshold = threshold, startweights = start_weight )
  mse_validation[i] = sum((compute(nn, validation[,-58])$net.result - validation[,58]) ^ 2) / nrow(validation)
  mse_traning[i] = sum((compute(nn, train[,-58])$net.result - train[,58]) ^ 2) / nrow(train) 
}

min_mse = which.min(mse_validation) / 100
plot(mse_validation, type = "o", main = "Mean square error", xlim = c(0,11), ylim = c(0, 0.2),
     ylab = "MSE", col = "red")
points(mse_traning, col = "green", type = "o")
legend(x = "topleft", legend = c("traning", "validation"), col = c("green", "red"), lty = c(1,1))

best_threshold = which.min(mse_validation) / 100
best_nn <- neuralnet(formula = eq, data = new_email_data , hidden = 10, threshold = best_threshold)
plot(best_nn)


# Plot of the predictions (green dots) and the data (blue dots)
plot(prediction(best_nn)$rep1[,58],prediction(best_nn)$rep1[,-58], main = "Predicsion of Best Neural Network", col = "green")
points(new_email_data$type, col = "blue")

pred = compute(best_nn, validation[,-58])
pred$net.result
plot(abs(pred$net.result), col = "blue", ylab = "Spam or not Spam")
points(validation$type, col = "red")
MSE = sum((pred$net.result - validation$type)^2) / nrow(validation)

mcrm = misclassification_rate_matrix(validation, pred_truncked)

# Matix
misclassification_rate_matrix <- function(data, prediction) {
  t = table(data$type, prediction)
  print(t)
  mcr = (1 - (sum(diag(t) / sum(t))))
  return (mcr)
}
# For trunkeded result
pred_truncked = round(pred$net.result)




