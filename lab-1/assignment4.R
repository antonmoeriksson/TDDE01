# Anton MO Eriksson
# anter491
# 2017-11-16
require(readxl)
library(readxl)

setwd("/home/ame/git/tdde01/TDDE01/lab-1")
set.seed(12345)

# 1-i 
# Import data.
input_data <- data.matrix(read_excel("tecator.xlsx"))

protein = data.frame(input_data)$Protein
fat = data.frame(input_data)$Fat
moisture = data.frame(input_data)$Moisture

plot(protein, moisture,
     xlab = "Protein",
     ylab = "Moisture")


# 1-iii
# Train the model.
# Split the data in validation and traning.

n = dim(input_data)[1]
set.seed(12345)
id = sample(1:n, floor(n * 0.5))
traning_data = input_data[id, ]
validation_data = input_data[-id, ]

mean_square_error_traning = numeric(length(1:6))
mean_square_error_validation = numeric(length(1:6))

for (index in 1:6) {
  M = lm(Moisture ~ poly(Protein, index), data = data.frame(traning_data))
  summary_M = summary(M)
  
  mean_square_error_traning[index] = mean(summary_M$residuals^2)

  prediction = predict(M, data.frame(validation_data))
  mean_square_error_validation[index] = mean((prediction - data.frame(validation_data)$Moisture)^2)
}

plot(1:6, mean_square_error_traning,
     ylab = "MSE", xlab = "Index",
     ylim = c(30, 36),
     type = "l",
     col = "green")
lines(1:6, mean_square_error_validation,
      col = "red")
legend(x = "topright",
       legend = c("MSE-traning", "MSE-validation"),
       lty = c(1,1),
       col = c("green", "red"))

# 1-iv
library(MASS)
M = lm(Fat ~ . -(Sample + Protein + Fat + Moisture), data = data.frame(input_data))
step_ACI = stepAIC(M)
summary_step_ACI = summary(step_ACI)

# 1-v
library(glmnet)
covariates = scale(data.frame(input_data)[,2:101])
response = scale(data.frame(input_data)[, "Fat"])

lambdas = exp(seq(-4.5, 6.5, 0.1))

ridge_regression = glmnet(as.matrix(covariates), response, alpha=0, family="gaussian", lambda = lambdas)
plot(ridge_regression, xvar="lambda", label=TRUE, ylim = c(-0.2, 0.5))

# 1-vi
lambdas = exp(seq(-10, 0, 0.1))
lambdas = c(0, lambdas)
LASSO = glmnet(as.matrix(covariates), response, alpha=1, family="gaussian", lambda = lambdas)
plot(LASSO, xvar="lambda", label=TRUE)


# 1-vii
M = cv.glmnet(as.matrix(covariates), response, alpha=1,family="gaussian")
min = M$lambda.min
plot(M)
coef(M, s="lambda.min")
print(min)
# 1-viii













