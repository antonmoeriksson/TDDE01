# Lab III-ii
# Anton Mo Eriksson
# 2018-12-21

library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
traning <- trva[1:25,] # Training
validation <- trva[26:50,] # Validation

mse_traning = mse_validation = c()
# Random initialization of the weights in the interval [-1, 1]
winit <- runif(n = 31, min = -1, max = 1)
  for(i in 1:10) {
    threshold = (i / 1000)
    nn <- neuralnet(Sin~Var, data = traning, hidden =  10, threshold = threshold, startweights = winit)
    
    mse_validation[i] = sum((compute(nn, validation$Var)$net.result - validation$Sin) ^ 2) / nrow(validation)
    mse_traning[i] = sum((compute(nn, traning$Var)$net.result - traning$Sin) ^ 2) / nrow(traning)    
      # Your code here
  }
min_mse = which.min(mse_validation) / 1000
plot(mse_validation, type = "o", main = "Mean square error", xlim = c(0,11), ylim = c(0, 0.001),
     ylab = "MSE", col = "red")
points(mse_traning, col = "green", type = "o")
legend(x = "topleft", legend = c("traning", "validation"), col = c("green", "red"), lty = c(1,1))

best_threshold = which.min(mse_validation) / 1000
best_nn <- neuralnet(Sin ~ Var, data = trva , hidden = 10, threshold = best_threshold, startweights = winit)
plot(best_nn)
  # Plot of the predictions (green dots) and the data (blue dots)
  plot(prediction(best_nn)$rep1, main = "Predicsion of Best Neural Network", col = "green")
  points(trva, col = "blue")
  