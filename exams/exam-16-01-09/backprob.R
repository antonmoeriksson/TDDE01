# JMP

set.seed(1234567890)
tanh(Var)
sin(Var)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation

 plot(trva)
 plot(tr)
 plot(va)

w_j <- runif(10, -1, 1)
b_j <- runif(10, -1, 1)
w_k <- runif(10, -1, 1)
b_k <- runif(1, -1, 1)

l_rate <- 1/nrow(tr)^2
n_ite = 5000
error <- rep(0, n_ite)
error_va <- rep(0, n_ite)

# SGD

for(i in 1:n_ite) {
  
  for(n in 1:nrow(tr)) {
    
    z_j <- tanh(w_j * tr[n,]$Var + b_j)
    y_k <- sum(w_k * z_j) + b_k
    
    error[i] <- error[i] + (y_k - tr[n,]$Sin)^2
    
  }
  
  for(n in 1:nrow(va)) {
    
    z_j <- tanh(w_j * va[n,]$Var + b_j)
    y_k <- sum(w_k * z_j) + b_k
    
    error_va[i] <- error_va[i] + (y_k - va[n,]$Sin)^2
    
  }
  
  cat("i: ", i, ", error: ", error[i]/2, ", error_va: ", error_va[i]/2, "\n")
  flush.console()
  
  for(n in 1:nrow(tr)) {
    
    # forward propagation
    
    z_j <- tanh(w_j * tr[n,]$Var + b_j)
    y_k <- sum(w_k * z_j) + b_k
    
    # backward propagation
    
    d_k <- y_k - tr[n,]$Sin
    d_j <- (1 - z_j^2) * w_k * d_k
    partial_w_k <- d_k * z_j
    partial_b_k <- d_k
    partial_w_j <- d_j * tr[n,]$Var
    partial_b_j <- d_j
    w_k <- w_k - l_rate * partial_w_k
    b_k <- b_k - l_rate * partial_b_k
    w_j <- w_j - l_rate * partial_w_j
    b_j <- b_j - l_rate * partial_b_j
    
  }
  
}

w_j
b_j
w_k
b_k

plot(error/2, ylim=c(0, 5))
points(error_va/2, col = "red")

# prediction on training data

pred <- matrix(nrow=nrow(tr), ncol=2)

for(n in 1:nrow(tr)) {
  
  z_j <- tanh(w_j * tr[n,]$Var + b_j)
  y_k <- sum(w_k * z_j) + b_k
  pred[n,] <- c(tr[n,]$Var, y_k)
  
}
pred_tr <- pred
plot(pred)
points(tr, col = "red")-

# prediction on validation data

pred <- matrix(nrow=nrow(tr), ncol=2)

for(n in 1:nrow(va)) {
  
  z_j <- tanh(w_j * va[n,]$Var + b_j)
  y_k <- sum(w_k * z_j) + b_k
  pred[n,] <- c(va[n,]$Var, y_k)
  
}
plot(1:n_ite, error, col = "red", type = "b", xlim = c(1,20))
points(error_va, col = "Blue", type = "b")
plot(pred, col = "brown")
points(pred_tr, col = "Blue")

points(tr, col = "pink")
points(va, col = "red")

### SVM

library(kernlab)
set.seed(1234567890)
spam_data <- data(spam)
response_a = c()
response_b = c()
response_c = c()

res_a = ksvm(x = spam, kpar = sigma(0.05), C = 1, cross = 2, kernel = rbfdot, type = "C-svc")

res_b = ksvm(x = spam, kpar = 0.05, C = 10, cross = 2, kernel = rbfdot, type = "C-svc")

res_c = ksvm(x = spam, C = 100, cross = 2, kpar = 0.05,  kernel = rbfdot, type = "C-svc")

test = c(cross(res_a), cross(res_b), cross(res_c))
plot(test)










