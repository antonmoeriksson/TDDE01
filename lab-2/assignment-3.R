# Lab 2 assignment III
# Anton Mo Eriksson
# anter491
# 2017-12-11


# Import data. III-i
setwd("/home/ame/git/tdde01/TDDE01/lab-2")

library(tree)

plot_tree <- function(tree) {
  plot(tree)
  text(tree)
  summary(tree)
}

state = read.csv2("State.csv")
state_ordered = state[order(state$MET),]
n = dim(state_ordered)[1]
tree_control = tree.control(n, minsize = 8)

plot(state_ordered$MET, state_ordered$EX, xlab = "MET", ylab = "EX", main = "MET vs EX " )

#  III-ii
fit_tree = tree(EX ~ MET, data = state_ordered, control = tree_control)

cross_validated_fit_tree = cv.tree(fit_tree)

plot(cross_validated_fit_tree$size, cross_validated_fit_tree$dev, type = "b", col = "darkblue")

best = cross_validated_fit_tree$size[which.min(cross_validated_fit_tree$dev)]

prune_fit_tree = prune.tree(fit_tree, best = best)
plot_tree(prune_fit_tree)

predict_prune_fit_tree = predict(prune_fit_tree, newdata = state_ordered)

plot(state_ordered$MET, state_ordered$EX, xlab = "MET", ylab = "EX", col = "Green")
points(state_ordered$MET, predict_prune_fit_tree, col = "darkblue")
legend(x = "top", col = c("Green", "Darkblue"), lty = c(1,1), legend = c("Original Values", "Predictions")) 

original_minus_predicted = state_ordered$EX - predict_prune_fit_tree
hist(original_minus_predicted, col = "darkblue", xlim = c(-80, 125))

# III-iii
require(boot)
library(boot)

non_parametric_bootstraping <- function(data, ind) {
  data_1 = data[ind,]
  fitting = tree(EX ~ MET, data = data_1, control = tree_control)
  best_fitting = prune.tree(fitting, best = best)
  
  prediction = predict(best_fitting, newdata = data)
  return (prediction)
}

bootstraping_non_parametric = boot(data = state_ordered, statistic = non_parametric_bootstraping, R = 1000)
envelop_NPB = envelope(bootstraping_non_parametric, level = 0.95)

plot_confidance_band <- function(envelop, fit, name) {
  plot(state_ordered$MET, state_ordered$EX, main = name, xlab = "MET", ylab = "EX", col = "darkblue",
       ylim = c(200,400))
  points(state_ordered$MET, fit, type = "l")
  points(state_ordered$MET, envelop$point[1, ], col = "red",  type = "l")
  points(state_ordered$MET, envelop$point[2, ], col = "red", type = "l")
}

plot_confidance_band(envelop_NPB, predict_prune_fit_tree, "test")

# III-iv

rng <- function(data, mle) {
  data_2 = data.frame(MET =data$MET, EX = data$EX)
  n = nrow(data)
  data_2$EX = rnorm(n, predict(mle, newdata = data_2), sd(residual(mle)))
  return(data_2)
}

parametric_bootstraping <- function(data) {
  fitting = tree(EX ~ MET, data = data, control = tree_control)
  best_fitting = prune.tree(fitting, best = best)
  
  prediction = predict(best_fitting, newdata = state_ordered)
  return (prediction)
}

residual <- function(mle) {
  fit_func = predict(mle, state_ordered)
  return (state_ordered$EX - fit_func)
}

bootstraping_parametric = boot(data = state_ordered, statistic = parametric_bootstraping, 
                               R = 1000, ran.gen = rng, mle = prune.tree(fit_tree, best = best), sim = "parametric")
envelop_PB = envelope(bootstraping_parametric)
plot_confidance_band(envelop_PB, predict_prune_fit_tree, "test2")



