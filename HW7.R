## Step 1
library(CVXR)
library(plyr)
data = read.csv("hw7.csv", header = TRUE)
data = data[, -1]
## Step 2
S = cov(data)
p = dim(data)[2]
theta_lasso = Variable(p,p)
lambda = 1
objective = Minimize(-log_det(theta_lasso) + matrix_trace(S %*% theta_lasso) +
                       lambda * sum(abs(theta_lasso)))
problem = Problem(objective)
result = solve(problem)
result$getValue(theta_lasso)
fitted_theta = result$getValue(theta_lasso)
fitted_theta = round(fitted_theta, digits = 2)

## Step 3
## When I try to do this with a subset of data, it doesn't work.  
subset = data[1:3, 1:3]
S = cov(data)
p = dim(data)[2]
theta_lasso = Variable(p,p)
lambda_search = 10^(seq(-2, 2, length.out = 40))
get_theta_hat_lasso = function(lambda) {
  objective = Minimize(-log_det(theta_lasso) + matrix_trace(S %*% theta_lasso) +
                         lambda * sum(abs(theta_lasso)))
  problem = Problem(objective)
  result = solve(problem)
  result$getValue(theta_lasso)
}
theta_hats = plyr::aaply(lambda_search, 1, get_theta_hat_lasso)
theta_hats = round(theta_hats, digits = 2)
colnames(theta_hats) = colnames(data)
theta_hats = cbind(lambda = lambda_search, theta_hats)      ## something goes wrong here. In her example in 
## lecture 21, beta_lasso was a vector, but ours is a matrix.  I'm not sure how things need to change to
## accomodate that.
theta_melted = reshape2::melt(data.frame(theta_hats), id.vars = "lambda", value.name = "coefficient")
ggplot(theta_melted) +
  geom_line(aes(x = lambda, y = coefficient, color = variable, lty = variable)) +
  scale_x_log10()

## Step 4
I = split(data, sample(rep(1:10, 5)))
cross_validation = function(subset){
  S = cov(subset)
  p = dim(subset)[2]
  theta_lasso = Variable(p,p)
  lambda = 1
  objective = Minimize(-log_det(theta_lasso) + matrix_trace(S %*% theta_lasso) +
                         lambda * sum(abs(theta_lasso)))
  problem = Problem(objective)
  result = solve(problem)
  result$getValue(theta_lasso)
  fitted_theta = result$getValue(theta_lasso)
  return(fitted_theta)
}
fittedThetasCross = lapply(I, cross_validation)

neg_log_lik = function(subset){
  S = cov(subset)
  p = dim(subset)[2]
  theta_lasso = Variable(p,p)
  lambda = 1
  objective = Minimize(-log_det(theta_lasso) + matrix_trace(S %*% theta_lasso))
  problem = Problem(objective)
  result = solve(problem)
  result$getValue(theta_lasso)
  fitted_theta = result$getValue(theta_lasso)
  return(fitted_theta)
}
fittedNegLogLik = laply(I, neg_log_lik)
overallNegLogLik = aaply(fittedNegLogLik, c(1,2), sum) 

plot(overallNegLogLik, fitted_theta)




                     
