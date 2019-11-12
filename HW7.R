## Step 1
library(CVXR)
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

# Step 4
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
fittedNegLogLik = lapply(I, neg_log_lik)
overallNegLogLik = lapply(fittedNegLogLik, sum) ## This is wrong. 
                    ## How to sum each element of the matricies?



# other garbage I might need
lambda_search = 10^(seq(-2, 2, length.out = 40))
get_theta_hat_lasso = function(lambda) {
  objective = Minimize(-log_det(theta_lasso) + matrix_trace(S %*% theta_lasso) +
                         lambda * sum(theta_lasso))
  problem = Problem(objective)
  result = solve(problem)
  result$getValue(theta_lasso)
}
theta_hats = plyr::aaply(lambda_search, 1, get_theta_hat_lasso)
colnames(theta_hats) = colnames(data$X)
theta_hats = cbind(lambda = lambda_search, theta_hats)
round(theta_hats, digits = 2)






objective = Minimize(-log_det(theta_lasso) + matrix_trace(S %*% theta_lasso))
constraints = list()
                     
