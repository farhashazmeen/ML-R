library(readxl)

# folding indexes, returns start indexes for each fold
indexes <- function(n,k){
  s = floor(n/k)
  indexes = matrix(0,k,2)
  for(i in 1:k){
    indexes[i,1] = (i-1) * s
    indexes[i,2] = i * s -1
  }
  indexes[k,2] = n
  return(indexes)
}

binary_permutations <- function(n){
  indexes <- t(sapply(1:(n^2-2),function(x){as.integer(intToBits(x))})[1:n,])
}

best_subset <- function(X,Y,K){
  n = nrow(X)
  indexes = indexes(n,K)
  for( i in 1:K){
    test_indexes = indexes[i,1]:indexes[i,2]
    test_fold = X[test_indexes,]
    training_fold = X[-test_indexes,]
    dummy_features = binary_permutations(ncol(X))
    errors = matrix(0,K,1)
    prediction = 
  }
}

# k_folds <- function(X,Y,K) {
#     col_x = ncol(X)
#     errors = matrix(0,K,1)
#     dummy_folds = matrix(0,K,col_x)
# 
#     # For every fold
#     for(i in 1:K){
#       # Pick random number of columns as sample
#       n = sample(1:col_x,1)
#       fold_indexes = sample(1:col_x,n)
#       # Dumyfy the columns
#       dummy_folds[i,fold_indexes] = 1
#       # Get X-values for current fold
#       fold_x = as.matrix(X[,fold_indexes])
#       res = linear_regression(fold_x,Y)
#       errors[i,] = res$err
#       intercept = res$param[1,]
#       plot(rowMeans(X),res$pred)
#       slope = mean(res$param[2:nrow(res$param),])
#       # points(1:nrow(data),res$pred, col="Green")
#       abline(intercept,slope)
#     }
#   best = which.min(errors)
#   print(best)
#   print(dummy_folds)
#   return(which(dummy_folds[best,] == 1))
# 
 # Linear regression between two samples, one as x-values and one as y-values
 linear_regression <- function(X,Y){
   # Add column with ones in X
   hx = matrix(1,nrow(X), ncol(X)+1)
   hx[,2:ncol(hx)] = X
   X = hx 
   # Find parameter vector
   w = solve(t(X) %*% X) %*% t(X) %*% Y
   # Predicted values
   pY = X %*% w
   # Calculate error rate
   errors = sum((pY - Y)^2)
   return(list(param=w, pred=pY, err=errors))
 }

data = swiss
# ============
#fold_indexes(nrow(data),5)
best_subset(data,data,3)
# 
# set.seed(12345)
# s = sample(1:nrow(data),nrow(data))
# data = data[s,]
# Y = data$Fertility
# X = data[,2:(ncol(data))]
#X = as.matrix(c(1,2,3,4,5,6,7,8,9))
#Y = as.matrix(c(2,4,6,8,10,12,14,16,18))
#folds = k_folds(X,Y,5)
#print(folds)
#print(t(folded_data))
#
# Test data
# X = matrix(c(1,2,3,4,5,6,7,8,9),9,1)
# Y = matrix(c(1,2,3,4,5,6,7,8,9),9,1)
# yh = linear_regression(X,Y)
# plot(X,Y,col="red")
# lines(X,yh,col="green")