library(readxl)

# folding indexes, returns start indexes for each fold
indexes <- function(n,k){
  s = floor(n/k)
  indexes = matrix(1,k,2)
  for(i in 1:k){
    indexes[i,1] = (i-1) * s
    indexes[i,2] = i * s -1
  }
  indexes = indexes +1
  indexes[k,2] = n
  return(indexes)
}

binary_permutations <- function(n){
  indexes = matrix(0,2^n-1,n)
  for(i in 1:2^n-1){
    indexes[i,] = as.numeric(intToBits(i))[1:n]
  }
  return(indexes)
}

best_subset <- function(X,Y,K){
  n = nrow(X)
  
  dummy_features = binary_permutations(ncol(X))
  feature_combinations = nrow(dummy_features)
  indexes = indexes(n,K)
  
  errors = matrix(0,feature_combinations,1)
  predictions = matrix(0,feature_combinations,n)
  for(combination in 1:feature_combinations){
    current_features = which(dummy_features[combination,] == 1)
    filtered_x = as.matrix(X[current_features])

    mean_fold_errors = matrix(0,K,1)
    for( i in 1:K){
      # 
      test_fold_indexes = indexes[i,1]:indexes[i,2]
      train_fold_indexes = (1:n)[-test_fold_indexes]
    
      test_y = Y[test_fold_indexes]
      test_x = filtered_x[test_fold_indexes,]
      train_x = filtered_x[train_fold_indexes,]
      train_y = Y[train_fold_indexes]
      result = linear_regression(as.matrix(train_x), train_y,as.matrix(test_x),test_y)
      mean_fold_errors[i,] = result$err
     # predictions[combination,] = result$pred
    }
    errors[combination,] = mean(mean_fold_errors)
 
  }
  print(errors)
  best_features = which(dummy_features[which.min(errors),] == 1)
  return(best_features)
}

 # Linear regression between two samples, one as x-values and one as y-values
 linear_regression <- function(x_train,y_train,x_test,y_test){
   # Add column with ones in X
  x_train = cbind(matrix(1,nrow(x_train),1),x_train)
  x_test = cbind(matrix(1,nrow(x_test),1),x_test)
   # Find parameter vector
   w = solve(t(x_train) %*% x_train) %*% t(x_train) %*% y_train
   w = as.vector(w)
   # Predicted values
   pY = x_test %*% w
   # Calculate error rate
   errors = sum((y_test - pY)^2)
   return(list(param=w, pred=pY, err=errors))
 }

data = swiss
# ============
#fold_indexes(nrow(data),5)
X = data[,2:ncol(data)]
Y = as.matrix(data$Fertility)
print(best_subset(X,Y,5))
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