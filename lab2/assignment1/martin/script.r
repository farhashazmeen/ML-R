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

# Deprecated because of reasons
binary_permutations <- function(n){
  indexes = matrix(0,2^n-1,n)
  for(i in 1:2^n-1){
    indexes[i,] = as.numeric(intToBits(i))[1:n]
  }
  return(indexes)
}

k_fold <- function(X,Y,K){
  n = nrow(X)
  fold_errors = matrix(0,K,1)
  fold_weights = matrix(0,K,(ncol(X)+1))
  indexes = indexes(n,K)
  for( i in 1:K){
    test_fold_indexes = indexes[i,1]:indexes[i,2]
    train_fold_indexes = (1:n)[-test_fold_indexes]
    
    test_y = Y[test_fold_indexes]
    test_x = X[test_fold_indexes,]
    train_x = X[train_fold_indexes,]
    train_y = Y[train_fold_indexes]
    result = linear_regression(as.matrix(train_x), train_y,as.matrix(test_x),test_y)
    fold_errors[i,] = result$err
    fold_weights[i,] = result$param
  }

  return (list(weights=colMeans(fold_weights), err=fold_errors))
}
best_subset <- function(X,Y,K){
  n = nrow(X)
  columns = ncol(X)
  print(columns)
  
  errors = matrix(0,columns,2^columns)
  mean_error_rates = matrix(0,columns,1)
  best_error = Inf
  best_indexes = c()
  best_weights = c()
  for(n_features in 1:columns){
    binary_permutations = binary_permutations(ncol(X))
    binary_permutations = t(combn(1:columns,n_features))
    n_combinations = nrow(binary_permutations)

    combination_errors = matrix(Inf,n_combinations,1)

    for(combination in 1:n_combinations){
      current_features = binary_permutations[combination,]
      filtered_x = as.matrix(X[current_features])
      k_fold = k_fold(filtered_x,Y,K)
      combination_errors[combination,] = mean(k_fold$err)
      best_combination_index = which.min(combination_errors)
  
      if(combination_errors[best_combination_index,] < best_error){
        best_weights = k_fold$weights
        best_indexes = binary_permutations[combination,]
        best_error = combination_errors[best_combination_index,]
      }
      
    }
    mean_error_rates[n_features,] = mean(combination_errors)
    
  }
  plot(1:columns,mean_error_rates, type = "l", ylim = c(500,1300))
  points(length(best_indexes),best_error, col="Red")
  print(best_error)
  return(list(indexes=best_indexes, weights=best_weights, err=best_error))
}

 # Linear regression between two samples, one as x-values and one as y-values
 linear_regression <- function(x_train,y_train,x_test,y_test){
    x_train = cbind(matrix(1,nrow(x_train),1),x_train)
    x_test = cbind(matrix(1,nrow(x_test),1),x_test)
    w = solve(t(x_train) %*% x_train) %*% t(x_train) %*% y_train
    w = as.vector(w)
    pY = x_test %*% w
    errors = sum((y_test - pY)^2)
   return(list(param=w, pred=pY, err=errors))
 }

 
data = swiss
set.seed(12345)
s = sample(1:nrow(data),nrow(data))
data = data[s,]
# ============
#fold_indexes(nrow(data),5)

X = data[,2:ncol(data)]
Y = as.matrix(data$Fertility)
best_features = best_subset(X,Y,5)
print(best_features)
X = X[,best_features$indexes]
print(X)
model = linear_regression(x_train = as.matrix(X),y_train = Y,x_test = as.matrix(X),y_test = Y)
for(i in 1:ncol(X)){
  plot(X[,i],Y, xlab = colnames(X)[i])
  # From best_features
  abline(best_features$weights[1],best_features$weights[i+1],col="Red")
  # From new regression
  abline(model$param[1],model$param[i+1],col="Green")
  
}
