library(readxl)

set.seed(12345)
#data = read_excel("data/samples.xlsx")

# Randomze the data rows
permute_data <- function(data) {
  s = sample(1:nrow(data),nrow(data))
  permuted_data = data[s,]
 return(permuted_data)
}

# Create a random  sub sample from data with n columns
fold <- function(data,n){
  s = sample(1:ncol(data),n)
  return(s)
}

k_folds <- function(X,Y,K) {
    col_x = ncol(X)
    intercepts = matrix(0,K,1)
    dummy_folds = matrix(0,K,col_x)
    for(i in 1:K){
      n = sample(1:col_x,1)
      fold_indexes = fold(X,n)
  
      dummy_folds[i,fold_indexes] = 1
      xh = as.matrix(X[,fold_indexes])
      print(xh)
     # print(dummy_folds)
      py = linear_regression(xh,Y)
      intercepts[i,] = mean((py - Y)^2)
    }
    best = which.min(intercepts)
  return(which(dummy_folds[best,] == 1))
}

# Linear regression between two samples, one as x-values and one as y-values
linear_regression <- function(X,Y){
  
  # Add column with ones in X
  hx = matrix(1,nrow(X), ncol(X)+1)
  hx[,2:ncol(hx)] = X
  X2 = hx
  # Find parameter vector
  w = solve(t(X2) %*% X2) %*% t(X2) %*% Y
  # Predicted values
  pY = X2 %*% w
  print(w)
  abline(w[1,],mean(w[2:nrow(w),]))
  return(pY)
}

data = permute(swiss)
Y = data$Fertility
X = data[,2:(ncol(data))]
#X = as.matrix(c(1,2,3,4,5,6,7,8,9))
#Y = as.matrix(c(2,4,6,8,10,12,14,16,18))
plot(1:nrow(X),Y)
folds = k_folds(X,Y,5)

#print(t(folded_data))
#
# Test data
# X = matrix(c(1,2,3,4,5,6,7,8,9),9,1)
# Y = matrix(c(1,2,3,4,5,6,7,8,9),9,1)
# yh = linear_regression(X,Y)
# plot(X,Y,col="red")
# lines(X,yh,col="green")