# Get number of observations
n=dim(spambase)[1]

# Set psuedo random seed
set.seed(12345)

# Get half of the indexes
id=sample(1:n, floor(n*0.5))

# Assign 50% of the observations as traning data
train=spambase[id,]

# Assign 50% of the observations as test data
test=spambase[-id,]


cost <- function(X,Y) {
  
  d = (t(X) %*% Y) / (sqrt(sum(X^2)) * sqrt(sum(Y^2)))
  return (1 - d)
}

spam <- function(indexes,lookup_table) {
  return (lookup_table[indexes,ncol(lookup_table)])
}

knearest <- function(data, K, newdata) {
  
  spam_free_x = data[,-ncol(data)]
  spam_free_y = newdata[, -ncol(newdata)]
  
  X_h = data/ sqrt(rowSums(spam_free_x^2))
  Y_h = newdata/ sqrt(rowSums(spam_free_y^2))
  C = X_h %*% t(Y_h)
  D = 1 - C
  D_o = t(apply(D,1,order)[,1:K])
  spam = apply(D_o,1,spam,lookup_table=data)
  spam_means = rowMeans(spam)
  result = round(spam_means)
  
  return (result )
  
}

