library(readxl)
library(kknn)

# Import data
spambase <- read_excel("spambase.xlsx")

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

knearest <- function(train, K, test, thresh=0.5) {
  
  nospam_train = train[,-ncol(train)]
  nospam_test = test[, -ncol(test)]
  
  X_h = train/ sqrt(rowSums(nospam_train^2))
  Y_h = test/ sqrt(rowSums(nospam_test^2))
  C = X_h %*% t(Y_h)
  distance = 1 - C
  k_distance = t(apply(distance,1,order)[,1:K])
  spam = apply(k_distance,1,spam,lookup_table=train)
  spam_means = rowMeans(spam)
  result = as.numeric(spam_means > thresh)
  return (result )
  
}

sensitivity <- function(observations, predictions){
  result = sum((observations == 1 & predictions == observations))/ sum(predictions == 1) 
  return(result);
}

specificity <- function(observations, predictions){
  result = sum((observations == 0 & predictions == observations))/ sum(predictions == 0) 
  return(result);
}

observations = test$Spam
knearest_k5 = knearest(as.matrix(train),5,as.matrix(test), 0.5)
knearest_k1 = knearest(as.matrix(train),1,as.matrix(test), 0.5)

print(table(observations,knearest_k5))
print(table(observations,knearest_k1))

print(mean(knearest_k5 != observations))
print(mean(knearest_k1 != observations))

# Check with build-in functions
kknn_predictions_k5 = kknn(formula = Spam ~ ., train = train, test = test ,k = 5)
kknn_predictions_k1 = kknn(formula = Spam ~ ., train = train, test = test, k = 1)

kknn_predictions_k5 = as.numeric(fitted.values(kknn_predictions_k5) > 0.5)
kknn_predictions_k1 = as.numeric(fitted.values(kknn_predictions_k1) > 0.5)

print(table(observations,kknn_predictions_k5))
print(table(observations,kknn_predictions_k1))

print(mean(kknn_predictions_k5 != observations))
print(mean(kknn_predictions_k1 != observations))

# Check with other prediction thresholds

plot(-1,-1, xlim = c(0,1), ylim = c(0,1), xlab = "FPR", ylab = "TPR")
for(p in seq(from = 0.05, to = 0.95, by = 0.05)) {
  kneares_predictions = knearest(as.matrix(train),5,as.matrix(test), p)
  
  kknn_predictions = kknn(formula = Spam ~ ., train = train, test = test ,k = 5)
  kknn_predictions = as.numeric(fitted.values(kknn_predictions) > p)
  
  knearest_sensitivity = sensitivity(observations,kneares_predictions)
  knearest_specificity = specificity(observations,kneares_predictions)
  
  kknn_sensitivity = sensitivity(observations,kknn_predictions)
  kknn_specificity = specificity(observations,kknn_predictions)
  
  points(1-knearest_specificity, knearest_sensitivity, col = "Green")
  points(1-kknn_specificity, kknn_sensitivity, col = "Red")
  
}
