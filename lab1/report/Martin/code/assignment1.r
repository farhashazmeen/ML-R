library(readxl)
library(kknn)

# Import data
spambase <- read_excel("spambase.xlsx")

# Get number of observations
n = dim(spambase)[1]

# Set psuedo random seed
set.seed(12345)

# Get half of the indexes
id = sample(1:n, floor(n * 0.5))

# Assign 50% of the observations as traning data
train = spambase[id, ]

# Assign 50% of the observations as test data
test = spambase[-id, ]

# Function to get the spam classification of a given index
spam_lookup <- function(indexes, lookup_table) {
  return (mean(lookup_table[indexes, ncol(lookup_table)]))
}

# Function of knearest algortihm, returns the predictited probabilites for each observation
knearest <- function(train, K, test) {
  nospam_train = train[, -ncol(train)]
  nospam_test = test[,-ncol(test)]
  euclidean_train = sqrt(rowSums(nospam_train ^ 2))
  euclidean_test = sqrt(rowSums(nospam_test ^ 2))
  
  train_hat = nospam_train / euclidean_train
  test_hat = nospam_test / euclidean_test
  cost = train_hat %*% t(test_hat)
  distance = 1 - cost
  
  k_nearest_observations = as.matrix(t(apply(distance, 2, order))[, 1:K])
  
  spam_predictions = apply(k_nearest_observations, 1, spam_lookup, lookup_table =
                             train)
  return (spam_predictions)
  
}

# Returns if a specific arrays elements are over a specific threshold
threshold <- function(threshold, data) {
  return (as.numeric(data > threshold))
}

#Function to calcuate the sensitivity of a data set
sensitivity <- function(observations, predictions) {
  
  result = sum((observations == 1 & predictions == 1)) / (sum((observations == 1 &predictions == 1)) + sum(observations == 1 & predictions == 0))
  return(result)
  
}

#Function to calcuate the specificity of a data set
specificity <- function(observations, predictions) {
  result = sum((observations == 0 & 
    predictions == 0)) / (sum((observations == 0 &
    predictions == 0)) + sum(observations == 0 & predictions == 1))
  return(result)
}

observations = test$Spam
knearest_k5 = as.numeric(knearest(as.matrix(train), 5, as.matrix(test)) > 0.5)
knearest_k1 = as.numeric(knearest(as.matrix(train), 1, as.matrix(test)) > 0.5)

mst_k5 = table(observations, knearest_k5)
mst_k1 = table(observations, knearest_k1)
print(mst_k5)
print(mst_k1)

print(1 - sum(diag(mst_k5) / sum(mst_k5)))
print(1 - sum(diag(mst_k1) / sum(mst_k1)))

# Check with build-in functions
kknn_predictions_k5 = kknn(
  formula = Spam ~ .,
  train = train,
  test = test ,
  k = 5
)
kknn_predictions_k1 = kknn(
  formula = Spam ~ .,
  train = train,
  test = test,
  k = 1
)

kknn_predictions_k5 = as.numeric(fitted.values(kknn_predictions_k5) > 0.5)
kknn_predictions_k1 = as.numeric(fitted.values(kknn_predictions_k1) > 0.5)

print(table(observations, kknn_predictions_k5))
print(table(observations, kknn_predictions_k1))

print(mean(kknn_predictions_k5 != observations))
print(mean(kknn_predictions_k1 != observations))

# Check with other prediction thresholds

# Converts to matrix for apply operations
thresholds = seq(from = 0.05, to = 0.95, by = 0.05)
thresholds = matrix(thresholds, length(thresholds), 1)

# Get the knearest predictions of a data set
knearest_predictions = knearest(as.matrix(train), 5, as.matrix(test))

# Apply the threshold function to get the predictions
knearest_outcomes = t(apply(thresholds, 1, threshold, data = knearest_predictions))

# Calculate the specificity and the sensitivty of said predictions
knearest_sensitivity = apply(knearest_outcomes, 1, sensitivity, observations = observations)
knearest_specificity = apply(knearest_outcomes, 1, specificity, observations = observations)


# Calcualte the kknn predictions
kknn_predictions = kknn(
  formula = Spam ~ .,
  train = train,
  test = test ,
  k = 5
)
kknn_outcomes = t(apply(thresholds, 1, threshold, data = fitted.values(kknn_predictions)))

# Calculate the sensitivty and specificty of the kknn predictions
kknn_sensitivity = apply(kknn_outcomes, 1, sensitivity, observations = observations)
kknn_specificity = apply(kknn_outcomes, 1, specificity, observations = observations)

# Plot the values as ROC curves
plot(
  1 - knearest_specificity,
  knearest_sensitivity,
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "FPR",
  ylab = "TPR"
)
lines(1 - knearest_specificity, knearest_sensitivity, col = "Blue")
lines(1 - kknn_specificity, kknn_sensitivity, col = "Green")
lines(c(0, 1), c(0, 1), col = "Red")