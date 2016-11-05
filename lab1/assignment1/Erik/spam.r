library("kknn")
source("distance.r")
source("knearest.r")

set.seed(12345)
data <- read.csv("data.csv")
# Pick randomly around half of the rows in dataset.
samples <- sample(1:nrow(data), floor(0.5*nrow(data)))
# Split given dataset evenly for training and tests.
learning <- data.matrix(data[samples,]) # Training.
testing <- data.matrix(data[-samples,]) # Testing.

cat("\nknearest: prediction with k=5\n")

# Predict spam for testing data K=5.
k5 <- round(knearest(learning, 5, testing))
# Generate the confusion matrix for K=5.
cm5 <- table(k5, testing[,ncol(testing)])
# Calculate given missclassification.
mc5 <- 1 - sum(diag(cm5)) / sum(cm5)
# Report confusion matrix and error.
print(cm5) ; print(mc5)

cat("\nknearest: prediction with k=1\n")

# Predict spam for testing data K=1.
k1 <- round(knearest(learning, 1, testing))
# Generate the confusion matrix for K=1.
cm1 <- table(k1, testing[,ncol(testing)])
# Calculate given missclassification.
mc1 <- 1 - sum(diag(cm1)) / sum(cm1)
# Report confusion matrix and error.
print(cm1) ; print(mc1)

cat("\nkknn: prediction with k=5\n")

m5 <- train.kknn(Spam ~ ., data = data[samples,], kmax = 5)
p5 <- round(predict(m5, data[-samples,]))
# Generate the confusion matrix for K=5.
cm5 <- table(p5, testing[,ncol(testing)])
# Calculate given missclassification.
mc5 <- 1 - sum(diag(cm5)) / sum(cm5)
# Report confusion matrix and error.
print(cm5) ; print(mc5)

cat("\nkknn: prediction with k=1\n")

m1 <- train.kknn(Spam ~ ., data = data[samples,], kmax = 1)
p1 <- round(predict(m1, data[-samples,]))
# Generate the confusion matrix for K=1.
cm1 <- table(p1, testing[,ncol(testing)])
# Calculate given missclassification.
mc1 <- 1 - sum(diag(cm1)) / sum(cm1)
# Report confusion matrix and error.
print(cm1) ; print(mc1)
