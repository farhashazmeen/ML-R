library("ggplot2")
library("reshape2")
library("grDevices")
library("e1071")

set.seed(12345) # As always.....
scores <- read.csv("scores.csv")
n <- nrow(scores) # Observation.
samples <- sample(1:n,  n / 2.0)
others <- setdiff(1:n, samples)
halves <- sample(others, n/4.0)

training <- scores[samples,]
trainingX <- training[,-ncol(training)]
trainingy <- training[,ncol(training)]

validation <- scores[halves,]
validationX <- validation[,-ncol(validation)]
validationy <- validation[,ncol(validation)]

testing  <- scores[-halves,]
testingX <- testing[,-ncol(testing)]
testingy <- testing[,ncol(testing)]

fit <- naiveBayes(good_bad ~ ., data = training)
training_prediction <- predict(fit, training, type = "class")
testing_prediction <- predict(fit, testing, type = "class")
cat("\nMissclassifications using Naive Bayes method:  (",
    mean(training_prediction != trainingy), "," ,
    mean(testing_prediction != testingy), ")\n")
cat("Confusion matrices for using Naive Bayes:\n")
print(table(training_prediction, trainingy))
print(table(testing_prediction, testingy))

training_probability <- predict(fit, training, type = "raw")
training_loss <- training_probability[,1] / training_probability[,2] > 10
training_loss[training_loss == FALSE] = "good"
training_loss[training_loss == TRUE] = "bad"
training_loss <- as.factor(training_loss)

testing_probability <- predict(fit, testing, type = "raw")
testing_loss <- testing_probability[,1] / testing_probability[,2] > 10
testing_loss[testing_loss == FALSE] = "good"
testing_loss[testing_loss == TRUE] = "bad"
testing_loss <- as.factor(testing_loss)

cat("\n\nMissclassifications using Naive Bayes loss method:  (",
    mean(training_loss != trainingy), "," ,
    mean(testing_loss != testingy), ")\n")
cat("Confusion matrices for using Naive Bayes loss:\n")
print(table(training_loss, trainingy))
print(table(testing_loss, testingy))
