library(pls)
library(tree)

# Part 1: Regression Tree Model

set.seed(12345)
glass <- read.csv2("glass.csv")
glassy <- glass[3] ; glassx <- glass[-3]
training_indices <- sample(1:nrow(glass), nrow(glass) / 2)
remaining_indices <- setdiff(1:nrow(glass), training_indices)
testing_indices <- sample(remaining_indices, length(remaining_indices) / 2)
validation_indices <- setdiff(remaining_indices, testing_indices)
validation <- glass[validation_indices,]
training <- glass[training_indices,]
testing <- glass[testing_indices,]

fit <- tree(Al ~ ., data = training)
validation_deviances <- rep(0, 8)
training_deviances <- rep(0, 8)

for (leaves in 2:8) {
    pruned <- prune.tree(fit, best = leaves)
    predicted <- predict(pruned, newdata = validation, type = "tree")
    validation_deviances[leaves] <- deviance(predicted)
    training_deviances[leaves] <- deviance(pruned)
}

png("training_deviance.png")
plot(2:8,training_deviances[2:8],
     xlab = "Leaves", ylab = "Deviance",
     main = "Training Tree Size")
dev.off()

png("validation_deviance.png")
plot(2:8,validation_deviances[2:8],
     xlab = "Leaves", ylab = "Deviance",
     main = "Validation Tree Size")
dev.off()

# Part 2: Analysis of the Results

# Derived from the above graphs.
fit <- prune.tree(fit, best = 5)
summary(fit) # Gives variables.

png("optimal_tree.png")
plot(fit) # Optimal P!
text(fit) # Nice tree!
title("Optimal Tree")
dev.off()

predicted <- predict(fit, newdata = testing, type = "vector")
mean_square_error <- mean((predicted - testing$Al)^2)

# Part 3: PLS Regression Model

full_training_indices <- c(training_indices, validation_indices)
full_training <- glass[full_training_indices,] # Using C-V here.
fit <- plsr(Al ~ ., data = full_training, validation = "CV")

summary(fit) # Gives us most of the relevant information.

png("validation_plot.png")
validationplot(fit)
dev.off()

png("loading_plot.png")
loadingplot(loadings(fit), label = "names")
dev.off()

predicted <- predict(fit, newdata = testing)
mean_square_error <- mean((predicted - testing$Al)^2)
