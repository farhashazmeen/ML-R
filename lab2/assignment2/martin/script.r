library(MASS)
library(readxl)
library(Matrix)
library(glmnet)

# task 1 
# load data and show plot, do you think the data can be represented by a linear model?
data = read_excel("tecator.xlsx")
plot(data$Protein,data$Moisture)
# answer: yes

# task 2
# Find a probailist model explaining Mi. where M is a polynomial model of the protein up to power i
# Why is it important to use mean squared error whne fitting model?

# We approximate the model as w0 + w1x^1 + w2x^2 ...
# Use MSE instead of SSE because MSE is less dependent on

# task 3
# Divide the data into traning and test set 50/50.
# Fit models Mi, i = 1:6
#   Record MSE for traning and validation, 
#   Show a plot of how traning and validation depends on i
set.seed(12345)
n = nrow(data)
train_indexes = sample(1:n,floor(n*0.5))
train_data = data[train_indexes,]
test_data = data[-train_indexes,]
power = 6
train_error = matrix(0,power,1)
test_error = matrix(0,power,1)

for(i in 1:power) {
  model = lm(Moisture ~ poly(Protein,i), data=train_data)
  train_predictions = predict(model,train_data)
  test_predictions = predict(model,test_data)
  
  train_error[i,] = mean((train_data$Moisture - train_predictions)^2)
  test_error[i,] = mean((test_data$Moisture - test_predictions)^2)
}

ylim = c(min(rbind(train_error,test_error)),max(rbind(train_error,test_error)))
plot(1:power,train_error, col="Green", ylim=ylim)
lines(1:power,train_error, col="Green")
points(1:power,test_error,col="Red")
lines(1:power,test_error, col="Red")

# Observation of the plot indicates that as we increase the polynomial level the functiuon becomes more fitted for the training data and results in increasingly worse fit for the test data

# task 4
# Use stepAIC to perform variable selection over a linear model with channel1-100 as predictors and Fat as response
#   Comment on how many predictors were selected
# Remove eveyrting except Fat and channels
# Perform feature slection and record number of features

model = lm(Fat ~ . - Protein - Moisture - Sample, data=data)
steps = stepAIC(model,direction="both", trace=FALSE)
coeff_aics = steps$coefficients
n_coeff_aics = length(coeff_aics)
print(n_coeff_aics)
# 64 columns were selected

# task 5
#  Fit a ridge regresion model to the same predictor/response varaibles
#  Present a plot on how the coefficients depend on the log of the penalty factor lamba.
#  Report how the coefficients change depending on lamda
data_y = data$Fat

data = data[,-which(colnames(data) == "Sample")]
data = data[,-which(colnames(data) == "Fat")]
data = data[,-which(colnames(data) == "Protein")]
data = as.matrix(data[,-which(colnames(data) == "Moisture")])

ridge = glmnet(x=data, y=data_y, alpha=0, nlambda=100)
plot(ridge, xvar="lambda")

# task 6
# Do the same stuff but with LASSO, compare with ridge
lasso = glmnet(x=data, y=data_y, alpha=1, nlambda=100)
plot(lasso, xvar="lambda")

# task 7
#   CV to find the optimal LASSO, report the optimal lambda and how many variables were chosen by the model '
#   make conculsions
#   show a plot of CV scores in comparasion to lambda

lasso_cv = cv.glmnet(data,data_y, alpha=1)
lasso_cv_lambda = lasso_cv$lambda.min
n_lass_cv = sum(as.matrix(coef(lasso_cv)) != 0)
print(n_lass_cv)
# task 8 
# compare result from 4 and 7