library(readxl)

machines <- data.matrix(read_excel("machines.xlsx"))


probability <- function(theta, data) {
  return(theta * exp((-1 * theta) * data))
}

log_likelihood <- function(theta, data) {
  ll = log(probability(theta, data))
  return(ll)
}

max_log_likelihood <- function(theta, data) {
  ll = log_likelihood(theta, data)
  return(sum(ll))
}

l <- function(log_likelihoods, theta, lambda) {
  return(log_likelihoods + log_likelihood(lambda, theta))
}

# Create a matrix with all the theta samples to be tested
theta = seq(from = 0.1, to = 100, by = 1)
theta = matrix(theta, length(theta), 1)

# Generate the disitrbution of the machines data set
distribution = t(apply(theta, 1, probability, data = machines))

# Display the plot
plot(
  theta,
  rowMeans(distribution),
  col = "Green",
  xlab = "Theta",
  ylab = "p(x|theta)"
)
theta = seq(from = 0.1, to = 20, by = 0.1)
theta = matrix(theta, length(theta), 1)

# Calculate max log likelihood for the full data set
max_likelihoods_full = apply(theta, 1, max_log_likelihood, data = machines)

#Get the best theta (for task 5)
max_theta = which.max(max_likelihoods_full) / 10
print(max_theta)
# Calculate max log likelihood for the first 6 observations in the data set
max_likelihoods_k6 = apply(theta, 1, max_log_likelihood, data = machines[1:6, ])

plot(
  theta,
  max_likelihoods_full,
  col = "Red",
  xlab = "Theta" ,
  ylab = "log(p(x|theta))",
  ylim = c(-600, 10)
)
points(theta, max_likelihoods_k6, col = "Green")

# l(theta) = log(p(x|theta) * p(theta)) = log(p(x|theta)) + log(p(theta)), log(p(x|theta)) = max_likelihoods_full
posteriori_likelihood = l(max_likelihoods_full, theta, 10)

# Looks like a linear function since log(p(theta)) is huge compared to log(p(x|theta))
plot(theta,
     posteriori_likelihood,
     xlab = "Theta" ,
     ylab = "log(p(x|theta) * p(theta))")

# Generate 50 samples from the exponential distribution with theta is equal to maximum likleihood theta for the given data set
set.seed(12345)
random_exp_distribution = rexp(50, max_theta)

hist(random_exp_distribution)
hist(machines)
