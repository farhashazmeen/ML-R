
machines <- data.matrix(read_excel("machines.xlsx"))


log_likelihood <- function(theta,data){
  prob = theta * exp(-theta * data)
  ll = log(prob)
  return(ll)
}

max_log_likelihood <- function(theta,data){
  ll = log_likelihood(theta,data)
  return(max(ll))
}

mystery <- function(theta, data) {
  return(log(probability(theta,data)) + log(probability(theta=10,data=theta)))  
}

theta = matrix(1:50,50,1)

likelihoods = t(apply(theta,1,log_likelihood,data=machines))

# Calculate max log likelihood for the full data set
max_likelihoods_full = apply(likelihoods,1,max)
print(max_likelihoods_full)
# Calculate max log likelihood for the first 6 observations in the data set
max_likelihoods_k6 = apply(theta,1,max_log_likelihood,data=machines[1:6,])

plot(theta,max_likelihoods_full, col="Red")
points(theta,max_likelihoods_k6, col="Green")

mystery_values = max_likelihoods_full + log_likelihood(10,theta)
plot(theta,mystery_values)