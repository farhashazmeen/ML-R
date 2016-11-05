source("likelihood.r") # sum of ln(p(x|theta)).
lifetimes <- read.csv("data.csv") # Non-matrix.
parameter <- seq(0.1, 8.0, by=0.1) # Testing...
p <- sapply(parameter, lnlikelihood, lifetimes)
average_lnlikelihoods <- p / dim(lifetimes)[1];
mle <- order(average_lnlikelihoods)[length(p)];
