# Not used, ignore it.
prop = function(x,theta){
  return(theta * exp(-theta*x))
}

forwin = machines[[1]]

# Log-likelihood function.
loglike = function(theta,vec){  
    return((length(vec)*log(theta)) - (theta*sum(vec)))  
}

# Fatal amount of values.
seqstep = seq(0.05,4,0.0001)

# Result of log-liklelohjdiofsyh given fatal many attempts with different theta.
loglikevec = loglike(seqstep,forwin)

# Draws a nice looking graph.
plot(seqstep,loglikevec, col="red",type = "l", xlab="theta", ylab="likelihood for theta")

# According to the plot the value of theta that yields highest likelihood.
maxlike = seqstep[which.max(loglikevec)]



# Not used, saved for later.
# estimatedlam = length(machines[,1])/(sum(machines))