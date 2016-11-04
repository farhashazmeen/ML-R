# Not used, ignore it.
prop = function(x,theta){
  return(theta * exp(-theta*x))
}

# Log-likelihood function.
loglike = function(theta,vec){  
    return(length(vec)*log(theta) - theta*sum(vec))  
}

# Fatal amount of values.
seqstep = seq(0.05,4,0.0001)

#Endast relevant för senare version
forwin = machines[[1]] #may work with machines[,1] in older version then remove bellow commented code

# Result of log-liklelohjdiofsyh given fatal many attempts with different theta.
#loglikevec = loglike(seqstep,machines[,1])
#loglikevec_six = loglike(seqstep, machines[1:6,])
 
# Alternative for later version 
loglikevec = loglike(seqstep,forwin)
loglikevec_six = loglike(seqstep, forwin[1:6])

# Draws a nice looking graph.
plot(seqstep, loglikevec, main="Machine Lifetime", ylim=range(c(loglikevec_six, loglikevec)), col="red", type = "l", 
     xlab="theta", ylab="Likelihood for theta");
lines(seqstep, loglikevec_six, col="blue", type="l");

# According to the plot the value of theta that yields highest likelihood.
maxlike = seqstep[which.max(loglikevec)]



# From what we can se in the plot, the estimation of maximum likelihood is both less
# definitie, meaning, the liklihood for theta to produce the observed vec is a very high
# for a lot of values.

#log(p(x|theta)p(teta)) = log(L(theta|x)p(theta))
# = log(L(theta|x)) + log(p(theta))
# = loglike(theta,x) + log(prop(10,theta))

l = function(theta){
  return(loglike(theta,forwin) + log(prop(10,theta)))
}


#Plots the probability of having observation x and theta.
plot(seqstep,l(seqstep),xlab="theta", ylab="l(theta)")


#plot(seqstep,prop(10,seqstep), col="blue", type="l")

# Not used, saved for later.
# estimatedlam = length(machines[,1])/(sum(machines))

# We will make cnnclusions for tomorrow i think
hist(rexp(50,maxlike))
hist(forwin)

