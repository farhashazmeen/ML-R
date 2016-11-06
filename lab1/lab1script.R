##################

if(!exists("spambase")){
  #Change this appropriatly
  spambase <- read_excel("C:/Users/Sebastian/Users/Desktop/TDDE01/TDDE01/lab1/spambase.xlsx") 
}

if(!exists("train")){
  n=dim(spambase)[1]
  set.seed(12345)
  id=sample(1:n,floor(n*0.5))
  train=data.matrix(spambase[id,])
  test=data.matrix(spambase[-id,])
}

#####Disatance calculation
Distance = function(x,y){
  xhat = x/sqrt(rowSums(x^2))
  yhat = y/sqrt(rowSums(y^2))
  C = xhat%*%t(yhat)
  return(1-C)
}

D = Distance(test,train)

########
########Classification

orderedDByIndex = apply(D,1,order)
neighbours = orderedDByIndex[,1:5] #### K = 5
trainSpamByIndex = train[,ncol(train)]

stupid = function(vec1,vec2){
  return(sum(vec2[vec1]))
}

isspam = function(x){
  if(x > 0.5){
    return(1)
  }
  return(0)
}

lotsofvalues = apply(neighbours,1,stupid,trainSpamByIndex)/5

testisspam = apply(t(t(lotsofvalues)),1,isspam) #This is stupid but i solved it with pure intuition

confusiontable = table(testisspam,test[,ncol(test)])
veci = table(testisspam + test[,ncol(test)])
missclassrate = veci[1]/sum(confusiontable)

#vecofneigh = trainSpamByIndex[neighbours[1:nrow(neighbours),]
#spamfortest = colSums(train[neighbours[1:nrow(neighbours),],ncol(train)])

#############################


##################################################################################################################
#Assignment 2
######################################

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
  return(loglike(theta,forwin) + log(prop(theta,10)))
}




#Plots the probability of having observation x and theta.
lfun = l(seqstep)
plot(seqstep,lfun,xlab="theta", ylab="l(theta)")


#plot(seqstep,prop(10,seqstep), col="blue", type="l")

# Not used, saved for later.
# estimatedlam = length(machines[,1])/(sum(machines))

# We will make cnnclusions for tomorrow i think
hist(rexp(50,maxlike))
hist(forwin)

################################################################################################