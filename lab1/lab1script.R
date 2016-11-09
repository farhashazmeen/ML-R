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

#####Distance calculation
Distance = function(x,y){
  xhat = x/sqrt(rowSums(x^2))
  yhat = y/sqrt(rowSums(y^2))
  C = xhat%*%t(yhat)
  return(1-C)
}

stupid = function(vec1,vec2){
  return(sum(vec2[vec1]))
}

isspam = function(x){
  if(x > 0.5){
    return(1)
  }
  return(0)
}

isspam2 = function(x){
  if(x > seq(0.05,0.95,0.05)){
    return(1)
  }
  return(0)
}

k_nearest = function(k, ftrain, ftest){

  #
  trainmat <- data.matrix(ftrain[,-ncol(ftrain)])
  testmat <- data.matrix(ftest[,-ncol(ftest)])
  D = Distance(trainmat, testmat)
  View(D)
  # Distance to all neighbours in sorted order.
  orderedByIndex = t(apply(D, 2, order))
  View(orderedByIndex)
  # Contains only the k nearest neighbors.
  neighbours = orderedByIndex[,1:k]

  # Selects the last row of train which contains the spam value
  # 0 or 1.
  trainSpamByIndex = ftrain[,ncol(ftrain)]
  
  # The mean of the neighbours spam values which is 0 or 1.
  View(neighbours)
  
  return (meanOfNeighbours = apply(t(t(neighbours)), 1, stupid, trainSpamByIndex)/k)
}



######################Senstitivity
sens = function(vec1,vec2){

  return(sum(vec1 == 1 & vec2 == 1)/(sum(vec2 == 1)))

}

spec = function(vec1,vec2){
  return(sum(vec1 == 0 & vec2 == 0)/(sum(vec2 == 0)))

}


#################

########Classification


########For k=5

knear5 = k_nearest(5, train, test)
knear5_spam = apply(t(t(knear5)),1,isspam)


confusiontablefor5 = table(knear5_spam,test[,ncol(test)])
veci = table(knear5_spam + test[,ncol(test)])
missclassratefor5 = veci[2]/sum(confusiontablefor5)
vecsome = veci
###############################


########for k=1

knear1 = k_nearest(1, train, test)
knear1_spam = apply(t(t(knear1)),1,isspam)

confusiontablefor1 = table(knear1_spam,test[,ncol(test)])
veci = table(knear1_spam + test[,ncol(test)])
missclassratefor1 = veci[2]/sum(confusiontablefor1)

###############################


########Time for kknn

kknnfor5 = kknn(formula = Spam ~ ., train = as.data.frame(train), test = as.data.frame(test) ,k = 5)
kknnfor1 = kknn(formula = Spam ~ ., train = as.data.frame(train), test = as.data.frame(test) ,k = 1)

kknnpredictfor5 = apply(t(t(fitted.values(kknnfor5))),1,isspam)
kknnpredictfor1 = apply(t(t(fitted.values(kknnfor1))),1,isspam)

kknnconfusiontablefor5 = table(kknnpredictfor5,test[,ncol(test)])
veci = table(kknnpredictfor5 + test[,ncol(test)])
KKnnmissclassratefor5 = veci[2]/sum(kknnconfusiontablefor5)



kknnconfusiontablefor1 = table(kknnpredictfor1,test[,ncol(test)])
veci = table(kknnpredictfor1 + test[,ncol(test)])
kknnmissclassratefor1 = veci[2]/sum(kknnconfusiontablefor1)



#############################Try pred2

knear5spampred2 = sapply(knear,function(x){as.integer(x>seq(0.05,0.95,0.05))})
knear5sensitivity = apply(knear5spampred2,1,sens,test[,ncol(test)])
knear5specificity = apply(knear5spampred2,1,spec,test[,ncol(test)])

kknn = fitted.values(kknnfor5)
kknn5spampred2 = sapply(kknn,function(x){as.integer(x>seq(0.05,0.95,0.05))})
kknn5sensitivity = apply(kknn5spampred2,1,sens,test[,ncol(test)])
kknn5specificity = apply(kknn5spampred2,1,spec,test[,ncol(test)])

plot(1-knear5specificity,knear5sensitivity,type = "l", col ="red", xlab = "False negative", ylab = "True positive")
lines(1-kknn5specificity,kknn5sensitivity, col = "blue")

#####################################
###############################

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
maxlike6 = seqstep[which.max(loglikevec_six)]


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
lines(seqstep,lfun)
#text(locator(), labels = c("When data size is 6", "When data size is 48", "Posteori Probability"))
maxlikepp = seqstep[which.max(lfun)]
#plot(seqstep,prop(10,seqstep), col="blue", type="l")

# Not used, saved for later.
# estimatedlam = length(machines[,1])/(sum(machines))

# We will make cnnclusions for tomorrow i think
hist(rexp(50,maxlike), xlab = "Randomly generated data")
hist(forwin, xlab = "Original data")

