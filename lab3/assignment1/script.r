library(readxl)

data = read.csv("australian-crabs.csv")
set.seed(12345)

plot(data$CL,data$RW, col=data$sex)
# Is the data easy to calssify be linear discriminant analysis?
# : Yes, very easy

mle <- function(X){
    
}
#     O 1  O 2  O 3  O 4
#    +----+----+----+----+
# C1 |  1 | 2  |  3 | 4  |
#    +----+----+----+----+
# C2 |  5 | 6  |  7 | 8  |
#    +----+----+----+----+
# C3 |  9 | 10 | 11 | 12 |
#    +----+----+----+----+
# C4 | 13 | 14 | 15 | 16 |
#    +----+----+----+----+
#

df <- function(X,C){
  # 
  for(i in 1:length(C))){
    estimated_means[i,] = mean
  }
  estimated_means[1,]  = rowMeans(X)
  estimated_sigmas = rowMeans((X-estimated_means)*t((X-estimated_means)))
  estimated_priors = ncol(X[,])

  
  W = matrix(1,nrow(X),2)
  W[,1] = -0.5*t(means)*solve(rowSums(means))
  W[,2] = solve(rowSums(means))
  return()
}

lda <- function(X,Y){
  mle = 1/N
}


