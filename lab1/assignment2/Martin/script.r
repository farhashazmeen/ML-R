ll <- function(theta,x) {
  
  return(log(theta * exp(-theta * x)))
}

theta = 1:200

y = 0
for(i in theta){
  y[i] = max(ll(i,machines))
}


y1 = 0
for(i in theta){
  y1[i] = max(ll(i,machines[1:6,]))
}

plot(theta,y)
points(theta,y1, col="green")
