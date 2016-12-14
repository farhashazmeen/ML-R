library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation
# Random initializaiton of the weights in the interval [-1, 1]
results = matrix(0,10,nrow(tr))
winit <- runif(10,-1,1)# Your code here
  for(i in 1:10) {
    nn <- neuralnet("Sin ~ Var",data=tr, hidden = 10, threshold = i/1000 ,startweights = winit)
    result = compute(nn, (1:nrow(tr)))$net.result
    results[i,] = result
}
plot(nn <- neuralnet())
  # Plot of the predictions (black dots) and the data (red dots)
  plot(prediction(nn)$rep1)
  points(trva, col = "red")