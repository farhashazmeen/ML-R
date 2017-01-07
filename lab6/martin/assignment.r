library(neuralnet)
library("grDevices")

set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation
# Random initializaiton of the weights in the interval [-1, 1]
results = rep(0,10)
print(results)
winit <- runif(250,-1,1)
  for(i in 1:10) {
    nn <- neuralnet(formula = Sin ~ Var, data=tr, hidden = 10, threshold = i/1000 ,startweights = winit)
    result = compute(nn, va$Var)$net.result 
    results[i] = mean((result - va$Sin)^2)
}
best = which.min(results)
nn <- neuralnet(formula = Sin ~ Var , data=trva, hidden = 10, threshold = best/1000, startweights = winit)
plot(nn)
# Plot of the predictions (black dots) and the data (red dots)
setEPS()
cairo_ps("predictions.eps")
  plot(prediction(nn)$rep1, col="Black")
  points(trva, col = "red")
  dev.off()