library(tree)
library(boot)
set.seed(12345)
data = read.csv("State.csv",sep = ";")

order_data = data[order(as.numeric(data$MET)),]

controll = tree.control(nrow(order_data), minsize = 8)
fit = tree( EX ~ as.numeric(MET), order_data, control = controll)

fit.cv = cv.tree(fit)
best_k = fit.cv$size[which.min(fit.cv$dev)]
optimal_tree = prune.tree(fit, best=best_k)

plot(optimal_tree)
text(optimal_tree)

plot(order_data$EX, order_data$MET, xlab = "expendature", ylab = "metropolitan")
plot(predict(optimal_tree), order_data$MET, xlab = "expendature", ylab = "metropolitan")

hist(residuals(optimal_tree))


f = function(data, index){
  data1 = data[index,]
  controll = tree.control(nrow(data1), minsize = 8)
  fit2 = tree( EX ~ as.numeric(MET), data=data1, control = controll)
  optimal_tree = prune.tree(fit2, best=best_k) 
  return(predict(optimal_tree, newdata = data1))
}

bs = boot(order_data, statistic = f, R=1000)
env = envelope(bs)
plot(t(env$overall))