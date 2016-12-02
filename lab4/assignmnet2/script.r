library(ggplot2)
library(fastICA)
library(pls)

fulldata = read.csv2("NIRSpectra.csv")
data = fulldata[, -ncol(fulldata)]

res = prcomp(data)
lambda = res$dev^2
#eigen
print(lambda)

#Variance
sprintf("%2.3f",lambda/sum(lambda)*100)
screeplot(res)
fig_data = data.frame(x=res$x[,1], y=res$x[,2], xlab = "PC1", ylab = "PC2")

fig = ggplot(fig_data) +
  geom_point(aes(x = x,y = y))
print(fig)

U = res$rotation
U = U[-nrow(U),]

fig_data = data.frame(x1=1:length(U[,1]), y1=U[,1])
fig = ggplot(fig_data, ylim = c(0,0.15), xlab = "Index" , ylab = "Loadings - PC1") +
  geom_point(aes(x1,y1), col="#AA1111") +
  geom_line((aes(x1,y1)))
print(fig)

fig_data = data.frame(x1=1:length(U[,2]), y1=U[,2])
fig = ggplot(fig_data, ylim = c(0,0.15), xlab = "Index" , ylab = "Loadings - PC2") +
  geom_point(aes(x1,y1), col="#00FF11") +
  geom_line((aes(x1,y1)))
  print(fig)

set.seed(12345)
ica = fastICA(data,2)
um = ica$K %*% ica$W 
fig_data = data.frame(x1=1:length(um[,1]), y1=um[,1])
fig = ggplot(fig_data, ylim = c(0,0.15), xlab = "Index", ylab = "ICA - W^1") +
  geom_point(aes(x1,y1), col="#AA1111") +
  geom_line((aes(x1,y1)))
print(fig)

fig_data = data.frame(x1=1:length(um[,2]), y1=um[,2])
fig = ggplot(fig_data, ylim = c(0,0.15), xlab = "Index", ylab = "ICA - W^2") +
  geom_point(aes(x1,y1), col="#00FF11") +
  geom_line((aes(x1,y1)))
print(fig)

fig_data = data.frame(xn=ica$S[,1], yn=ica$S[,2],xo=res$x[,1], yo=res$x[,2])
fig = ggplot(fig_data, ylim = c(0,0.15), xlab = "ICA/PCA - PC1", ylab = "ICA/PCA - PC2") +
  geom_point(aes(x = xn,y=yn), col="#0033FF") + 
  geom_point(aes(x = xo, y=yo), col="#FF6666")
print(fig)

set.seed(12345)
pcr.fit <- pcr(Viscosity ~ ., data=fulldata, validation="CV")
validationplot(pcr.fit,val.type = "MS")