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

setEPS()
postscript('A2_pcahist.eps')
screeplot(res)
dev.off()

fig_data = data.frame(x=res$x[,1], y=res$x[,2], xlab = "PC1", ylab = "PC2")
fig = ggplot(fig_data) +
  geom_point(aes(x = x,y = y)) +
  labs(x = "PC1") +
  labs(y = "PC2")
print(fig)
ggsave(file="A2_pcascore.eps")

U = res$rotation
U = U[-nrow(U),]

fig_data = data.frame(x1=1:length(U[,1]), y1=U[,1])
fig = ggplot(fig_data, ylim = c(0,0.15), xlab = "Index" , ylab = "Loadings - PC1") +
  geom_point(aes(x1,y1), col="#AA1111") +
  geom_line((aes(x1,y1))) +
  labs(x = "Index") +
  labs(y = "Loadings - PC1")
print(fig)
ggsave(file="A2_trace_PC1.eps")


fig_data = data.frame(x1=1:length(U[,2]), y1=U[,2])
fig = ggplot(fig_data, ylim = c(0,0.15), xlab = "Index" , ylab = "Loadings - PC2") +
  geom_point(aes(x1,y1), col="#00FF11") +
  geom_line((aes(x1,y1))) +
  labs(x = "Index") +
  labs(y = "Loadings - PC2")
print(fig)
ggsave(file="A2_trace_PC2.eps")
  
set.seed(12345)
ica = fastICA(data,2)
um = ica$K %*% ica$W 
fig_data = data.frame(x1=1:length(um[,1]), y1=um[,1])
fig = ggplot(fig_data, ylim = c(0,0.15), xlab = "Index", ylab = "ICA - W^1") +
  geom_point(aes(x1,y1), col="#AA1111") +
  geom_line((aes(x1,y1))) +
  labs(x = "Index") +
  labs(y = "ICA - W^1")
print(fig)
ggsave(file="A2_trace_ICA1.eps")


fig_data = data.frame(x1=1:length(um[,2]), y1=um[,2])
fig = ggplot(fig_data, ylim = c(0,0.15), xlab = "Index", ylab = "ICA - W^2") +
  geom_point(aes(x1,y1), col="#00FF11") +
  geom_line((aes(x1,y1))) +
  labs(x = "Index") +
  labs(y = "ICA - W^2")
print(fig)
ggsave(file="A2_trace_ICA2.eps")

fig_data = data.frame(xn=ica$S[,1], yn=ica$S[,2],xo=res$x[,1], yo=res$x[,2])
fig = ggplot(fig_data, ylim = c(0,0.15), xlab = "ICA/PCA - PC1", ylab = "ICA/PCA - PC2") +
  geom_point(aes(x = xn,y=yn), col="#0033FF") + 
  geom_point(aes(x = xo, y=yo), col="#FF6666")  +
  labs(x = "ICA/PCA - PC1") +
  labs(y = "ICA/PCA - PC2")
print(fig)
ggsave(file="A2_icascore.eps")

set.seed(12345)
pcr.fit <- pcr(Viscosity ~ ., data=fulldata, validation="CV")

setEPS()
postscript("A2_viscosity.eps")
validationplot(pcr.fit,val.type = "MS")
dev.off()
