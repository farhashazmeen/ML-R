library(readxl)
library(tree)
library(e1071)
data = read_excel("creditscoring.xls")
data$good_bad = data$good_bad == "good"
n = nrow(data)
set.seed(12345)

indexes = sample(1:n,n)
end_traning = floor(n*0.5)
end_validation = end_traning + floor(n*0.25)

traning_indexes = indexes[1:end_traning]
validation_indexes = indexes[(end_traning+1):end_validation]
testing_indxes = indexes[(end_validation+1):n]

train = data[traning_indexes,]
validation = data[validation_indexes,]
testing = data[testing_indxes,]

dtreefit <- tree(as.factor(good_bad) ~ ., data=train, split = c("deviance"))
gtreefit <- tree(as.factor(good_bad) ~ ., data=train, split = c("gini"))

d_yfit = predict(dtreefit, newdata = testing,type="class")
g_yfit = predict(gtreefit, newdata = testing,type="class")
plot(dtreefit)
plot(gtreefit)

d_table = table(d_yfit,testing$good_bad)
g_table = table(g_yfit,testing$good_bad)

print(d_table)
print(1-sum(diag(d_table))/sum(d_table))
print(g_table)
print(1-sum(diag(g_table))/sum(g_table))

nv = 15
train_score = 1:nv
test_score = 1:nv
for(i in 2:nv){
  pruned=prune.tree(dtreefit,best=i)
  pred=predict(pruned, newdata=validation, type="tree")
  train_score[i] = deviance(pruned)
  test_score[i] = deviance(pred)
}
plot(2:nv,train_score[2:nv], col="Red",type = "b", ylim=c(0,max(train_score)))
points(2:nv,test_score[2:nv],col="Blue",type="b")

final = prune.tree(dtreefit,best=8)
yfit = predict(final,newdata=validation,type="class")
f_table = table(validation$good_bad,yfit)
print(f_table)
print(1-sum(diag(f_table))/sum(f_table))
plot(final)

# Naive bayes ????
bayes_model = naiveBayes(good_bad ~., data=train)
test_yfit = predict(bayes_model, testing[,-ncol(testing)], type = "raw")
train_yfit = predict(bayes_model, train[,-ncol(train)])
table(test_yfit,testing$good_bad)
