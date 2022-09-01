library(e1071)

#EXO 1 

naive_base2
tab=naive_base2
tab
nb.model <- naiveBayes(playTennis ~ ., data = tab)
nb.model
Pred=predict(object = nb.model, newdata = tab)
Pred
tab.mod <- cbind(tab, Pred)
head(tab.mod, 5)
Confusion = table(tab.mod$playTennis, tab.mod$Pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err
naive_base3
tab1=naive_base3
tab1
nb.model <- naiveBayes(playTennis ~ ., data = tab1)
nb.model
Pred=predict(object = nb.model, newdata = tab1)
Pred

#IRIS

iris
data=iris
data
T1=Sys.time()
nb.model <- naiveBayes(Species ~ ., data = Appren)
T2=Sys.time()
T2-T1
nb.model
Pred=predict(object = nb.model, newdata = Test)
Pred
Test.mod <- cbind(Test, Pred)
head(Test.mod, 5)
Confusion = table(Test.mod$Species, Test.mod$Pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err

#GLASS

library(mlbench)
data(Glass)
Glass
data1=Glass
n=dim(data1)[1]
index = sample(n, 0.7 * n)
Appren = data1[index, ]
Test = data1[-index, ]
T1=Sys.time()
nb.model <- naiveBayes(Type ~ ., data = Appren)
T2=Sys.time()
T2-T1
nb.model
Pred=predict(object = nb.model, newdata = Test)
Test.mod <- cbind(Test, Pred)
head(Test.mod, 5)
Confusion = table(Test.mod$Type, Test.mod$Pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err



