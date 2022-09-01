### IRIS 
## SVM 
library(e1071)
data=iris
n=dim(data)[1]
index = sample(n, 0.7 * n)
Appren = data[index, ]
Test = data[-index, ]
T1=Sys.time()
model <- svm(Species ~ ., data = Appren)
T2=Sys.time()
T2-T1
model
Pred <- predict(model, newdata=Test)
Test.mod <- cbind(Test, Pred)
Confusion = table(Test.mod$Species, Test.mod$Pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err
head(Test.mod, 5)
# visualiser les classes par couleur, et les SV par des croix:
plot(cmdscale(dist(data[,-5])),col = as.integer(data[,5]),pch = c("o","+")[1:150 %in% model$index + 1])

## MODEL1_NAIVE_BASE
T1=Sys.time()
model1 <- naiveBayes(Species ~ ., data = Appren)
T2=Sys.time()
T2-T1
Pred=predict(object = model1, newdata = Test)
Pred
Test.mod <- cbind(Test, Pred)
head(Test.mod, 5)
Confusion = table(Test.mod$Species, Test.mod$Pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err

#arbre de decision
library(rpart)
T1=Sys.time()
model2<- rpart(Species~.,data=Appren)
T2=Sys.time()
T2-T1
pred=predict(model2,Test, type="class")
pred
tab=table(Test$Species, pred)
tab
err <- 1-sum(diag(tab))/sum(tab)
err

# Random forest 
library( randomForest)
T1=Sys.time()
RF=randomForest(Species~., data=iris)
T2=Sys.time()
T2-T1
RF

## GLASS 
 
## SVM 
library(e1071)
data("Glass")
Glass
data1=Glass
n=dim(data1)[1]
index = sample(n, 0.7 * n)
Appren = data1[index, ]
Test = data1[-index, ]
T1=Sys.time()
modelG<- svm(Type~ ., data = Appren)
T2=Sys.time()
T2-T1
modelG
Pred <- predict(modelG, newdata=Test)
Test.mod <- cbind(Test, Pred)
Confusion = table(Test.mod$Type, Test.mod$Pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err
head(Test.mod, 5)
# visualiser les classes par couleur, et les SV par des croix:
plot(cmdscale(dist(data1[,-5])),col = as.integer(data1[,5]),pch = c("o","+")[1:150 %in% modelG$index + 1])

## MODEL1_NAIVE_BASE
T1=Sys.time()
model1 <- naiveBayes(Type~ ., data = Appren)
T2=Sys.time()
T2-T1
Pred=predict(object = model1, newdata = Test)
Pred
Test.mod <- cbind(Test, Pred)
head(Test.mod, 5)
Confusion = table(Test.mod$Type, Test.mod$Pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err

#arbre de decision
library(rpart)
T1=Sys.time()
model2<- rpart(Type~.,data=Appren)
T2=Sys.time()
T2-T1
pred=predict(model2,Test, type="class")
pred
tab=table(Test$Type, pred)
tab
err <- 1-sum(diag(tab))/sum(tab)
err

# Random forest 
library( randomForest)
data("Glass")
T1=Sys.time()
RF=randomForest(Type~., data=Glass)
T2=Sys.time()
T2-T1
RF

## SONAR
## SVM 
library(mlbench)
data("Sonar")
Sonar
data1=Sonar
n=dim(data1)[1]
index = sample(n, 0.7 * n)
Appren = data1[index, ]
Test = data1[-index, ]
T1=Sys.time()
modelG<- svm(Class~ ., data = Appren)
T2=Sys.time()
T2-T1
modelG
Pred <- predict(modelG, newdata=Test)
Test.mod <- cbind(Test, Pred)
Confusion = table(Test.mod$Class, Test.mod$Pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err
head(Test.mod, 5)
# visualiser les classes par couleur, et les SV par des croix:
plot(cmdscale(dist(data1[,-5])),col = as.integer(data1[,5]),pch = c("o","+")[1:150 %in% modelG$index + 1])

## MODEL1_NAIVE_BASE
T1=Sys.time()
model1 <- naiveBayes(Class~ ., data = Appren)
T2=Sys.time()
T2-T1
Pred=predict(object = model1, newdata = Test)
Pred
Test.mod <- cbind(Test, Pred)
head(Test.mod, 5)
Confusion = table(Test.mod$Class, Test.mod$Pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err

#arbre de decision
library(rpart)
T1=Sys.time()
model2<- rpart(Class~.,data=Appren)
T2=Sys.time()
T2-T1
pred=predict(model2,Test, type="class")
pred
tab=table(Test$Class, pred)
tab
err <- 1-sum(diag(tab))/sum(tab)
err 

# Random forest 
library( randomForest)
data("Sonar")
T1=Sys.time()
RF=randomForest(Class~., data=Sonar)
T2=Sys.time()
T2-T1
RF 

## SHUTTLE 
## SVM 
library(mlbench)
data("Shuttle")
Shuttle
data1=Shuttle
n=dim(data1)[1]
index = sample(n, 0.7 * n)
Appren = data1[index, ]
Test = data1[-index, ]
T1=Sys.time()
modelG<- svm(Class~ ., data = Appren)
T2=Sys.time()
T2-T1
modelG
Pred <- predict(modelG, newdata=Test)
Test.mod <- cbind(Test, Pred)
Confusion = table(Test.mod$Class, Test.mod$Pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err
head(Test.mod, 5)
# visualiser les classes par couleur, et les SV par des croix:
plot(cmdscale(dist(data1[,-5])),col = as.integer(data1[,5]),pch = c("o","+")[1:150 %in% modelG$index + 1])

## MODEL1_NAIVE_BASE
T1=Sys.time()
model1 <- naiveBayes(Class~ ., data = Appren)
T2=Sys.time()
T2-T1
Pred=predict(object = model1, newdata = Test)
Pred
Test.mod <- cbind(Test, Pred)
head(Test.mod, 5)
Confusion = table(Test.mod$Class, Test.mod$Pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err

#arbre de decision
library(rpart)
T1=Sys.time()
model2<- rpart(Class~.,data=Appren)
T2=Sys.time()
T2-T1
pred=predict(model2,Test, type="class")
pred
tab=table(Test$Class, pred)
tab
err <- 1-sum(diag(tab))/sum(tab)
err

# Random forest 
library( randomForest)
data("Shuttle")
T1=Sys.time()
RF=randomForest(Class~., data=Shuttle)
T2=Sys.time()
T2-T1
RF 

# Random forest TUNING 
library(e1071)
data("Shuttle")
Appren
tune.randomForest(Class~., data=Appren, ntree= c(10,100,500))
T1=Sys.time()
RF=tune.randomForest(Class~., data=Appren,ntree=c(10,100,500))
T2=Sys.time()
T2-T1
RF 

