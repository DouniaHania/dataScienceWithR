## IRIS
library(nnet)
iris
n=dim(iris)[1]
index = sample(n, 0.7 * n)
Appren = iris[index, ]
Test = iris[-index, ]
T1=Sys.time()
RN <- nnet(Species~., Appren, size = 4)
T2=Sys.time()
T2-T1
summary(RN)
pred=predict(RN,Test, type="class")
pred
Confusion = table(Test$Species,pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err
## TUNING 
library(e1071)
tune.nnet(Species~.,data=iris,size=4:5,decay=1:10)
RN <- nnet(Species ~ ., data = Appren,decay=1, size = 4)
tune.nnet(Species~.,data=iris,size=2:5,decay=c(0,0.001,0.1,10))
T1=Sys.time()
RN <- nnet(Species ~ ., data = Appren,decay=0.001, size = 2)
T2=Sys.time()
T2-T1
summary(RN)
pred=predict(RN,Test, type="class")
pred
Confusion = table(Test$Species,pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err

## selection d'attribut 
T1=Sys.time()
RN <- nnet(Species~Petal.Length+Petal.Width, Appren, size = 2, decay=0.001)
T2=Sys.time()
T2-T1
pred=predict(RN,Test, type="class")
pred
Confusion = table(Test$Species,pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err


### GLASS 
library(mlbench)
#partitionnement des données
data(Glass)
Glass
data1=Glass
n=dim(data1)[1]
index = sample(n, 0.7 * n)
Appren = data1[index, ]
Test = data1[-index, ]
T1=Sys.time()
RN <- nnet(Type~., Appren, size = 9)
T2=Sys.time()
T2-T1
summary(RN)
pred=predict(RN,Test, type="class")
pred
Confusion = table(Test$Type,pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err

## TUNING 
library(e1071)
tune.nnet(Type~.,data=Glass,size=2:12,decay=c(0,0.001,0.1,10))
T1=Sys.time()
RN <- nnet(Type ~ ., data = Appren,decay=0.1, size = 9)
T2=Sys.time()
T2-T1
summary(RN)
pred=predict(RN,Test, type="class")
pred
Confusion = table(Test$Species,pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err

# selection d'attribut 
library(FSelector)
library(mlbench)
data(Glass)
at=information.gain(Type~.,Glass)
at
subset=cutoff.k.percent(at,0.5)
subset
f<- as.simple.formula(subset,"Type")
f
T1=Sys.time()
RN <- nnet(Type~AL+Mg+K+Ca, Appren, size = 9, decay=0.1)
T2=Sys.time()
T2-T1
pred=predict(RN,Test, type="class")
pred
Confusion = table(Test$Type,pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err

## SONAR
library(mlbench)
data("Sonar")
Sonar
data1=Sonar
n=dim(data1)[1]
index = sample(n, 0.7 * n)
Appren = data1[index, ]
Test = data1[-index, ]
T1=Sys.time()
RN <- nnet(Class~., Appren, size = 16)
T2=Sys.time()
T2-T1
summary(RN)
pred=predict(RN,Test, type="class")
pred
Confusion = table(Test$Class,pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err

# selection d'attribut 
library(FSelector)
library(mlbench)
data(Sonar)
at=information.gain(Class~.,Sonar)
at
subset=cutoff.k.percent(at,0.5)
subset
f<- as.simple.formula(subset,"Class")
f
T1=Sys.time()
RN <- nnet(Class~V11+V12+V9+V10+V13+V48+V49+V51+V47+V45+V52+V21+V44+V4+V36+V28+V46+V5+V54+V20+V35+V1+V2+V3+V6+V7+V8+V14+V15+V16, Appren, size = 16, decay=0.1)
T2=Sys.time()
T2-T1
pred=predict(RN,Test, type="class")
pred
Confusion = table(Test$Class,pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err 

