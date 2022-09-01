
# DATASET IRIS 
library(e1071)

#partitionnement des données
data=iris
n=dim(data)[1]
index = sample(n, 0.7 * n)
Appren = data[index, ]
Test = data[-index, ]

library(rpart)
library(rpart.plot)
data(iris)
#nature des données
lapply(iris,class)
#arbre de decision
T1=Sys.time()
Tree <- rpart(Species~.,data=Appren)
T2=Sys.time()
T2-T1
#par classe
pred=predict(Tree,Test, type="class")
pred
#Performance: table de confusion
tab=table(Test$Species, pred)
tab
err <- 1-sum(diag(tab))/sum(tab)
err

Tree
rpart.plot(Tree)
prp(Tree)
##Par validation croisée l'erreur en fonction du nombre de feuilles
plotcp(Tree)
#obtention de l'arbre simplifié
TreeSimple <- prune(Tree,cp=0.068)
#arbre optimal
prp(TreeSimple,extra=1)
#prevision
predict(TreeSimple,iris[1:10,])
predict(Tree,iris)
#par classe
pred=predict(Tree,Test, type="class")
pred
#####elagage
prune(Tree,0.02)

######

# DATASET GLASS
library(mlbench)

#partitionnement des données
data(Glass)
Glass
data1=Glass
n=dim(data1)[1]
index = sample(n, 0.7 * n)
Appren = data1[index, ]
Test = data1[-index, ]

library(rpart)
library(rpart.plot)
data(Glass)
#nature des données
lapply(Glass,class)
#arbre de decision
T1=Sys.time()
Tree <- rpart(Type~.,data=Appren)
T2=Sys.time()
T2-T1
#par classe
pred=predict(Tree,Test, type="class")
pred
#Performance: table de confusion
tab=table(Test$Type, pred)
tab
err <- 1-sum(diag(tab))/sum(tab)
err
Tree
rpart.plot(Tree)
prp(Tree)
##Par validation croisée l'erreur en fonction du nombre de feuilles
plotcp(Tree)
#obtention de l'arbre simplifié
TreeSimple <- prune(Tree,cp=0.037)
#arbre optimal
prp(TreeSimple,extra=1)

#####

# Random forest 
##IRIS
library( randomForest)
T1=Sys.time()
RF=randomForest(Species~., data=iris)
T2=Sys.time()
T2-T1
RF

#GLASS 
data("Glass")
T1=Sys.time()
RF=randomForest(Type~., data=Glass)
T2=Sys.time()
T2-T1
RF




