library(FactoMineR)
data<-iris 
data$Species <- NULL
acp= PCA(data)
library(FSelector)
data<-iris
result <- cfs(Species ~ ., data)
result
f <- as.simple.formula(result, "Species")
f
data(iris)
weights <- information.gain(Species~., data)
print(weights)
subset <- cutoff.k(weights, 2)
f <- as.simple.formula(subset, "Species")
print(f)
library(mlbench)
data(Vehicle)
data<- Vehicle
str(data)
data$Class <- NULL
acp= PCA(data)
data<-Vehicle
result <- cfs(Class ~ ., data)
result
f <- as.simple.formula(result, "Class")
f
data(Vehicle)
weights <- information.gain(Class~., data)
print(weights)