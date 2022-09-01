library(arules)
a_list=list(
  c("po","la","pa","fr"),
  c("la","pa","fr"),
  c("pa","fr"),
  c("po","pa","fr")
          )
### doner un nom aux transactions 
names(a_list)<- paste("Tr",c(1:4),sep="")
a_list

#### Former le tab de transactions 
trans1<- as(a_list,"transactions")
trans1 

## analyser les transactions 
summary(trans1)
image(trans1)
itemFrequencyPlot(trans1)

### extraction des regles d'association 
rules<-apriori(trans1,parameter=list(support=0.33,confidence=0.7))
summary(rules)
inspect(rules)
