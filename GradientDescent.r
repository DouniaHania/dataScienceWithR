#Importation des données
data=reg1
data
#Affectation de la 1ere colonne du tableau à X
X=data[,1]
X

#Affectation de la 2eme colonne du tableau à Y
Y=data[,2]
Y
#Initialisation de p0 et p1
p1=0
p1
p0=0
p0
a=0.01
m=3
j=0
cpt=0


som<-function(k,X,Y)
{
  res=0
  for(i in 1:k){
  res=res+((p0+p1*X[i,])-Y[i,])

}
return(res)
}


som1<-function(k,X,Y)
{
  res1=0
  for(i in 1:k){
    res1=res1+((p0+p1*X[i,])-Y[i,])*X[i,]
  }
  return(res1)
}

som2<-function(k,X,Y)
{
  res=0
  z=0
  for(i in 1:k){
    
    z=(p0+p1*X[i,])-Y[i,]
    res=res+(z*z)
  }
  return(res)
}
#1ere itération de l'algorithme
p0= p0-a*(1/m)*som(3,X,Y)
print('Valeur de p0')
p0
p1= p1-a*(1/m)*som1(3,X,Y)
print('Valeur de p1')
p1
j=(1/(2*m))*som2(3,X,Y)
print('Erreur : ')
j

while(cpt<100)
{  
  p0= p0-a*(1/m)*som(3,X,Y)
  p0
  p1= p1-a*(1/m)*som1(3,X,Y)
  p1
  j=(1/(2*m))*som2(3,X,Y)
  cpt=cpt+1
  print(cpt)
  print('Erreur :')
  print(j)
}


str(cars)
datac=cars
carsX=datac[1]
carsX
carsY=datac[2]
carsY
carsY[1,]
p0=0
p1=0
a1=0.005
m2=50
cpt=0

p0= p0-a1*(1/m2)*som(50,carsX,carsY)
p0
p1= p1-a1*(1/m2)*som1(50,carsX,carsY)
p1
j=(1/(2*m2))*som2(50,carsX,carsY)
j

while(j>20)
{  
  p0= p0-a1*(1/m2)*som(50,carsX,carsY)
  p0
  p1= p1-a1*(1/m2)*som1(50,carsX,carsY)
  p1
  j=(1/(2*m2))*som2(50,carsX,carsY)
  cpt=cpt+1
  print(cpt)
  print('Erreur :')
  print(j)
}

