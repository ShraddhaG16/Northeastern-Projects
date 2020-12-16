#Variables to be used
#Per Unit Cost  C
#Supplier Cost per Order S
#Opportunity Cost for 1 year O
#Carrying Cost per unit H
#Economic Ordering Quantity Q

C <- 77
S <- 225
O <- 19/100
H <- O*C
C
S
O
H

#Traingular Probability limits and function
a<-13000 #Loweer Limit
b<-18000 #Upper Limit
c<-16000 #Peak

ptriangular<-function(x,a,b,c) 
{ KK<-(1/((b-a)*(c-a)))*(x-a)^2 
PP<-1-(1/((b-a)*(b-c)))*(b-x)^2 
prob<-ifelse(x<c,KK,PP) 
return(prob) }

qtriangular<-function(p,a,b,c) 
{ QQ<-a+sqrt((b-a)*(c-a)*p) 
qq<-b-sqrt((b-a)*(b-c)*(1-p)) 
q<-ifelse(p<(c-a)/(b-a),QQ,qq) 
return(q) }

set.seed(1)
rano <-qtriangular(runif(1000),a,b,c)
rano
head(rano)

#Arrays for using 1000 generated values and performing cost equations
total.cost.array <- NULL
minimalcost.array <- NULL

for (i in rano){
  minimalcost = (2*i*S)/ (O*C)
  minimalcost.array = append(minimalcost.array,minimalcost)
  annual.S = (S*i)/minimalcost
  avgcost = i/12
  H.for.i = O*avgcost
  total.cost = H.for.i + avgcost
  total.cost.array = append(total.cost.array,total.cost)
}

total.cost.array
minimalcost.array
head(total.cost.array)
head(minimalcost.array)

summary(total.cost.array)
summary(minimalcost.array)

min(minimalcost.array)

#Frequency Histograms for Total minimal cost,order quantity and annual number of orders
hist(total.cost.array,freq=F,main="Distribution of the Simulation of Total Cost") 
lines(density(total.cost.array),lwd=2,col="grey")

hist(minimalcost.array,freq=F,main="Distribution of the Simulation of Order Quantity") 
lines(density(minimalcost.array),lwd=2,col="orange")

hist(rano,freq=F,main="Distribution of the Simulation of Annual Number of Orders") 
lines(density(rano),lwd=2,col="blue")



