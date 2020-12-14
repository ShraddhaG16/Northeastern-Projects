#Authors: Keyur, Jayeta, Shraddha

library(readr)
library(dplyr)
library(caret)
library(tidyverse)

# Data import and summary
data<-read.csv(file.choose())
View(data)
summary(data)
attach(data)

 # Handling missing values
missingvalues<-data %>% summarise_each(funs(sum(is.na(.))*100/n()))
missingvalues<- gather(missingvalues,key = "feature",value = "missing_pct")
missingvalues

data$Death.Year[is.na(data$Death.Year)] <- 0
data$Book.of.Death[is.na(data$Book.of.Death)] <- 0
data$Death.Chapter[is.na(data$Death.Chapter)] <- 0

for (i in 1:nrow(data)) {
  if (data[i,3] == '0') {
    data[i,14] <- 1
  }
  else {
    data[i,14] <- 0
  }
}
colnames(data)[14]<-"Status"
resultDF = data[complete.cases(data), ]
View(resultDF)
attach(resultDF)


# model <- lm(Status ~., data = data)
# summary(model)




###### Logistic Model #####
set.seed(1234)

ind <- sample(2, nrow(resultDF), replace = T, prob = c(0.7, 0.3))
train <- resultDF[ind==1,]
test <- resultDF[ind==2,]
ind
model <- glm(Status ~ Gender+Nobility+GoT+CoK+SoS+FfC+DwD,family=binomial(link='logit'),data=train)
summary(model)
model


fitted.results <- predict.glm(model,newdata=subset(test,select=c(7,8,9,10,11,12,13)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results
view(train)

misClasificError <- mean(fitted.results != test$Status)
print(paste('Accuracy',1-misClasificError))





##### New Model using Stepwise Algorithm #####

model.null <- glm(Status ~ Gender+Nobility+GoT+CoK+FfC+DwD, family = "binomial" ,data = train)
summary(model.null)
model.null
library(leaps)
library(MASS)
stepmodel<- stepAIC(model.null, direction = "both", trace=FALSE)

summary(stepmodel)
pred <- predict.glm(model.null,newdata =  test, type = "response")
mean(pred)
pred <- ifelse(pred > 0.7,1,0)
view(pred)
pred <- as.factor(pred)
levels(pred)
library(caret)
test$Status <- as.factor(test$Status)

install.packages("e1071")
library(e1071)
??e1071
confusionMatrix(data = pred, reference = test$Status)
