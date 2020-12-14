library(tidyverse)
library(caret)
library(glmnet)

healthcare<-read.csv(file.choose())
healthcare
view(healthcare)
str(healthcare)

healthcare$smoking_status<-as.character(healthcare$smoking_status)
healthcare$gender<-as.character(healthcare$gender)
healthcare$work_type<-as.character(healthcare$work_type)
healthcare$stroke<-as.factor(healthcare$stroke)
str(healthcare)

summary(healthcare)


#Taking mean of BMI
mean_bmi<-mean(healthcare$bmi,na.rm = T)
mean_bmi

#Filling blank by mean of BMI
healthcare<-healthcare %>%
  mutate(bmi=ifelse(is.na(bmi),mean_bmi,bmi))
healthcare

#fill blank values by NA

healthcare$smoking_status<-healthcare$smoking_status<-ifelse((healthcare$smoking_status==""),"NA",healthcare$smoking_status)

#if age us <18,change smoking status to never smoked
healthcare<-healthcare %>%
  mutate(smoking_status=ifelse(age<18 & smoking_status == "NA","never smoked",smoking_status))
healthcare

view(healthcare)
str(healthcare)

id<-healthcare[healthcare$id==47350,]
id

---------------------------------------------------------------------------------------------------------------
  #if age<18 replace work type as children
  
  healthcare<-healthcare %>%
  mutate(work_type=ifelse(age<18 & work_type == "Never_worked","children",work_type))
healthcare

id1<-healthcare[healthcare$id==16556,]
id1

view(healthcare)

#checking nevwer smoked status
filter_healthcare<-healthcare %>%
  filter(smoking_status=="never smoked" )

#checking smoking status NA
healthcare %>% 
  count(smoking_status=="NA")

#checking never smoked 
healthcare %>%
  count(smoking_status=="never smoked" )

healthcare %>%
  count(work_type=="children" )

sum(healthcare$stroke=='1')



#Week 3 Code- Start
attach(healthcare)

dmy<-dummyVars("~ smoking_status + work_type + Residence_type",data = healthcare)
trsf<- data.frame(predict(dmy,newdata = healthcare))
View(trsf)
length(stroke)
length(trsf)
healthcare<-cbind(healthcare,trsf)
library("leaps")
library("MASS")
attach(healthcare)
train_healthcare<-healthcare[1:39000,]
test_healthcare<-healthcare[39001:43400,]
fullModel<- glm(stroke ~ .,family = binomial,data=train_healthcare)
summary(fullModel)
stepModel <-stepAIC(fullModel,direction="both",trace=FALSE)
summary(stepModel)
resultStepwise<- predict.glm(fullModel,newdata = test_healthcare,type = 'response')
resultStepwise <- ifelse(resultStepwise > 0.02,1,0)
resultStepwise<-as.factor(resultStepwise)
library(caret)
confusionMatrix(data=resultStepwise, reference=test_healthcare$stroke)


#Week 3 Code- End

#Prediction of stroke

train_healthcare<-healthcare[1:39000,]
test_healthcare<-healthcare[39001:43400,]

str(train_healthcare)
str(test_healthcare)


model<-glm(stroke ~ gender+age+hypertension+heart_disease+ever_married,family=binomial ,data=train_healthcare)

summary(model)


result <- predict.glm(model,newdata=test_healthcare,type='response')
result <- ifelse(result > 0.01,1,0)
result<-as.factor(result)

sum(test_healthcare$stroke=='1')
sum(train_healthcare$stroke=='1')

library(caret)
confusionMatrix(data=result, reference=test_healthcare$stroke)

typeof(test_healthcare$stroke)

attach(healthcare)
table(result, test_healthcare$stroke)

--------------------------------------------------------------------------------------------------------------------------
#prediction of hypertension status
train_healthcare1<-healthcare[1:39000,]
test_healthcare1<-healthcare[39001:43400,]

str(train_healthcare1)
str(test_healthcare1)

test_healthcare1$hypertension<-as.factor(test_healthcare1$hypertension)

model1<-glm(hypertension ~.,family=binomial ,data=train_healthcare1)

summary(model1)


result1 <- predict.glm(model1,newdata=test_healthcare1,type='response')
result1 <- ifelse(result1 > 0.1,1,0)
result1<-as.factor(result1)

str(result1)

sum(test_healthcare1$hypertension=='1')
sum(train_healthcare1$hypertension=='1')

library(caret)
confusionMatrix(data=result1, reference=test_healthcare1$hypertension)

typeof(test_healthcare$stroke)

attach(healthcare)
table(result1, test_healthcare1$hypertension)

------------------------------------------------------------------------------------------------------------------
#prediction of heart disease status
train_healthcare2<-healthcare[1:39000,]
test_healthcare2<-healthcare[39001:43400,]

str(train_healthcare2)
str(test_healthcare2)

test_healthcare2$heart_disease<-as.factor(test_healthcare1$heart_disease)

model2<-glm(heart_disease ~.,family=binomial ,data=train_healthcare2)

summary(model2)


result2 <- predict.glm(model2,newdata=test_healthcare2,type='response')
result2 <- ifelse(result2 > 0.1,1,0)
result2<-as.factor(result2)

str(result2)

sum(test_healthcare2$heart_disease=='1')
sum(train_healthcare2$heart_disease=='1')

library(caret)
confusionMatrix(data=result2, reference=test_healthcare2$heart_disease)

typeof(test_healthcare$stroke)

attach(healthcare)
table(result2, test_healthcare2$heart_disease)

------------------------------------------------------------------------------------------
#Prediction of stroke with smoking status parameters
train_healthcare3<-healthcare[1:39000,]
test_healthcare3<-healthcare[39001:43400,]

str(train_healthcare3)
str(test_healthcare3)


model3<-glm(stroke ~ smoking_status,family=binomial ,data=train_healthcare3)

summary(model3)


result3 <- predict.glm(model3,newdata=test_healthcare3,type='response')
result3 <- ifelse(result3 > 0.01,1,0)
result3<-as.factor(result3)

sum(test_healthcare3$stroke=='1')
sum(train_healthcare3$stroke=='1')

library(caret)
confusionMatrix(data=result3, reference=test_healthcare3$stroke)

typeof(test_healthcare$stroke3)

attach(healthcare)
table(result3, test_healthcare3$stroke)

------------------------------------------------------------------------------------------------
#Prediction of stroke with all parameters
train_healthcare4<-healthcare[1:31000,]
test_healthcare4<-healthcare[31001:43400,]

str(train_healthcare4)
str(test_healthcare4)

model4<-glm(stroke ~. ,family=binomial ,data=train_healthcare4)

summary(model4)

result4 <- predict.glm(model4,newdata=test_healthcare4,type='response')
result4 <- ifelse(result4 > 0.02,1,0)
result4<-as.factor(result4)

sum(test_healthcare4$stroke=='1')
sum(test_healthcare4$stroke=='0')

library(caret)
confusionMatrix(data=result4, reference=test_healthcare4$stroke)

typeof(test_healthcare$stroke3)

attach(healthcare)
table(result3, test_healthcare3$stroke)

view(healthcare)
