setwd("C:\\Users\\Owner\\Documents\\Masters\\Data Mining Applications")
trainHC <- read.csv("HC_TRAIN.csv")
testHC <- read.csv("HC_TEST.csv")
trainHC
summary(trainHC)
summary(testHC)


all_data = rbind(trainHC[,-12],testHC)
?rbind
all_data$hypertension = as.factor(all_data$hypertension)
all_data$heart_disease = as.factor(all_data$heart_disease)

Missing_Data_Smoking = round(100*sum(all_data$smoking_status =="")/nrow(all_data),2)
Missing_Data_Smoking
Missing_Data_BMI = round(100*sum(is.na(all_data$bmi))/nrow(all_data),2)
Missing_Data_BMI

mean_bmi = mean(all_data$bmi,na.rm = T)
mean_bmi

library(dplyr)
install.packages("caret")
all_data = all_data %>% mutate(bmi=ifelse(is.na(bmi),mean_bmi,bmi))
all_data

train_data_With_Smoke_Data = all_data[ all_data$id %in% trainHC$id & all_data$smoking_status != "", ]
train_data_Without_Smoke_Data = all_data[ all_data$id %in% trainHC$id & all_data$smoking_status == "", ]

train_data_With_Smoke_Data = merge(train_data_With_Smoke_Data,trainHC[,c(1,12)],by = c("id"))
train_data_Without_Smoke_Data = merge(train_data_Without_Smoke_Data,trainHC[,c(1,12)],by = c("id"))

test_data_With_Smoke_Data = all_data[ all_data$id %in% testHC$id & all_data$smoking_status != "",]
test_data_Without_Smoke_Data = all_data[ all_data$id %in% testHC$id & all_data$smoking_status == "",]

table(train_data_With_Smoke_Data$stroke)
table(train_data_Without_Smoke_Data$stroke)

summary(train_data_With_Smoke_Data)
summary(train_data_Without_Smoke_Data)

write.csv(all_data,"C:/Users/Owner/Documents/Masters/Data Mining Applications/alldata.csv")
