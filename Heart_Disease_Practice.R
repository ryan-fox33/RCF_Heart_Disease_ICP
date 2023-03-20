library(tidyverse)
dataset=read_csv('heart.data.csv')

#Exploring
view(dataset)
glimpse(dataset)
length(dataset)
names(dataset)
summary(dataset)

#Checking for null values
colSums(is.na(dataset))

bike_median=median(dataset$biking, na.rm = TRUE)
dataset$biking=ifelse(is.na(dataset$biking),
                   bike_median,
                   dataset$biking)

colSums(is.na(dataset))

smoking_median=median(dataset$smoking, na.rm = TRUE)
dataset$smoking=ifelse(is.na(dataset$smoking),
                      smoking_median,
                      dataset$smoking)

hd_median=median(dataset$heart.disease, na.rm = TRUE)
dataset$heart.disease=ifelse(is.na(dataset$heart.disease),
                      hd_median,
                      dataset$heart.disease)

colSums(is.na(dataset))

#Splitting the data
library(caTools)
set.seed(100)
split=sample.split(dataset$heart.disease, SplitRatio = 0.8) #training 80% testing 20%
Training_Set=subset(dataset,split==TRUE)
Test_set=subset(dataset,split==FALSE)

#Finding MLR
MLR=lm(formula = heart.disease~ ., 
       data=Training_Set)
summary(MLR)

#Finding MSE
summ=summary(MLR)
MSE=(mean(summ$residuals^2))
paste("Mean Squared Error: ", MSE)

#Predicting Heart Disease
y_pred=predict(MLR, newdata = Test_set)
data=data.frame(Test_set$heart.disease,y_pred)
head(data)

#Validation
new=read.csv("Heart_validation.csv")
head(new)
new_x=new[c(1,2)]

new_x
data.frame(new[c(3)], predict(MLR,newdata = new_x))

#My linear Equation is: 
#y=14.98 + (-0.200)biking + (0.179)smoking 
#The p value is much smaller than 0.05 meaning that biking and smoking are statistically significant 
#My r squared value is 0.978 meaning that biking and smoking have a strong positive correlation with heart disease


