#load libraries
library(tidyverse)

#set working directory (adjust this for your own computer)
setwd("/Users/pbunyasr/Desktop/Eastern_DS/DTSC560/Module_4")

#read dataset into R
amazondf <- read.csv("amazon_web_services.csv")
View(amazondf)

#create a time series plot showing quarterly revenue 
ggplot(data = amazondf, mapping = aes(x = Quarter, y = Revenue)) +
  geom_line (group=1) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Amazon Revenue by Quarter, 2014 to 2021", 
       x = "Quarter", y = "Revenue")

#Add a column of consecutive numbers corresponding with each quarter
amazondf$Trend <- 1:nrow(amazondf) 

#Use simple linear regression analysis to create a regression equation for 
#forecasting
amazonreg<-lm(Revenue ~ Trend, data = amazondf)
summary(amazonreg)

amazon_pred = predict(amazonreg)

mae<-function(actual,pred){
  mae <- mean(abs(actual-pred), na.rm=TRUE)
  return (mae)
}

mse<-function(actual,pred){
  mse <- mean((actual-pred)^2, na.rm=TRUE)
  return (mse)
}

rmse<-function(actual,pred){
  rmse <- sqrt(mean((actual-pred)^2, na.rm=TRUE))
  return (rmse)
}  

mape<-function(actual,pred){
  mape <- mean(abs((actual - pred)/actual), na.rm=TRUE)*100
  return (mape)
}

mae (amazondf$Revenue, amazon_pred)
mse (amazondf$Revenue, amazon_pred)
rmse (amazondf$Revenue, amazon_pred)
mape (amazondf$Revenue, amazon_pred)

amazondf$Q1 <- ifelse(grepl("Q1",amazondf$Quarter), 1, 0)
amazondf$Q2 <- ifelse(grepl("Q2",amazondf$Quarter), 1, 0)
amazondf$Q3 <- ifelse(grepl("Q3",amazondf$Quarter), 1, 0)
amazondf$Q4 <- ifelse(grepl("Q4",amazondf$Quarter), 1, 0)

#-------Seasonality Regression----------



amazonreg_2<-lm(Revenue ~ Trend + Q2 + Q3 + Q4, data = amazondf)
summary(amazonreg_2)

amazon_pred_2 = predict(amazonreg_2)

mae (amazondf$Revenue, amazon_pred_2)
mse (amazondf$Revenue, amazon_pred_2)
rmse (amazondf$Revenue, amazon_pred_2)
mape (amazondf$Revenue, amazon_pred_2)

new_amz <- data.frame(Trend = c(33,34,35,36), Q2 = c(0,1,0,0), Q3 = c(0,0,1,0) , Q4 = c(0,0,0,1))
predict(amazonreg_2, newdata = new_amz)


#read dataset into R
amazon_newdf <- read.csv("amazon_web_services.csv")
View(amazon_newdf)

#Add a column of consecutive numbers corresponding with each quarter
amazon_newdf$Time <- 1:nrow(amazon_newdf) 

#Use simple linear regression analysis to create a regression equation for 
#forecasting
amzreg<-lm(Revenue ~ Time, data = amazon_newdf)
summary(amzreg)

#Create a vector of predicted values generated from the 
#regression above
amz_pred = predict(amzreg)

#calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae (amazon_newdf$Revenue, amz_pred)
mse (amazon_newdf$Revenue, amz_pred)
rmse (amazon_newdf$Revenue, amz_pred)
mape (amazon_newdf$Revenue, amz_pred)

#Create a new variable that squares the Time variable
amazon_newdf$Time2 <- amazon_newdf$Time^2

#Use a quadratic regression model to create a regression equation for 
#forecasting
amzregquad<-lm(Revenue ~ Time + Time2, data = amazon_newdf)
summary(amzregquad)

amz_pred2 = predict(amzregquad)

#calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae (amazon_newdf$Revenue, amz_pred2)
mse (amazon_newdf$Revenue, amz_pred2)
rmse (amazon_newdf$Revenue, amz_pred2)
mape (amazon_newdf$Revenue, amz_pred2)

new <- data.frame(Time = c(33, 34, 35, 36), Time2 = c(1089, 1156, 1225, 1296))
predict(amzregquad, newdata = new)



