#load libraries
library(tidyverse)

#set working directory (adjust this for your own computer)
setwd("/Users/pbunyasr/Desktop/Eastern_DS/DTSC560/Module_4")

#read dataset into R
warnerdf <- read.csv("warner_music.csv")
View(warnerdf)

#create a time series plot showing quarterly evenue 
ggplot(data = warnerdf, mapping = aes(x = Quarter, y = Revenue)) +
  geom_line () +
  geom_point() +
  labs(title = "Warner Music Quarterly Revenue, 
       2015 to 2021", x = "Quarter", y = "Revenue")

#Add a column of consecutive numbers corresponding with each quarter
warnerdf$Trend <- 1:nrow(warnerdf) 

#Use simple linear regression analysis to create a regression equation for 
#forecasting
sbreg<-lm(Revenue ~ Trend, data = warnerdf)
summary(sbreg)

#Create functions for the accuracy measures (we've done this before)
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

#Create a vector of predicted values generated from the 
#regression above (sbreg)
sb_pred = predict(sbreg)

#Run the accuracy measure functions with vector of actual values and vector
#of predicted values as inputs
mae (warnerdf$Revenue, sb_pred)
mse (warnerdf$Revenue, sb_pred)
rmse (warnerdf$Revenue, sb_pred)
mape (warnerdf$Revenue, sb_pred)

#Modeling trend and seasonality in a time series using regression

warnerdf$Q1 <- ifelse(grepl("Q1",warnerdf$Quarter), 1, 0)
warnerdf$Q2 <- ifelse(grepl("Q2",warnerdf$Quarter), 1, 0)
warnerdf$Q3 <- ifelse(grepl("Q3",warnerdf$Quarter), 1, 0)
warnerdf$Q4 <- ifelse(grepl("Q4",warnerdf$Quarter), 1, 0)

#Use multiple regression with the time and quarters variables to generate 
#a regression equation for forecasting
sbreg2<-lm(Revenue ~ Trend + Q2 + Q3 + Q4, data = warnerdf)
summary(sbreg2)

#Create a vector of predicted values generated from the multiple 
#regression above
sb_pred2 = predict(sbreg2)
sb_pred2

#calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae(warnerdf$Revenue, sb_pred2)
mse(warnerdf$Revenue, sb_pred2)
rmse(warnerdf$Revenue, sb_pred2)
mape(warnerdf$Revenue, sb_pred2)

#Create an object with the time periods to use for the prediction
new <- data.frame(Trend = c(26, 27, 28, 29), Q2 = c(0,1,0,0), Q3 = c(0,0,1,0), 
                  Q4 = c(0,0,0,1)) 
predict(sbreg2, newdata = new)

