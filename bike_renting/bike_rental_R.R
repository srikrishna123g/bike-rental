rm(list=ls(all=T))
setwd("/home/srikrishna/Edwisor/Project/bike_renting")
library(corrplot)
library(rpart)
library(MASS)
bike = read.csv('day.csv')

#Missing Value Analysis
missing_val = data.frame(apply(bike,2,function(x){sum(is.na(x))}))
#There is no need for imputation since there are no missing values

bike$season = as.factor(bike$season)
bike$holiday = as.factor(bike$holiday)
bike$workingday = as.factor(bike$workingday)
bike$weathersit = as.factor(bike$weathersit)
bike$yr = as.factor(bike$yr)
bike$mnth = as.factor(bike$mnth)
bike$weekday = as.factor(bike$weekday)

str(bike)

numeric_index = sapply(bike,is.numeric)

numeric_data = bike[,numeric_index]

cnames = colnames(numeric_data)

bike_corr = cor(numeric_data)

corrplot(bike_corr,method = 'color',title = 'Correalation Analysis')
boxplot(bike$temp,bike$atemp,bike$hum,bike$windspeed,
        main = "Multiple boxplots for comparision",
        names = c('Temperature','Atemp','Humidity','Windspeed'),
        col = c("#4287f5","#f58d42",'#f54242','#d7f542'))

hist(bike$temp,col = '#4287f5')
hist(bike$atemp,col = '#f58d42')
hist(bike$hum,col = '#f54242')
hist(bike$windspeed,col = '#d7f542')


train_index = sample(1:nrow(bike), 0.8 * nrow(bike))
train = bike[train_index,]
test = bike[-train_index,]
train = subset(train,select =-c(dteday,casual,temp,registered,mnth,instant,holiday))

model = lm(cnt~ .,data = train)
predictions_DT = predict(model, subset(test,newdata = -c(dteday,casual,temp,registered,mnth,instant,holiday)))
summary(model)

MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}


MAPE(test$cnt, predictions_DT)
library(Metrics)
rmse(test$cnt,predictions_DT)
