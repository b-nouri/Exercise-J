#---------Load Libraries ------------------------------------
library(plyr)
library(tidyverse)
library(visdat)
library(stringr)
library(ggplot2)
library(forecast)
library(imputeTS)
library(tseries)

#--------Load Data-------------------------------------------
main_data <- read.csv("c:/excercise/excercise.csv", 
                    quote = "",na.strings = c("", "NA"))
colnames(main_data)[2] <- "freight"



####-----------Preprocessing Data-----------###
df <- main_data %>%
  separate(Year.week, c("year", "week"), "/") %>%
  mutate(week = as.numeric(week)) %>%
  mutate(year = as.numeric(year))

summary(df)

df <- df %>%
  filter(week %in% c(1:52))

#searching for the missing weeks
for(i in df$year){
  for(j in 1:52){
    if(!(j %in% df[which(df$year==i),c("week")])){
      print(paste("year" , i , "week" , j))
      break
    }
  }
}

miss<-data.frame(c(2017,2018),c(13,21),c(NA,NA))
names(miss)<-names(df)
df2 <- rbind(df, miss)

df2 <- df2 %>%
  arrange(year,week)

##Visualization of missing values
vis_miss(df2)
ggplot_na_distribution(df2$freight)

df1 <- df2 %>%
  mutate(freight = ifelse(df2$freight == 0, NA,df2$freight))

#interpolation of missing values
df2 <- df1 %>%
  mutate(freight = na_interpolation(df1$freight,option = "stine"))
ggplot_na_imputations(df1$freight, df2$freight)


df2 <- df2 %>%
  arrange(year,week) 


####-----------EDA--------------------###########
ggplot(df2,aes(x=as.factor(year),y=freight,col=as.factor(year))) +
  geom_boxplot()

ggplot(df2,aes(x=week,y=freight),col=as.factor(year),col = factor(year)) +
  geom_line(aes(color=as.factor(year)),size=1.1) +
  theme_minimal() + 
  facet_wrap(~as.factor(year))

ggplot(df2,aes(x=as.factor(week),y=freight,col=as.factor(week))) +
  geom_boxplot()

boxplot(df2$freight)


###----------Time series analysis----##########
ts1 <- ts(df2$freight, frequency = 52, start = c(2012, 1))
ts1

monthplot(ts1,labels = 1:52, xlab = "weeks")
seasonplot(ts1,season.labels = TRUE)

autoplot(decompose(ts1))
plot(forecast(auto.arima(ts1)))

fit <- auto.arima(ts1)
print(summary(fit))
checkresiduals(fit)


ggseasonplot(ts1)
ggsubseriesplot(ts1)



###--------Test autocorrelation----######
#Testing the stationarity of the data
first_years_df <- df2[1:(7*52),]
last_years_df <- df2[(7*52+1):(9*52),]
year_eight_df <- df2[(7*52+1):(8*52),]
year_nine_df <- df2[(8*52+1):(9*52),]

#Augmented Dickey-Fuller Test
adf.test(ts1) 
adf.test(first_years_df$freight)
adf.test(last_year_df$freight) #!!!!!!!last two years are non stationary!!!!!!!!!


#Autocorrelation test
autoplot(acf(last_year_df$freight,plot=TRUE))+ labs(title="Correlogram of frieght data")
autoplot(acf(first_years_df$freight,plot=TRUE))+ labs(title="Correlogram of frieght data")
autoplot(acf(year_eight_df$freight,plot=TRUE))+ labs(title="Correlogram of frieght data")
autoplot(acf(year_nine_df$freight,plot=TRUE))+ labs(title="Correlogram of frieght data")


#partial Autocorrelation test
autoplot(pacf(first_years_df$freight,plot=TRUE))+ labs(title="Correlogram of frieght data")
autoplot(pacf(last_year_df$freight,plot=TRUE))+ labs(title="Correlogram of frieght data")
autoplot(pacf(year_eight_df$freight,plot=TRUE))+ labs(title="Correlogram of frieght data")
autoplot(pacf(year_nine_df$freight,plot=TRUE))+ labs(title="Correlogram of frieght data")


###------Premilnary Model selection--------####
train_data <- window(ts1,start =2012,2015)
plot(train_data)


test_data <- window(ts1,start=c(2015,1),end=c(2015,14))

harmonics <- fourier(train_data, K = 13)

# Fit regression model with ARIMA errors
fit <- auto.arima(train_data, xreg = harmonics, seasonal = FALSE)

# Forecasts next 3 years
newharmonics <- fourier(train_data, K = 13, h = length(test_data))
fc <- forecast(fit, xreg = newharmonics)

accuracy(fc,test_data)

xreg = harmonics
gastbats <- tbats(train_data)
fc2 <- forecast(gastbats, h= length(test_data))
best_train_2 <- auto.arima(train_data,trace = T,stepwise = F, approximation = F)
#best_model <- Arima(train_data,c(0,1,2),c(0,1,0))

armia_forecast <- forecast(best_train_2,h = length(test_data), level = 0.89)
#best_forecast <- forecast(best_model,h = length(test_data), level = 0.89)


accuracy(fc2,test_data)
accuracy(armia_forecast,test_data)
#accuracy(best_forecast,test_data)


#Best model1: ARIMA(0,1,2)(0,1,0)[52]  # for 2012-2014                

#Best model2: ARIMA(1,1,2)(0,1,0)[52]  #for 2013-2015

#Best model3: ARIMA(1,0,1)(1,1,0)[52]   #for 2014-2016 

#Best model: ARIMA(3,1,2)(0,1,0)[52]       #2016-2018             




###########Cross Validation 1##########
k <- 2 # minimum data length for fitting a model
n <- 9
mae1 <- mae2 <- mae3 <-mae4 <-mae5 <- mae6 <- matrix(NA,8,14)
st <- tsp(ts1)[1]

for(i in 1:7)
{
  xshort <- window(ts1, end=2013 +i)
  xnext <- window(ts1, start=c((st +1 + i),1), end=c((st+ i+1 ),15))
  fit1 <- tslm(xshort~ trend + season, lambda=0) ##h=5
  fcast1 <- forecast(fit1, h=14)
  

fit2 <- Arima(xshort,c(0,1,2),c(0,1,0))
fcast2 <- forecast(fit2, h=14)

fit3 <- Arima(xshort,c(1,1,2),c(0,1,0))
fcast3 <- forecast(fit3, h=14)

fit4 <- Arima(xshort,c(3,1,2),c(0,1,0))
fcast4 <- forecast(fit4, h=14)

fit5 <- tbats(xshort) ##h = 4,7
fcast5 <- forecast(fit5, h=14)

fit6 <- auto.arima(xshort)
fcast6 <- forecast(fit6, h=14)
  
  mae1[i,1:14] <- abs(fcast1[['mean']]-xnext)
  mae2[i,1:14] <- abs(fcast2[['mean']]-xnext)
  mae3[i,1:14] <- abs(fcast3[['mean']]-xnext)
  mae4[i,1:14] <- abs(fcast4[['mean']]-xnext)
  mae5[i,1:14] <- abs(fcast5[['mean']]-xnext)
  mae6[i,1:14] <- abs(fcast6[['mean']]-xnext)
}



mae1_dt <- as.data.frame(colMeans(mae1,na.rm=TRUE))
names(mae1_dt)[1] <- "MAE"
ggplot(mae1_dt, aes(x=1:14,y=MAE)) +
  geom_line()

MAE1 <- as.data.frame(na.remove(rowMeans(mae1)))
MAE2 <- as.data.frame(na.remove(rowMeans(mae2)))
MAE3 <- as.data.frame(na.remove(rowMeans(mae3)))
MAE4 <- as.data.frame(na.remove(rowMeans(mae4)))
MAE5 <- as.data.frame(na.remove(rowMeans(mae5)))
MAE6 <- as.data.frame(na.remove(rowMeans(mae6)))

names(MAE1)[1] <- "MAE"
names(MAE2)[1] <- "MAE"
names(MAE3)[1] <- "MAE"
names(MAE4)[1] <- "MAE"
names(MAE5)[1] <- "MAE"
names(MAE6)[1] <- "MAE"



###-----Forecasting based on the best selected models----####

k <- 2 # minimum data length for fitting a model
n <- 9
mae1 <- mae2 <- mae3 <-mae4 <-mae5 <- mae6 <- matrix(NA,8,14)
st <- tsp(ts1)[1]

  x_test <- window(ts1, start=2019)
  
  X_train1 <- window(ts1, start=2014,end=2019)
  fit1 <- tslm(X_train1~ trend + season, lambda=0) ##h=5
  fcast1 <- forecast(fit1, h=14)

  
  X_train2 <- window(ts1, start=2017,end=2019)
  fit2 <- Arima(X_train2,c(3,1,2),c(0,1,0))
  fcast2 <- forecast(fit2, h=14)
  
  X_train3 <- window(ts1, start=2015,end=2019)
  fit3 <- tbats(X_train3) ##h = 4
  fcast3 <- forecast(fit3, h=14)
  
  X_train4 <- window(ts1, start=2012,end=2019)
  fit4 <- tbats(X_train4) ##h 7
  fcast4 <- forecast(fit4, h=14)
  
  X_train5 <- window(ts1, start=2017,end=2019)
  fit5 <- auto.arima(X_train5)
  fcast5 <- forecast(fit5, h=14)
  
  X_train6 <- window(ts1, start=2016,end=2019)
  fit6 <- auto.arima(X_train6)
  fcast6 <- forecast(fit6, h=14)

  
  
  mae_11 <- mean(abs(fcast1[['mean']]-x_test))
  mae_21 <- mean(abs(fcast2[['mean']]-x_test))
  mae_31 <- mean(abs(fcast3[['mean']]-x_test))
  mae_41 <- mean(abs(fcast4[['mean']]-x_test))
  mae_51 <- mean(abs(fcast5[['mean']]-x_test))
  mae_61 <- mean(abs(fcast6[['mean']]-x_test))

  mae_1
  mae_2
  mae_3
  mae_4
  mae_5
  mae_6

  mae_11
  mae_21
  mae_31
  mae_41
  mae_51
  mae_61


  
  X_train_best <- window(ts1, start=2014)
  fit_best <- tbats(X_train_best) ##h 7
  fcast_best <- forecast(fit_best, h=14)
  
  
  forescasted <- as.data.frame(fcast_best)
  names(forescasted)[2] <- "pred"


  
    autoplot(fcast_best) + 
    ggtitle("Freight Forecast Westbound (RTM to KGH) in Q1 of 2021 ") + 
    ylab("Freight") +
    coord_cartesian(xlim = c(2020.5, 2021.5)) +
    theme_minimal()



