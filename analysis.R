#---------Load Libraries ------------------------------------
library(plyr)
library(tidyverse)
library(visdat)
library(stringr)
library(ggplot2)
library(forecast)
library(imputeTS)
library(tseries)
library(summarytools)

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

vis_miss(df2)
ggplot_na_distribution(df2$freight)

df1 <- df2 %>%
  mutate(freight = ifelse(df2$freight == 0, NA,df2$freight))

df2 <- df1 %>%
  mutate(freight = na_interpolation(df1$freight,option = "stine"))
ggplot_na_imputations(df1$freight, df2$freight)


df2 <- df2 %>%
  arrange(year,week) 

# %>%
#   mutate(year_week = paste(week,"-",year)) %>%
#   mutate(year_week = gsub(" ", "", year_week)) %>%
#   mutate(date= as.Date(paste(df2$year, df2$week, 2, sep="-"), "%Y-%W-%w"))

# Make the plot
# for(i in 1:9){
#   p <- ggplot(df2[((i-1)*52+ 1):((i-1)*52+ 52),], aes(x=date, y=freight)) +
#     geom_line(color="red", size=1.1) +
#     theme_minimal() +
#     xlab("") +
#     ylab("Fright size") +
#     ggtitle("freight units being shipped Westbound (RTM to KGH)", 
#             subtitle = paste("From January", 2011+i, "Until December", 2011+i))
#   print(p)
# }
# 
# for(i in 1:3){
#   p <- ggplot(df2[((i-1)*156+ 1):((i-1)*156+ 156),], aes(x=week, y=freight)) +
#     geom_line(color="red", size=1.1) +
#     theme_minimal() +
#     xlab("") +
#     ylab("Fright size") +
#     ggtitle("freight units being shipped Westbound (RTM to KGH)", 
#             subtitle = paste("From January", (2012+(i-1)*3), "Until December", (2011+(i*3))))
#   print(p)
# }
# 
# p <- ggplot(newdf[,], aes(x=date, y=freight)) +
#   geom_line(color="red", size=1.1) +
#   theme_minimal() +
#   xlab("") +
#   ylab("Fright size") +
#   ggtitle("freight units being shipped Westbound (RTM to KGH)", subtitle = "From December 2002 Until April 2020")
# p

# ggplot(df2[1:156,], aes(x=date, y=freight)) +
#   geom_line(color="red", size=1.1) +
#   theme_minimal()


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


plot(forecast(auto.arima(ts1)))

fit <- auto.arima(ts1)
print(summary(fit))
checkresiduals(fit)


ggseasonplot(ts1)
ggsubseriesplot(ts1)



###--------Test autocorrelation----######
#Testing the stationarity of the data
first_years_df <- df2[1:(7*52),]
last_year_df <- df2[(7*52+1):(9*52),]
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




###------Train-Test Split--------####
train_data <- window(ts1,start = 2012,end = 2018)
plot(train_data)

test_data <- window(ts1,start=2019)
plot(test_data)

dev.off()

mydec <- decompose(ts1,type="additive")

tsadjusted <- ts1-mydec$seasonal
plot(tsadjusted)
plot(mydec$seasonal)

mydec2 <- decompose(ts1,type="multiplicative")

autoplot(mydec2)
tsadjusted2 <- ts1-mydec2$seasonal
plot(tsadjusted2)
plot(mydec2$seasonal)


myarima <- stlf(ts1,method="arima")

plot(myarima)

autoplot(decompose(ts1,type="additive"))
autoplot(decompose(train_data))
autoplot(decompose(test_data))


##-----forecasts----####
meanm <- meanf(train_data,h=52)
naivem <- naive(train_data,h=52)
driftm <- rwf(train_data,h=52,drift=TRUE)

accuracy(meanm,test_data)
accuracy(naivem,test_data)
accuracy(driftm,test_data)

hist(driftm$residuals)

acf(train_data, plot = T)
acf(test_data, plot = T)

##Model selection##

train_data2 <- window(ts1,start = 2018,end = 2019)

etsmodel <- ets(train_data)

plot(train_data2,lwd=3)
lines(etsmodel$fitted,col="red")

plot(forecast(etsmodel,h=52,level=95))

auto.arima(train_data,trace = T,stepwise = F, approximation = F)


best_model <- Arima(train_data,c(1,0,2),c(2,1,0))
autoplot(forecast(best_model,h=12))
#ARIMA(1,0,2)(2,1,0)[52]
autoplot(test_data)

Box.test(best_model$residuals,lag=5,type="Ljung-Box")


adf.test(ts1) # p-value < 0.05 indicates the TS is stationary
kpss.test(ts1)
#plot.ts(ts1)

#as.Date(paste(df$year, df$week, 1, sep="-"), "%Y-%U-%u")


acf(ts1, plot = T, main = "ACF Plot of CPI", xaxt="n")
pacf(ts1, plot = T, main = "ACF Plot of CPI", xaxt="n")


train_data <- window(ts1,start = 2012,end = 2019)


##AR MODEL
model = ARIMA(ts_log, order=c(2, 1, 0))  
results_AR = model.fit(disp=-1)  
plt.plot(ts_log_diff)
plt.plot(results_AR.fittedvalues, color='red')
plt.title('RSS: %.4f'% sum((results_AR.fittedvalues-ts_log_diff)**2))

###MA Model
model = ARIMA(ts_log, order=c(0, 1, 2))  
results_MA = model.fit(disp=-1)  
plt.plot(ts_log_diff)
plt.plot(results_MA.fittedvalues, color='red')
plt.title('RSS: %.4f'% sum((results_MA.fittedvalues-ts_log_diff)**2))

#combined model
model = ARIMA(ts_log, order=(2, 1, 2))  
results_ARIMA = model.fit(disp=-1)  
plt.plot(ts_log_diff)
plt.plot(results_ARIMA.fittedvalues, color='red')
plt.title('RSS: %.4f'% sum((results_ARIMA.fittedvalues-ts_log_diff)**2))


