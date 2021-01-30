#---------Load Libraries ------------------------------------
library(plyr)
library(tidyverse)
library(naniar)
library(VIM)
library(DMwR)
library(caret)
library(PerformanceAnalytics)
library(visdat)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)



#--------Load Data-------------------------------------------
main_data <- read.csv("c:/excercise/excercise.csv", 
                    quote = "",na.strings = c("", "NA"))
colnames(main_data)[2] <- "freight"


df <- main_data %>%
  separate(Year.week, c("year", "week"), "/") %>%
  mutate(week = as.numeric(week)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(week %in% c(1:52)) %>%
  mutate(freight = replace_na(freight, median(freight,na.rm = TRUE)))

#check and impute NAs after done

for(i in df$year){
  for(j in 1:52){
    if(!(j %in% df[which(df$year==i),c("week")])){
      print(paste("year" , i , "week" , j))
      break
    }
  }
}

miss<-data.frame(c(2017,2018),c(13,21),c(0,0))
names(miss)<-names(df)
newdf <- rbind(df, miss)


newdf <- newdf %>%
  arrange(year,week) %>%
  mutate(year_week = paste(week,"-",year)) %>%
  mutate(year_week = gsub(" ", "", year_week)) %>%
  mutate(date= as.Date(paste(newdf$year, newdf$week, 1, sep="-"), "%Y-%W-%u")) %>%
  mutate(freight = ifelse(freight==0, median(freight,na.rm = TRUE),freight))

#check for NA impute

sum(is.na(df))




# Make the plot
for(i in 1:9){
  p <- ggplot(newdf[((i-1)*52+ 1):((i-1)*52+ 51),], aes(x=date, y=freight)) +
    geom_line(color="red", size=1.1) +
    theme_minimal() +
    xlab("") +
    ylab("Fright size") +
    ggtitle("freight units being shipped Westbound (RTM to KGH)", 
            subtitle = paste("From January", 2011+i, "Until December", 2011+i))
  print(p)
}
p <- ggplot(newdf[,], aes(x=date, y=freight)) +
  geom_line(color="red", size=1.1) +
  theme_minimal() +
  xlab("") +
  ylab("Consumer Price Index") +
  ggtitle("Indonesia's Consumer Price Index", subtitle = "From December 2002 Until April 2020")
p


# Get the statistical summary
# Returns data frame and sort based on the CPI
newdf %>%
  arrange(desc(freight))


ts1 <- ts(newdf$freight, frequency = 52, start = c(2012, 1))
ts1

library(tseries)
adf.test(ts1) # p-value < 0.05 indicates the TS is stationary
kpss.test(ts1)
#plot.ts(ts1)

#as.Date(paste(df$year, df$week, 1, sep="-"), "%Y-%U-%u")


acf(ts1, plot = T, main = "ACF Plot of CPI", xaxt="n")
pacf(ts1, plot = T, main = "ACF Plot of CPI", xaxt="n")



