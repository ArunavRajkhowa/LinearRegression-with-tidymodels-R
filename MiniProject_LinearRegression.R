####importing libraries
library(tidymodels)
library(visdat) 
library(tidyr)
library(car)
library(lubridate)

#### Reading the dataset
setwd("D:\\IITK Data Analytics\\R\\R-problem-solving\\")
file=read.csv("Cycle_Shared (1).csv",stringsAsFactors = F)
glimpse(file)
vis_dat(file)

### Data Preparation and cleaning

## first we seperate day and then drop dteday......month and weekday already given in data
file$date=parse_date_time(file$dteday,'ymd')
#file$year=year(file$date)
#$month=month(file$date)
file$day=day(file$date)
file$date=NULL
file$dteday=NULL
file$year=NULL
file$month=NULL


## dropping instant
file$instant=NULL



## Everything is numeric now and we dont have to worry about creating dummies 
## we can proceed to the fitting part straight away
## first let us split the dataset into train,val and test part

set.seed(2)
s=sample(1:nrow(file),0.8 * nrow(file))
df_trainval=file[s,]
df_test=file[-s,]

s=sample(1:nrow(df_trainval),0.8 * nrow(df_trainval))
df_train=df_trainval[s,]
df_val=df_trainval[-s,]

rm(s,df_trainval)


### Model fitting --- training
fit_train=lm(cnt~.,data=df_train)
summary(fit_train)

sort(vif(fit_train),decreasing = T) 
fit_train=lm(cnt~.-casual - registered-atemp,data=df_train)
sort(vif(fit_train),decreasing = T)  

fit_train=stats::step(fit_train)
summary(fit_train)

#removing day because of larger p-value
formula(fit_train)

fit_train=lm(cnt ~ season + yr + mnth + holiday + weekday + weathersit + temp + 
               windspeed,data=df_train)
summary(fit_train) #all looks ok



### Model Validation starts

fit_val=lm(cnt ~.-casual-registered-atemp,data=df_val)
summary(fit_val)
sort(vif(fit_val),decreasing = T) 

fit_val=stats::step(fit_val)

#removing weathersit 
formula(fit_val)
fit_val=lm(cnt ~ season + yr + weekday + workingday  + temp + 
             hum + windspeed,data=df_val)

summary(fit_val)
summary(fit_train)
#picking consistent variable from the comparison and building model with those on train data
fit_final=lm(cnt ~ season + yr + workingday + weathersit + temp + windspeed,data=df_train)
summary(fit_final)


#Performance on test data
rmse=sqrt(mean((predict(fit_final,df_test)-df_test$cnt)**2))
rmse


plot(df_test$cnt,predict(fit_final,df_test))



plot(fit_final,1) # residual vs fitted values => non-linearity in the data exists or not

plot(fit_final,2) # errors are normal or not

plot(fit_final,3) # variance is constant or not

plot(fit_final,4) # outliers in the data if cook's distance >1
