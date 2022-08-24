# Predictive modeling : linear regression

#importing necessary libraries
library(tidymodels)
library(visdat) 
library(tidyr)
library(car)

#reading out dataset
setwd("D:\\IITK Data Analytics\\R\\LinearRegression-with-tidymodels-R\\")
df_train = read.csv("loan_data_train (1).csv",stringsAsFactors = F)
df_test = read.csv("loan_data_test (1).csv",stringsAsFactors = F)

#studying the dataset
glimpse(df_train)
vis_dat(df_train)
#we see that we need to change columns to numeric type


###Data Preparation starts here

#Interest Rate(custom): remove % and change to numeric (custom fn)
#Debt.to.income ratio (custom): remove % and change to numeric
#FICO(custom): custom function and conv to numeric ()
# Employment length(custom): string cleaning & conv to numeric


#ID(drop) : drop this 
#Amount.Requested(num): convert to numeric
#Amount.Funded.by.investors(drop): drop this

#Loan.Length(duuumy) :  we can use this as numeric feature or as a category. 
                #lets use this as a category and create dummies

#Loan.Purpose (dummy): club low freq together & create dummies 

#state (dummies) :  club low freq together & create dummies 
#Homeownership (dummy) : club low freq together & create dummies 
# Monthly.Income: already numeric, impute missing values
# OPEN CREDIT LINES, revolving credit lines(num):  convert to numeric, impute missing values
#inquiries in last 6 months: do nothing, impute missing values

####

# we need to make 3 custom function

fico_fn=function(x){
  temp=data.frame(fico=x)
  
  temp=temp %>% 
    separate(fico,into = c('f1','f2')) %>% 
    mutate(f1=as.numeric(),
           f2=as.numeric(),
           fico = 0.5*(f1+f2) ) %>% 
    select(-f1,-f2)
  
  return(temp[,'fico'])
  
}

emp_len_fn=function(x){
  x=ifelse(x=="< 1 year",0,x)
  x=gsub("years","",x)
  x=gsub("year","",x)
  x=gsub("+","",x,fixed=T) #without fixed=T, + has some special meaning
  x=as.numeric(x)
  return(x)
  
}


percent_to_numeric_fn=function(x){
  
  x=gsub("%","",x)
  x=as.numeric(x)
  return(x)
}
###




