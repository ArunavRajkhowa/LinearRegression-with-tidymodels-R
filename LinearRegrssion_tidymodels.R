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
    mutate(f1=as.numeric(f1),
           f2=as.numeric(f2),
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

#now we will use functions recipe(),prep() & bake() of library tidymodels 
#this will make our data preprocessing code more......tidy

#run the code stepbystep to understand
dp_pipe = recipe(Interest.Rate ~ .,data=df_train) %>%  
  # update role of 'to-drop' columns first
  #update_role(ID,Amount.Funded.By.Investors,new_role = "drop_vars") %>% 
  # update role of to convert to numeric columns
  update_role(Amount.Requested,
              Open.CREDIT.Lines,
              Revolving.CREDIT.Balance,new_role="to_numeric") %>% 
  update_role(Home.Ownership,State,Loan.Length,Loan.Purpose,new_role="to_dummies") %>% 
  
  
  #now we drop 'drop_vars'
  step_rm(has_role('drop_vars')) %>% 
  
  #applying the custom functions
  step_mutate_at(FICO.Range,fn=fico_fn) %>% 
  step_mutate_at(Employment.Length,fn=emp_len_fn) %>% 
  step_mutate_at(Debt.To.Income.Ratio,fn=percent_to_numeric_fn) %>% 
  step_mutate_at(Interest.Rate,fn=percent_to_numeric_fn,skip=TRUE) %>% #only for ld_train
  step_mutate_at(has_role("to_numeric"),fn=as.numeric) %>% 
  
  # creating n-1 dummies for categoricl variables
  #???????????????????????????????????????????????????????????????????
  step_unknown(has_role("to_dummies"),new_level="__missing__") %>% 
  #cleaning & grouping them into __other__ . choice of threshold is subjective
  step_other(has_role("to_dummies"),threshold =0.02,other="__other__") %>% 
  #create dummies
  step_dummy(has_role("to_dummies")) %>% #creates n-1 dummies
  
  #imputing values
  step_impute_median(all_numeric(),-all_outcomes())


dp_pipe=prep(dp_pipe)

train=bake(dp_pipe,new_data = NULL)  
test=bake(dp_pipe,new_data=df_test)


vis_dat(train) # lines show the NAs..
vis_dat(df_train)
### data preprocessing ends here

