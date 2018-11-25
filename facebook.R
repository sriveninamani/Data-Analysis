#setting a working directory
setwd("c:/DAM/Project")
getwd()

#remove all the variable list stored
rm(list=ls(all=TRUE))

#read Facebook.csv file into variable fb
fb=read.csv("Facebook.csv")

#Getting the number of rows and columns in fb
dim(fb)

#Getting Variable/Column names
names(fb)

#Renaming the variable names
names(fb)=c("Tot_Likes","Type","Cat","Month","Day","Hour","Paid","Consumers","Tot_Inter")
names(fb)

#Getting the histogram plots for each of the variables
library(ggplot2)
library (reshape2)
ggplot(data = melt(fb), mapping = aes(x = value)) + 
  geom_histogram(bins=10) + facet_wrap(~variable, scales = 'free_x')

#Checking for missing values/null values
sum(is.na(fb))
apply(fb,2, function(x){sum(is.na(x))})

#plotting each regressor with the response variable
#Here Consumers is our response variable
par(mfrow=c(2,4))
plot(fb$Tot_Likes,fb$Consumers)
plot(fb$Cat,fb$Consumers)
plot(fb$Month,fb$Consumers)
plot(fb$Day,fb$Consumers)
plot(fb$Hour,fb$Consumers)
plot(fb$Paid,fb$Consumers)
plot(fb$Tot_Inter,fb$Consumers)

#Removing an oulier where fb$Consumers is maximum as it is influencing the whole plot
fbnew=fb[-c(which(fb$Consumers==max(fb$Consumers))),]
dim(fbnew)

#Now fbnew is our dataset.
#Basic multiple regression taking all regressors into consideration
model1=lm(Consumers~Tot_Likes+Type+Cat+Month+Day+Hour+Paid+Tot_Inter,data=fbnew)
summary(model1)


#Plotting the residual vs fitted values
par(mfrow=c(1,1))
plot(model1$fitted.values,model1$residuals)
abline(h=0,col="grey",lwd=3)

#Plotting the qqplot
qqnorm(model1$residuals)
qqline(model1$residuals)


#Implementing the linear regression for each of the input vs y and ploting the residual vs fitted values 
a=lm(fbnew$Consumers~fbnew$Tot_Likes)
summary(a)
plot(fbnew$Tot_Likes,a$residuals)
abline(h=0,col="grey",lwd=3)

plot(a$fitted.values,a$residuals)
plot(a$fitted.values,rstudent(a))

#Checking the qqplot
qqnorm(a$residuals)
qqline(a$residuals)

#Doing boxcox to know the lambda value
library(car)
require(MASS)
boxcox(a)

#We get the same boxcox plot for each of the regressors if taken individually with response.
#Since lambda =! 1, we need to transform the variables

install.packages("rcompanion")
library(rcompanion)


#Code to get lambda values
x1=transformTukey(fbnew$Tot_Likes,plotit=FALSE)
x2=transformTukey(fbnew$Cat,plotit=FALSE)
x3=transformTukey(fbnew$Month,plotit=FALSE)
x4=transformTukey(fbnew$Day,plotit=FALSE)
x5=transformTukey(fbnew$Hour,plotit=FALSE)
x6=transformTukey(fbnew$Paid,plotit=FALSE)
x7=transformTukey(fbnew$Tot_Inter,plotit=FALSE)

y=transformTukey(fbnew$Consumers,plotit=FALSE)

#Now the transform variables depending on the lambda values 
t1=fbnew$Tot_Likes^7.425
t2=fbnew$Cat^0.775
t3=fbnew$Month^0.925
t4=fbnew$Day^0.975
t5=fbnew$Hour^0.85
t6=fbnew$Paid^0.025
t7=fbnew$Tot_Inter^0.25
ty=fbnew$Consumers^0.15

#Multiple Linear regression with the transformed values
m=lm(ty~t1+t2+t3+t4+t5+t6+t7)
summary(m)

#Checking for Multicollinearity
vif(m)

#Removing t1,t3 due to high VIF, Final regression is as below
d=lm(ty~t2+t4+t5+t6+t7)
summary(d)
boxCox(d)

#Checking the residual plots, qqplots
plot(d$fitted.values,d$residuals)
qqnorm(d$residuals)
qqline(d$residuals)


###Model Selection
##Getting the summary of all regressors 
d=lm(ty~t2)
summary(d)$coef[,1]
summary(d)$adj.r.square
ssres=sum((y-d$fitted.values)^2)
Mres=ssres/(499-1-1)
Mres
  
d=lm(ty~t2+t4)
summary(d)$coef[,1]
summary(d)$adj.r.square
ssres=sum((y-d$fitted.values)^2)
Mres=ssres/(499-1-2)
Mres
  
d=lm(ty~t4+t5)
summary(d)$coef[,1]
summary(d)$adj.r.square
ssres=sum((y-d$fitted.values)^2)
Mres=ssres/(499-1-2)
Mres
  
d=lm(ty~t2+t4+t5)
summary(d)$coef[,1]
summary(d)$adj.r.square
ssres=sum((y-d$fitted.values)^2)
Mres=ssres/(499-1-1)
Mres
  
d=lm(ty~t2+t4+t5+t6)
summary(d)$coef[,1]
summary(d)$adj.r.square
ssres=sum((y-d$fitted.values)^2)
Mres=ssres/(499-1-4)
Mres
  
d=lm(ty~t2+t4+t5+t6+t7)
summary(d)$coef[,1]
summary(d)$adj.r.square
ssres=sum((y-d$fitted.values)^2)
Mres=ssres/(499-1-5)
Mres
  
#From checking the values, we select t2,t4,t5,t7 as our covariates

dnew=lm(ty~t2+t4+t5+t7)
summary(dnew)

#Getting the confidence interval of the model and the predictions
confint(dnew)
head((predict(dnew,fbnew,interval = c("pred"),level = 0.95,type="response"))^(1/0.15))

# Predictions for various other values of covariates

(predict(dnew,level=0.95,list(t2=4^0.775,t4=5^0.975,t5=6^0.85,t7=1000^0.25),interval="pred"))^(1/0.15)
(predict(dnew,level=0.95,list(t2=2^0.775,t4=4^0.975,t5=3^0.85,t7=1500^0.25),interval="pred"))^(1/0.15)


