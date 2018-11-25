# Read the .csv file to a dataframe
housing <- read.csv("USA_Housing.csv")

dim(housing)
names(housing)

#renaming the variables
names(housing)<-c("income","age","rooms","beds","population","price","address")

#checkng for any missing values
sum(is.na(housing))

#spiltting the dataframe to 2 dataframes, one to train and second to test
h_train<-housing[1:2500,1:6]
h_test<-housing[2501:5000,1:6]


dim(h_train)
names(h_train)

dim(h_test)
names(h_test)

#plotting the histograms of each variable
library(ggplot2)
hist(h_train$age)
hist(h_train$rooms)
hist(h_train$income)
hist(h_train$beds)
hist(h_train$population)


#Basic Scatterplot Matrix
pairs(h_train,pch=20,main="Simple Scatterplot Matrix")

#Linear regression model building
model=lm(price~.,data=h_train)
summary(model)

#since beds is not significant in determining, we remove it from the model
model_new=lm(price~income+age+rooms+population,data=h_train)
summary(model_new)

#plotting the graph of fitted values vs residual for the new model
plot(model_new$fitted.values,model_new$residuals)
abline(h=0,col="blue")


plot(model_new$fitted.values,rstudent(model_new))

#Checking the qqplot
qqnorm(model_new$residuals)
qqline(model_new$residuals)


#Doing boxcox to know the lambda value
library(car)
require(MASS)
boxcox(model_new)

#since lamda=1, there no need for any transformation

#checking for any mutlicollinearity
vif(model_new)

#Confindence interval for the build model
confint(model_new)


# housing_unit_normal=as.data.frame(apply(h_train,2,function(x){(x-mean(x))/sd(x)}))
# model_unit_normal=lm(price~.,data=housing_unit_normal)
# model_unit_normal
# summary(model_unit_normal)


#Adjusted R square is 0.91, so the model build is 91% accurate.
#Predicting the values of price for the test dataframe with 99% confidence and this 91% accurate with the actual results.
head(predict(model_new,h_test,interval=c("pred"),level=0.99, type="response"))

head(h_test)



