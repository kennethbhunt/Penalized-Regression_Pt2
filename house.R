#Data set: housedata.csv
#Find the best predictors for a house price (price) out of the
#following variables: bedrooms, bathrooms, sqft_living, sqft_lot, floors, grade,
#sqft_basement and old. Use all of the following techniques:
#  - best subset selection regression
#- forward and backward stepwise regression
#- ridge regression
#- lasso regression
#- PLS regression
#Discover the model that ensures the best prediction accuracy in the test set.

House <-read.csv('housedata.csv')
str(House)

#Check for missisng values 
sapply(House, function(x) sum(is.na(x)))

#Remove variables
House$id <-NULL
House$date <-NULL
House$waterfront <-NULL
House$view <-NULL
House$condition <-NULL
House$sqft_above <-NULL
House$yr_renovated <-NULL
House$zipcode <-NULL
House$lat<-NULL
House$long <-NULL
House$sqft_living15 <-NULL
House$sqft_lot15 <-NULL

head(House)
#The variables of interest are in place

library(psych)
pairs.panels(House)

library(leaps)

#Run the Best Subset regression from the leaps package
bss_Fit <- regsubsets(price~.,House, nvmax=8)
summ <-summary(bss_Fit)
summ

##To asses the goodness of fit, we use the adjusted R squared
summ$adjr2

## To find the maximum adjusted R squared --11 Is the optimal model
which.max(summ$adjr2)

#Print the coefficients of this model 
coef(bss_Fit, 8)

plot(bss_Fit)

##Forward Stepwise
fsr_Fit <- regsubsets(price~.,House, nvmax=8, method="forward")
summ <-summary(bss_Fit)
summ

##To asses the goodness of fit, we use the adjusted R squared
summ$adjr2

## To find the maximum adjusted R squared --11 Is the optimal model
which.max(summ$adjr2)

#Print the coefficients of this model 
coef(fsr_Fit, 8)

plot(fsr_Fit)


####Backward Stepwise regression
bsr_Fit <- regsubsets(price~.,House, nvmax=8, method="backward")
summ <-summary(bss_Fit)
summ

##To asses the goodness of fit, we use the adjusted R squared
summ$adjr2

## To find the maximum adjusted R squared --11 Is the optimal model
which.max(summ$adjr2)

#Print the coefficients of this model 
coef(bsr_Fit, 8)

plot(bsr_Fit)

####Ridge Regression
library(glmnet)
## A matix and vector needs to be created

x <- model.matrix(price~., House)

y <- House$price # Vector of the dependent variables

##Lambda values are power of 10
w <- seq(10,-3, length= 100)
lvalues <-10^w
lvalues

#fit the ridge regression 
rr_fit <- glmnet(x,y, alpha=0, lambda = lvalues)
rr_fit$lambda[40]
coef(rr_fit)[,40]

predict(rr_fit, s=1200, type="coefficients")

plot(rr_fit)

n <-sample(21613, 10500)

#Fit model on the training set 
#Perform 10 fold cross validation 
library(glmnet)

cv_fit <- cv.glmnet(x[n,], y[n], alpha=0, nfolds=10)
## No need to set lambda in the function. 

optLambda <- cv_fit$lambda.min
optLambda

###Predict Y values for the test set
### Using the optimum lambda
### & compute MSE

pred <- predict(cv_fit, s=optLambda, newx=x[-n,])
head(pred)

mse_test <- mean((pred-y[-n])^2)
mse_test

###Lasso regression

x <- model.matrix(quality~., wine)

y <- wine$quality # Vector of the dependent variables

##Lambda values are power of 10
w <- seq(10,-3, length= 100)
lvalues <-10^w
lvalues

#fit the ridge regression 
lr_fit <- glmnet(x,y, alpha=1, lambda = lvalues)#alpha set to one for lasso
lr_fit$lambda[3]
coef(lr_fit)[,3]

## a model with low lamda 
lr_fit$lambda[99]
coef(lr_fit)[,99]

## Intermediate lambda
lr_fit$lambda[70]
coef(lr_fit)[,70]

## Coefficients different from zero 
coef(lr_fit)[,70][coef(lr_fit)[,70]!=0]

#### Validating lasso regression 

n <-sample(21613, 10500)
## perform k-fold CV

cv_fit <- cv.glmnet(x[n,], y[n], alpha=1, nfolds=10)
optLambda <- cv_fit$lambda.min
optLambda

###Predict Y values for the test set
### Using the optimum lambda
### & compute MSE

pred <- predict(cv_fit, s=optLambda, newx=x[-n,])
head(pred)

mse_test <- mean((pred-y[-n])^2)
mse_test

## Create vector of lambdas and fit lasso again 
w <- seq(10,-3, length=100)
lvalues <- 10^w
lr_fit <- glmnet(x,y, alpha=1, lambda = lvalues)

##print coefficients of the best model 
predict(lr_fit, s=optLambda, type="coefficients")

##### Partial Least Squares Regression

library(pls)
#Fit model with k-fold cv
plsr_fit <-plsr(price~., data =House, scale=T, validation="CV")
#The variable are standardized 
summary(plsr_fit)
MSE <- 227806^2
MSE

#predictor coefficients for the model with 8 components 

coef(plsr_fit, 8)

## Model with optimal numer of components 
plsr_fit2 <- plsr(price~., data=House, scale=T, ncomp=8)
summary(plsr_fit2)

coef(plsr_fit2)

## Validation PLS regression approach to obtain test MSE
n <-sample(21613, 10500)
house_train <- House[n,]
house_test <-House[-n,]

##Fit PLS model on the training set 
plsr_fit <- plsr(price~., data=house_train, scale=T, ncomp=8)
## Compute the MSE on the test set 

pred <- predict(plsr_fit, house_test, ncomp=8)
head(pred)

mse <- mean((pred-house_test$price)^2)
mse


plot(plsr_fit2)






