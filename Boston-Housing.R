#Boston-housing case study!
#Get the data set from the 'mlbench' package, briefly have a look about the type of data in the columns of the data set.
install.packages("mlbench")
library("mlbench")
data("BostonHousing")
str(BostonHousing)
#We see that there's 14 columns. And we will be going to predict the 'medv' variable which says about the median values of all of the owner occupied homes in 1000 dollars.
#I suggest you to take a look for the definitions of all the columns in tha data set to understand the working model better.
#Look for the variables that correlates the most, do it by having a correlation matrix and find out the relation between the numeric variables.
library(dplyr)
corr_mat <- BostonHousing %>% select(-chas) %>% cor()
#To visualize it better, get the corplot library.
install.packages("corrplot")
library("corrplot")
corrplot(corr_mat, method = "number")
#We see that rm and lstat has a strong correlation with the medv variable, and we also see 2-3 such correlations like tax&rad.
#Lets see the relation between the variable medv and the categorical variable chas.
library(ggplot2)
ggplot(BostonHousing, aes(x=chas, y=medv)) + geom_boxplot()
#We see that the price of the houses near river fence are a little higher.

#Lets split our data with 70% of the total no. of rows of the data set being on the training set.
set.seed(1)
idx <- sample(nrow(BostonHousing), size = round(0.7*nrow(BostonHousing)))
idx
#We see that, there are 354 random values in the idx variable.
train <- BostonHousing[idx,]
test <- BostonHousing[-idx,]

#Create a linear regression model!
lm_model <- lm(medv~., data = BostonHousing)
summary(lm_model)
#Look at the coefficients, and see the variables about how they are related with the 'medv' column.
#For ex. the categorical variable chas has a positive relation with medv. (The price of a house near a river tends to have higher price, nearly 2687 dollars.)

#Let us now predict our model on the test set.
library(broom)
pred_test <- lm_model %>% augment(newdata=test)

#Get the accuracy of our model by calculating RMSE & R squared values.
library(caret)
RMSE(pred = pred_test$.fitted, obs = test$medv)
postResample(pred = pred_test$.fitted, obs = test$medv)["Rsquared"]
#We see that Rmse = 4.958, Rsquared=0.648.
#Lets compare the Rsquared value of the test set with that of the training set.
summary(lm_model)$r.squared

#We see that Rsquared value of training set(0.74) is a little bit higher than of the test set.
#Which means there's some degree of overfitting!
