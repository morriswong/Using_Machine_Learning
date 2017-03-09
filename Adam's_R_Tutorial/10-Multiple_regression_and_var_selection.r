
# Title: Multiple linear regression with evaluation and visualisation
#  
#
# Data: http://archive.ics.uci.edu/ml/datasets/Automobile
# Data consist of the following entities: 
# (a) the specification of an auto in terms of various characteristics, 
# (b) its assigned insurance risk rating, i.e.
#     degree to which the auto is more risky than its price indicates
# (c) its normalized losses in use as compared to other cars, i.e.
#     the relative average loss payment per insured vehicle year.
# We will be predicting price based on car specifications.

##### Activate libraries

# install.packages("Hmisc", dependencies = TRUE)
# install.packages("psych", dependencies = TRUE)
# install.packages("car", dependencies = TRUE)
library(Hmisc)
library(psych)
library(car)

##### Read and clean the data and select variables

# Set a working directory
setwd("/Users/AdamLiu/Google Drive/Machine_Learning/R_Exercises")

# Read the Automobile.csv data set and eliminate missing values
auto <- read.csv(file = "automobile.csv", header=TRUE, na.strings="?")
summary(auto)
auto$Price <- as.numeric(impute(auto$Price, mean))
auto$Normalized.losses <- as.numeric(impute(auto$Normalized.losses, mean))
auto$Num.of.doors <- as.numeric(impute(auto$Num.of.doors, median))
auto$Horsepower <- as.numeric(impute(auto$Horsepower, mean))
auto$Peak.rpm <- as.numeric(impute(auto$Peak.rpm, mean))
auto$Bore <- as.numeric(impute(auto$Bore, mean))
auto$Stroke <- as.numeric(impute(auto$Stroke, mean))
summary(auto)

# Select a subset of numeric variables for regression modelling
auto.sel <- subset(auto, select = c(Horsepower, City.mpg, Peak.rpm, Curb.weight, Num.of.doors, Price))


##### Analyse variables for
#     - Normality of distribution
#     - Multiple collinearity
#     - Extreme values 
#     - Homoscedasticity (even distribution of residuals)
##### All such problems should have been fixed here

# Here we'll only make a brief visual inspection of vars
pairs.panels(auto.sel, col="red")


##### Develop a linear model
#     The model will be built using the training sample of the data
#     The model will be validated using the validation sample of the data

# Split data into training and validation samples
# We will use (train.size)% for training and (100-train.size)% for validation
set.seed(2017)
train.size <- 0.8 
train.index <- sample.int(length(auto.sel$Price), round(length(auto.sel$Price) * train.size))
train.sample <- auto.sel[train.index,]
valid.sample <- auto.sel[-train.index,]


### Multiple regression model utilises a simple formula:
#    Price = B0 + B1 x Horsepower + B2 x Curb.weight + B3 x City.mpg
#
# We will perform additional tests on the trainig data

# We will use a stepwise selection of variables by backwards elimination
# We will consider all candidate variables and eliminate one at the time

fit <- lm(Price ~ Horsepower+City.mpg+Peak.rpm+Curb.weight+Num.of.doors, data=train.sample)
summary(fit) # R2=73%

fit <- lm(Price ~ Horsepower+City.mpg+Peak.rpm+Curb.weight, data=train.sample)
summary(fit) # R2=73%

fit <- lm(Price ~ Horsepower+City.mpg+Curb.weight, data=train.sample)
summary(fit) # R2=73%

fit <- lm(Price ~ Horsepower+Curb.weight, data=train.sample)
summary(fit) # R2=72.7%
op <- par(mfraw = c(2, 2))
plot(fit)

# Observe that R-Sq almost did not change and all Ps are good


##### Now evaluate the final linear model
#     Find all predicted values for both a training set and a validation set
train.sample$Pred.Price <- predict(fit, 
    newdata = subset(train.sample, select=c(Price, Horsepower, Curb.weight)))
valid.sample$Pred.Price <- predict(fit, 
    newdata = subset(valid.sample, select=c(Price, Horsepower, Curb.weight)))

# The theoretical model performance is defined here as R-Squared
summary(fit)

# Check how good is the model on the training set - correlation^2, RME and MAE
train.corr <- round(cor(train.sample$Pred.Price, train.sample$Price), 2)
train.RMSE <- round(sqrt(mean((train.sample$Pred.Price - train.sample$Price)^2)))
train.MAE <- round(mean(abs(train.sample$Pred.Price - train.sample$Price)))
c(train.corr^2, train.RMSE, train.MAE)
# 0.7225 3997.0000 2676.0000

# Check how good is the model on the validation set - correlation^2, RME and MAE
valid.corr <- round(cor(valid.sample$Pred.Price, valid.sample$Price), 2)
valid.RMSE <- round(sqrt(mean((valid.sample$Pred.Price - valid.sample$Price)^2)))
valid.MAE <- round(mean(abs(valid.sample$Pred.Price - valid.sample$Price)))
c(valid.corr^2, valid.RMSE, valid.MAE)
# 0.6889 4927.0000 3208.0000

# This results could be improved when eliminating extreme values and normalising vars
