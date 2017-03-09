
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
auto <- read.csv(file = "Data/automobile.csv", header=TRUE, na.strings="?")
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


##### While developing the model, iteratively analyse variables for
#     - Normality of distribution
#     - Extreme values 
#     - Multiple collinearity
#     - Homoscedasticity (even distribution of residuals)
#     - p-value of coefficients and R2/F-statistic of the model


### Check for non-linearity (visually) and transform vars
pairs.panels(auto.sel, col="red")
auto.sel$Price <- log10(auto.sel$Price)
auto.sel$Horsepower <- log10(auto.sel$Horsepower)
pairs.panels(auto.sel, col="red")

### Develop a linear model
#   The model will be built using the training sample of the data
#   The model will be validated using the validation sample of the data

# Split data into training and validation samples
# We will use (train.size)% for training and (100-train.size)% for validation
set.seed(2017)
train.size <- 0.8 
train.index <- sample.int(length(auto.sel$Price), round(length(auto.sel$Price) * train.size))
train.sample <- auto.sel[train.index,]
valid.sample <- auto.sel[-train.index,]


### Multiple regression model utilises a simple formula:
#    Price = B0 + B1 x Horsepower + B2 x Curb.weight + B3 x City.mpg + ...
#
# We will perform additional tests on the trainig data

# We will use a stepwise selection of variables by backwards elimination
# We will consider all candidate variables and eliminate one at the time
# As we have quite a few variables will not consider variable interaction at this stage, i.e.
#   Price ~ Horsepower*Curb.weight*City.mpg*Peak.rpm*Num.of.doors

### Fit the model (1)
fit <- lm(Price ~ Horsepower+Curb.weight+City.mpg+Peak.rpm+Num.of.doors, data=train.sample)
summary(fit) # R2=81%

# First check for non-linearity properly (from "car"), if good go further
# This can only be done after the model was created
crPlots(fit)

# Eliminate extreme values
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
    %in% c("74", "91", "167")),]

### Refit the model (2)
fit <- lm(Price ~ Horsepower+Curb.weight+City.mpg+Peak.rpm+Num.of.doors, data=train.sample)
summary(fit) # R2=84.7%

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
   %in% c("22", "114", "197")),]     

### Refit the model (3)
fit <- lm(Price ~ Horsepower+Curb.weight+City.mpg+Peak.rpm+Num.of.doors, data=train.sample)
summary(fit) # R2=87.6%

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
   %in% c("35", "44", "125")),]     

### Refit the model (4)
fit <- lm(Price ~ Horsepower+Curb.weight+City.mpg+Peak.rpm+Num.of.doors, data=train.sample)

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)                        # We should continue checking Cook!

# Check for multi-collinearity with Variance Inflation Factor
# Correlated: none VIF=1, moderately 1<VIF<5, ** highly 5<VIF<10, ...
summary(fit) # R2=88%
vif(fit)

### Refit the model (5) - drop Horsepower due to multiple collinearity
fit <- lm(Price ~ Curb.weight+Peak.rpm+Num.of.doors, data=train.sample)
summary(fit) # R2=87.7% but tough it was inflated
vif(fit)

### Refit the model (6) - drop Num.of.doors due to p-value
fit <- lm(Price ~ Curb.weight+Peak.rpm, data=train.sample)
summary(fit) # R2=87.7%
vif(fit)

### VIF and p-values say it good, so no need to do anything else

##### Now evaluate the final linear model
#     Find all predicted values for both a training set and a validation set
train.sample$Pred.Price <- predict(fit, 
    newdata = subset(train.sample, select=c(Price, Peak.rpm, Curb.weight)))
valid.sample$Pred.Price <- predict(fit, 
    newdata = subset(valid.sample, select=c(Price, Peak.rpm, Curb.weight)))

# The theoretical model performance is defined here as R-Squared
summary(fit)

# Check how good is the model on the training set - correlation^2, RME and MAE
train.corr <- round(cor(train.sample$Pred.Price, train.sample$Price), 2)
train.RMSE <- round(sqrt(mean((10 ^ train.sample$Pred.Price - 10 ^ train.sample$Price)^2)))
train.MAE <- round(mean(abs(10 ^ train.sample$Pred.Price - 10 ^ train.sample$Price)))
c(train.corr^2, train.RMSE, train.MAE)
# With all prep is: 0.8836 2670.0000 1759.0000 / As above
# Do nothing was:   0.7225 3997.0000 2676.0000 / See previous lesson

# Check how good is the model on the validation set - correlation^2, RME and MAE
valid.corr <- round(cor(valid.sample$Pred.Price, valid.sample$Price), 2)
valid.RMSE <- round(sqrt(mean((10 ^ valid.sample$Pred.Price - 10 ^ valid.sample$Price)^2)))
valid.MAE <- round(mean(abs(10 ^ valid.sample$Pred.Price - 10 ^ valid.sample$Price)))
c(valid.corr^2, valid.RMSE, valid.MAE)
# With all prep is: 0.7396 5723.0000 3334.0000 / As above
# Do nothing was:   0.6889 4927.0000 3208.0000 / See previous lesson

# Small data set - Cross-validation should have been used!
# These results and the model should now be interpreted
