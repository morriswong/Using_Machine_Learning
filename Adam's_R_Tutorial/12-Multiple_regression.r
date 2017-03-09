
# 
# 
# Multiple linear regression
# Data: http://www-bcf.usc.edu/~gareth/ISL/data.html
# Source:
# An Introduction to Statistical Learning: with Applications in R
# By Gareth James, Daniela Witten, Trevor Hastie, Robert Tibshirani

# Define a working directory
# Locate your project directory and use the forward / slash
setwd("/Users/AdamLiu/Google Drive/Machine_Learning/R_Exercises")

# Read the data set, check for missing values and non-numerics
ads <- read.csv(file = "advertising.csv", header = T)
summary(ads)
View(ads)

##### Make visual inspection of data

# We have three variables to influence sales
# One way is to analyse them separately
# Problem is that the vars may be interacting
# Another way to use multiple regression and coplots

# Let us delete a column that is irrelevant
ads <- subset(ads, select = -Id)

# Let's look at the data
pairs(ads, col="blue")

# A more comprehensive look at the same data
# Install it if working on a home computer
# install.packages("psych", dependencies = TRUE)
library("psych")
pairs.panels(ads, col="red")

# Questions to ask:
# Is at least one of the predictors useful in predicting the response?
# Do all the predictors help to explain or only a subset?
# How well does the model fit the data?
# Given predictor values, what are response values and their accuracy?

# Plot with Radio variable as conditioning var
# We plot with Radio in different ranges and multiple plots
summary(ads)
op <- par(mfrow=c(2,3))
plot(Sales ~ TV, data=ads[ads$Radio >= 18 & ads$Radio < 34,], col="blue")
plot(Sales ~ TV, data=ads[ads$Radio >= 27 & ads$Radio < 42,], col="blue")
plot(Sales ~ TV, data=ads[ads$Radio > 42,], col="blue")
plot(Sales ~ TV, data=ads[ads$Radio < 12,], col="blue")
plot(Sales ~ TV, data=ads[ads$Radio >= 5 & ads$Radio < 18,], col="blue")
plot(Sales ~ TV, data=ads[ads$Radio >= 12 & ads$Radio < 27,], col="blue")
par(op)

# Or we could use a co-plot with a similar effect but better
coplot(Sales ~ TV | Radio, pch=19, data=ads, col="blue")

# What if we try to support TV with Radio and Newspapers?

# Add regression lines to the plot
panel.lm = function(x, y, ...) {
  tmp <- lm(y ~ x, na.action = na.omit)
    abline(tmp, col="red")
    points(x, y, ...) 
}

# Now plot with regression lines
coplot(Sales ~ TV | Newspaper, pch=19, data=ads, col="blue",
         panel = panel.lm)
coplot(Sales ~ TV | Radio, pch=19, data=ads, col="blue",
       panel = panel.lm)
coplot(Sales ~ TV | Radio + Newspaper, pch=19, data=ads, col="blue",
       panel = panel.lm)

# Plot with Lowess smoother panel - locally-weighted ploynomial regression
coplot(Sales ~ Radio | Newspaper, pch=19, panel=panel.smooth, data=ads, 
       col="blue", col.smooth="red")

##### Develop a model

# So far we have intuition about data but no predictive model,
# such as: Sales = B0 + B1 x TV + B2 x Radio + B3 x Newspaper + Error
#
# Let us create the model iteratively, either by selecting variables
# Foward (we add vars), Backwards (remove vars) or Mixed (add and remove)

# Let us look at the vars correlations
cor(ads)

# We will use a Backwards methods (cannot be always used)
# We will consider TV, Radio, Newspaper
model <- lm(Sales ~ TV + Radio + Newspaper, data=ads)
summary(model)

# We can also consider TV, Radio, Newspaper and their crossings
model <- lm(Sales ~ TV * Radio * Newspaper, data=ads)
summary(model)

# Remove the var with the largest P value
model <- lm(Sales ~ TV * Radio, data=ads)
summary(model)
op <- par(mfrow=c(2,2))
plot(model)

# Observe that R-Sq almost did not change and all Ps are good

# Now try predicting new sales value depending on TV and Radio ads
# let us see if radio could help at a range of TV exposure and how

# Define level of reliance on radio media 
summary(ads$Radio)
RadioMin = 0
Radio1Q = 9.975
RadioMean = 23.260
Radio3Q = 36.52
RadioMax = 49.600
TVLowRange = seq(from = 0, to = 300, by = 0.5)

# Predict sales at different radio levels
PredRadioMin = predict(model,
    newdata = data.frame(Radio=rep(RadioMin, length(TVLowRange)), 
                         TV=TVLowRange))
PredRadio1Q = predict(model, 
    newdata = data.frame(Radio=rep(Radio1Q, length(TVLowRange)),
                         TV=TVLowRange))
PredRadioMean = predict(model, 
    newdata = data.frame(Radio=rep(RadioMean, length(TVLowRange)), 
                         TV=TVLowRange))
PredRadio3Q = predict(model, 
    newdata = data.frame(Radio=rep(Radio3Q, length(TVLowRange)), 
                         TV=TVLowRange))
PredRadioMax = predict(model, 
    newdata = data.frame(Radio=rep(RadioMax, length(TVLowRange)), 
                         TV=TVLowRange))

# Plot it all and wonder
plot(Sales ~ TV, data=ads, xlim=c(min(TVLowRange), max(TVLowRange)), 
     xlab = "TV (+ Radio Levels)", ylab = "Sales",
     main = "Influence of TV and Radio Ads on Sales",
     sub = "Multiple regression",
     col="gray")
lines(TVLowRange, PredRadioMin, col="blue")
lines(TVLowRange, PredRadio1Q, col="green")
lines(TVLowRange, PredRadioMean, col="yellow")
lines(TVLowRange, PredRadio3Q, col="darkorange")
lines(TVLowRange, PredRadioMax, col="red")

# Surprised?

# Consider glm and gls, especially for logistic prediction

##### Let's try some really cool multiple regression plots
# Ref: http://www.sthda.com/english/wiki/amazing-interactive-3d-scatter-plots-r-software-and-data-visualization

# Install it if working on a home computer
# install.packages("rgl", dependencies = TRUE)
library(rgl)

# Install it if working on a home computer
# install.packages("car", dependencies = TRUE)
library("car")

# Let us check influence of pairs of factors on sales
scatter3d(x=ads$Radio, z=ads$Newspaper, y=ads$Sales)
scatter3d(x=ads$TV, z=ads$Newspaper, y=ads$Sales)
scatter3d(x=ads$TV, z=ads$Radio, y=ads$Sales)

# Can we differentiate regression depending on the volume of sales
Sales.class <- ifelse(ads$Sales < 9, "Low", ifelse(ads$Sales < 18, "Medium", "High"))
scatter3d(Sales ~ TV * Radio, data=ads, groups=factor(Sales.class))

# Let us if there is any impact of newspapers and TV, grouped by radio ads on sales
Radio.class <- ifelse(ads$Radio < Radio1Q, "Low", ifelse(ads$Radio < Radio3Q, "Medium", "High"))
scatter3d(Sales ~ TV * Newspaper, data=ads, groups=factor(Radio.class))
scatter3d(Sales ~ TV * Newspaper, data=ads, groups=factor(Radio.class), fit="smooth")
scatter3d(Sales ~ TV * Newspaper, data=ads, groups=factor(Radio.class), fit="smooth", surface=FALSE, ellipsoid=TRUE, grid=FALSE)

# Save the last plot to a JPG file
rgl.snapshot(filename = "last-plot.jpg")
