

# 
# 
# Simple linear regression
# Data: http://www-bcf.usc.edu/~gareth/ISL/data.html

# Plot points, regression line and residuals
# using the standard built-in plot functions
# Refer back to M1T1 demo for details of plots

# Dependencies for textxy function
# Install it if working on a home computer
# install.packages("calibrate", dependencies = TRUE)
library(calibrate)

# Read the Advertising.csv data set
# Source: An Introduction to Statistical Learning: with Applications in R
# By Gareth James, Daniela Witten, Trevor Hastie, Robert Tibshirani
# It shows the impact of exposure to different media on Sales

# As a test let's read a version of this file but with some dirty records
# Get summary, view the data and check meta data for the variable type
ads.dirty <- read.csv(file = "/Users/AdamLiu/Google Drive/Machine_Learning/R_Exercises/advertising-dirty.csv", header = TRUE)
summary(ads.dirty)
View(ads.dirty)

# Regression hates missing and non-numeric values, so clean up the file
ads <- read.csv(file = "/Users/AdamLiu/Google Drive/Machine_Learning/R_Exercises/advertising.csv", header = TRUE)
summary(ads)
View(ads)

##### Create a linear regression model and investigate it

# Create a linear model
fit <- lm(Sales ~ TV, data=ads)
fit
summary(fit)

# Predict Sales in response to investment in TV commercials
TV.invested <- c(160, 305)
Sales.predicted <- predict(fit, interval = "none", newdata=data.frame(TV=TV.invested))
print(data.frame(TV.invested, Sales.predicted))

# Plot the scatterplot to see if it is suitable for regression
# Note: pch is a shape, cex is its scaling, for more info see:
#   http://www.statmethods.net/advgraphs/parameters.html
plot(Sales ~ TV, data=ads, 
     xlim=c(min(ads$TV)-10, max(ads$TV)+15), 
     ylim=c(0, max(ads$Sales)+10),
     col=rgb(0,0,1,0.5), pch=20, cex=1.5,
     xlab = "TV ($K)", ylab = "Sales (Units)",
     main = "Influence of TV Ads on Sales",
     sub = "Regression with Residuals and Prediction")

# Now plot it gradually, to add regression line, segments, points and more
plot(Sales ~ TV, data=ads, type="n",
     xlim=c(min(ads$TV)-10, max(ads$TV)+15), 
     ylim=c(0, max(ads$Sales)+10),
     xlab = "TV ($K)", ylab = "Sales (Units)",
     main = "Influence of TV Ads on Sales",
     sub = "Regression with Residuals and Prediction")

# Calculate and plot residual line segments 
# between residuals and their predicted values
ads$Pred.Sales <- predict(fit)
segments(ads$TV, ads$Sales, ads$TV, ads$Pred.Sales, col="gray")
abline(fit, lwd=1.5, col="orange")
points(Sales ~ TV, data=ads, col=rgb(0,0,1,0.5), pch=20, cex=0.5)

# Add two new points with their predicted values
# To make some fancy (x,y) labels we use "paste" function
new.TV <- c(160, 305)
new.Sales <- predict(fit, interval = "none", newdata=data.frame(TV=new.TV))
labels.new <- paste("(",signif(new.TV, 5),", ",
                    signif(new.Sales, 5),")",sep="")
segments(new.TV, new.Sales, new.TV, new.Sales+9, col="black")
textxy(new.TV-35, new.Sales+10, labels.new, cex=0.7, col="black")
points(new.TV, new.Sales, cex=1.5, pch=18, col="red")

##### This is only a prediction within a certain range!
# So, check the confidence intervals of these predictions
# Note RSE=3.259, R-Sq=0.61 (unexplained variab from mean 0.39)
# Intercept and Slope and confidence that they are correct
conf.interv <- predict(fit, interval = "confidence", newdata=data.frame(TV=new.TV))
TV.vs.Sales <- data.frame(cbind(TV=new.TV, conf.interv))
TV.vs.Sales

# Add a rectangle around the points to signify the confidence interval
rect(xleft=(TV.vs.Sales$TV-3), 
     ybottom=TV.vs.Sales$lwr, 
     xright=(TV.vs.Sales$TV+3), 
     ytop=TV.vs.Sales$upr, 
     col="green", density = -1, border = NA)

# We could have drawn it in a different order, otherwise...
points(new.TV, new.Sales, cex=1.5, pch=18, col="red")


# To present this graphically, predicted points may include
# mini whiskers under/on top of each of the plotted points

##### We can plot is all simpler, see these three examples
# Note that ggplot2 is a large package with lots of dependencies
# install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# Plot all data points and a regression line 
ggplot(ads, aes(x=TV, y=Sales)) + 
  ggtitle("TV Ads vs. Sales") +
  geom_point() +
  stat_smooth(method="lm", color="blue")

# Plot data points, regression line and Loess fitting line
ggplot(ads, aes(x=Newspaper, y=Sales)) + 
  ggtitle("Newspaper Ads vs. Sales") +
  geom_point() + 
  stat_smooth(method="loess", color="red")

# Plot data points, regression line and both fitting lines
ggplot(ads, aes(x=TV, y=Sales)) + 
  ggtitle("TV Ads vs. Sales") +
  geom_point() + 
  stat_smooth(method="lm", color="blue") +
  stat_smooth(method="loess", color="red")

# Plot data points, regression line and predicted points
ggplot(ads, aes(x=TV, y=Sales)) + 
  ggtitle("TV Advertising vs. Sales") +
  labs(x="TV ($K)", y="Sales (Units)") +
  geom_point() + 
  stat_smooth(method="lm", fullrange=TRUE) +
  xlim(min(ads$TV)-20, max(ads$TV)+20) +
  geom_point(data=TV.vs.Sales, 
             aes(x=TV, y=fit), 
             color="red", size=5, alpha=0.5, shape=18) +
  geom_errorbar(data=TV.vs.Sales, 
             aes(x=TV, y=fit, ymin=lwr, ymax=upr),
             width=5)

# Finished
