
# Title: R Stats
# 
#
# Data: http://docs.health.vic.gov.au/docs/doc/2013-LGA-profiles-data
# Source: Australian Department of Health & Human Services
#
# Data: https://en.wikipedia.org/wiki/Local_government_areas_of_Victoria
# Source: Wikipedia (LGA geo-locations reconstructed)
# 
# Using Naive Bayes model for prediction
# We will try to predict people's wellbeing
# based on several socio-economic factors



# Load Naive Bayes (and more) libraries
# Install it if working on a home computer
# install.packages("e1071", dependencies = TRUE)
library("e1071")

# - Read all of the LGA profiles into a data frame
# - Get sample vectors (as follows) and explore them

lga.profile <- read.csv("/Users/AdamLiu/Google Drive/Machine_Learning/R_Exercises/Vic 2013 LGA Profiles NoPc.csv")

# Select candidate variables (based on my personal hunch)
lga <- lga.profile$LGA
crime <- lga.profile$SocEngag.11
soc <- lga.profile$SocEngag.21
sitting <- lga.profile$Health.25
safe <- lga.profile$SocEngag.13
education <- lga.profile$Edu.10
highblood <- lga.profile$Medical.5
poorteeth <- lga.profile$Medical.31
avoidabledeath <- lga.profile$Injury.9
doctors <- lga.profile$HealthServices.4
wellbeing <- lga.profile$WellBeing.15

# View the variables and explore the class
plot(density(wellbeing))
plot(density(crime))
plot(density(soc))
plot(density(sitting))
plot(density(safe))
plot(density(highblood))
plot(density(poorteeth))
plot(density(doctors))

plot(wellbeing)
hist(wellbeing, breaks=20)
summary(wellbeing)

# Based on the distribution of "wellbeing" 
#   define "wellcl" to be used in Naive-Bayes classification
#   with breaks: 0-0.47 Low, 0.47-0.55 Medium, >0.55 High

wellcl <- ifelse(wellbeing < 0.47, "Low", ifelse(wellbeing < 0.55, "Medium", "High"))


##### Create a data set to investigate

# Use all variables, except for the class/factor variable
wb <- data.frame(education, soc, safe, sitting, crime,
                 highblood, poorteeth, avoidabledeath, doctors,
                 wellbeing)

##### Explore data to check for variable dependencies

# Check if there are any visible correlations
library(psych)
pairs.panels(wb, 
  main = "Wellbeing Correlations (g=Low,b=Med,r=High)",
  pch = 21, bg = c("red", "green3", "blue")[unclass(factor(wellcl))])



##### Let us check predictor association, which is to be removed
#     If we deal with nominal / discrete vars use Chi-Sq
#     If we deal with continuous vars use correlation function and plots

# Get a correlation matrix
corrm <- cor(wb)
round(corrm, digits = 3)

# Print a simple correlation matrix
# Install this package if working on your computer
# install.packages("corrplot", dependencies = TRUE)
library(corrplot)
corrplot(corrm, method = "circle")

# Create a correlation corgram
# Install this package if working on your computer
# install.packages("corrgram", dependencies = TRUE)
library(corrgram)
corrgram(wb, order=TRUE,
         main="LGA Wellbeing",
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)
corrgram(wb, order=TRUE,
         main="LGA Wellbeing",
         panel=panel.ellipse,
         text.panel=panel.txt, diag.panel=panel.minmax)

# Note strong correlation between pairs (>0.6):
#   education++sitting, education--highblood
#   soc++sitting 
#   
# Note medium level correlation between pairs (>0.4):
#   education+*soc, education-avoidabledeath, education+*doctors
#   *soc-safe, *soc+*crime, *soc+wellbeing
#   sitting-highblood, sitting-avoidabledeath
#   highblood+avoidabledeath, highblood-*doctors
#
# Poor correlation with target (<0.1):
#   crime+wellbeing, doctors+wellbeing
#
# Remove: crime, doctors, education, soc

# Not very promissing, note that:
# - Wellbeing was only used for analysis
# - Crime does not seem to be in any way correlated with wellbeing
# - Sitting is correlated with both education and socialising
# Let us remove the "offending" variables one at the time

##### Prepare data for use with Naive-Bayes method

# Redefine wb data frame by removing highly correlated vars
wb <- data.frame(safe, sitting, highblood, poorteeth,
                 avoidabledeath, wellcl)
View(wb)

# Understand variable selection from a data frame
subset(wb, select = -wellcl)[1:5,]
subset(wb, select = c(safe, highblood, wellcl))[1:5,]
subset(wb, select = c(-safe, -highblood))[1:5,]

# Split data into training and testing parts
# set random seed to some value so that results are consistent
set.seed(2016)

wb.size <- length(wellbeing)
wb.train.size <- round(wb.size * 0.5) # 50% for training
wb.test.size <- wb.size - wb.train.size # The rest for testing
wb.train.idx <- sample(seq(1:wb.size), wb.train.size) # Indeces for training
wb.train.sample <- wb[wb.train.idx,]
wb.test.sample <- wb[-wb.train.idx,]

##### Test NB classifiers, test and refine them

# Let's see the performance of all variables, excluding wellbeing
classf <- naiveBayes(
  subset(wb.train.sample, select = -wellcl), 
  wb.train.sample$wellcl, laplace=1)
preds <- predict(classf, 
  subset(wb.test.sample, select = -wellcl))
table(preds, wb.test.sample$wellcl)
round(sum(preds == wb.test.sample$wellcl, na.rm=TRUE) / 
        length(wb.test.sample$wellcl), digits = 2)

# We could better report the performance using "caret" package
# which has lots of other very useful functions in there

# Install this package if working on your computer
# install.packages("caret", dependencies = TRUE)
library(caret)
confusionMatrix(table(preds, wb.test.sample$wellcl))


##### Many mislassifications, how about the following ideas:
#     - Change the mix of variables, e.g. replace sitting with soc
#     - Remove badly skewed vars, or transform them, e.g. safe
wb <- data.frame(highblood, poorteeth, avoidabledeath, wellcl)
#     - Add more variables and make a better selevction
#     - Redefine "wellcl", e.g.
wellcl <- ifelse(wellbeing < 0.49, "Low", ifelse(wellbeing < 0.57, "Medium", "High"))

##### And then compare NB with k-NN

# Create a simple k-NN classifier
# Install this package if working on your computer
# install.packages("class", dependencies = TRUE)
library(class)

preds <- knn(
  subset(wb.train.sample, select = -wellcl), 
  subset(wb.test.sample, select = -wellcl),
  factor(wb.train.sample$wellcl),
  k = 3, prob=TRUE, use.all = TRUE)
confusionMatrix(table(preds, wb.test.sample$wellcl))

preds <- knn(
  subset(wb.train.sample, select = -wellcl), 
  subset(wb.test.sample, select = -wellcl),
  factor(wb.train.sample$wellcl),
  k = 10, prob=TRUE, use.all = TRUE)
confusionMatrix(table(preds, wb.test.sample$wellcl))


##### Summary

# Which one is better?
# What does it all mean?
# Can we still improve the NB performance?
# How can we improve its performance?

##### If you got stuck earlier on...

### If you have a problem understanding the way
#   colouring was done in pairs, reflect on the following

# See these
factor(wellcl)
unclass(factor(wellcl))
c("red", "green3", "blue")[unclass(factor(wellcl))]

# Now build your understanding with these
c("red", "green3", "blue")
c("red", "green3", "blue")[2]
length(2)
length(c(1, 3, 2, 1))
c("red", "green3", "blue")[c(1, 3, 2, 1)]

### We can also consider categorical variables
#   Test their independence of wellcl
#   We can also consider some discrete vars
#   Use Chi-Sq to test their independence
remoteness <- lga.profile$Geo.7
osbirth1 <- lga.profile$Ancestry.8
osbirth2 <- lga.profile$Ancestry.10
oslang1 <- lga.profile$Ancestry.20
oslang2 <- lga.profile$Ancestry.22

# Got it?
