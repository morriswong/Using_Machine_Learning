# Title: Decision trees vs neural nets
# 
# 
# Decision trees vs neural nets for automobile file
# Data: https://archive.ics.uci.edu/ml/datasets/Automobile
#
#
# Problem:
# Allow insurance valuers to predict the estimated price of
# an imported car for insurance purposes. The estmation is
# based on the previous quotes and not its worth, which is
# represented by the "symboling" variable - not used in this
# model.

##### Read the data set
auto <- read.csv(file = "/Users/AdamLiu/Google Drive/Machine_Learning/R_Exercises/automobile-cleaner.csv", header = T)


##### Function to find columns with missing data
# Collects information about all columns
# Missing columns are placed at the bottom of the frame
propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      nmiss=sum(is.na(x)), 
      n=length(x), 
      propmiss=sum(is.na(x))/length(x)
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$propmiss, decreasing = T), ])
}

##### Impute missing values - Optional - Skip if not training NN
# For tree training alone we do not need to impute missing values
# If needed we could use a sophisticate Amelia package
# Here we will use a simple Hmisc package for data imputation
# install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)

# find missing columns
auto.miss_cols <- propmiss(auto)
View(auto.miss_cols)

# Save the levels of all columns, to make them standard
# There is one missing value in the target Price
auto$Num.of.doors <- as.numeric(impute(auto$Num.of.doors, median))
auto$Horsepower <- as.numeric(impute(auto$Horsepower, mean))
auto$Peak.rpm <- as.numeric(impute(auto$Peak.rpm, mean))
auto$Bore <- as.numeric(impute(auto$Bore, mean))
auto$Stroke <- as.numeric(impute(auto$Stroke, mean))
auto$Price <- as.numeric(impute(auto$Price, mean))
auto$Normalized.losses <- as.numeric(impute(auto$Normalized.losses, mean))

##### Split data into training and testing
# Split rate: Training = 70%, Validation = 30%
set.seed(2015)
ind <- sample(2, nrow(auto), replace = TRUE, prob = c(0.7, 0.3))
auto.train <- auto[ind == 1, ]
auto.test <- auto[ind == 2, ]


##### Make and draw Decision Tree
# Other possible packages: party, partykit, rpart
# Surrogates needed if data has missing values
# install.packages("party", dependencies = TRUE)
library(party)

set.seed(2015)

# Create a decison tree
auto.tree <- ctree(Price ~ ., data = auto.train,
                   controls = ctree_control(maxsurrogate = 3))

# Visually inspect the tree
print(auto.tree)
plot(auto.tree, type="simple")
plot(auto.tree)

# Test it on training data, use correlation as a measure
auto.preds <- predict(auto.tree)
cor(auto.preds, auto.train$Price)

# Validate the tree on test data
auto.preds <- predict(auto.tree, newdata = auto.test)
cor(auto.preds, auto.test$Price)


##### Test against a Neural Network model
# For this code to work on the continuous target variable
# Make sure that all values are numerical and normalised to 0..1

# Prepare just the numeric data for NN processing
# If we wanted to use it all, we'd have to cast it to numeric

# Select numeric columns only
auto.sel <- subset(auto, select=c(Normalized.losses, Num.of.doors,
  Wheel.base, Length, Width, Height, Curb.weight, Num.of.cylinders, 
  Engine.size, Bore, Stroke, Compression.ratio, Horsepower, Peak.rpm, 
  City.mpg, Highway.mpg, Price))

# Normalise all data into interval 0..1
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
auto.sel.norm <- as.data.frame(lapply(auto.sel, normalise))

# Split data into training and testing
set.seed(2015)
ind.sel <- sample(2, nrow(auto.sel.norm), replace = TRUE, prob = c(0.7, 0.3))
auto.train.sel <- auto.sel.norm[ind.sel == 1, ]
auto.test.sel <- auto.sel.norm[ind.sel == 2, ]

### First, let us use a very simple "nnet" package
# install.packages("nnet", dependencies = TRUE)
library("nnet")

# For clarity pepare training and testing data sets
examples <- subset(auto.train.sel, select=-c(Price))
targets <- auto.train.sel$Price
examples.test <- subset(auto.test.sel, select=-c(Price))

# Create a network
set.seed(2015)
net <- nnet(examples, targets, size=8, maxit=1000, rang = 0.1,decay = 5e-4)

# Test it using training data, use correlation as a measure
net.out <- predict(net, examples)
cor(net.out, examples$Price)

# Use it for prediction, use correlation as a measure
net.out <- predict(net, examples.test)
cor(net.out, auto.test.sel$Price)

# This package cannot plot the net, so we use another
# This is a development version from the Github
# install.packages("devtools", dependencies = TRUE)
# install.packages("Rtools", dependencies = TRUE)
library(devtools)
source_url('https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r')
plot.nnet(net)


### Next let us use the "neuralnet" package
# install.packages("neuralnet", dependencies = TRUE)
library("neuralnet")

# Create the net and display it
set.seed(2015)
net <- neuralnet(Price ~ Normalized.losses+Num.of.doors+Wheel.base+Length+
                   Width+Height+Curb.weight+Num.of.cylinders+Engine.size+
                   Bore+Stroke+Compression.ratio+Horsepower+Peak.rpm+
                   City.mpg+Highway.mpg,
                 data=auto.train.sel, hidden=8, linear.output=TRUE)
plot(net) # Here we use a built in package
plot.nnet(net) # Here we use the imported package

# Check its performance on training data, use correlation as a measure
net.out <- compute(net, subset(auto.train.sel, select=-c(Price)))
net.preds <- net.out$net.result
cor(net.preds, auto.train.sel$Price)

# Validate its performance on test data, use correlation as a measure
net.out <- compute(net, subset(auto.test.sel, select=-c(Price)))
net.preds <- net.out$net.result
cor(net.preds, auto.test.sel$Price)

# Clearly the two NNs and DT all give similar results, with DT in the middle