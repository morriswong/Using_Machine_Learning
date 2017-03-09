# Title: Decision Tables vs Naive Bayes 
# 
# 
# Decision Trees vs Naive Bayes for automobiles classification
# Data: https://archive.ics.uci.edu/ml/datasets/Automobile
#
# Problem:
# Allow insurance valuers to assess the risk associate with
# the price of an imported car for insurance purposes. 
# The prediction is based on the previous quotes and the
# valuers assessment of those quotes as represented by the
# "symboling" variable, which is from -2 (no risk) to 3 (risky)



##### Read the data set
auto <- read.csv(file = "/Users/AdamLiu/Google Drive/Machine_Learning/R_Exercises/automobile-cleaner.csv", header = T)
auto$Symboling <- as.factor(auto$Symboling)

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
  return(d[order(d$propmiss), ])
}

# find missing columns
auto.miss_cols <- propmiss(auto)
auto.miss_cols

##### Impute missing values - Optional - Skip if wish to
# For tree training we do not need to impute missing values
# If needed we could use a sophisticate Amelia package
# Optionally here we will use a simple Hmisc package
# install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)

# Save the levels of all columns, to make them standard
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


##### Make and draw decision tree
# Other possible packages: party, partykit rpart
# Surrogates needed if data has missing values
# install.packages("party", dependencies = TRUE)
library(party)

# Create a tree
auto.tree <- ctree(Symboling ~ ., data = auto.train,
                   controls = ctree_control(maxsurrogate = 3))

# Plot the tree structure
print(auto.tree)
plot(auto.tree, type="simple")
plot(auto.tree)

# Test it using training examples
auto.preds <- predict(auto.tree)
table(auto.preds, auto.train$Symboling)
round(sum(auto.preds == auto.train$Symboling, na.rm=TRUE) / 
        length(auto.train$Symboling), digits = 2)

# Validate the tree using a test set
auto.preds <- predict(auto.tree, newdata = auto.test)
table(auto.preds, auto.test$Symboling)
round(sum(auto.preds == auto.test$Symboling, na.rm=TRUE) / 
        length(auto.test$Symboling), digits = 2)

##### Test Naive Bayes
# Note that there is another "impute" function inside
# When installed you have detach the package to do "impute" above
# install.packages("e1071", dependencies = TRUE)
library("e1071")

classf <- naiveBayes(subset(auto.train, select=-Symboling), auto.train$Symboling, laplace=1)
preds <- predict(classf, subset(auto.test, select=-Symboling))
table(preds, auto.test$Symboling)
round(sum(preds == auto.test$Symboling, na.rm=TRUE) / 
        length(auto.test$Symboling), digits = 2)

# Happy? Now detatch
detach("package:e1071", unload=TRUE)
