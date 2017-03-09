# R Introduction
# 
# 
# Installation and basic R

setwd("R_exercises")
getwd()

# Define two variables, one a number, another a list
a <- 10
b <- c(2, 3, 4)
length(a)
length(b)
length(c(a, b))


# Define a list of numbers and plot it
x <- seq(from = 3, to = 22)
x
plot(x)


# Get a random sample of numbers from the list and plot it
y <- sample(x, size = 20)
y
plot(x, y, type="b", col="red")


# Create a sample of numbers normally distributed and plot it
e <- rnorm(n = 10000, mean = 2.5, sd = 0.1)
hist(e, col="blue")
plot(density(e), type="h", col="orange")
