# Dealing with Vic 2013 LGA file
#
# 


##### First organise your workspace for R scripts and data, e.g.


# Read the LGA profiles in
lga.profile <- read.csv("/Users/AdamLiu/Google Drive/Machine_Learning/R_Exercises/Vic 2013 LGA Profiles NoPc.csv")

# Focus on three vectors, i.e. health vs smokers
lga <- lga.profile$LGA
peopleno <- lga.profile$Population.23
notwell <- lga.profile$WellBeing.5 * 1000 # per 1000
smokers <- lga.profile$Health.1 * 1000 # per 1000

# View the two vectors and check against the data
notwell
plot(hist(notwell))
hist(notwell)

smokers
plot(smokers)
hist(smokers)


# We now have both in so plot them
# However, a better plot would be shown below
plot(x=smokers, y=notwell, col=rgb(0,0,1,0.5), pch=20, cex=1.5, main="Smoking vs Poor Health")
boxplot(notwell, add = T)
boxplot(smokers, add = T)

# If you want to combine these vectors to make a matrix
matrix <- cbind(smokers, notwell)
summary(mat)
mat[2:5, 1:2]
mat[2, 2]
mat[2,]

# Creating a data frame of smokers and notwell
smokers_and_notwell <- data.frame(smokers, notwell)
smokers_and_notwell
mean(smokers_and_notwell$smokers)
mean(smokers_and_notwell$notwell)
max(smokers_and_notwell$smokers)
max(smokers_and_notwell$notwell)
mean(mat$notwell)


#### Ploting smokers_and_notwell
#
# 1. create the frame for the plot
plot(smokers_and_notwell, col="red", main="Smoking vs Poor Health",
     type="n", xlab="Smokers / 1000", ylab="Not Well / 1000" )

# 2. draw the segment of each point from the mean value
segments(x0=smokers_and_notwell$smokers, y0=smokers_and_notwell$notwell, x1=smokers_and_notwell$smokers, 
         y1=rep(mean(smokers_and_notwell$notwell), length(smokers_and_notwell$notwell)), 
         col=rgb(0,0,1,0.3))

# 3. draw a line that indicates the mean value
abline(h=mean(smokers_and_notwell$notwell), col="orange")

# 4. emphasize the dots
points(smokers_and_notwell, col=rgb(0,0,1,0.5), pch=20, cex=1)






# If wondered what LGA has such high crime rate, would be better to
# add a point on the plot to highlight the worst LGA

# install.packages("calibrate", dependencies = TRUE)
library(calibrate)
max(smokers_and_notwell$notwell)
match(max(smokers_and_notwell$notwell), smokers_and_notwell$notwell)
lga[match(max(smokers_and_notwell$notwell), smokers_and_notwell$notwell)]

notwell.highest <- match(max(smokers_and_notwell$notwell), smokers_and_notwell$notwell)
textxy(smokers_and_notwell$smokers[notwell.highest]-10, 
       smokers_and_notwell$notwell[notwell.highest]-10, 
       lga[notwell.highest], col="red", cex=0.7)



##### Ploting data on Google Maps

# Load the LGA geo locations, get rid of all past 79 row
# Reading with a trick to keep only lines we want with header

# install.packages("ggmap", dependencies = TRUE)
library("ggplot2")
library("ggmap")

# Read the coordinates of known LGAs
lga.profile[1:5,]
is.data.frame(lga.profile)
lga.profile["notwell"] <- notwell
lga.profile[1:5,]

# Get the map of victoria, try maptype satellite or hybrid
map <- qmap("Melbourne, Victoria, Australia", zoom=8, maptype="terrain")
plot(map)
# Plot the map with all points
lga.coords <- read.csv("/Users/AdamLiu/Google Drive/Machine_Learning/R_Exercises/Vic 2013 LGA Locs.csv")
lga.coords <- lga.coords[1:79,]
map + 
    geom_point(data=lga.profile, 
               aes(x=lga.coords$long, y=lga.coords$lat, size=notwell), 
               color="red", alpha=0.3) +
    scale_size_continuous(range = c(1, 15), 
                          breaks=c(100, 150, 200))


##### Now let us do some new exploration in Google Maps++
#     Let's use k-NN model to "predict" properties for unknown geo locations
#     All LGAs will be referred to as "known" locations
#     All investigated locations will be referred to as "unknown" locations

# Define some "unknown" Vic locations
unknown.names <- c("Pie in the Sky", "Great Surfing", "Middle of Nowhere", "Bonnie Doon", "Gooram", 
               "Euroa", "Goulburn River", "Korweinguboora")
unknown.lat <- c(-37.855596, -38.351386, -37.733653, -37.026837, -36.892962, -36.752250, -36.785253, -37.456643)
unknown.long <- c(145.365799, 144.300879, 145.166513, 145.880146, 145.610981, 145.569783, 145.144062, 144.135963)
unknown.places <- data.frame(unknown.names, unknown.lat, unknown.long)
View(unknown.places)

# Existing LGAs with their geo-locations is our training data
# The corresponding LGA names ia our classification (note that we have no overlaps)
lga.locs <- cbind(lga.coords$lat, lga.coords$long)
lga.names <- lga.coords$lga

# New locations without LGA is our test data
unknown.locs <- cbind(unknown.lat, unknown.long)

##### Let us use k-nn classifier to find the best matching smoking habit, try k=1-5 
? knn

# Find the exact LGA based on the physical proximity - they have unique names

library(class)
unknown.lga.names <- knn(lga.locs, unknown.locs, lga.names, k = 1, prob=TRUE, use.all = TRUE)
unknown.lga.names

# Create a full description of the new location with their LGA details
place.descr <- cbind(unknown.places,lga.profile[unknown.lga.names,])
place.descr

# Get the map of victoria, try maptype satellite or hybrid
map <- qmap("Melbourne, Victoria, Australia", zoom=8, maptype="terrain")

# Plot the map with all points
map + 
  geom_point(data=lga.profile, 
             aes(x=lga.coords$long, y=lga.coords$lat, size=notwell), 
             color="red", alpha=0.3) +
  geom_point(data=place.descr, 
             aes(x=lga.coords$long, y=lga.coords$lat, size=notwell), 
             color="blue", alpha=0.7) +
  geom_segment(data=place.descr, 
             aes(x=lga.coords$long, y=lga.coords$lat, xend=lga.coords$long, yend = lga.coords$lat),
             color="blue") +
  scale_size_continuous(range = c(1, 10), 
                        breaks=c(50, 100, 150, 200))
  
# How could you change this code to include the new place's crime rates to determine the "closest" LGA
# Would the results be the same? If not what is going? Is it what the intuition tells us? How to fix it?

##### Some administrative checks
# Normally we'd also do the following as well

# Check where is your home directory
path.expand("~")

# Check where is your user library and what's in it
.libPaths()
list.files(.libPaths()[1])

