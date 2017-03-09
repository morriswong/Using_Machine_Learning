
# Title: R Introduction
# 
#
# Data: http://docs.health.vic.gov.au/docs/doc/2013-LGA-profiles-data
# Source: Australian Department of Health & Human Services
#
# Data: https://en.wikipedia.org/wiki/Local_government_areas_of_Victoria
# Source: Wikipedia (LGA geo-locations reconstructed)
# 
# Using k-NN and displaying results on Google Maps

# Define your working directory
# Ensure to use the forward / slash
setwd("/Users/AdamLiu/Google Drive/Machine_Learning/R_Exercises")

##### Let us now explore a sample data set

# install.packages("ggmap", dependencies = TRUE)
# install.packages("class", dependencies = TRUE)
library("ggmap")
library("class")

# Make sure you place prepared LGA CSV file in your "Data" directory

# Read all of the LGA profiles and their geo-locations, unincorporated ignored
lga.profile <- read.csv("/Users/AdamLiu/Google Drive/Machine_Learning/R_Exercises/Vic_2013_LGA_Profiles_NoPc.csv")
lga.coords <- read.csv(text=readLines("Vic_2013_LGA_Locs.csv")[(1:80)])

# Focus on three vectors, i.e. health vs smokers
lga <- lga.profile$LGA
notwell <- lga.profile$WellBeing.5 * 1000 # per 1000
smokers <- lga.profile$Health.1 * 1000 # per 1000

##### Let us analyse these variables

# Establish what does it mean light and heavy smokers
snw.qrtl <- summary(smokers)
habits <- 
  ifelse(smokers < snw.qrtl[1], "clear",
         ifelse(smokers < snw.qrtl[2], "light",
                ifelse(smokers < snw.qrtl[5], "medium",
                       "heavy")))

##### Time for ploting data on Google Maps

# Read the coordinates of known LGAs
lga.coords["notwell"] <- notwell
lga.coords["smokers"] <- smokers
lga.coords["habits"] <- factor(habits)
lga.coords[(1:10),]

# Plot the map of Vic closer to Melbourne with all points
map.larger_melb <- qmap("Melbourne, Victoria, Australia", zoom=8, maptype="terrain")
map.larger_melb + 
  ggtitle("Victoria: Smoking vs Ill-Health") +
  geom_point(data=lga.coords, 
             aes(x=long, y=lat, size=notwell, color=habits, shape=habits), 
             alpha=0.5) +
  scale_colour_manual(name = "Smoking Habits",
                      labels = c("Heavy Smokers", "Light Smokers", "Medium Smokers"),
                      values = c("red", "blue", "orange")) +   
  scale_shape_manual(name = "Health",
                     labels = c("Heavy Smokers", "Light Smokers", "Medium Smokers"),
                     values = c(18, 19, 17)) +
  scale_size_continuous(name = "Degree of Ill Health",
                        range = c(1, 10), 
                        breaks=c(100, 150, 200))

##### Now let us do some new exploration in Google Maps++
#     Let's use k-NN model to "predict" properties for unknown geo locations
#     All LGAs will be referred to as "known" locations
#     All investigated locations will be referred to as "unknown" locations

# Define some "unknown" Vic locations
unk.names <- c("Pie in the Sky", "Great Surfing", "Middle of Nowhere", "Bonnie Doon", "Gooram", 
               "Euroa", "Goulburn River", "Korweinguboora")
unk.lat <- c(-37.855596, -38.351386, -37.733653, -37.026837, -36.892962, -36.752250, -36.785253, -37.456643)
unk.long <- c(145.365799, 144.300879, 145.166513, 145.880146, 145.610981, 145.569783, 145.144062, 144.135963)
unk.places <- data.frame(unk.names, unk.lat, unk.long)
View(unk.places)

# Existing LGAs with their geo-locations is our training data
# The corresponding LGA names ia our classification (note that we have no overlaps)
lga.locs <- cbind(lga.coords$lat, lga.coords$long)
lga.habits <- lga.coords$habits
lga.names <- lga.coords$lga

# New locations without LGA is our test data
unk.locs <- cbind(unk.lat, unk.long)

##### Let us use k-nn classifier to find the best matching smoking habit, try k=1-5 
? knn

# Find approximate smoking habits based on 3 neighbours - classification is not unique
unk.habits <- knn(lga.locs, unk.locs, lga.habits, k = 3, prob=TRUE, use.all = TRUE)
unk.habits

# Find the exact LGA based on the physical proximity - they have unique names
unk.lga.names <- knn(lga.locs, unk.locs, lga.names, k = 1, prob=TRUE, use.all = TRUE)
unk.lga.names

# Create a full description of the new location with the details of the closest LGA
place.descr <- cbind(unk.places, unk.habits,lga.coords[unk.lga.names,])
place.descr

# Plot the results
# Plot the map of Vic closer to Melbourne with all points
map.larger_melb <- qmap("Melbourne, Victoria, Australia", zoom=9, maptype="terrain")
map.larger_melb + 
  ggtitle("PLaces of Interest: Smoking vs Ill-Health") +
  geom_segment(data=place.descr, # Plot lines from unknown to LGA locations
               aes(x=unk.long, y=unk.lat, xend=long, yend = lat),
               color="purple") +
  geom_point(data=lga.coords, # Plot all known LGAs in colour of smoking habit
             aes(x=long, y=lat, size=notwell, color=habits), 
             alpha=0.5) +
  geom_point(data=place.descr, # Plot all unknown points in colour of learnt habit
             aes(x=unk.long, y=unk.lat, size=150, color=unk.habits), 
             alpha=0.5) +
  geom_point(data=place.descr, # Plot a purple dot in the middle of unknown locations
             aes(x=unk.long, y=unk.lat, size=100), 
             color="purple", alpha=0.9) +
  scale_colour_manual(name = "Smoking Habits",
                      labels = c("Heavy Smokers", "Light Smokers", "Medium Smokers"),
                      values = c("red", "blue", "orange")) +   
  scale_size_continuous(name = "Degree of Ill Health",
                        range = c(1, 7), 
                        breaks=c(100, 150, 200))

##### Thank you very much
