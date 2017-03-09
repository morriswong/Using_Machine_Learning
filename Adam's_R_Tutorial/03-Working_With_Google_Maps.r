
# Title: R Introduction
# 
#
# Data: http://docs.health.vic.gov.au/docs/doc/2013-LGA-profiles-data
# Source: Australian Department of Health & Human Services
#
# Data: https://en.wikipedia.org/wiki/Local_government_areas_of_Victoria
# Source: Wikipedia (LGA geo-locations reconstructed)
# 
# Using Google Maps and explaining variables types



# Read all of the LGA profiles in
lga.profile <- read.csv("/Users/AdamLiu/Google Drive/Machine_Learning/R_Exercises/Vic 2013 LGA Profiles NoPc.csv")
View(lga.profile)

# Focus on three vectors, i.e. health vs smokers
lga <- lga.profile$LGA
peopleno <- lga.profile$Population.23
notwell <- lga.profile$WellBeing.5 * 1000 # per 1000
smokers <- lga.profile$Health.1 * 1000 # per 1000

##### Let us analyse these variables

# Establish what does it mean light and heavy smokers
boxplot(smokers, notwell, names = c("Smokers", "Unhealthy"))
summary(smokers)
snw.box <- summary(smokers)
non.smokers.lga <- snw.box[1] # Min
light.smokers.lga <- snw.box[2] # 1st Qu.
medium.smokers.lga <- snw.box[5] # Median / 2nd Qu.
heavy.smokers.lga <- snw.box[6] # Max

# Let us classify all LGAs depending on smoking habits
habits <- 
  ifelse(smokers < non.smokers.lga, "clear",
    ifelse(smokers < light.smokers.lga, "light",
      ifelse(smokers < medium.smokers.lga, "medium",
        "heavy")))
habits

# Let's make a data frame of LGA smoking vs wellness
# Note that classification variable needs to be factor
# In other software called nominal or class variable
snw <- data.frame(lga, smokers, notwell)
snw["habits"] <- habits
snw["habits"] <- factor(habits)
snw <- data.frame(lga, smokers, notwell, habits)
levels(snw$habits)
levels(habits)

##### Time for ploting data on Google Maps

# Now load the LGA geo locations, get rid all past 79 row
# Ensure they are in exactly the same order as in LGA profiles
# Reading with a trick to keep only lines we want with header

# install.packages("ggmap", dependencies = TRUE)
library("ggmap")

# Read the coordinates of known LGAs
map.coords <- read.csv(text=readLines("/Users/AdamLiu/Google Drive/Machine_Learning/R_Exercises/Vic 2013 LGA Profiles NoPc.csv")[1:80])
map.coords[1:5,]
map.coords["notwell"] <- notwell
map.coords["smokers"] <- smokers
map.coords["habits"] <- factor(habits)
map.coords[(1:10),]

# Let us investigate our data
# Let us investigate LGAs based on smoking habits
map.coords[1:3,]
map.coords.light = map.coords[map.coords$habits == "light",]
map.coords.light
map.coords.medium = map.coords[map.coords$habits == "medium",]
map.coords.medium
map.coords.heavy = map.coords[map.coords$habits == "heavy",]
map.coords.heavy

# Plot the map of all Vic with all heavy smoking LGAs
map.vic.heavy <- qmap("Melbourne, Victoria, Australia", zoom=7, maptype="terrain")
map.vic.heavy + 
  ggtitle("Victoria: Smoking vs Ill-Health") +
  geom_point(data=map.coords.heavy, 
             aes(x=long, y=lat, size=notwell), 
             color="red", alpha=0.5) +
  scale_size_continuous(name="Degree of Ill-Health",
                        range = c(1, 10), 
                        breaks=c(100, 150, 200))

# Plot the map of Vic closer to Melbourne with all points
map.vic.all <- qmap("Melbourne, Victoria, Australia", zoom=8, maptype="terrain")
map.vic.all + 
  ggtitle("Victoria: Smoking vs Ill-Health") +
  geom_point(data=map.coords.light, 
             aes(x=long, y=lat, size=notwell), 
             color="darkgreen", alpha=0.5) +
  geom_point(data=map.coords.medium, 
             aes(x=long, y=lat, size=notwell), 
             color="orange", alpha=0.5) +
  geom_point(data=map.coords.heavy, 
             aes(x=long, y=lat, size=notwell), 
             color="red", alpha=0.5) +
  scale_size_continuous(name="Degree of Ill-Health",
                        range = c(1, 10), 
                        breaks=c(100, 150, 200))

# Plot the map of Vic around Melbourne with all LGA of heavy smokers
levels(map.coords$habits)
map.larger_melb <- qmap("Melbourne, Victoria, Australia", zoom=9, maptype="terrain")
map.larger_melb + 
  ggtitle("Victoria: Smoking vs Ill-Health") +
  geom_point(data=map.coords, 
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
