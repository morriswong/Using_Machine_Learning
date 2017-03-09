
# Title: R Introduction
# 
#
# Data: http://docs.health.vic.gov.au/docs/doc/2013-LGA-profiles-data
# Source: Australian Department of Health & Human Services
#
# Data: https://en.wikipedia.org/wiki/Local_government_areas_of_Victoria
# Source: Wikipedia (LGA geo-locations reconstructed)
# 
# Data analysis and plotting


# Read all of the LGA profiles in
lga.profile <- read.csv("/Users/AdamLiu/Google Drive/Machine_Learning/R_Exercises/Vic 2013 LGA Profiles NoPc.csv")
lga.profile$LGA

# Focus on three vectors, i.e. health vs smokers
lga <- lga.profile$LGA
peopleno <- lga.profile$Population.23
notwell <- lga.profile$WellBeing.5 * 1000 # per 1000
smokers <- lga.profile$Health.1 * 1000 # per 1000

# View the two vectors and check against the data
notwell
plot(notwell)
hist(notwell)

smokers
plot(smokers)
hist(smokers)


# We now have both in so plot them
plot(x=smokers, y=notwell, col="red", main="Smoking vs Poor Health")
boxplot(notwell)
boxplot(smokers)

# If you want to combine these vectors to make a matrix
mat <- cbind(smokers, notwell)
mat
mat[2:5, 1:2]
mat[2, 2]
mat[2,]

# It is more useful to crate a data frame (~Excel worksheet)
# It then becomes a data set with its own variables
snw <- data.frame(smokers, notwell)
snw
mean(snw$smokers)
max(snw$notwell)
mean(mat$notwell)

# We can still refer to the columns of the matrix or any vectors
plot(x = mat[,1], y=mat[,2], main="Smoking vs Poor Health",
     xlab="Smoking / 1000", ylab="Not Well / 1000" )

# All plotting functions however recognise frames
plot(snw, col="blue", main="Smoking vs Poor Health",
     xlab="Smokers / 1000", ylab="Not Well / 1000" )
abline(h=mean(snw$notwell), col="orange")

# We can make it much nicer
plot(snw, col="red", main="Smoking vs Poor Health",
     type="n", xlab="Smokers / 1000", ylab="Not Well / 1000" )
segments(x0=snw$smokers, y0=snw$notwell, x1=snw$smokers, 
         y1=rep(mean(snw$notwell), length(snw$notwell)), 
         col=rgb(0,0,1,0.3))
abline(h=mean(snw$notwell), col="orange")
points(snw, col=rgb(0,0,1,0.5), pch=20, cex=1)

# Finally learn about libraries and packages
# Add a point on the plot to highlight the worst LGA

# Dependencies for textxy function, used at the end
# install.packages("calibrate", dependencies = TRUE)
library(calibrate)

# If wondered what LGA has such high crime rate, try this:
max(snw$notwell)
match(max(snw$notwell), snw$notwell)
lga[match(max(snw$notwell), snw$notwell)]

notwell.highest <- match(max(snw$notwell), snw$notwell)
textxy(snw$smokers[notwell.highest]-10, 
       snw$notwell[notwell.highest]-10, 
       lga[notwell.highest], col="red", cex=0.7)
