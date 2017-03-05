#Solution to question 2: PM2.5 Emission by year in Baltimore City

library(dplyr)

#Please set your working directory here:
setwd("D:/ApplicationFiles/Aprendizaje/DataScience/DataScience/Module4/Data/Module4Project2")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Select data for Baltimore City only
NEI.f <- filter( NEI,fips == 24510)

par(mfrow=c(1,1)) # all plots on one page 
par(mar=c(5,2,2,2))
par(pch=22, col="black") # plotting symbol and color 

groupByYear <- group_by( NEI.f, year)
emissionsByYear<-summarize( groupByYear, yearEmissions = sum( Emissions, na.rm = TRUE))
emissionsByYear
plot(emissionsByYear$year,
     emissionsByYear$yearEmissions,
     type="n", 
     main= "Total PM2.5 Emission by Year in Baltimore City",
     xlab="year",
     ylab="Emissions")
lines(emissionsByYear$year,
      emissionsByYear$yearEmissions,
      type="b")

dev.copy(png, file="plot2.png",units="px",width=480,height=480)#<--------------------------------- copy plot to a file
dev.off()
dev.off()
