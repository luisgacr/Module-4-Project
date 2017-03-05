#Solution to question 1: PM2.5 Emission by year in th United States

library(dplyr)

#Please set your working directory here:
setwd("D:/ApplicationFiles/Aprendizaje/DataScience/DataScience/Module4/Data/Module4Project2")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

par(mfrow=c(1,1)) # all plots on one page 
par(mar=c(5,2,2,2))
par(pch=22, col="black") # plotting symbol and color 

groupByYear <- group_by( NEI, year)

emissionsByYear<-summarize( groupByYear, yearEmissions = sum( Emissions, na.rm = TRUE))

plot(emissionsByYear$year,
     emissionsByYear$yearEmissions,
     type="n", 
     main= "Total PM2.5 Emission by Year",
     xlab="year",
     ylab="Emissions")
lines(emissionsByYear$year,
      emissionsByYear$yearEmissions,
      type="b")

dev.copy(png, file="plot1.png",units="px",width=480,height=480)#<--------------------------------- copy plot to a file
dev.off()
dev.off()
