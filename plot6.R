#Solution to question 6: PM2,5 Emissions percentage change change by vehicle related sources in Baltimore City and Los Angeles

library(dplyr)
library( ggplot2) 
library(stringr)

#Please set your working directory here:
setwd("D:/ApplicationFiles/Aprendizaje/DataScience/DataScience/Module4/Data/Module4Project2")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Select data from Baltimore City and Los Angeles
NEI.f <- filter( NEI,fips == "24510" | fips == "06037" )

joinedData<-left_join(NEI.f,SCC)

#Assumed that motor vehicle related sources have the string "Vehicle" in SCC.Level.Four or SCC.Level.Three
motorVehicleRelatedRecords<-joinedData[(grepl("[Vv][Ee][Hh][Ii][Cc][Ll][Ee]",joinedData$SCC.Level.Four)
                                        | grepl("[Vv][Ee][Hh][Ii][Cc][Ll][Ee]",joinedData$SCC.Level.Three)),]

grp_cols <- c("fips","year")
# Convert character vector to list of symbols
dots <- lapply(grp_cols, as.symbol)
dots
# Perform sum
groupByCityAndYear<- motorVehicleRelatedRecords %>%
  group_by_(.dots=dots) %>%
  summarize(yearAndSourceEmissions = sum( Emissions, na.rm = TRUE))
groupByCityAndYear

#Calculates percentage change for every 3 years and stores it in result
result <- data.frame(city=character(6), year=numeric(6), emissions=numeric(6), stringsAsFactors = FALSE)
lfips<-c("06037","24510")
lcities<-c("Los Angeles","Baltimore")
lyears=c(2002,2005,2008)
for (fipNumber in 1:length(lfips)) {
  for (yearNumber in 1:length(lyears)) {
    lineIn<-4*(fipNumber-1)+yearNumber
    lineOut<-3*(fipNumber-1)+yearNumber
    result[lineOut,1]<-lcities[fipNumber]
    result[lineOut,2]<-lyears[yearNumber]
    result[lineOut,3]<-(groupByCityAndYear[lineIn+1,3]-groupByCityAndYear[lineIn,3])/groupByCityAndYear[lineIn,3]
    
  }
}
result

par(mar=c(5,5,5,5))

r<-ggplot(result, aes(year,emissions))#<--------------------------------------------ggplot
r+geom_line(aes(color=city)) + labs(title = "Emissions percent 3 year change for motor vehicle related sources by City",x = "Year" , y = "Emissions")

dev.copy(png, file="plot6.png",units="px",width=480,height=480)#<--------------------------------- copy plot to a file
dev.off()
dev.off()