#Solution to question 5: PM2,5 Emissions change by vehicle related sources in Baltimore City

library(dplyr)
library( ggplot2) 
library(stringr)

#Please set your working directory here:
setwd("D:/ApplicationFiles/Aprendizaje/DataScience/DataScience/Module4/Data/Module4Project2")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Selecet data for Baltimore City
NEI.f <- filter( NEI,fips == 24510)

joinedData<-left_join(NEI.f,SCC)

#Assumed that motor vehicle related sources have the string "Vehicle" in SCC.Level.Four or SCC.Level.Three
motorVehicleRelatedRecords<-joinedData[(grepl("[Vv][Ee][Hh][Ii][Cc][Ll][Ee]",joinedData$SCC.Level.Four)
                                        | grepl("[Vv][Ee][Hh][Ii][Cc][Ll][Ee]",joinedData$SCC.Level.Three)),]

#The result was grouped by SCC.Level.One
grp_cols <- c("SCC.Level.One","year")
# Convert character vector to list of symbols
dots <- lapply(grp_cols, as.symbol)

# Perform sum
groupByType<- motorVehicleRelatedRecords %>%
  group_by_(.dots=dots) %>%
  summarize(yearAndSourceEmissions = sum( Emissions, na.rm = TRUE))

par(mar=c(5,5,5,5))

r<-ggplot(groupByType, aes(year,yearAndSourceEmissions))#<--------------------------------------------ggplot
r+geom_line(aes(color=SCC.Level.One)) + labs(title = "Emissions from motor vehicle related sources in Baltimore City",x = "Year" , y = "Emissions")

dev.copy(png, file="plot5.png",units="px",width=480,height=480)#<--------------------------------- copy plot to a file
dev.off()
dev.off()
