#Solution to question 4: PM2,5 Emissions change by coal combustion related sources in the United States

library(dplyr)
library( ggplot2) 
library(stringr)

#Please set your working directory here:
setwd("D:/ApplicationFiles/Aprendizaje/DataScience/DataScience/Module4/Data/Module4Project2")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Assumed that coal combustion related sources have the string "Coal" in SCC.Level.Four or SCC.Level.Three
joinedData<-left_join(NEI,SCC)
coalCombustionRelatedRecords<-joinedData[(grepl("[Cc][Oo][Aa][Ll]",joinedData$SCC.Level.Four) 
                                          | grepl("[Cc][Oo][Aa][Ll]",joinedData$SCC.Level.Three)),]


#The result was grouped by SCC.Level.One
grp_cols <- c("SCC.Level.One","year")
# Convert character vector to list of symbols
dots <- lapply(grp_cols, as.symbol)

# Perform sum
groupByType<- coalCombustionRelatedRecords %>%
  group_by_(.dots=dots) %>%
  summarize(yearAndSourceEmissions = sum( Emissions, na.rm = TRUE))

par(mar=c(5,5,5,5))

r<-ggplot(groupByType, aes(year,yearAndSourceEmissions))#<--------------------------------------------ggplot
r+geom_line(aes(color=SCC.Level.One)) + labs(title = "Emissions from coal combustion related sources",x = "Year" , y = "Emissions")

dev.copy(png, file="plot4.png",units="px",width=480,height=480)#<--------------------------------- copy plot to a file
dev.off()
dev.off()
