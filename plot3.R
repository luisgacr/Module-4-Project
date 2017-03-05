#Solution to question 3: PM2.5 Baltimore City, Emissions by type, increase/decrease 1999-2008

library(dplyr)
library( ggplot2) 

#Please set your working directory here:
setwd("D:/ApplicationFiles/Aprendizaje/DataScience/DataScience/Module4/Data/Module4Project2")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


NEI.f <- filter( NEI,fips == 24510)
head(NEI.f)
groupByYear <- group_by( NEI.f, year)
emissionsByYear<-summarize( groupByYear, yearEmissions = sum( Emissions, na.rm = TRUE))
emissionsByYear

grp_cols <- c("type","year")
# Convert character vector to list of symbols
dots <- lapply(grp_cols, as.symbol)
dots
# Perform sum
groupByType<- NEI.f %>%
  group_by_(.dots=dots) %>%
  summarize(yearAndTypeEmissions = sum( Emissions, na.rm = TRUE))

par(mar=c(5,2,2,2))

r<-ggplot(groupByType, aes(year,yearAndTypeEmissions))#<--------------------------------------------ggplot
r+geom_point()+facet_grid(.~type)+geom_smooth()+labs( title = "Baltimore City, Emissions by type, increase/decrease 1999-2008",
                                                      x = "Year" , y = "Emissions")
dev.copy(png, file="plot3.png",units="px",width=480,height=480)#<--------------------------------- copy plot to a file
dev.off()
dev.off()
