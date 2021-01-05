#Exploratory Data Analysis - Course Project - Alvaro Lozano Alonso

library(ggplot2)
library(dplyr)
setwd("~/Documents/Dokumente - MacBook Air/Data Science with R - Johns Hopkins University/C4")
list.files("./exdata-data-NEI_data")
SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")
NEI <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")

aggregateByYear <- aggregate(NEI$Emissions, by=list(Category=NEI$year), FUN=sum)
barplot(aggregateByYear$x, names.arg=aggregateByYear$Category, ylab="Total PM2.5 emission", xlab="Year",
        main="Emissions per year")

NEI_BC <- NEI[NEI$fips == "24510",]
aggregateByYearBaltimoreCity <- aggregate(NEI_BC$Emissions, by=list(Category=NEI_BC$year), FUN=sum)
barplot(aggregateByYearBaltimoreCity$x, names.arg=aggregateByYearBaltimoreCity$Category,
        ylab="Total PM2.5 emission in Baltimore City", xlab="Year", main="Emissions per year in Baltimore City")

aYBC_Type <- NEI %>% group_by(year, type) %>% summarise(Emissions = sum(Emissions))
ggplot(aYBC_Type, aes(x=type, y=Emissions, fill=factor(year))) +
       geom_bar(stat="identity", position="dodge") +
       scale_fill_discrete((name="Year")) +
       xlab("Type") +
       ylab("Emissions") + 
       ggtitle("Emissions per year and Type in Baltimore City [1999 to 2008]")


table(SCC$Data.Category)
