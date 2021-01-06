#Exploratory Data Analysis - Course Project - Alvaro Lozano Alonso

library(ggplot2)
library(dplyr)
library(data.table)
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

coalCombustion <- filter(SCC, grepl("Fuel Comb.*Coal", EI.Sector))
coalCombustion.list <- unique(coalCombustion$SCC)
NEI.coalCombustion <- subset(NEI, SCC %in% coalCombustion.list)
NEI.coalCombustion <- NEI.coalCombustion %>% group_by(type, year) %>% summarize(Annual.Total = sum(Emissions))
NEI.coalCombustion.total <- NEI.coalCombustion %>% group_by(year) %>% summarize(Annual.Total = sum(Annual.Total)) %>%
        mutate(type = "TOTAL")
NEI.coalCombustion <- NEI.coalCombustion %>% select(Annual.Total, type, year)
NEI.coalCombustion <- bind_rows(NEI.coalCombustion, NEI.coalCombustion.total)
NEI.coalCombustion$type <- factor(NEI.coalCombustion$type, levels = c("TOTAL", "ON-ROAD", "NON-ROAD", "POINT", "NONPOINT"))
ggplot(NEI.coalCombustion, aes(x = factor(year), y = Annual.Total, fill = type)) +
        geom_bar(stat = "identity") +
        facet_grid(. ~ type) +
        xlab("year") +
        ylab(expression("Total Tons of PM2.5 Emissions")) +
        ggtitle(expression(atop("Total Tons of PM2.5 Emissions in the US", paste("from Coal Combustion-Related Sources")))) +
        theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
        scale_y_continuous(labels = comma) +
        scale_fill_brewer(palette = "Dark2") +
        guides(fill = FALSE)

scc.vehicles <- SCC[grep("Mobile.*Vehicles", SCC$EI.Sector),  ]; # Pattern match mobile vehicles in SCC description
scc.vehicles.list <- unique(scc.vehicles$SCC); # Create motor vehicle lookup list by SCC
nei.vehicles <- subset(NEI, SCC %in% scc.vehicles.list); # Filter for motor vehicle sources
nei.vehicles <- nei.vehicles %>% filter(fips == "24510") # Filter for Baltimore
nei.vehicles <- merge(x = nei.vehicles, y = scc.vehicles[, c("SCC", "SCC.Level.Two", "SCC.Level.Three")], by = "SCC") # Join in descriptive data on SCC codes
nei.vehicles <- nei.vehicles %>% group_by(year, SCC.Level.Two, SCC.Level.Three) %>% summarize(Annual.Total = sum(Emissions))
nei.vehicles.total <- nei.vehicles %>% group_by(year) %>% summarize(Annual.Total = sum(Annual.Total)) %>% mutate(SCC.Level.Two = "Total")
nei.vehicles <- bind_rows(nei.vehicles, nei.vehicles.total);
nei.vehicles$SCC.Level.Two <- factor(nei.vehicles$SCC.Level.Two, levels = c("Total", "Highway Vehicles - Diesel", "Highway Vehicles - Gasoline"));
ggplot(nei.vehicles, aes(x = factor(year), y = Annual.Total, fill = SCC.Level.Two)) +
        geom_bar(stat = "identity") +
        facet_grid(. ~ SCC.Level.Two) +
        xlab("Year") +
        ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
        ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions in Baltimore City", paste("from Motor Vehicle Sources")))) +
        theme(plot.title = element_text(hjust = 0.5)) + # Center the plot title
        theme(plot.margin = unit(c(1,1,1,1), "cm")) + # Adjust plot margins
        scale_fill_brewer(palette = "Set1") +
        guides(fill = FALSE)

scc.vehicles <- SCC[grep("Mobile.*Vehicles", SCC$EI.Sector),  ]; # Pattern match mobile vehicles in SCC description
scc.vehicles.list <- unique(scc.vehicles$SCC); # Create motor vehicle lookup list by SCC
nei.vehicles <- subset(NEI, SCC %in% scc.vehicles.list); # Filter for motor vehicle sources
nei.vehicles <- nei.vehicles %>% filter(fips == "24510"| fips == "06037"); # Filter for Baltimore City or Los Angeles County
nei.vehicles$fips[nei.vehicles$fips == "24510"] <- "Baltimore";
nei.vehicles$fips[nei.vehicles$fips == "06037"] <- "Los Angeles";
nei.vehicles <- merge(x = nei.vehicles, y = scc.vehicles[, c("SCC", "SCC.Level.Two")], by = "SCC"); # Join in descriptive data on SCC codes
nei.vehicles <- nei.vehicles %>% group_by(fips, year, SCC.Level.Two) %>% summarize(Annual.Total = sum(Emissions));
nei.vehicles.total <- nei.vehicles %>% group_by(fips, year) %>% summarize(Annual.Total = sum(Annual.Total)) %>% mutate(SCC.Level.Two = "Total");
nei.vehicles <- bind_rows(nei.vehicles, nei.vehicles.total);
nei.vehicles$SCC.Level.Two <- factor(nei.vehicles$SCC.Level.Two, levels = c("Total", "Highway Vehicles - Diesel", "Highway Vehicles - Gasoline"));
ggplot(nei.vehicles, aes(x = factor(year), y = Annual.Total, fill = SCC.Level.Two)) +
        geom_bar(stat = "identity") +
        facet_grid(fips ~ SCC.Level.Two) + 
        xlab("Year") +
        ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
        ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions from Motor Vehicle Sources", paste("in Baltimore City, MD and Los Angeles County, CA")))) +
        theme(plot.title = element_text(hjust = 0.5)) + # Center the plot title
        theme(plot.margin = unit(c(1,1,1,1), "cm")) + # Adjust plot margins
        scale_fill_brewer(palette = "Set1") +
        guides(fill = FALSE)

scc.vehicles <- SCC[grep("Mobile.*Vehicles", SCC$EI.Sector),  ]; # Pattern match mobile vehicles in SCC description
scc.vehicles.list <- unique(scc.vehicles$SCC); # Create motor vehicle lookup list by SCC
nei.vehicles <- subset(NEI, SCC %in% scc.vehicles.list); # Filter for motor vehicle sources
nei.vehicles <- nei.vehicles %>% filter(fips == "24510"| fips == "06037"); # Filter for Baltimore City or Los Angeles County
nei.vehicles$fips[nei.vehicles$fips == "24510"] <- "Baltimore";
nei.vehicles$fips[nei.vehicles$fips == "06037"] <- "Los Angeles";
nei.vehicles <- merge(x = nei.vehicles, y = scc.vehicles[, c("SCC", "SCC.Level.Two")], by = "SCC"); # Join in descriptive data on SCC codes
nei.vehicles <- nei.vehicles %>% group_by(fips, year, SCC.Level.Two) %>% summarize(Annual.Total = sum(Emissions));
nei.vehicles.total <- nei.vehicles %>% group_by(fips, year) %>% summarize(Annual.Total = sum(Annual.Total)) %>% mutate(SCC.Level.Two = "Total");
nei.vehicles <- bind_rows(nei.vehicles, nei.vehicles.total);
nei.vehicles$SCC.Level.Two <- factor(nei.vehicles$SCC.Level.Two, levels = c("Total", "Highway Vehicles - Diesel", "Highway Vehicles - Gasoline"));
ggplot(nei.vehicles, aes(x = factor(year), y = Annual.Total, fill = SCC.Level.Two)) +
        geom_bar(stat = "identity") +
        facet_grid(fips ~ SCC.Level.Two, scales = "free") + # Setup facets and allow scales to adjust to data in each location
        xlab("Year") +
        ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
        ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions from Motor Vehicle Sources", paste("in Baltimore City, MD and Los Angeles County, CA")))) +
        theme(plot.title = element_text(hjust = 0.5)) + # Center the plot title
        theme(plot.margin = unit(c(1,1,1,1), "cm")) + # Adjust plot margins
        scale_fill_brewer(palette = "Set1") +
        guides(fill = FALSE)

nei.vehicles.DT <- data.table(nei.vehicles)
yoyFunc <- function(x) {x/shift(x)}
yoy.cols <- c("Annual.Total")
nei.vehicles.DT <- nei.vehicles.DT[, paste0("Percent.Change.", yoy.cols) := lapply(.SD, yoyFunc), by = "fips,SCC.Level.Two", .SDcols = yoy.cols]
nei.vehicles.DT <- mutate(nei.vehicles.DT, Percent.Change.Annual.Total = Percent.Change.Annual.Total - 1)
ggplot(nei.vehicles.DT, aes(x = factor(year), y = Percent.Change.Annual.Total, fill = SCC.Level.Two)) +
        geom_bar(stat = "identity") +
        facet_grid(fips ~ SCC.Level.Two) +
        xlab("Year") +
        ylab(expression("% Change From Prior Measurement")) + 
        ggtitle(expression(atop("Percentage Change in Total Tons of PM"[2.5]*" Emissions from Motor Vehicle", paste("Sources in Baltimore City, MD and Los Angeles County, CA")))) +
        theme(plot.title = element_text(hjust = 0.5)) + # Center the plot title
        theme(plot.margin = unit(c(1,1,1,1), "cm")) + # Adjust plot margins
        scale_fill_brewer(palette = "Set1") +
        guides(fill = FALSE)

CAGR.df <- nei.vehicles.DT %>% 
        group_by(fips, SCC.Level.Two) %>% 
        summarize(N.Years = max(year) - min(year), 
                  Beginning.Qty = Annual.Total[which(year==min(year))],
                  Ending.Qty = Annual.Total[which(year==max(year))],
                  CAGR = ((Ending.Qty-Beginning.Qty)/N.Years)/Beginning.Qty);
CAGR.df;

summary(nei.vehicles.DT$Percent.Change.Annual.Total[nei.vehicles.DT$fips=="Baltimore"]);

summary(nei.vehicles.DT$Percent.Change.Annual.Total[nei.vehicles.DT$fips=="Los Angeles"]);
