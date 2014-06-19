require(ggplot2)
require(data.table)

# set work dir and list file
setwd("D:/doc/study/dataScientists/4-Exploratory Data Analysis/course project/course project 2")
dir()

## This first line will likely take a few seconds. Be patient!
NEI <- data.table(readRDS("summarySCC_PM25.rds"))
SCC <- readRDS("Source_Classification_Code.rds")

# 6.Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

# guess: motor vehicle sources --> SCC$EI.Sector like : Mobile - On-Road Gaso * Vehicles
Vehicles_scc<-data.table(SCC[grepl("Mobile - On-Road(.)*Vehicles",SCC$EI.Sector,perl = TRUE,ignore.case=TRUE),])

# inter join 2 table with SCC_id
Vehicles_NEI<-merge(NEI[NEI$fips %in% c("24510","06037")], Vehicles_scc, by="SCC")

# sum Emissions group by year
Vehicles_emssions_each_year<-Vehicles_NEI[,sum(Emissions),by=list(year,fips)]
Vehicles_emssions_each_year[Vehicles_emssions_each_year$fips=="24510", "city"]<-"Baltimore City, Maryland"
Vehicles_emssions_each_year[Vehicles_emssions_each_year$fips=="06037", "city"]<-"Los Angeles County, California"

png(filename = "plot6.png", width=480*2,height=480)
qplot(year, V1, data=Vehicles_emssions_each_year, facets=.~city)+geom_line()+labs(y="total emssions from motor vehicle sources")
dev.off()
