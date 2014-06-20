#require(ggplot2)
require(reshape2)
require(data.table)

# set work dir and list file
#setwd("D:/doc/study/dataScientists/4-Exploratory Data Analysis/course project/course project 2")
#dir()
setwd("d:/study/git/Exploratory-Data-Analysis/courseProject2")

## This first line will likely take a few seconds. Be patient!
NEI <- data.table(readRDS("summarySCC_PM25.rds"))
SCC <- readRDS("Source_Classification_Code.rds")

# 6.Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

# guess: motor vehicle sources --> SCC$EI.Sector like : Mobile - On-Road * Vehicles
Vehicles_scc<-data.table(SCC[grepl("Mobile - On-Road(.)*Vehicles",SCC$EI.Sector,perl = TRUE,ignore.case=TRUE),])

# inter join 2 table with SCC_id
Vehicles_NEI_LA<-merge(NEI[NEI$fips=="06037",], Vehicles_scc, by="SCC")
Vehicles_NEI_Baltimore<-merge(NEI[NEI$fips=="24510",], Vehicles_scc, by="SCC")

# sum Emissions group by year
Vehicles_emssions_each_year_LA<-Vehicles_NEI_LA[,sum(Emissions),by=list(year,fips)]
Vehicles_emssions_each_year_Baltimore<-Vehicles_NEI_Baltimore[,sum(Emissions),by=list(year,fips)]

# sort by year
sorted_LA<-Vehicles_emssions_each_year_LA[do.call(order, Vehicles_emssions_each_year_LA)]
sorted_Baltimore<-Vehicles_emssions_each_year_Baltimore[do.call(order, Vehicles_emssions_each_year_Baltimore)]


png(filename = "plot6.png", width=480*2,height=480)
#qplot(year, V1, data=Vehicles_emssions_each_year, facets=.~city)+geom_line()+labs(y="total emssions")+labs(title="emissions from motor vehicle sources in 2 Cities")
par(mfrow=c(1,2))
plot(sorted_LA$year, sorted_LA$V1, type="l", xlab="year", ylab="Total Emissions(tons)", sub="Los Angeles County, California")
plot(sorted_Baltimore$year, sorted_Baltimore$V1, type="l", xlab="year", ylab="Total Emissions(tons)", sub="Baltimore City, Maryland")
dev.off()
