require(ggplot2)
require(data.table)

# set work dir and list file
setwd("D:/doc/study/dataScientists/4-Exploratory Data Analysis/course project/course project 2")
dir()

## This first line will likely take a few seconds. Be patient!
NEI <- data.table(readRDS("summarySCC_PM25.rds"))
SCC <- readRDS("Source_Classification_Code.rds")

# 5.How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? 

# guess: motor vehicle sources --> SCC$EI.Sector like : Mobile - On-Road Gaso * Vehicles
Vehicles_scc<-data.table(SCC[grepl("Mobile - On-Road(.)*Vehicles",SCC$EI.Sector,perl = TRUE,ignore.case=TRUE),])

# inter join 2 table with SCC_id
Vehicles_NEI<-merge(NEI[NEI$fips=="24510",], Vehicles_scc, by="SCC")

# sum Emissions group by year
Vehicles_emssions_each_year<-Vehicles_NEI[,sum(Emissions),by=year]

png(filename = "plot5.png", width=480,height=480)
qplot(year, V1, data=Vehicles_emssions_each_year)+geom_line()+labs(y="total emssions")
dev.off()
