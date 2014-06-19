require(reshape2)
require(data.table)

# set work dir and list file
setwd("D:/doc/study/dataScientists/4-Exploratory Data Analysis/course project/course project 2")
dir()

## This first line will likely take a few seconds. Be patient!
NEI <- data.table(readRDS("summarySCC_PM25.rds"))

# 2.Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# get the data about  City, Maryland
DT<-NEI[NEI$fips=="24510",]

# sum Emissions group by year
Baltimore_emssions_each_year<-DT[,sum(Emissions),by=year]


png(filename = "plot2.png", width=480,height=480)
plot(Baltimore_emssions_each_year$year, Baltimore_emssions_each_year$V1, type="l", xlab="year", ylab="Total Emissions(tons)")
dev.off()

