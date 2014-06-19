require(reshape2)
require(data.table)

# set work dir and list file
setwd("D:/doc/study/dataScientists/4-Exploratory Data Analysis/course project/course project 2")
dir()

## This first line will likely take a few seconds. Be patient!
NEI <- data.table(readRDS("summarySCC_PM25.rds"))

# Q1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from 
# all sources for each of the years 1999, 2002, 2005, and 2008.

# sum PM2.5 emission of the all sources group by each year
total_emssions_each_year<-NEI[,sum(Emissions),by=year]

png(filename = "plot1.png", width=480,height=480)
plot(total_emssions_each_year$year, total_emssions_each_year$V1, type="l", xlab="year", ylab="", main="Total Emissions(tons)")
dev.off()
