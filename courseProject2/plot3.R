require(ggplot2)
require(data.table)

# set work dir and list file
setwd("D:/doc/study/dataScientists/4-Exploratory Data Analysis/course project/course project 2")
dir()

## This first line will likely take a few seconds. Be patient!
NEI <- data.table(readRDS("summarySCC_PM25.rds"))

# 3.Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to 
# make a plot answer this question.

# get the data about  City, Maryland
DT<-NEI[NEI$fips=="24510",]

# sum Emissions group by year, type
Baltimore_emssions_each_year_type<-DT[,sum(Emissions),by=list(year,type)]

png(filename = "plot3.png", width=480,height=480)
qplot(year, V1, data=Baltimore_emssions_each_year_type,facet=.~type)+geom_line()+labs(y="total emssions")+facet_wrap(~type, nrow = 2, ncol = 2)
dev.off()

