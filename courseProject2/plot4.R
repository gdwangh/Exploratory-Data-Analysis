require(ggplot2)
require(data.table)

# set work dir and list file
#setwd("D:/doc/study/dataScientists/4-Exploratory Data Analysis/course project/course project 2")
#dir()
setwd("d:/study/git/Exploratory-Data-Analysis/courseProject2")


## This first line will likely take a few seconds. Be patient!
NEI <- data.table(readRDS("summarySCC_PM25.rds"))
SCC <- readRDS("Source_Classification_Code.rds")

# 4.Across the United States, how have emissions from coal combustion-related sources changed from 1999â€?2008?

# guess: coal combustion-related sources --> SCC$EI.Sector like : Fuel Comb * Coal
coal_scc<-data.table(SCC[grepl("Fuel Comb(.)*Coal",SCC$EI.Sector,perl = TRUE,ignore.case=TRUE),])

# inter join 2 table with SCC_id
coal_NEI<-merge(NEI, coal_scc, by="SCC")

# sum Emissions group by year, type
coal_emssions_each_year<-coal_NEI[,sum(Emissions),by=year]

png(filename = "plot4.png", width=480,height=480)
qplot(year, V1, data=coal_emssions_each_year)+geom_line()+labs(y="total emssions")+labs(title="emissions from coal combustion-related sources in united state")
dev.off()
