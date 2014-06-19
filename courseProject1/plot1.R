library(datasets)
#setwd("D:/doc/study/dataScientists/4-Exploratory Data Analysis/ExData_Plotting1")

rec=read.table("household_power_consumption.txt",header=TRUE,sep=";",na.strings = "?",colClasses=c("character", "character",rep("numeric",7)))
dt=rec[(rec$Date %in% c("1/2/2007","2/2/2007")), ]

png(filename = "plot1.png", width=480,height=480)
hist(dt$Global_active_power, col="orangered1", xlab = "Global Active Power(kilowatts)", main ="Global Active Power" )
dev.off()
