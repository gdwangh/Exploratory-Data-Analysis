library(datasets)
#setwd("D:/doc/study/dataScientists/4-Exploratory Data Analysis/ExData_Plotting1")

rec<-read.table("household_power_consumption.txt",header=TRUE,sep=";",na.strings = "?",colClasses=c("character", "character",rep("numeric",7)))
dt<-rec[(rec$Date %in% c("1/2/2007","2/2/2007")), ]

Sys.setlocale("LC_TIME", "English_Australia.1252")
dt$Time<-as.POSIXlt(strptime(paste(dt$Date, dt$Time), "%d/%m/%Y %H:%M:%S"))
dt$Date<-as.Date(strptime(dt$Date, "%d/%m/%Y"))

png(filename = "plot2.png", width=480,height=480)
plot(dt$Time, dt$Global_active_power, type="l", xlab="", ylab="Global Active Power(kilowatts)")
dev.off()
