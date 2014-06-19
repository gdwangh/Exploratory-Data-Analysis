library(datasets)
setwd("D:/doc/study/dataScientists/4-Exploratory Data Analysis/git/ExData_Plotting1")

rec<-read.table("household_power_consumption.txt",header=TRUE,sep=";",na.strings = "?",colClasses=c("character", "character",rep("numeric",7)))
dt<-rec[(rec$Date %in% c("1/2/2007","2/2/2007")), ]

Sys.setlocale("LC_TIME", "English_Australia.1252")
dt$Time<-as.POSIXlt(strptime(paste(dt$Date, dt$Time), "%d/%m/%Y %H:%M:%S"))
dt$Date<-as.Date(strptime(dt$Date, "%d/%m/%Y"))

png(filename = "plot3.png", width=480,height=480)
with(dt, plot(dt$Time, dt$Sub_metering_1, type="l", col="black", xlab="",ylab="Energy sub metering"))
with(dt, points(dt$Time, dt$Sub_metering_2, type="l", col="red"))
with(dt, points(dt$Time, dt$Sub_metering_3, type="l", col="blue"))
legend("topright", col=c("black","red","blue"), lty=1, legend=c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"))


dev.off()
