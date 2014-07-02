# 关于�?6个图的进一步思考和学习

require(ggplot2)
require(reshape2)
require(data.table)

# set work dir and list file
#setwd("D:/doc/study/dataScientists/4-Exploratory Data Analysis/course project/course project 2")
#dir()
setwd("D:/workspace/dataScientists/4-ExploratoryDataAnalysis/tmp")

## This first line will likely take a few seconds. Be patient!
NEI <- data.table(readRDS("summarySCC_PM25.rds"))
SCC <- readRDS("Source_Classification_Code.rds")

# 6.Compare emissions from motor vehicle sources in Baltimore City
# with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?

# guess: motor vehicle sources --> SCC$EI.Sector like : Mobile - On-Road * Vehicles
Vehicles_scc<-data.table(SCC[grepl("Mobile - On-Road(.)*Vehicles",SCC$EI.Sector,perl = TRUE,ignore.case=TRUE),])

# get the records of LA and Baltimore
city_vehicles_NIE<-subset(NEI, (fips %in% c("06037","24510") & SCC %in% Vehicles_scc$SCC), select = c("year","fips", "Emissions"))


summary(city_vehicles_NIE)

# To see the statics every year
city_vehicles_NIE[, logE:=log10(Emissions)]
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
boxplot(logE ~ fips,city_vehicles_NIE)
boxplot(logE~year,city_vehicles_NIE, subset=fips=="24510",col="blue",boxwex = 0.25)
boxplot(logE~year,city_vehicles_NIE, subset=fips=="06037",col="red", at = 1:4 + 0.25,add=TRUE,boxwex = 0.25,xaxs="")

# To see the trends
city_sum<-city_vehicles_NIE[, sum(Emissions), by=list(year,fips)]
# it's said names()<- duplicate the data.frame and slow while setnames is faster for no dup
# names(city_sum)[3]<-"Emission_sum"
setnames(city_sum, "V1", "total_Emission")

# 两个城市的值差几倍，必须设置range否则放到一张图上时，无法显示完�?
rng <- range(city_sum$total_Emission, na.rm = T)
par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))

with(city_sum, plot(year, total_Emission, col="red",ylab="Total Emissions(tons)"),ylim = rng,pch=1)

with(subset(city_sum,fips=="06037"), lines(year, total_Emission, type="o", col="red"))
with(subset(city_sum,fips=="06037"), abline(lm(total_Emission~year)))

with(subset(city_sum,fips=="24510"), lines(year, total_Emission, type="o", col="blue"))
with(subset(city_sum,fips=="24510"), abline(lm(total_Emission~year)))
#legend("topright",legend=c("LA","Baltimore"),pch=1, col=c("red","blue"))


# 看不清楚，分在多张图
par(mfrow=c(1,2))
with(subset(city_sum,fips=="06037"), plot(year, total_Emission, type="b", xlab="year", ylab="Total Emissions(tons)", sub="Los Angeles County, California", col="red"))
with(subset(city_sum,fips=="06037"), abline(lm(total_Emission~year)))

with(subset(city_sum,fips=="24510"), plot(year, total_Emission, type="b", xlab="year", ylab="Total Emissions(tons)", sub="Baltimore City, Maryland",col="blue"))
with(subset(city_sum,fips=="24510"), abline(lm(total_Emission~year)))

#ggplot2
qplot(year,total_Emission, data=city_sum, facets=.~fips,geom=c("point","smooth"),method="lm")+geom_line()+labs(y="total emssions")+labs(title="emissions from motor vehicle sources in 2 Cities")

# 将ID转换为name，注意，id不同，排列顺序不同，可能对应的label会不�?
city_sum$city_name<-factor(city_sum$fips,labels=c("LA", "Baltimore"))
qplot(year,total_Emission, data=city_sum)+geom_line()+labs(title="emissions from motor vehicle sources in 2 Cities", y="total emssions")+geom_smooth(method="lm")+facet_grid(facets=.~city_name,labeller=label_parsed)


# 关于年增长率
cal_grouprate <- function(x){
  # x: year, n，计算n按增长率
  x1<-x[do.call(order, list(x$year))]
  x2<-x1[1:nrow(x1)-1,]
  x3<-x1[2:nrow(x1),]
  gr<-(x3$total_Emission - x2$total_Emission)/x2$total_Emission
  x1$growth_rate<-c(0,gr)
  return(x1)
}


x<-subset(city_sum,fips=="24510",select=c("year","total_Emission","city_name"))
bal_gr<-cal_grouprate(x)

x<-subset(city_sum,fips=="06037",select=c("year","total_Emission","city_name"))
LA_gr<-cal_grouprate(x)

rng <- range(bal_gr$growth_rate,LA_gr$growth_rate,na.rm = T)
par(mfrow = c(1, 2))
with(bal_gr, plot(year, growth_rate, type="o", xlab="year", ylab="Emission growth rate", main="Baltimore City, Maryland", col="blue",ylim = rng))
with(LA_gr, plot(year, growth_rate, type="o", xlab="year", ylab="Emission growth rate", main="Los Angeles County, California", col="red",ylim = rng))

# 折线图看不出感觉，改为bar�?
par(mfrow=c(1,1))
m_bal<-matrix(bal_gr$growth_rate, ncol=nrow(bal_gr),byrow=TRUE)
m_LA<-matrix(LA_gr$growth_rate, ncol=nrow(LA_gr),byrow=TRUE)
m<-rbind(m_bal,m_LA)
dimnames(m)<-list(c("Baltimore City","LA"), bal_gr$year)

barplot(m,beside=TRUE,col=c("red","blue"),main = "Emission growth Rates",legend.text=rownames(m),args.legend=list(x="bottomright"))

r1<-rbind(bal_gr,LA_gr)
r2<-r1[do.call(order, r1)]
qplot(year, growth_rate, data=r2, geom="bar", stat="identity",facets=.~city_name,fill=city_name)+labs(y="Emssion year Growth rate",title="Emissions from motor vehicle sourcess")

g<-ggplot(r, aes(x=year, y=growth_rate,fill=city_name))
g+geom_bar(stat="identity",position="dodge")+labs(y="Emssion year Growth rate",title="Emissions from motor vehicle sourcess")

# 将变化都按第一年折算，可能更容易看出来
jz<-city_sum[city_sum$fips=="24510" & year==1999,]$total_Emission
city_sum[city_sum$fips=="24510", multiple:=total_Emission/jz]

jz<-city_sum[city_sum$fips=="06037" & year==1999,]$total_Emission
city_sum[city_sum$fips=="06037", multiple:=total_Emission/jz]

# 2张图
par(mfrow=c(1,2))
with(city_sum[city_sum$fips=="24510",], plot(year, multiple, type="o", xlab="year", ylab="Emission growth rate", sub="Baltimore City, Maryland", col="blue"))
with(city_sum[city_sum$fips=="06037",], plot(year, multiple, type="o", xlab="year", ylab="Emission growth rate", sub="Los Angeles County, California", col="red"))

qplot(year, multiple, data=city_sum, geom="bar", stat="identity",facets=.~city_name,fill=city_name)+labs(y="Emssion year Growth rate",title="Emissions from motor vehicle sourcess")

g<-ggplot(city_sum, aes(x=year, y=multiple,fill=city_name))
g+geom_bar(stat="identity",position="dodge")+labs(y="Emssion year Growth rate",title="Emissions from motor vehicle sourcess")


# 1张图�?
par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))
rng<-range(city_sum$multiple)
with(city_sum, plot(year, multiple, col="red",ylab="Total Emissions(tons) growth multiple"),ylim = rng,pch=1)

with(subset(city_sum,fips=="06037"), lines(year, multiple, type="o", col="red"))
with(subset(city_sum,fips=="24510"), lines(year, multiple, type="o", col="blue"))

qplot(year,multiple, data=city_sum)+geom_line()+labs(title="emissions from motor vehicle sources in 2 Cities", y="total emssions")+facet_grid(facets=.~city_name)

