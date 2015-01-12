setwd("~/Projects/LaddeR/Exploratory_Data_Analysis/")
df <- read.csv("household_power_consumption.txt",sep=";",na.strings="?",as.is=T)
tdf <- df
df <- tdf
names(df)
summary(df)
sdf <- df[df$Date=="1/2/2007" | df$Date=="2/2/2007",]
# df$Time <- strptime(df$Time, "%H:%M:%S")
# df$Date <- as.Date(df$Date, "%d/%m/%Y")
sdf$datetime <- strptime(paste(sdf$Date, sdf$Time),"%d/%m/%Y%H:%M:%S")
sdf$weekday <- weekdays(sdf$datetime)

save(sdf, file="sdf.Rda")
load("sdf.Rda")

# Plot 1
png(file="Plot1.png",width = 480, height = 480)
hist(sdf$Global_active_power,col="red",breaks=12,
     main="Global Active Power",xlab="Global Active Power (kilowatts)",ylab="Frequency")
# dev.copy(png,file="Plot1.png",width = 480, height = 480)
dev.off()

# Plot 2
png(file="Plot2.png",width = 480, height = 480)
with(sdf,plot(datetime,Global_active_power,type="s",xlab="",ylab="Global Active Power (kilowatts)"))
# with(sdf,lines(datetime,Global_active_power,type="s"))

# dev.copy(png,file="Plot2.png",width=480,height=480)
dev.off()

# Plot 3
png(file="Plot3.png",width = 480, height = 480)
with(sdf,plot(datetime,Sub_metering_1,type="s",xlab="",ylab="Energy sub metering"))
with(sdf,lines(datetime,Sub_metering_3,type="s",xlab="",ylab="Energy sub metering",col="blue"))
with(sdf,lines(datetime,Sub_metering_2,type="s",xlab="",ylab="Energy sub metering",col="red"))
legend("topright",lty=1,col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
dev.off()

# Plot 4
png(file="Plot4.png",width = 480, height = 480)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(sdf, {
  plot(datetime, Global_active_power,type="s",xlab="",ylab="Global Active Power")
  plot(datetime, Voltage,type="s")
  plot(datetime,Sub_metering_1,type="s",xlab="",ylab="Energy sub metering")
    lines(datetime,Sub_metering_3,type="s",xlab="",ylab="Energy sub metering",col="blue")
    lines(datetime,Sub_metering_2,type="s",xlab="",ylab="Energy sub metering",col="red")
    legend("topright",lty=1,col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  plot(datetime, Global_reactive_power, type="s")
})
dev.off()