# 1. RUN THIS FIRST SECTION BEFORE THE CODE UNDER HEADING 2. BELOW
# read data
zk202010 <- read.csv("2020-10-ZHOUKOU.csv")
zk202010$dt <- as.POSIXct(zk202010$time)
# generate days centered on half-day
zk202010$day <- as.POSIXct(paste0(substr(zk202010$time,1,10), " 12:00:00"))
# generate hours centered on half-hour
zk202010$hour <- as.POSIXct(paste0(substr(zk202010$time,1,13), ":30:00"))
str(zk202010)

# split file into individual devices
for(i in unique(zk202010$device.ID)){
  assign(paste0("device",i), subset(zk202010, zk202010$device.ID==i))
}
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# 2. HOW TO USE IT...
# search and replace device number then re-run all code below this point

# compare using hourly average to calculate daily average ####

# make daily mean for a device from raw data
dayz <- unique(device125$day)
device125byday <- data.frame(date=dayz, VOC=rep(NA,length(dayz)),
                            temp=rep(NA,length(dayz)),
                            humidity=rep(NA,length(dayz)))
for(i in 1:length(dayz)){
  device125byday[i,2] <- with(subset(device125, device125$day==dayz[i]),
                             mean(VOC, na.rm=T))
  device125byday[i,"temp"] <- with(subset(device125, device125$day==dayz[i]),
                             mean(VOC, na.rm=T))
  device125byday[i,"humidity"] <- with(subset(device125, device125$day==dayz[i]),
                             mean(VOC, na.rm=T))
}

# make hourly mean for a device
hrz <- unique(device125$hour)
device125byhour <- data.frame(date=hrz,
                             VOC=rep(NA,length(hrz)),
                             temp=rep(NA,length(hrz)),
                             humidity=rep(NA,length(hrz)))
for(i in 1:length(hrz)){
  device125byhour[i,"VOC"] <- with(subset(device125, device125$hour==hrz[i]),
                                  mean(VOC, na.rm=T))
  device125byhour[i,"temp"] <- with(subset(device125, device125$hour==hrz[i]),
                                 mean(temp, na.rm=T))
  device125byhour[i,"humidity"] <- with(subset(device125, device125$hour==hrz[i]),
                                 mean(humidity, na.rm=T))
}

# make daily mean for a device from hourly averages
device125byhour$day <-
  as.POSIXct(paste(substr(as.character(device125byhour$date),1,10)," 12:00:00"))
dayz <- unique(device125byhour$day)
device125byday2 <- data.frame(date=dayz, VOC=rep(NA,length(dayz)),
                             temp=rep(NA,length(dayz)),
                             humidity=rep(NA,length(dayz)))
for(i in 1:length(dayz)){
  device125byday2[i,2] <- with(subset(device125byhour,
                                      device125byhour$day==dayz[i]),
                              mean(VOC, na.rm=T))
  device125byday2[i,"temp"] <- with(subset(device125byhour,
                                           device125byhour$day==dayz[i]),
                                   mean(VOC, na.rm=T))
  device125byday2[i,"humidity"] <- with(subset(device125byhour,
                                               device125byhour$day==dayz[i]),
                                       mean(VOC, na.rm=T))
}


# check different averaging periods with a plot
par(mfrow=c(2,1), mar=c(3.5,3.5,0.5,0.5), mgp=c(2,0.4,0), cex.lab=1.2, font.lab=2)
with(device125, plot(VOC ~ dt, pch=20, cex=0.5, col="gray60", log="y",
     xlab="Date/time"))
with(device125byday, lines(VOC ~ date, col="tomato", lwd=3, type="o"))
with(device125byday2, lines(VOC~date, col="chocolate4", lwd=2, pch=3, cex=1.5,
                            lty=3, type="o"))
with(device125byhour, lines(VOC ~ date, col="#0080ff80",
                           cex=0.85, pch=16, lwd=2, type="o"))
legend("topright", bty="n", cex=1.2, inset=c(0.02,0),
       seg.len=3, title=expression(bold("device125")),
       legend=c("Raw data", "Daily mean", "Daily mean from hourly", "Hourly mean"),
       pch=c(20,1,3,1), pt.cex=c(0.5,1,1.5,0.85), pt.lwd=c(1,3,3,2),
       lty=c(NA,1,3,1), lwd=c(NA,3,2,2),
       col=c("gray60","tomato","chocolate4","#0080ff80"), y.int=0.75)
with(device125byhour, plot(temp ~ date, type="l", col="gold3", lwd=2,
                    xlab="Date/time", ylab="Temperature °C or Humidity %", ylim=c(0,100)))
with(device125byhour, lines(humidity ~ date, col="darkcyan"))
mtext("device125: hourly means", 3, -1.25, adj=0.02, font=2)
legend("top", bty="n", cex=1.2, y.int=0.75,
       ncol=2, legend=c("Temperature °C", "Humidity %"),
       pch=NA, lwd=c(2,1), col = c("gold3","darkcyan"))

# Compare different daily averaging methods directly:
par(mfrow=c(1,1))
plot(device125byday$VOC, device125byday2$VOC)
abline(0,1,col=2)
