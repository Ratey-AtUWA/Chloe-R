# read data
zk202010 <- read.csv("2020-10-ZHOUKOU.csv")
zk202010$dt <- as.POSIXct(zk202010$time)
zk202010$day <- as.POSIXct(substr(zk202010$time,1,10))
zk202010$hour <- as.POSIXct(substr(zk202010$time,1,13),
                            format="%Y-%m-%d %H")
str(zk202010)

# split file into individual devices
for(i in unique(zk202010$device.ID)){
  assign(paste0("device",i), subset(zk202010, zk202010$device.ID==i))
}
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# HOW TO USE IT...
# search and replace device number then re-run all code below this point

# make daily mean for a device
dayz <- unique(device93$day)
device93byday <- data.frame(date=dayz, VOC=rep(NA,length(dayz)),
                            temp=rep(NA,length(dayz)),
                            humidity=rep(NA,length(dayz)))
for(i in 1:length(dayz)){
  device93byday[i,2] <- with(subset(device93, device93$day==dayz[i]),
                             mean(VOC, na.rm=T))
  device93byday[i,"temp"] <- with(subset(device93, device93$day==dayz[i]),
                             mean(VOC, na.rm=T))
  device93byday[i,"humidity"] <- with(subset(device93, device93$day==dayz[i]),
                             mean(VOC, na.rm=T))
}

# make hourly mean for a device
hrz <- unique(device93$hour)
device93byhour <- data.frame(date=hrz,
                             VOC=rep(NA,length(hrz)),
                             temp=rep(NA,length(hrz)),
                             humidity=rep(NA,length(hrz)))
for(i in 1:length(hrz)){
  device93byhour[i,"VOC"] <- with(subset(device93, device93$hour==hrz[i]),
                                  mean(VOC, na.rm=T))
  device93byhour[i,"temp"] <- with(subset(device93, device93$hour==hrz[i]),
                                 mean(temp, na.rm=T))
  device93byhour[i,"humidity"] <- with(subset(device93, device93$hour==hrz[i]),
                                 mean(humidity, na.rm=T))
}


# check different averaging periods with a plot
par(mfrow=c(2,1), mar=c(3.5,3.5,0.5,0.5), mgp=c(2,0.4,0), cex.lab=1.2, font.lab=2)
with(device93, plot(VOC ~ dt, pch=20, cex=0.5, log="y",
     xlab="Date/time"))
with(device93byday, lines(VOC ~ date, col="coral", lwd=3, type="o"))
with(device93byhour, lines(VOC ~ date, col="#0080ff80",
                           cex=0.85, pch=16, lwd=2, type="o"))
legend("topright", bty="n", cex=1.2, inset=c(0.02,0),
       title=expression(bold("device93")),
       legend=c("Raw data", "Daily mean", "Hourly mean"),
       pch=c(20,1,1), pt.cex=c(0.5,1,0.85), pt.lwd=c(1,3,2),
       lty=c(NA,1,1), lwd=c(NA,3,2),
       col=c(1,"coral","#0080ff80"), y.int=0.75)
with(device93byhour, plot(temp ~ date, type="l", col="gold3", lwd=2,
                    xlab="Date/time", ylab="Temperature °C or Humidity %", ylim=c(0,100)))
with(device93byhour, lines(humidity ~ date, col="darkcyan"))
legend("topleft", bty="n", cex=1.2, inset=c(0.02,0), y.int=0.75,
       title=expression(bold("device93: hourly means")),
       ncol=2, legend=c("Temperature °C", "Humidity %"),
       pch=NA, lwd=c(2,1), col = c("gold3","darkcyan"))
