#################################################################################
#             Thermo-hydrological behaviour of green infrastructure:            #
#                   a comparative field and laboratory study	                 #
#                         Yildiz, A. and Stirling, R.A.                         #
#      This script plots Fig. 8 of the manuscript Yildiz & Stirling (2020)      #
#                      written by Anil Yildiz, Dr. sc.                          #
#                         Created on 17.07.2020                                 #
#                       Last edited on 13.08.2020                               #
#################################################################################
#----------------------------------------------------------------------------------#
# Data used in this script is hosted on the data repository of Newcastle University
# To access the data file(s):
# https://doi.org/10.25405/data.ncl.12613880.v1
# To access the collection of datasets used in this manuscript, please visit
# https://doi.org/10.25405/data.ncl.c.5088419
#-------------------------------------------------------------------------------#
# Data from 03.05.2019 until 18.07.2019
#-------------------------------------------------------------------------------#
startdate <- as.POSIXct("2019-05-03 00:00:00",tz="UTC")
enddate <- as.POSIXct("2019-07-18 00:00:00",tz="UTC")
#-------------------------------------------------------------------------------#
# Importing data file from the repository
#-------------------------------------------------------------------------------#
link_soiltemp <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194099/PLEXUS_SoilTemp_15Min.csv"
link_airtemp <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194087/PLEXUS_AirTemp_15Min.csv"
soiltemp <- read.csv(link_soiltemp,stringsAsFactors=F)
airtemp <- read.csv(link_airtemp,stringsAsFactors=F)
#-------------------------------------------------------------------------------#
# correcting the date format of airtemp and soiltemp
#-------------------------------------------------------------------------------#
day <-substr(soiltemp[,1],start=0,stop=2)
month <- substr(soiltemp[,1],start=4,stop=5)
year <- substr(soiltemp[,1],start=7,stop=10)
hour <- substr(soiltemp[,1],start=12,stop=13)
min <- substr(soiltemp[,1],start=15,stop=16)
soiltemp[,1] <- paste0(year,"-",month,"-",day," ",hour,":",min)
airtemp[,1] <- paste0(year,"-",month,"-",day," ",hour,":",min)
#-------------------------------------------------------------------------------#
# x-axis tick marks and labels
#-------------------------------------------------------------------------------#
axis_seq <- as.character(seq.Date(as.Date(startdate)-2,as.Date(enddate),"month"))
axis_names <- paste0("01-",substr(axis_seq,start=6,stop=7))
axis_months <- as.POSIXct(axis_seq,"UTC")
axis_days <- seq(startdate,enddate,60*60*24)
#-------------------------------------------------------------------------------#
# performing linear regressions
#-------------------------------------------------------------------------------#
slope <- NA
intercept <- NA
r2 <- NA
for(i in 1:13)
{
   fit <- lm(soiltemp[,i+1]~airtemp[,2])
   
   slope[i] <- fit$coefficients[2]
   intercept[i] <- fit$coefficients[1]
   r2[i] <-  round(as.numeric(summary(fit)[8]),2)
   rm(list="fit")
}
#-------------------------------------------------------------------------------#
# calculating daily maximums, minimums and difference between these values
#-------------------------------------------------------------------------------#
day_steps <- seq(startdate,enddate-1,60*60*24)
which_step <- findInterval(as.POSIXct(soiltemp[,1],"UTC"),day_steps)
daily_max <- as.data.frame(matrix(NA,nrow=76,ncol=14),stringsAsFactors = F)
daily_min <- as.data.frame(matrix(NA,nrow=76,ncol=14),stringsAsFactors = F)
daily_diff <- as.data.frame(matrix(NA,nrow=76,ncol=14),stringsAsFactors = F)

daily_max[,1] <- as.character(day_steps)
daily_min[,1] <- as.character(day_steps)

for(j in 2:14)
{
   daily_max[,j] <- aggregate(soiltemp[,j],list(day_steps[which_step]),max,na.rm=T)[,2]   
   daily_min[,j] <- aggregate(soiltemp[,j],list(day_steps[which_step]),min,na.rm=T)[,2]
}
daily_diff[,2:14] <- daily_max[,2:14]-daily_min[,2:14]
air_diff <- aggregate(airtemp[,2],list(day_steps[which_step]),max,na.rm=T)[,2]-aggregate(airtemp[,2],list(day_steps[which_step]),min,na.rm=T)[,2]
#-------------------------------------------------------------------------------#
# setting depths of measurements, time axis and temperature contours
#-------------------------------------------------------------------------------#
depth <- as.numeric(substr(colnames(soiltemp[2:14]),start=2,stop=4))
time <- seq(startdate+60*15,enddate-60*15,60*60*0.25)
temp <- seq(1,26,1)
#----------------------------------------------------------------------------------#
# Defining file location
#----------------------------------------------------------------------------------#
file.loc <- "C:/Users/Anil/Newcastle University/PLEXUS Publications - General/GETE-D-20-00044/GETE-D-20-00044R1/Figures/"
#-------------------------------------------------------------------------------#
#                Generating a 1000-dpi figure in tiff format                    #
#-------------------------------------------------------------------------------#
tiff(paste0(file.loc,"GETE-D-20-00044R1_FIG8.tiff"),width=150,height=150,res=1000,units="mm")

layout(matrix(c(1,2,3,4,4,5,6,6,6),nrow=3,ncol=3,byrow = T),widths=c(50,50,50),heights=c(50,90,10))

par(mar=c(2,2.25,0.25,0.25),mgp=c(0.1,0.1,0),ps=10,family="serif",cex=1,cex.main=1,las=1)
plot(0,0,xlim=c(0,10),ylim=c(950,0),axes=F,xlab=NA,ylab=NA,pch="")
segments(x0=seq(0,10,1),y0=0,x1=seq(0,10,1),y1=950,col="gray87")
segments(x0=0,y0=seq(0,950,50),x1=10,y1=seq(0,950,50),col="gray87")
axis(1,tck=0.02)
axis(2,tck=0.02,at=seq(0,900,150))
box()
points(depth~intercept,pch=16)
par(las=0)
mtext("Depth [mm]",side=2,line=1.25)
mtext("Intercept [-]",side=1,line=1)
text(0,0,"(a)",adj=c(0,1))

par(mar=c(2,2.25,0.25,0.25),mgp=c(0.1,0.1,0),ps=10,family="serif",cex=1,cex.main=1,las=1)
plot(0,0,xlim=c(0,1),ylim=c(950,0),axes=F,xlab=NA,ylab=NA,pch="")
segments(x0=seq(0,1,0.1),y0=0,x1=seq(0,1,0.1),y1=950,col="gray87")
segments(x0=0,y0=seq(0,950,50),x1=1,y1=seq(0,950,50),col="gray87")
axis(1,tck=0.02)
axis(2,tck=0.02,at=seq(0,900,150))
box()
points(depth~slope,pch=16)
par(las=0)
mtext("Depth [mm]",side=2,line=1.25)
mtext("Slope [-]",side=1,line=1)
text(0,0,"(b)",adj=c(0,1))

par(mar=c(2,2.25,0.25,0.25),mgp=c(0.1,0.1,0),ps=10,family="serif",cex=1,cex.main=1,las=1)
plot(0,0,xlim=c(0,1),ylim=c(950,0),axes=F,xlab=NA,ylab=NA,pch="")
segments(x0=seq(0,1,0.1),y0=0,x1=seq(0,1,0.1),y1=950,col="gray87")
segments(x0=0,y0=seq(0,950,50),x1=1,y1=seq(0,950,50),col="gray87")
axis(1,tck=0.02)
axis(2,tck=0.02,at=seq(0,900,150))
box()
points(depth~r2,pch=16)
par(las=0)
mtext("Depth [mm]",side=2,line=1.25)
mtext(expression(paste("R"^2," ","[-]")),side=1,line=1)
text(0,0,"(c)",adj=c(0,1))

par(mar=c(2,2.25,0.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,xlim=c(startdate,enddate),ylim=c(950,-70),type="l",axes=F,xlab=NA,ylab=NA)
box()
for(j in 1:length(depth))
{
   for(i in 1:7295)
   {
      rect(xleft=as.POSIXct(airtemp[i,1],"UTC"),ybottom=-50,
           xright=as.POSIXct(airtemp[i+1,1],"UTC"),ytop=-10,
           col=heat.colors(length(temp))[(length(temp)+1)-findInterval(airtemp[i,2],temp)],border=NA)
      
      rect(xleft=as.POSIXct(soiltemp[i,1],"UTC"),ybottom=depth[j]-20,
           xright=as.POSIXct(soiltemp[i+1,1],"UTC"),ytop=depth[j]+20,
           col=heat.colors(length(temp))[(length(temp)+1)-findInterval(soiltemp[i,j+1],temp)],border=NA)
   }
}

rect(xleft=startdate,ybottom=-10,xright=enddate,ytop=-50,border = T)
rect(xleft=startdate,ybottom=950,xright=enddate,ytop=150,border = T)
rect(xleft=startdate,ybottom=150,xright=enddate,ytop=0,border = T)
par(las=0)
mtext("Depth [mm]",side=2,line=1.25)
mtext("Time [dd-mm-2019]",side=1,line=1)
text(enddate,-68,"Air temperature [°C]",adj=c(1,0))
text(startdate,-68,"(d)",adj=c(0,0))
par(las=1)
segments(x0=seq(startdate,enddate,60*60*24),y0=0,x1=seq(startdate,enddate,60*60*24),y1=950,col=rgb(0,0,0,0.1))
segments(x0=startdate,y0=seq(0,950,50),x1=enddate,y1=seq(0,950,50),col=rgb(0,0,0,0.1))
axis(1,tck=0.02,labels=NA,at=axis_days)
axis(1,tck=0.03,labels=axis_names,at=axis_months)
axis(2,tck=0.02,at=seq(0,900,150))

par(mar=c(2,2.25,0.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,xlim=c(0,10),ylim=c(950,-70),type="l",axes=F,xlab=NA,ylab=NA)
segments(x0=seq(0,10,1),y0=950,x1=seq(0,10,1),y1=0,col="gray87")
segments(x0=0,y0=seq(0,950,50),x1=10,y1=seq(0,950,50),col="gray87")
axis(1,tck=0.02)
axis(2,tck=0.02,at=seq(0,900,150))
box()
for(k in 1:13)
{
points(mean(daily_diff[,k+1]),depth[k],pch=16,cex=0.5)   
arrows(x0=mean(daily_diff[,k+1]),y0=depth[k],x1=mean(daily_diff[,k+1])+sd(daily_diff[,k+1]),y1=depth[k],
       length=0.025,angle = 90)
arrows(x0=mean(daily_diff[,k+1]),y0=depth[k],x1=mean(daily_diff[,k+1])-sd(daily_diff[,k+1]),y1=depth[k],
       length=0.025,angle = 90)
}
points(mean(air_diff),-30,pch=16,cex=0.5)
arrows(x0=mean(air_diff),y0=-30,x1=mean(air_diff)+sd(air_diff),y1=-30,
       length=0.025,angle = 90)
arrows(x0=mean(air_diff),y0=-30,x1=mean(air_diff)-sd(air_diff),y1=-30,
       length=0.025,angle = 90)
rect(xleft=0,ybottom=-10,xright=10,ytop=-50,border = T)
text(10,-68,"Air temperature [°C]",adj=c(1,0))
text(0,-68,"(e)",adj=c(0,0))
par(las=0)
mtext(expression(paste("T"[max],"-T"[min]," [°C]")),side=1,line=1)
mtext("Depth [mm]",side=2,line=1.25)

par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="",ylim=c(0,1),xlim=c(0,1))
legend("right",c(rev(as.character(temp))),col=1,pch=22,pt.bg = heat.colors(length(temp)),
       y.intersp=0.8,bty="n",pt.cex = 1.3,ncol=13,x.intersp = 0.6)
text(0,0.5,"Soil temp. \n[°C]",adj=c(0,0.5))

dev.off()
