#################################################################################
#             Thermo-hydrological behaviour of green infrastructure:            #
#                   a comparative field and laboratory study	                 #
#                         Yildiz, A. and Stirling, R.A.                         #
#      This script plots Fig. 9 of the manuscript Yildiz & Stirling (2020)      #
#                      written by Anil Yildiz, Dr. sc.                          #
#                         Created on 17.07.2020                                 #
#                       Last edited on 13.08.2020                               #
#################################################################################
#----------------------------------------------------------------------------------#
# Data used in this script is hosted on the data repository of Newcastle University
# To access the data file(s): 
# https://doi.org/10.25405/data.ncl.12800669.v1
# https://doi.org/10.25405/data.ncl.12167487.v2
# To access the collection of datasets used in this manuscript, please visit
# https://doi.org/10.25405/data.ncl.c.5088419
#-------------------------------------------------------------------------------#
setwd("C:/Users/Anil/Newcastle University/PLEXUS Publications - General/GETE-D-20-00044/GETE-D-20-00044R1")
#-------------------------------------------------------------------------------#
#                         RGB codes of NGIF colours                             #
#-------------------------------------------------------------------------------#
green <- rgb(157/256,175/256,33/256)
blue <- rgb(32/256,137/256,203/256)
#-------------------------------------------------------------------------------#
#                    Time frame presented in the paper                          #
#-------------------------------------------------------------------------------#
startdate <- as.POSIXct("2019-05-03 00:00:00",tz="UTC")
enddate <- as.POSIXct("2019-07-18 00:00:00",tz="UTC")
#-------------------------------------------------------------------------------#
#                         Importing data sets
#-------------------------------------------------------------------------------#
link_Z250 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194804/Z250.csv"
link_Z350 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194807/Z350.csv"
link_Z550 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194813/Z550.csv"
link_Z750 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194819/Z750.csv"
link_tc <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24234173/PLEXUS_ThermalCond_L10.csv"

Z250 <- read.csv(link_Z250,stringsAsFactors=F)
Z350 <- read.csv(link_Z350,stringsAsFactors=F)
Z550 <- read.csv(link_Z550,stringsAsFactors=F)
Z750 <- read.csv(link_Z750,stringsAsFactors=F)
tc <- read.csv(link_tc,stringsAsFactors=F)
#-------------------------------------------------------------------------------#
# axis tick marks and labels                             
#-------------------------------------------------------------------------------#
axis_seq <- as.character(seq.Date(as.Date(startdate)-2,as.Date(enddate),"month"))
axis_names <- paste0("01-",substr(axis_seq,start=6,stop=7))
axis_months <- as.POSIXct(axis_seq,"UTC")
axis_days <- seq(startdate,enddate,60*60*24)
#-------------------------------------------------------------------------------#
# layout for panel plot                            
#-------------------------------------------------------------------------------#
panel <- matrix(NA,6,6)
panel[2,2:3] <- c(1,1)
panel[2,4:5] <- c(2,2)
panel[3,2:3] <- c(3,3)
panel[3,4:5] <- c(4,4)
panel[5,2:5] <- c(5,6,7,8)
panel[1,2:5] <- rep(9,4)
panel[4,2:5] <- rep(10,4)
panel[6,2:5] <- rep(11,4)
panel[1:3,1] <- c(12,12,12)
panel[4:6,1] <- c(13,13,13)
panel[1:3,6] <- c(14,14,14)
panel[4:6,6] <- c(15,15,15)
#----------------------------------------------------------------------------------#
# Defining file location
#----------------------------------------------------------------------------------#
file.loc <- "C:/Users/Anil/Newcastle University/PLEXUS Publications - General/GETE-D-20-00044/GETE-D-20-00044R1/Figures/"
#-------------------------------------------------------------------------------#
#                Generating a 1000-dpi figure in tiff format                    #
#-------------------------------------------------------------------------------#
tiff(paste0(file.loc,"GETE-D-20-00044R1_FIG9.tiff"),width=150,height=100,res=1000,units="mm")
#-------------------------------------------------------------------------------#
# setting the layout
#-------------------------------------------------------------------------------#
layout(panel,heights=c(5,25,25,5,35,5),widths=c(7.5,70,70,70,70,7.5))
#-------------------------------------------------------------------------------#
# figures 8a-8d
#-------------------------------------------------------------------------------#
for(i in 4:1)
{
   par(mar=c(1,1.25,0.25,1.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
   plot(0,0,type="l",lwd=3,ylim=c(0.6,1.6),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
   segments(x0=seq(startdate,enddate,60*60*24),y0=0.6,x1=seq(startdate,enddate,60*60*24),y1=1.6,col="gray87",lty=1)
   segments(x0=startdate,y0=seq(0.6,1.6,0.2),x1=enddate,y1=seq(0.6,1.6,0.2),col="gray87",lty=1)
   axis(2,tck=0.02)     
   axis(1,tck=0.02,at=axis_days,labels=NA)
   axis(1,tck=0.04,at=axis_months,labels=axis_names)
   box()
   lines(tc[,i+1]~as.POSIXct(as.character(tc[,1]),tz="UTC"),type="l",lwd=2,lty=3,col=green)
   par(new=T)
   plot(0,0,type="l",lwd=3,ylim=c(0,30),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
   axis(4,tck=0.02,at=seq(0,30,6))
   lines(VWC~as.POSIXct(Time,tz="UTC"),get(paste0(colnames(tc)[2:5][i])),col=blue,lwd=2)
   par(las=0)
   #mtext("Time [dd-mm]",side=1,line=1)
   #   mtext(expression(paste("Thermal conductivity, ",lambda," [W/m"^2,"]")),side=2,line=1.2)
   #  mtext(expression(paste("Volumetric water content, ",theta," [%]")),side=4,line=1.2)
   text(enddate,30,paste0("(",letters[5-i],")"),adj=c(1,1))
}
#-------------------------------------------------------------------------------#
# figures 8e-8h
#-------------------------------------------------------------------------------#
breaks <- seq(0.6,1.5,0.05)
for(i in 4:1)
{
   freq <- aggregate(tc[,i+1],list(breaks[findInterval(tc[,i+1],breaks)]),length)
   par(las=1,mar=c(1,1.25,0.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,pty="s")
   plot(0,0,ylim=c(0,450),xlim=c(0.6,1.5),axes=F,pch="",xlab=NA,ylab=NA)
   segments(x0=seq(0.6,1.50,0.05),y0=0,x1=seq(0.6,1.50,0.05),y1=450,col="gray87")
   segments(x0=0.6,y0=seq(0,450,50),x1=1.5,y1=seq(0,450,50),col="gray87")
   axis(1,tck=0.02,at=seq(0.6,1.4,0.2))
   axis(2,tck=0.02)
   par(new=T)
   box()
   for(j in 2:nrow(freq))
   {
      rect(xleft=freq[j-1,1],ybottom=0,
           xright=freq[j,1],ytop=freq[j-1,2],
           col=green,border=T)
   }
   segments(x0=0.6,y0=0,x1=1.5,y1=0,col=1)
   text(0.6,450,paste0("(",letters[9-i],")"),adj=c(0,1))
rm(list="freq")
}
#-------------------------------------------------------------------------------#
# legend
#-------------------------------------------------------------------------------#
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1,pty="m")
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
legend("center",c("Thermal conductivity","Volumetric water content"),
       col=c(green,blue),lty=c(3,1),lwd=c(2,2),hor=T,bty="n")
#-------------------------------------------------------------------------------#
# Figs 8a-8d x-axis label
#-------------------------------------------------------------------------------#
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1,pty="m")
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
text(0,0,"Time [dd-mm-2019]",adj=c(0.5,0.5))
#-------------------------------------------------------------------------------#
# Figs 8e-8h x-axis label
#-------------------------------------------------------------------------------#
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1,pty="m")
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
text(0,0,expression(paste("Thermal conductivity, "," ",lambda," ","[W/mK]")),adj=c(0.5,0.5))
#-------------------------------------------------------------------------------#
# Figs 8a-8d first y-axis label
#-------------------------------------------------------------------------------#
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1,pty="m")
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
par(las=0)
text(0,0,expression(paste("Thermal conductivity, "," ",lambda," ","[W/mK]")),
     adj=c(0.5,0.5),srt=90)
#-------------------------------------------------------------------------------#
# Figs 8e-8h first y-axis label
#-------------------------------------------------------------------------------#
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1,pty="m")
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
par(las=0)
text(0,0,"No. of observations [-]",
     adj=c(0.5,0.5),srt=90)
#-------------------------------------------------------------------------------#
# Figs 8a-8d second y-axis label
#-------------------------------------------------------------------------------#
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1,pty="m")
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
par(las=0)
text(0,0,expression(paste("Volumetric water content, "," ",theta," ","[%]")),
     adj=c(0.5,0.5),srt=90)
#-------------------------------------------------------------------------------#
# empty plot
#-------------------------------------------------------------------------------#
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1,pty="m")
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
dev.off()

