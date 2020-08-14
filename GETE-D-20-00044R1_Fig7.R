#################################################################################
#             Thermo-hydrological behaviour of green infrastructure:            #
#                   a comparative field and laboratory study	                 #
#                         Yildiz, A. and Stirling, R.A.                         #
#      This script plots Fig. 7 of the manuscript Yildiz & Stirling (2020)      #
#                      written by Anil Yildiz, Dr. sc.                          #
#                         Created on 17.07.2020                                 #
#                       Last edited on 13.08.2020                               #
#################################################################################
#----------------------------------------------------------------------------------#
# Data used in this script is hosted on the data repository of Newcastle University
# To access the data file(s):
# https://doi.org/10.25405/data.ncl.12789323.v3
# To access the collection of datasets used in this manuscript, please visit
# https://doi.org/10.25405/data.ncl.c.5088419
#-------------------------------------------------------------------------------#
# Time frame of the heavy rainfall event
#-------------------------------------------------------------------------------#
startdate <- as.POSIXct("2019-06-12 00:00:00",tz="UTC")
enddate <- as.POSIXct("2019-06-14 00:00:00",tz="UTC")
#-------------------------------------------------------------------------------#
# importing data sets
#-------------------------------------------------------------------------------#
link_heavyrain <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24241967/PLEXUS_HeavyRain_15Min.csv"
heavyrain <- read.csv(link_heavyrain,stringsAsFactors=F)

link_Z100 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194801/Z100.csv"
link_Z250 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194804/Z250.csv"
link_Z350 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194807/Z350.csv"
link_Z450 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194810/Z450.csv"
link_Z550 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194813/Z550.csv"
link_Z650 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194816/Z650.csv"
link_Z750 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194819/Z750.csv"
dataname <- c("Z100","Z250","Z350","Z450","Z550","Z650","Z750")

for(i in 1:length(dataname))
{
   data <- read.csv(get(paste0("link_",dataname[i])),stringsAsFactors=F)
   data <- data[which(as.POSIXct(data[,1],"UTC")>startdate-1&as.POSIXct(data[,1],"UTC")<enddate-1),]
   assign(dataname[i],data)
   rm(list="data")
}
#-------------------------------------------------------------------------------#
#                         RGB codes of NGIF colours                             #
#-------------------------------------------------------------------------------#
green <- rgb(157/256,175/256,33/256)
blue <- rgb(32/256,137/256,203/256)
#-------------------------------------------------------------------------------#
#                         Axis locations and labels                             #
#-------------------------------------------------------------------------------#
axis_seq <- as.character(seq(startdate,enddate,60*60*6))
axis_names <- substr(axis_seq,start=12,stop=16)
axis_months <- as.POSIXct(axis_seq,"UTC")
axis_days <- seq(startdate,enddate,60*60)
time <- seq(startdate,enddate,60*15)
#----------------------------------------------------------------------------------#
# Defining file location
#----------------------------------------------------------------------------------#
file.loc <- "C:/Users/Anil/Newcastle University/PLEXUS Publications - General/GETE-D-20-00044/GETE-D-20-00044R1/Figures/"
#-------------------------------------------------------------------------------#
#                Generating a 1000-dpi figure in tiff format                    #
#-------------------------------------------------------------------------------#
tiff(paste0(file.loc,"GETE-D-20-00044R1_FIG7.tiff"),height=75,width=150,res=1000,units="mm")
#-------------------------------------------------------------------------------#
#       Generating a panel layout
#-------------------------------------------------------------------------------#
layout(matrix(c(1,2,3),nrow=3,ncol=1),heights=c(5,10,60))
#-------------------------------------------------------------------------------#
#                           Legend of the figure                                #
#-------------------------------------------------------------------------------#
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
legend("center",c("Rainfall","Drainage"),pch=22,col=0,pt.bg=c(8,blue),hor=T,bty="n")
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
legend("center",c("@ 100 mm","@ 250 mm","@ 350 mm","@450 mm","@ 550 mm","@650 mm","@ 750 mm"),
       col=c(1,1,1,blue,blue,blue,blue),lty=c(1,2,3,1,2,3,1),lwd=c(2,2,2,2,2,2,4),ncol=4,bty="n")
#-------------------------------------------------------------------------------#
# Fig. 7
#-------------------------------------------------------------------------------#
par(mar=c(2,2,0.2,2),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,xlim=c(startdate,enddate),ylim=c(0,90),type="l",axes=F,xlab=NA,ylab=NA)
segments(x0=seq(startdate,enddate,60*60),y0=0,x1=seq(startdate,enddate,60*60),y1=90,col="gray87")
segments(x0=startdate,y0=seq(0,90,15),x1=enddate,y1=seq(0,90,15),col="gray87")
axis(4,tck=0.02,at=seq(0,90,15),labels=seq(0,90,15))
for(i in 1:192)
{
   rect(xleft=time[i],ybottom=0,
        xright=time[i+1],ytop=heavyrain$Rain[i],col=rgb(0,0,0,0.2),border=NA)
   rect(xleft=time[i],ybottom=0,
        xright=time[i+1],ytop=heavyrain$Drainage[i],col=rgb(32/256,137/256,203/256,0.4),border=NA)

}
par(new=T)
plot(0,0,xlim=c(startdate,enddate),ylim=c(0,30),type="l",axes=F,xlab=NA,ylab=NA)
axis(2,tck=0.02)
axis(1,tck=0.02,labels=NA,at=axis_days)
axis(1,tck=0.04,labels=axis_names,at=axis_months)
box()

lines(Z250$VWC~as.POSIXct(Z250$Time,tz="UTC"),col=1,lwd=2,lty=2)
lines(Z350$VWC~as.POSIXct(Z350$Time,tz="UTC"),col=1,lwd=2,lty=3)

lines(Z450$VWC~as.POSIXct(Z450$Time,tz="UTC"),col=blue,lwd=2,lty=1)
lines(Z550$VWC~as.POSIXct(Z550$Time,tz="UTC"),col=blue,lwd=2,lty=2)
lines(Z650$VWC~as.POSIXct(Z650$Time,tz="UTC"),col=blue,lwd=2,lty=3)
lines(Z750$VWC~as.POSIXct(Z750$Time,tz="UTC"),col=blue,lwd=4,lty=1)
lines(Z100$VWC~as.POSIXct(Z100$Time,tz="UTC"),col=1,lwd=2,lty=1)

par(las=0)
mtext("2019-06-12",side=1,at=as.POSIXct("2019-06-12 12:00:00",tz="UTC"),line=1)
mtext("2019-06-13",side=1,at=as.POSIXct("2019-06-13 12:00:00",tz="UTC"),line=1)
mtext(expression(paste("Volumetric water content, ",theta," [%]")),side=2,line=1)
mtext("Cumulative rainfall / drainage [mm]",side=4,line=1)
dev.off()
