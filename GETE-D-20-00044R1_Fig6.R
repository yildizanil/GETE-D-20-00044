#################################################################################
#             Thermo-hydrological behaviour of green infrastructure:            #
#                   a comparative field and laboratory study	                 #
#                         Yildiz, A. and Stirling, R.A.                         #
#      This script plots Fig. 6 of the manuscript Yildiz & Stirling (2020)      #
#                      written by Anil Yildiz, Dr. sc.                          #
#                         Created on 17.07.2020                                 #
#                       Last edited on 13.08.2020                               #
#################################################################################
#----------------------------------------------------------------------------------#
# Data used in this script is hosted on the data repository of Newcastle University
# To access the data file(s):
# https://doi.org/10.25405/data.ncl.12167487.v2
# https://doi.org/10.25405/data.ncl.12789323.v3
# To access the collection of datasets used in this manuscript, please visit
# https://doi.org/10.25405/data.ncl.c.5088419
#-------------------------------------------------------------------------------#
#                    Time frame presented in the paper                          #
#-------------------------------------------------------------------------------#
startdate <- as.POSIXct("2019-05-03 00:00:00",tz="UTC")
enddate <- as.POSIXct("2019-07-18 00:00:00",tz="UTC")
#-------------------------------------------------------------------------------#
#                      Importing data file from the repository                  #
#-------------------------------------------------------------------------------#
link_meteo <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194090/PLEXUS_Meteo_Daily.csv"
link_Z100 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194801/Z100.csv"
link_Z250 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194804/Z250.csv"
link_Z350 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194807/Z350.csv"
link_Z450 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194810/Z450.csv"
link_Z550 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194813/Z550.csv"
link_Z650 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194816/Z650.csv"
link_Z750 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194819/Z750.csv"
dataname <- c("Z100","Z250","Z350","Z450","Z550","Z650","Z750")

meteo <- read.csv(link_meteo,stringsAsFactors=F)
Z100 <- read.csv(link_Z100,stringsAsFactors=F)
Z250 <- read.csv(link_Z250,stringsAsFactors=F)
Z350 <- read.csv(link_Z350,stringsAsFactors=F)
Z450 <- read.csv(link_Z450,stringsAsFactors=F)
Z550 <- read.csv(link_Z550,stringsAsFactors=F)
Z650 <- read.csv(link_Z650,stringsAsFactors=F)
Z750 <- read.csv(link_Z750,stringsAsFactors=F)
#-------------------------------------------------------------------------------#
# Reformatting the date column
#-------------------------------------------------------------------------------#
day <- substr(meteo[,1],start=0,stop=2)
month <- substr(meteo[,1],start=4,stop=5)
year <- substr(meteo[,1],start=7,stop=10)
meteo[,1] <- paste0(year,"-",month,"-",day)
#-------------------------------------------------------------------------------#
#                         RGB codes of NGIF colours                             #
#-------------------------------------------------------------------------------#
green <- rgb(157/256,175/256,33/256)
blue <- rgb(32/256,137/256,203/256)
#-------------------------------------------------------------------------------#
#                         Axis locations and labels                             #
#-------------------------------------------------------------------------------#
axis_seq <- as.character(seq.Date(as.Date(startdate)-2,as.Date(enddate),"month"))
axis_names <- paste0("01-",substr(axis_seq,start=6,stop=7))
axis_months <- as.POSIXct(axis_seq,"UTC")
axis_days <- seq(startdate,enddate,60*60*24)
#----------------------------------------------------------------------------------#
# Defining file location
#----------------------------------------------------------------------------------#
file.loc <- "C:/Users/Anil/Newcastle University/PLEXUS Publications - General/GETE-D-20-00044/GETE-D-20-00044R1/Figures/"
#-------------------------------------------------------------------------------#
#                Generating a 1000-dpi figure in tiff format                    #
#-------------------------------------------------------------------------------#
tiff(paste0(file.loc,"GETE-D-20-00044R1_FIG6.tiff"),height=175,width=90,res=1000,units="mm")
#-------------------------------------------------------------------------------#
#       Generating a panel layout consisting of two subfigures and a legend     #
#-------------------------------------------------------------------------------#
layout(matrix(c(rep(1,10),seq(2,11,1),rep(12,10)),nrow=10,ncol=3),heights=c(5,rep(20,8),10),widths=c(0.5,8,0.5))
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
#-------------------------------------------------------------------------------#
#                           Legend of the figure                                #
#-------------------------------------------------------------------------------#
par(mar=c(0,1.25,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
legend("center",c("Vol. water content","Matric suction"),lwd=2,
       col=c(blue,green),ncol=2,bty="n",lty=c(1,3),adj=c(0,0.5))
#-------------------------------------------------------------------------------#
#                Daily rainfall and range of air temperature                    #
#-------------------------------------------------------------------------------#
par(mar=c(0.25,1.25,0.25,1.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,type="l",lwd=3,ylim=c(0,50),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
segments(x0=seq(startdate,enddate,60*60*24),y0=0,x1=seq(startdate,enddate,60*60*24),y1=50,col="gray87",lty=1)
segments(x0=startdate,y0=seq(0,50,10),x1=enddate,y1=seq(0,50,10),col="gray87",lty=1)
axis(1,tck=0.02,at=axis_days,labels=NA)
axis(1,tck=0.04,at=axis_months,labels=NA)
axis(2,tck=0.02)   
box()
for(i in 1:nrow(meteo))
{
   rect(xleft=as.POSIXct(meteo$Time[i],"UTC"),ybottom=0,
        xright=as.POSIXct(meteo$Time[i+1],"UTC"),ytop=meteo$Rain[i],border=T,col=8)  
}
par(las=0)
mtext("Rainfall [mm]",side=2,line=1)
text(enddate,50,"(a)",adj=c(1,1))

for(i in 1:6)
{
   par(mar=c(0.25,1.25,0.25,1.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
   plot(0,0,xlim=c(startdate,enddate),ylim=c(0,30),type="l",axes=F,xlab=NA,ylab=NA)
   segments(x0=seq(startdate,enddate,60*60*24),y0=0,x1=seq(startdate,enddate,60*60*24),y1=30,col="gray87")
   segments(x0=startdate,y0=seq(0,30,5),x1=enddate,y1=seq(0,30,5),col="gray87")
   axis(1,tck=0.02,at=axis_days,labels=NA)
   axis(1,tck=0.04,at=axis_months,labels=NA)
   axis(2,tck=0.02,at=c(0,10,20,30),labels=c(0,10,20,30))
   box()
   lines(VWC~as.POSIXct(Time,tz="UTC"),get(dataname[i]),lwd=2,col=blue)
   text(enddate,30,paste0("(",letters[i+1],")"),adj=c(1,1))
   par(new=T)
   plot(1,1,xlim=c(startdate,enddate),ylim=c(10,10000),type="l",axes=F,xlab=NA,ylab=NA,log="y")
   ticks <- seq(1,4,by=1)
   labels <- sapply(ticks, function(i) as.expression(bquote(10^ .(i))))
   axis(4,at=c(10,100,1000,10000),labels=labels,tck=0.04)
   axis(4,at=c(seq(10,90,10),seq(100,900,100),seq(1000,10000,1000)),labels=NA,tck=0.02)
   lines(Suction~as.POSIXct(Time,tz="UTC"),get(dataname[i]),lwd=2,col=green,lty=3)
   if(i==4){
      par(las=0)
      mtext(expression(paste("Volumetric water content,"," ",theta," ","[%]")),side=2,line=1)
      mtext(expression(paste("Matric suction,"," ",psi," ","[kPa]")),side=4,line=1)
   }
}

par(mar=c(0.25,1.25,0.25,1.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,xlim=c(startdate,enddate),ylim=c(0,30),type="l",axes=F,xlab=NA,ylab=NA)
segments(x0=seq(startdate,enddate,60*60*24),y0=0,x1=seq(startdate,enddate,60*60*24),y1=30,col="gray87")
segments(x0=startdate,y0=seq(0,30,5),x1=enddate,y1=seq(0,30,5),col="gray87")
axis(1,tck=0.02,at=axis_days,labels=NA)
axis(1,tck=0.04,at=axis_months,labels=axis_names)
axis(2,tck=0.02,at=c(0,10,20,30),labels=c(0,10,20,30))
box()
lines(VWC~as.POSIXct(Time,tz="UTC"),Z750,lwd=2,col=blue)
text(enddate,30,"(h)",adj=c(1,1))
par(new=T)
plot(1,1,xlim=c(startdate,enddate),ylim=c(10,10000),type="l",axes=F,xlab=NA,ylab=NA,log="y")
ticks <- seq(1,4,by=1)
labels <- sapply(ticks, function(i) as.expression(bquote(10^ .(i))))
axis(4,at=c(10,100,1000,10000),labels=labels,tck=0.04)
axis(4,at=c(seq(10,90,10),seq(100,900,100),seq(1000,10000,1000)),labels=NA,tck=0.02)
lines(Suction~as.POSIXct(Time,tz="UTC"),Z750,lwd=2,col=green,lty=3) 
par(las=0)
mtext("Time [dd-mm-2019]",side=1,line=1)
par(mar=c(0.25,1.25,0.25,1.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,xlim=c(startdate,enddate),ylim=c(0,30),type="l",axes=F,xlab=NA,ylab=NA)

dev.off()
