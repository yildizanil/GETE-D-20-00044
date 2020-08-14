#################################################################################
#             Thermo-hydrological behaviour of green infrastructure:            #
#                   a comparative field and laboratory study	                 #
#                         Yildiz, A. and Stirling, R.A.                         #
#      This script plots Fig. 4 of the manuscript Yildiz & Stirling (2020)      #
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
#                    Time frame presented in the paper                          #
#-------------------------------------------------------------------------------#
startdate <- as.POSIXct("2019-05-03 00:00:00",tz="UTC")
enddate <- as.POSIXct("2019-07-18 00:00:00",tz="UTC")
#-------------------------------------------------------------------------------#
#                      Importing data file from the repository                  #
#-------------------------------------------------------------------------------#
link <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194090/PLEXUS_Meteo_Daily.csv"
meteo <- read.csv(link,stringsAsFactors=F)
#-------------------------------------------------------------------------------#
# Reformatting the date column
#-------------------------------------------------------------------------------#
day <- substr(meteo[,1],start=0,stop=2)
month <- substr(meteo[,1],start=4,stop=5)
year <- substr(meteo[,1],start=7,stop=10)
meteo[,1] <- paste0(year,"-",month,"-",day)
#-------------------------------------------------------------------------------#
# Summary statistics
#-------------------------------------------------------------------------------#
numberofrainydays <- length(which(meteo$Rain>0.2))
sum(meteo$Rain)
summary(meteo$Rain)
summary(meteo$AirTemp_Max)
summary(meteo$AirTemp_Min)
summary(meteo$Humid_Mean)
summary(meteo$Humid_Min)
summary(meteo$Humid_Max)
summary(meteo$NetRad)
#-------------------------------------------------------------------------------#
# correlation matrix
#-------------------------------------------------------------------------------#
p <- as.data.frame(matrix(NA,nrow=11,ncol=11))
for(i in 2:11)
{
   for(j in (i+1):12)
   {
      p_val <- summary(lm(meteo[,i]~meteo[,j]))$coefficients[2,4]
      if(p_val<0.05)
      {
         p[i-1,j-1] <- round((summary(lm(meteo[,i]~meteo[,j]))$r.squared),2)
      }else{
         p[i-1,j-1] <- "NS"
      }
   }
}
row.names(p) <- colnames(meteo)[2:12]
colnames(p) <- colnames(meteo)[2:12]
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
#----------------------------------------------------------------------------------#
# Generating figure
#----------------------------------------------------------------------------------#
tiff(paste0(file.loc,"GETE-D-20-00044R1_FIG4.tiff"),height=105,width=150,res=1000,units="mm")
#-------------------------------------------------------------------------------#
#       Generating a panel layout consisting of two subfigures and a legend     #
#-------------------------------------------------------------------------------#
layout(matrix(c(1,2,3,4),nrow=4,ncol=1),heights=c(10,45,45,5))
#-------------------------------------------------------------------------------#
#                           Legend of the figure                                #
#-------------------------------------------------------------------------------#
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,xlab=NA,ylab=NA,axes=F,pch="")
legend("center",c("Rainfall","Relative Humidity","Air temperature","Net radiation"),
       bty="n",ncol=2,
       pch=c(15,NA,NA,NA),lwd=c(NA,2,2,2),
       lty=c(NA,2,1,3),col=c(8,blue,2,1),adj=c(0,0.5),xjust=0.5)
#-------------------------------------------------------------------------------#
#                Daily rainfall and range of air temperature                    #
#-------------------------------------------------------------------------------#
par(mar=c(1,2.25,0.25,2.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,type="l",lwd=3,ylim=c(0,50),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
segments(x0=seq(startdate,enddate,60*60*24),y0=0,x1=seq(startdate,enddate,60*60*24),y1=50,col="gray87",lty=1)
segments(x0=startdate,y0=seq(0,50,10),x1=enddate,y1=seq(0,50,10),col="gray87",lty=1)
axis(1,tck=0.02,at=axis_days,labels=NA)
axis(1,tck=0.04,at=axis_months,labels=axis_names)
axis(2,tck=0.02)     
for(i in 1:nrow(meteo))
{
   rect(xleft=as.POSIXct(meteo$Time[i],"UTC"),ybottom=0,
        xright=as.POSIXct(meteo$Time[i+1],"UTC"),ytop=meteo$Rain[i],border=T,col=8)  
}
text(enddate,50,"(a)",adj=c(1,1))
par(new=T)
plot(0,0,type="l",lwd=3,ylim=c(0,30),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
axis(4,tck=0.02,at=seq(0,30,6),labels=seq(0,30,6))
polygon(x=c(as.POSIXct(meteo$Time,"UTC"),rev(as.POSIXct(meteo$Time,"UTC"))),
        y=c(meteo$AirTemp_Max,rev(meteo$AirTemp_Min)),
        border=NA,col=rgb(1,0,0,0.2))
lines(meteo$AirTemp_Mean~as.POSIXct(meteo$Time,"UTC"),col=2,lwd=2)
par(las=0)
box()
mtext("Daily rainfall [mm]",side=2,line=1.25)
mtext("Air temperature [°C]",side=4,line=1.25)
#-------------------------------------------------------------------------------#
#                Net radiation and range of relative humidity                   #
#-------------------------------------------------------------------------------#
par(mar=c(1,2.25,0.25,2.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,type="l",lwd=3,ylim=c(0,100),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
segments(x0=seq(startdate,enddate,60*60*24),y0=0,x1=seq(startdate,enddate,60*60*24),y1=100,col="gray87",lty=1)
segments(x0=startdate,y0=seq(0,100,20),x1=enddate,y1=seq(0,100,20),col="gray87",lty=1)
axis(1,tck=0.02,at=axis_days,labels=NA)
axis(1,tck=0.04,at=axis_months,labels=axis_names)
axis(2,tck=0.02)
polygon(x=c(as.POSIXct(meteo$Time,"UTC"),rev(as.POSIXct(meteo$Time,"UTC"))),
        y=c(meteo$Humid_Max,rev(meteo$Humid_Min)),
        border=NA,col=rgb(32/256,137/256,203/256,0.2))
lines(meteo$Humid_Mean~as.POSIXct(meteo$Time,"UTC"),col=blue,lwd=2,lty=2)
text(enddate,100,"(b)",adj=c(1,1))
par(new=T)
plot(0,0,type="l",lwd=3,ylim=c(-100,400),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
axis(4,tck=0.02,at=seq(-100,400,100))
lines(meteo$NetRad~as.POSIXct(meteo$Time,"UTC"),col=1,lwd=2,lty=3)
par(las=0)
par(las=0)
mtext("Time [dd-mm-2019]",side=1,line=1)
mtext("Relative humidity [%]",side=2,line=1.25)
mtext(expression(paste("Net radiation [W/m"^2,"]")),side=4,line=1.25)
box()
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,xlab=NA,ylab=NA,axes=F,pch="")

dev.off()