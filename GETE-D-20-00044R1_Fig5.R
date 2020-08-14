#################################################################################
#             Thermo-hydrological behaviour of green infrastructure:            #
#                   a comparative field and laboratory study	                 #
#                         Yildiz, A. and Stirling, R.A.                         #
#      This script plots Fig. 5 of the manuscript Yildiz & Stirling (2020)      #
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
link_meteo <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194090/PLEXUS_Meteo_Daily.csv"
link_drainage <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194855/PLEXUS_Drainage_Daily.csv"
link_pet <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24232886/PLEXUS_PET_daily.csv"
meteo <- read.csv(link_meteo,stringsAsFactors=F)
drainage <- read.csv(link_drainage,stringsAsFactors=F)
pet <- read.csv(link_pet,stringsAsFactors=F)
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
tiff(paste0(file.loc,"GETE-D-20-00044R1_FIG5.tiff"),height=70,width=90,res=1000,units="mm")
#-------------------------------------------------------------------------------#
#       Generating a panel layout consisting of two subfigures and a legend     #
#-------------------------------------------------------------------------------#
layout(matrix(c(1,2),nrow=2,ncol=1),heights=c(5,65))
#-------------------------------------------------------------------------------#
#                           Legend of the figure                                #
#-------------------------------------------------------------------------------#
par(mar=c(0,2.25,0,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
legend("center",c("Rainfall","PET","Drainage"),pch=c(NA,22,22),col=c(1,1,1),
       pt.bg=c(NA,green,blue),ncol=3,bty="n",x.intersp=0.5,lwd=c(2,1,0),lty=c(1,NA,NA),adj=c(0,0.5))
#-------------------------------------------------------------------------------#
#                Daily rainfall and range of air temperature                    #
#-------------------------------------------------------------------------------#
par(mar=c(2,2.25,0.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,type="l",lwd=3,ylim=c(0,250),xlim=c(startdate-60*60*48,enddate),axes=F,xlab=NA,ylab=NA,pch="")
segments(x0=seq(startdate,enddate,60*60*24),y0=0,x1=seq(startdate,enddate,60*60*24),y1=250,col="gray87",lty=1)
segments(x0=startdate,y0=seq(0,250,50),x1=enddate,y1=seq(0,250,50),col="gray87",lty=1)
axis(1,tck=0.02,at=axis_days,labels=NA)
axis(1,tck=0.04,at=axis_months,labels=axis_names)
axis(2,tck=0.02)     
for(i in 1:nrow(meteo))
{
   rect(xleft=as.POSIXct(meteo[i,1],tz="UTC"),ybottom=0,
        xright=as.POSIXct(meteo[i+1,1],tz="UTC"),ytop=cumsum(pet[,2])[i],
        border=T,col=rgb(157/256,175/256,33/256,0.6),lwd=0.5)
   rect(xleft=as.POSIXct(meteo[i,1],tz="UTC"),ybottom=cumsum(pet[,2])[i],
        xright=as.POSIXct(meteo[i+1,1],tz="UTC"),ytop=cumsum(pet[,2])[i]+cumsum(drainage[,2])[i],
        border=NA,col=rgb(32/256,137/256,203/256,0.6),lwd=0.5)
}

box()
lines(cumsum(meteo$Rain)~as.POSIXct(meteo[,1],tz="UTC"),col=1,lwd=2)
par(las=0)
mtext("Time [dd-mm-2019]",side=1,line=1)
mtext("Water inflow/outflow [mm]",side=2,line=1.25)

dev.off()
