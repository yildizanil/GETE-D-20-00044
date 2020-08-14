#################################################################################
#             Thermo-hydrological behaviour of green infrastructure:            #
#                   a comparative field and laboratory study	                 #
#                         Yildiz, A. and Stirling, R.A.                         #
#      This script plots Fig. 10 of the manuscript Yildiz & Stirling (2020)      #
#                      written by Anil Yildiz, Dr. sc.                          #
#                         Created on 17.07.2020                                 #
#                       Last edited on 13.08.2020                               #
#################################################################################
#----------------------------------------------------------------------------------#
# Data used in this script is hosted on the data repository of Newcastle University
# To access the data file(s): 
# https://doi.org/10.25405/data.ncl.12167487.v2
# https://doi.org/10.25405/data.ncl.12609602.v1
# To access the collection of datasets used in this manuscript, please visit
# https://doi.org/10.25405/data.ncl.c.5088419
#-------------------------------------------------------------------------------#
#                    Time frame presented in the paper                          #
#-------------------------------------------------------------------------------#
startdate <- as.POSIXct("2019-05-03 00:00:00",tz="UTC")
enddate <- as.POSIXct("2019-07-18 00:00:00",tz="UTC")
#----------------------------------------------------------------------------------#
# Data used in this script is hosted on the data repository of Newcastle University
#----------------------------------------------------------------------------------#
link_143 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/23663864/PLEXUS_Sand_143.csv"
link_143_scanning <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/23663867/PLEXUS_Sand_143_Scanning.csv"   
link_Z250 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194804/Z250.csv"
link_Z350 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194807/Z350.csv"
Z250 <- read.csv(link_Z250,stringsAsFactors=F)
Z350 <- read.csv(link_Z350,stringsAsFactors=F)
#----------------------------------------------------------------------------------#
# RGB codes of NGIF colours 
#----------------------------------------------------------------------------------#
green <- rgb(157/256,175/256,33/256)
blue <- rgb(32/256,137/256,203/256)
#----------------------------------------------------------------------------------#
# van Genuchten (1980) equation
#----------------------------------------------------------------------------------#
vg <- function(suction,ws,wr,a,n)
{(((ws-wr)/((((a*suction)^n)+1)^(1-(1/n))))+wr)}
inverse_vg <- function(vwc)
{(((((41.5-5.92)/(vwc-5.92))^(5.64/(5.64-1)))-1)^(1/5.64))*(1/0.25)}
#----------------------------------------------------------------------------------#
# Importing data files
#----------------------------------------------------------------------------------#
sand_143 <- read.csv(link_143,stringsAsFactors=F)
sand_143_scanning <- read.csv(link_143_scanning,stringsAsFactors=F)
#----------------------------------------------------------------------------------#
# Defining file location
#----------------------------------------------------------------------------------#
file.loc <- "C:/Users/Anil/Newcastle University/PLEXUS Publications - General/GETE-D-20-00044/GETE-D-20-00044R1/Figures/"
#-------------------------------------------------------------------------------#
#                Generating a 1000-dpi figure in tiff format                    #
#-------------------------------------------------------------------------------#
tiff(paste0(file.loc,"GETE-D-20-00044R1_FIG10.tiff"),width=90,height=90,res=1000,units="mm")

layout(matrix(c(1,2,3,4,5,6,7),nrow=7,ncol=1),heights=c(5,5,5,5,5,5,60))

par(mar=c(0,2.25,0,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
legend("center",c("Laboratory"),lwd=2,col=1,lty=c(1),bty="n")

par(mar=c(0,2.25,0,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
legend("center",c("Laboratory - scannning"),lwd=2,col=1,lty=c(2),bty="n")

par(mar=c(0,2.25,0,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
legend("center",c(expression(paste(psi," > 20 kPa @ 250 mm"))),
       col=c(blue),lty=c(1),lwd=2,ncol=1,bty="n")

par(mar=c(0,2.25,0,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
legend("center",c(expression(paste(psi," > 20 kPa @ 250 mm"))),
       col=c(green),lty=c(2),lwd=2,ncol=1,bty="n")

par(mar=c(0,2.25,0,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
legend("center",c(expression(paste(psi," < 20 kPa @ 250 mm - Uncorrected"))),
       col=c(blue),lty=c(3),lwd=2,ncol=1,bty="n")

par(mar=c(0,2.25,0,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
legend("center",c(expression(paste(psi," < 20 kPa @ 250 mm - Corrected"))),
       col=c(8),lty=c(1),lwd=3,ncol=1,bty="n")

par(mar=c(2,2.25,0.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0.1,0.1,xlim=c(0.1,10000),ylim=c(0,45),axes=F,log="x",xlab=NA,ylab=NA,pch="")
segments(x0=c(seq(0.1,0.9,0.1),seq(1,9,1),seq(10,100,10),seq(100,1000,100),seq(1000,10000,1000)),y0=0,
         x1=c(seq(0.1,0.9,0.1),seq(1,9,1),seq(10,100,10),seq(100,1000,100),seq(1000,10000,1000)),y1=45,col=rgb(0,0,0,0.2))
segments(x0=0.1,y0=seq(0,45,5),x1=10000,y1=seq(0,45,5),col=rgb(0,0,0,0.2))
axis(2,tck=0.01)
ticks <- seq(-1,4,by=1)
labels <- sapply(ticks, function(i) as.expression(bquote(10^ .(i))))
axis(side=1, at=c(seq(0.1,1,0.1),seq(1,10,1),seq(10,100,10),seq(100,1000,100),seq(1000,10000,1000)),labels=NA,tck=0.01)
axis(side=1,at=c(0.1,1,10,100,1000,10000),labels=labels,tck=0.02)

lines(VWC~Suction,sand_143,lwd=2,col=1,lty=1)
lines(VWC~GeoMean,sand_143_scanning,lwd=2,col=1,lty=2)
lines(VWC~Suction,Z250[Z250$Suction>20,],lwd=2,col=blue)
lines(VWC~Suction,Z350[Z350$Suction>20,],lwd=2,col=green,lty=2)
lines(VWC~Suction,Z250[Z250$Suction<20,],lwd=1,col=blue,lty=3)
lines(inverse_vg(Z250[Z250$Suction<20,3])~Z250[Z250$Suction<20,3],col=8,lty=1,lwd=3)
arrows(x0=min(Z250[Z250$Suction<20,2]),y0=max(Z250[Z250$Suction<20,3],na.rm=T),
       x1=30,y1=20,length = 0.05)
text(30,20,"Uncorrected \n@ 250 mm",adj=c(0,0.5))
arrows(x0=max(inverse_vg(Z250[Z250$Suction<20,2])),y0=max(inverse_vg(Z250[Z250$Suction<20,3]),na.rm=T),
       x1=2,y1=13,length = 0.05)
text(2,13,"Corrected \n@ 250 mm",adj=c(1,0.5))

par(las=0,ps=10)
mtext(expression(paste("Matric suction, ",psi," [kPa]")),side=1,line=1)
mtext(expression(paste("Volumetric water content, ",theta," [%]")),side=2,line=1.25)
box()
dev.off()



