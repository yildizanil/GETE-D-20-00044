#################################################################################
#             Thermo-hydrological behaviour of green infrastructure:            #
#                   a comparative field and laboratory study	                 #
#                         Yildiz, A. and Stirling, R.A.                         #
#      This script plots Fig. 2 of the manuscript Yildiz & Stirling (2020)      #
#                      written by Anil Yildiz, Dr. sc.                          #
#                         Created on 17.07.2020                                 #
#                       Last edited on 13.08.2020                               #
#################################################################################
#----------------------------------------------------------------------------------#
# Data used in this script is hosted on the data repository of Newcastle University
# To access the data file(s): 
# https://doi.org/10.25405/data.ncl.12609602.v1
# To access the collection of datasets used in this manuscript, please visit
# https://doi.org/10.25405/data.ncl.c.5088419
#----------------------------------------------------------------------------------#
link_163 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/23663861/PLEXUS_Sand_163.csv"
link_136 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/23663858/PLEXUS_Sand_136.csv"
link_143 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/23663864/PLEXUS_Sand_143.csv"
link_143_scanning <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/23663867/PLEXUS_Sand_143_Scanning.csv"   
#----------------------------------------------------------------------------------#
# Importing data files
#----------------------------------------------------------------------------------#
sand_136 <- read.csv(link_136,stringsAsFactors=F)
sand_143 <- read.csv(link_143,stringsAsFactors=F)
sand_143_scanning <- read.csv(link_143_scanning,stringsAsFactors=F)
sand_163 <- read.csv(link_163,stringsAsFactors=F)
#----------------------------------------------------------------------------------#
# Obtaining volumetric water content and matric suction values from data sets
#----------------------------------------------------------------------------------#
y_136 <- sand_136$VWC
x_136 <- sand_136$Suction
y_143 <- sand_143$VWC
x_143 <- sand_143$Suction
y_163 <- sand_163$VWC
x_163 <- sand_163$Suction
#----------------------------------------------------------------------------------#
# van Genuchten (1980) equation
#----------------------------------------------------------------------------------#
vg <- function(suction,ws,wr,a,n)
{(((ws-wr)/((((a*suction)^n)+1)^(1-(1/n))))+wr)}
#----------------------------------------------------------------------------------#
# Fitting the measured data to van Genuchten (1980) equation
#----------------------------------------------------------------------------------#
fit_136 <- nls(y_136~vg(x_136,ws,wr,a,n),start=list(ws=42,wr=5,a=0.33,n=5))
summary(fit_136)
fit_143 <- nls(y_143~vg(x_143,ws,wr,a,n),start=list(ws=42,wr=5,a=0.33,n=5))
summary(fit_143)
fit_163 <- nls(y_163~vg(x_163,ws,wr,a,n),start=list(ws=42,wr=5,a=0.33,n=5))
summary(fit_163)
#----------------------------------------------------------------------------------#
# RGB codes of NGIF colours 
#----------------------------------------------------------------------------------#
green <- rgb(157/256,175/256,33/256)
blue <- rgb(32/256,137/256,203/256)
#----------------------------------------------------------------------------------#
# Defining file location
#----------------------------------------------------------------------------------#
file.loc <- "C:/Users/Anil/Newcastle University/PLEXUS Publications - General/GETE-D-20-00044/GETE-D-20-00044R1/Figures/"
#----------------------------------------------------------------------------------#
# Generating figure
#----------------------------------------------------------------------------------#
tiff(paste0(file.loc,"GETE-D-20-00044R1_FIG2.tiff"),width=90,height=80,res=1000,units="mm")

layout(matrix(c(1,2),nrow=2,ncol=1),heights=c(20,60))
par(mar=c(0,2.25,0,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
legend("center",c("1.36 - Measured","1.43 - Measured","1.63 - Measured","1.43 - Scanning","1.36 - Fitted","1.43 - Fitted","1.63 - Fitted"),
       col=c(blue,blue,blue,blue,1,1,1),lty=c(2,1,3,1,2,1,3),
       lwd=2,ncol=2,bty="n",pch=c(NA,NA,NA,15,NA,NA,NA))

par(mar=c(2,2.25,0.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0.1,0.1,xlim=c(0.1,100),ylim=c(0,45),axes=F,log="x",xlab=NA,ylab=NA,pch="")
segments(x0=c(seq(0.1,0.9,0.1),seq(1,9,1),seq(10,100,10)),y0=0,x1=c(seq(0.1,0.9,0.1),seq(1,9,1),seq(10,100,10)),y1=45,col="gray87")
segments(x0=0.1,y0=seq(0,45,5),x1=100,y1=seq(0,45,5),col="gray87")
axis(2,tck=0.01)
ticks <- seq(-1,2,by=1)
labels <- sapply(ticks, function(i) as.expression(bquote(10^ .(i))))
axis(side=1, at=c(seq(0.1,1,0.1),seq(1,10,1),seq(10,100,10)),labels=NA,tck=0.01)
axis(side=1,at=c(0.1,1,10,100),labels=labels,tck=0.02)
box()

lines(vg(seq(0.1,100,0.1),42.3,5.94,0.27,7.42)~seq(0.1,100,0.1),lwd=2,col=rgb(0,0,0,0.5),lty=2)
lines(vg(seq(0.1,100,0.1),41.6,5.92,0.25,5.64)~seq(0.1,100,0.1),lwd=2,col=rgb(0,0,0,0.5),lty=1)
lines(vg(seq(0.1,100,0.1),41.5,6.19,0.22,5.72)~seq(0.1,100,0.1),lwd=2,col=rgb(0,0,0,0.5),lty=3)
lines(y_136~x_136,lwd=2,col=blue,lty=2)
lines(y_143~x_143,lwd=2,col=blue)
lines(y_163~x_163,lwd=2,col=blue,lty=3)

lines(VWC~GeoMean,sand_143_scanning,col=blue,lwd=2)
points(VWC~GeoMean,sand_143_scanning[seq(1,1500,50),],col=blue,cex=0.5,pch=15)

par(las=0,ps=10)
mtext(expression(paste("Matric suction, ",psi," [kPa]")),side=1,line=1)
mtext(expression(paste("Volumetric water content, ",theta," [%]")),side=2,line=1.25)
box()

dev.off()