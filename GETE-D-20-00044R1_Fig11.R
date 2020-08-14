#################################################################################
#             Thermo-hydrological behaviour of green infrastructure:            #
#                   a comparative field and laboratory study	                 #
#                         Yildiz, A. and Stirling, R.A.                         #
#      This script plots Fig. 11 of the manuscript Yildiz & Stirling (2020)      #
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
# RGB codes of NGIF colours
#-------------------------------------------------------------------------------#
green <- rgb(157/256,175/256,33/256)
blue <- rgb(32/256,137/256,203/256)
#-------------------------------------------------------------------------------#
# Time frame presented in the paper
#-------------------------------------------------------------------------------#
startdate <- as.POSIXct("2019-05-03 00:00:00",tz="UTC")
enddate <- as.POSIXct("2019-07-18 00:00:00",tz="UTC")
#-------------------------------------------------------------------------------#
# Importing data sets
#-------------------------------------------------------------------------------#
link_Z250 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194804/Z250.csv"
link_Z350 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194807/Z350.csv"
link_Z550 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194813/Z550.csv"
link_Z750 <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194819/Z750.csv"
link_tc <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24234173/PLEXUS_ThermalCond_L10.csv"
Z250_3h <- read.csv(link_Z250,stringsAsFactors=F)[seq(9,7296,12),]
Z350_3h <- read.csv(link_Z350,stringsAsFactors=F)[seq(9,7296,12),]
Z550_3h <- read.csv(link_Z550,stringsAsFactors=F)[seq(9,7296,12),]
Z750_3h <- read.csv(link_Z750,stringsAsFactors=F)[seq(9,7296,12),]
tc <- read.csv(link_tc,stringsAsFactors=F)
#-------------------------------------------------------------------------------#
# Functions to plot CKM and laboratory TCDC
#-------------------------------------------------------------------------------#
cond_sand <- function(vwc)
{k <- 0.261+(0.915+0.025*(vwc-6.04))/(1+exp(-1.64*(vwc-6.04)))
return(k)
}
tl2016 <- function(sand,silt){
   0.0668+0.395*silt-1.958*sand+14.973*(sand^2)-26.120*(sand^3)+13.926*(sand^4)
}
lambda_solid <- function(q){
   (2^(1-q))*(7.70^q)
}
lambda_sat <- function(solid,n){
   (solid^(1-n))*(0.594^n)
}
sat <- round(lambda_sat(round(lambda_solid(round(tl2016(0.99,0.01),2)),2),0.466),2)
dry <- round(-1.92*0.466+1.18,3)
vwc <- ((100*((seq(0,1,0.001)*0.874)/2.68))*1.43)
ke <- (3.55*seq(0,1,0.001))/(1+(3.55-1)*seq(0,1,0.001))
ckm <- (((sat-dry)*ke)+dry)
#-------------------------------------------------------------------------------#
# Calculating mean and standard deviations of the thermal conductivity measurements
# at 0.5 water content steps
#-------------------------------------------------------------------------------#
val <- seq(0,30,0.5)
which_250 <- findInterval(Z250_3h$VWC,val)
which_350 <- findInterval(Z350_3h$VWC,val)
which_550 <- findInterval(Z550_3h$VWC,val)
which_750 <- findInterval(Z750_3h$VWC,val)

mean_250 <- aggregate(tc[,5],list(val[which_250]),mean)
sd_250 <- aggregate(tc[,5],list(val[which_250]),sd)

mean_350 <- aggregate(tc[,4],list(val[which_350]),mean)
sd_350 <- aggregate(tc[,4],list(val[which_350]),sd)

mean_550 <- aggregate(tc[,3],list(val[which_550]),mean)
sd_550 <- aggregate(tc[,3],list(val[which_550]),sd)

mean_750 <- aggregate(tc[,2],list(val[which_750]),mean)
sd_750 <- aggregate(tc[,2],list(val[which_750]),sd)
#-------------------------------------------------------------------------------#
# Calculating coefficient of variation of the thermal conductivity measurements
# at 0.5 water content steps
#-------------------------------------------------------------------------------#
cov_250 <- 100*sd_250[,2]/mean_250[,2]
cov_350 <- 100*sd_350[,2]/mean_350[,2]
cov_550 <- 100*sd_550[,2]/mean_550[,2]
cov_750 <- 100*sd_750[,2]/mean_750[,2]
#----------------------------------------------------------------------------------#
# Defining file location
#----------------------------------------------------------------------------------#
file.loc <- "C:/Users/Anil/Newcastle University/PLEXUS Publications - General/GETE-D-20-00044/GETE-D-20-00044R1/Figures/"
#-------------------------------------------------------------------------------#
#                Generating a 1000-dpi figure in tiff format                    #
#-------------------------------------------------------------------------------#
tiff(paste0(file.loc,"GETE-D-20-00044R1_FIG11.tiff"),width=90,height=90,res=1000,units="mm")

layout(matrix(c(1,2,3),3,1),heights=c(5,10,75))
par(mar=c(0,1.25,0,0),family="serif",ps=10,cex=1,cex.main=1,las=1,mgp=c(0.1,0.1,0),pty="m")
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F)
legend("center",c("Laboratory TCDC","Côté and Konrad"),
       lwd=2,col=c(1,8),hor=T,bty="n",x.intersp = 0.5)

par(mar=c(0,1.25,0,0),family="serif",ps=10,cex=1,cex.main=1,las=1,mgp=c(0.1,0.1,0),pty="m")
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F)
legend("center",c("@ 250 mm","@ 350 mm","@ 550 mm","@ 750 mm"),
       col=1,pt.bg=green,pch=c(22,25,21,24),ncol=2,bty="n")

par(mar=c(2,2.25,0.25,0.25),family="serif",ps=10,cex=1,cex.main=1,las=1,mgp=c(0.1,0.1,0))
plot(0,0,xlim=c(0,30),ylim=c(0,2),pch="",xlab=NA,ylab=NA,axes=F)
segments(x0=seq(0,30,2.5),y0=0,x1=seq(0,30,2.5),y1=2,col=rgb(0,0,0,0.1))
segments(x0=0,y0=seq(0,2,0.25),x1=30,y1=seq(0,2,0.25),col=rgb(0,0,0,0.1))
axis(1,tck=0.02)
axis(2,tck=0.02)
box()

lines(cond_sand(seq(0,30,0.1))~seq(0,30,0.1),col=1,lwd=2)
lines(ckm[vwc<30.1]~vwc[vwc<30.1],lwd=2,col=8)

for(i in 1:40)
{
   segments(x0=sd_250[i,1]+0.25,y0=mean_250[i,2],
            x1=sd_250[i,1]+0.25,y1=mean_250[i,2]+sd_250[i,2],col=1)
   segments(x0=sd_250[i,1]+0.25,y0=mean_250[i,2],
            x1=sd_250[i,1]+0.25,y1=mean_250[i,2]-sd_250[i,2],col=1)
   segments(x0=sd_350[i,1]+0.25,y0=mean_350[i,2],
            x1=sd_350[i,1]+0.25,y1=mean_350[i,2]+sd_350[i,2],col=1)
   segments(x0=sd_350[i,1]+0.25,y0=mean_350[i,2],
            x1=sd_350[i,1]+0.25,y1=mean_350[i,2]-sd_350[i,2],col=1)
   segments(x0=sd_550[i,1]+0.25,y0=mean_550[i,2],
            x1=sd_550[i,1]+0.25,y1=mean_550[i,2]+sd_550[i,2],col=1)
   segments(x0=sd_550[i,1]+0.25,y0=mean_550[i,2],
            x1=sd_550[i,1]+0.25,y1=mean_550[i,2]-sd_550[i,2],col=1)
   segments(x0=sd_750[i,1]+0.25,y0=mean_750[i,2],
            x1=sd_750[i,1]+0.25,y1=mean_750[i,2]+sd_750[i,2],col=1)
   segments(x0=sd_750[i,1]+0.25,y0=mean_750[i,2],
            x1=sd_750[i,1]+0.25,y1=mean_750[i,2]-sd_750[i,2],col=1)
}

points((mean_250[,1]+0.25),(mean_250[,2]),pch=22,col=1,bg=green,cex=0.6)
points((mean_350[,1]+0.25),(mean_350[,2]),pch=25,col=1,bg=green,cex=0.6)
points((mean_550[,1]+0.25),(mean_550[,2]),pch=21,col=1,bg=green,cex=0.6)
points((mean_750[,1]+0.25),(mean_750[,2]),pch=24,col=1,bg=green,cex=0.6)

par(las=0)
mtext(expression(paste("Volumetric water content,"," ",theta," ","[%]")),side=1,line=1)
mtext(expression(paste("Thermal conductivity,"," ",lambda," ","[W/mK]")),side=2,line=1.25)

dev.off()