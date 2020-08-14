#################################################################################
#             Thermo-hydrological behaviour of green infrastructure:            #
#                   a comparative field and laboratory study	                 #
#                         Yildiz, A. and Stirling, R.A.                         #
#      This script plots Fig. 3 of the manuscript Yildiz & Stirling (2020)      #
#                      written by Anil Yildiz, Dr. sc.                          #
#                         Created on 17.07.2020                                 #
#                       Last edited on 13.08.2020                               #
#################################################################################
#----------------------------------------------------------------------------------#
# Data used in this script is hosted on the data repository of Newcastle University
# To access the data file(s): 
# https://doi.org/10.25405/data.ncl.12789842.v1
# To access the collection of datasets used in this manuscript, please visit
# https://doi.org/10.25405/data.ncl.c.5088419
#----------------------------------------------------------------------------------#
link_tcdc <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/24194579/PLEXUS_Sand_TCDC.csv"
#-------------------------------------------------------------------------------#
# importing data files and changing column names
#-------------------------------------------------------------------------------#
sand_therm <-  read.csv(link_tcdc,stringsAsFactors=F)
#-------------------------------------------------------------------------------#
# obtaining the thermal conductivity (K) and volumetric water content
#-------------------------------------------------------------------------------#
y <- sand_therm$ThermCond
x <- sand_therm$VWC
#-------------------------------------------------------------------------------#
# curve fitting using the formula from Yildiz (2020) - doi:10.1680/jgele.20.00055
#-------------------------------------------------------------------------------#
fitmodel <- nls(y~(d+((a+k*(x-c))/(1+exp(-b*(x-c))))),start=list(a=1,b=2.2,c=5,d=0.2,k=7))
summary(fitmodel)
#-------------------------------------------------------------------------------#
# defining TCDC function using the fitting parameters and literature values
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
dry <- round(-1.92*0.466+1.18,2)
vwc <- ((100*((seq(0,1,0.001)*0.874)/2.68))*1.43)
ke <- (3.55*seq(0,1,0.001))/(1+(3.55-1)*seq(0,1,0.001))
ckm <- (((sat-dry)*ke)+dry)
sr <- (((sand_therm$VWC*2.68)/1.43)/0.874)/100
#-------------------------------------------------------------------------------#
#                         RGB codes of NGIF colours                             #
#-------------------------------------------------------------------------------#
green <- rgb(157/256,175/256,33/256)
blue <- rgb(32/256,137/256,203/256)
#----------------------------------------------------------------------------------#
# Defining file location
#----------------------------------------------------------------------------------#
file.loc <- "C:/Users/Anil/Newcastle University/PLEXUS Publications - General/GETE-D-20-00044/GETE-D-20-00044R1/Figures/"
#----------------------------------------------------------------------------------#
# Generating figure
#----------------------------------------------------------------------------------#
tiff(paste0(file.loc,"GETE-D-20-00044R1_FIG3.tiff"),width=90,height=70,res=1000,units="mm")

layout(matrix(c(1,2,3),3,1),heights=c(5,5,60))
par(mar=c(0,1.25,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
legend("center",c("Côté and Konrad"),
       col=c(8),lty=c(1),lwd=c(2),bty="n",hor=T,x.intersp = 0.5)

par(mar=c(0,1.25,0,0),mgp=c(0.3,0.3,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
legend("center",c("Measured","Fitted"),col=c(blue,1),lty=c(1,2),lwd=2,hor=T,bty="n")

par(mar=c(2,2.25,0.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,type="l",lwd=3,xlim=c(0,40),ylim=c(0,2),axes=F,xlab=NA,ylab=NA,pch="")
segments(x0=seq(0,40,5),y0=0,x1=seq(0,40,5),y1=2,col="gray87",lty=1)
segments(x0=0,y0=seq(0,2,0.25),x1=40,y1=seq(0,2,0.25),col="gray87",lty=1)
axis(1,tck=0.02)
axis(2,tck=0.02)     
box()

lines(ckm[vwc<40]~vwc[vwc<40],col=8,lwd=2,lty=1)
lines(sand_therm$ThermCond~sand_therm$VWC,lwd=2,col=blue)
lines(cond_sand(seq(0,40,0.01))~seq(0,40,0.01),lty=2,lwd=2,col=1)

par(las=0)
mtext(expression(paste("Thermal conductivity, ",lambda," [W/mK]")),side=2,line=1.25)
mtext(expression(paste("Volumetric water content, ",theta," [%]")),side=1,line=1)

dev.off()
#----------------------------------------------------------------------------------#
# Calculation root mean square error
#----------------------------------------------------------------------------------#
ke_test <- (3.55*sr)/(1+(3.55-1)*sr)
ckm_test <- ((sat-dry)*ke_test+dry)

rmse_yildiz <- sqrt(sum((sand_therm$ThermCond-cond_sand(sand_therm$VWC))^2)/1133)
rmse_ckm <- sqrt(sum((sand_therm$ThermCond-ckm_test)^2)/1133)
