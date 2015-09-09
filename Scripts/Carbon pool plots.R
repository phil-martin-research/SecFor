############################################################
#script to draw plots of predicted changes in carbon pools##
#with time since disturbance in tropical secondary forests##
############################################################

#Name:Phil Martin
#Date: 18/07/2013

#load up the packages

library(RODBC)
library(ggplot2)
library(plyr)
library(reshape)

#import data from model predictions
AGB_pred<-read.table("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics/Model predictions - Biomass.csv",header=T,sep=",")
AGB_pred$Age<-seq(0.5,82,.1)
AGB_pred$Disturbance<-"All combined"
Soil_pred<-read.table("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics/Model predictions - Soil C.csv",header=T,sep=",")
BGB_pred<-read.table("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics/Model predictions - BGB.csv",header=T,sep=",")
colnames(AGB_pred)
colnames(BGB_pred)
Soil_pred$Disturbance<-"NULL"
#combine datasets
All<-rbind(AGB_pred,BGB_pred)
head(All)

#produce upper and lower CIs for all predictions
All$Upper<-All$Prediction+(1.96*All$SE)
All$Lower<-All$Prediction-(1.96*All$SE)
All<-subset(All,Age<=82)
All

#bring in data

#connect to database
sec <- odbcConnect("Secondary/Degraded forests")
sqlTables(sec)
Sites<- sqlFetch(sec, "Site characteristics")
colnames(Sites)<-c("ID","ID2","ID3","Disturbance")
Sites<-subset(Sites,Disturbance!="Logging")
Sites<-subset(Sites,Disturbance!="Fire")
Sites<-subset(Sites,Disturbance!="Agroforestry")
Sites<-subset(Sites,Disturbance!="Plantation")

#import aboveground biomass query
AGB<- sqlFetch(sec, "Aboveground biomass query")
colnames(AGB) <- c("ID", "Site","Disturbance","Age","Type","Measurement","AGB_Ref","AGB_Sec","SS","Lat")
AGB<-subset(AGB,Disturbance!="Fire")
AGB<-subset(AGB,Disturbance!="Logging")
AGB<-subset(AGB,Disturbance!="Plantation")
AGB<-subset(AGB,Disturbance!="Agroforestry")
AGB$Disturbance<-"All combined"
BGB<- sqlFetch(sec, "Belowground biomass query")
Soil_C<- sqlFetch(sec, "Soil carbon query")

head(Soil_C)
#Rename columns
colnames(AGB) <- c("ID", "Site","Disturbance","Age","Type","Measurement","AGB_Ref","AGB_Sec","SS","Lat")
AGB$Type<-"Aboveground biomass"
colnames(BGB) <- c("ID", "Site","Disturbance","Age","Type","Measurement","AGB_Ref","AGB_Sec","SS")
BGB$Type<-"Belowground biomass"
colnames(Soil_C) <- c("ID", "Site","Depth","Disturbance","Age","Type","Measurement","AGB_Ref","AGB_Sec","SS")
Soil_C$Type<-"Soil Carbon"
AGB<-subset(AGB,select=c(Disturbance,Age,AGB_Ref,AGB_Sec,Type))
BGB<-subset(BGB,select=c(Disturbance,Age,AGB_Ref,AGB_Sec,Type))
Soil_C<-subset(Soil_C,select=c(Disturbance,Age,AGB_Ref,AGB_Sec,Type))
Soil_C$Disturbance<-"All combined"
head(AGB)
head(BGB)
head(Soil_C)
#bind all carbon data together
Combined<-rbind(AGB,BGB,Soil_C)
Combined$Disturbance<-as.factor(Combined$Disturbance)
levels(Combined$Disturbance)
Combined<-subset(Combined,Disturbance!="Fire")
Combined<-subset(Combined,Disturbance!="Logging")
Combined<-subset(Combined,Disturbance!="Plantation")
Combined<-subset(Combined,Disturbance!="Agroforestry")
Combined<-subset(Combined,Age>0)


#calculate prop differences
colnames(Combined)
Combined$prop<-Combined$AGB_Sec/Combined$AGB_Ref
Combined<-subset(Combined,Age<=82)


#plot figures
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")


#plot colour figure

theme_set(theme_bw(base_size=12))
windowsFonts(Times=windowsFont("TT Times New Roman"))
a<-ggplot(All,aes(x=Age,y=plogis(Prediction)*2,colour=Disturbance))+geom_line(size=2)+geom_point(data=Combined,aes(x=Age,y=prop),shape=1,size=2)+facet_wrap(~Type,ncol=1)
b<-a+opts(panel.grid.major = theme_line(colour =NA),panel.grid.minor = theme_line(colour =NA))
c<-b
c
c<-c+coord_cartesian(xlim=c(0,85),ylim=c(0,2))+ylab("measure relative to primary forest")
d<-c+xlab("time since last disturbance (years)")
e<-d+geom_hline(y=1,lty=2,size=1)+ scale_colour_discrete(name = "Land use prior\nto regrowth")
e+theme(text=element_text(family="Times"))
ggsave(filename="Carbon_pools_colour.png",height=180,width=175,dpi=1200,units="mm")

#plot black and white figure
theme_set(theme_bw(base_size=12))
windowsFonts(Times=windowsFont("TT Times New Roman"))
a<-ggplot(All,aes(x=Age,y=plogis(Prediction)*2,colour=Disturbance))+geom_line(size=1.5)+geom_point(data=Combined,aes(x=Age,y=prop),shape=1,size=2)+facet_wrap(~Type,ncol=1)
b<-a+opts(panel.grid.major = theme_line(colour =NA))+geom_line(data=All,aes(x=Age,y=plogis(Prediction+(1.96*SE))*2),lty=2)
c<-b+geom_line(data=All,aes(x=Age,y=plogis(Prediction-(1.96*SE))*2),lty=2)
c<-c+coord_cartesian(xlim=c(0,85),ylim=c(0,2))+ylab("measure relative to primary forest")
d<-c+xlab("time since last disturbance (years)")
e<-d+geom_hline(y=1,lty=2)+ scale_colour_discrete(name = "Land use prior\nto regrowth")
e+theme(text=element_text(family="Times"))+scale_colour_manual(name = "Land use prior\nto regrowth",values=c("grey90","grey70","grey30","black"))
ggsave(filename="Carbon_pools_bw.png",height=180,width=175,dpi=300,units="mm")

#plot figures for presentation

#aboveground biomass first
theme_set(theme_bw(base_size=30))
a<-ggplot(AGB_pred,aes(x=Age,y=plogis(Prediction)*2,colour=Disturbance))+geom_line(size=2)+geom_point(data=subset(Combined,Type=="Aboveground biomass"),aes(x=Age,y=prop),shape=1,size=4)
b<-a+opts(panel.grid.major = theme_line(colour =NA),panel.grid.minor = theme_line(colour =NA))
c<-b+coord_cartesian(xlim=c(0,85),ylim=c(0,2))+ylab("Aboveground biomass \nrelative to undisturbed forest")
d<-c+xlab("Time since last disturbance (Years)")
e<-d+geom_hline(y=1,lty=2,size=1)+ theme(legend.position="none")
e
ggsave(filename="AGB_presentation.jpeg",height=8,width=12,dpi=400)

#blank version of the same plot
a<-ggplot(AGB_pred,aes(x=Age,y=plogis(Prediction)*2,colour=Disturbance))+geom_blank()
b<-a+xlab("Time since last disturbance (Years)")+ylab("Aboveground biomass \nas a relative to undisturbed forest")
c<-b+coord_cartesian(xlim=c(0,85),ylim=c(0,2))
c+geom_hline(lty=2,y=1)
ggsave(filename="AGB_blank_presentation.jpeg",height=8,width=12,dpi=400)


#belowground biomass next
theme_set(theme_bw(base_size=30))
a<-ggplot(BGB_pred,aes(x=Age,y=plogis(Prediction)*2,colour=Disturbance))+geom_line(size=2)+geom_point(data=subset(Combined,Type=="Belowground biomass"),aes(x=Age,y=prop),shape=1,size=4)
b<-a+opts(panel.grid.major = theme_line(colour =NA),panel.grid.minor = theme_line(colour =NA))
c<-b+coord_cartesian(xlim=c(0,85),ylim=c(0,2))+ylab("Belowground biomass \nrelative to undisturbed forest")
d<-c+xlab("Time since last disturbance (Years)")
e<-d+geom_hline(y=1,lty=2,size=1)
e
ggsave(filename="BGB_presentation.jpeg",height=8,width=12,dpi=400)

#and finally soil carbon
theme_set(theme_bw(base_size=30))
a<-ggplot(data=subset(Combined,Type=="Soil Carbon"),aes(x=Age,y=prop))+geom_point(shape=1,size=4)
b<-a+opts(panel.grid.major = theme_line(colour =NA),panel.grid.minor = theme_line(colour =NA))
c<-b+coord_cartesian(xlim=c(0,85),ylim=c(0,2))+ylab("Soil carbon \nrelative to undisturbed forest")
d<-c+xlab("Time since last disturbance (Years)")
e<-d+geom_hline(y=1,lty=2,size=1)
e
ggsave(filename="SoilC_presentation.jpeg",height=8,width=12,dpi=400)

