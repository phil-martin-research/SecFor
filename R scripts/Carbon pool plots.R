#script to draw plots of predicted changes in carbon pools
#with time since disturbance in tropical secondary forests

#load packages

library(RODBC)
library(ggplot2)
library(plyr)
library(reshape)

#import data from model predictions
AGB_pred<-read.table("C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics/Model predictions - Biomass.csv",header=T,sep=",")
AGB_pred$Age<-seq(0.5,82,.1)
AGB_pred$Disturbance<-"All combined"
Soil_pred<-read.table("C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics/Model predictions - Soil C.csv",header=T,sep=",")
BGB_pred<-read.table("C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics/Model predictions - BGB.csv",header=T,sep=",")
colnames(AGB_pred)
colnames(BGB_pred)
#combine datasets
All<-rbind(AGB_pred,BGB_pred,Soil_pred)
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

#Rename columns
colnames(AGB) <- c("ID", "Site","Disturbance","Age","Type","Measurement","AGB_Ref","AGB_Sec","SS","Lat")
AGB$Type<-"Aboveground biomass"
colnames(BGB) <- c("ID", "Site","Disturbance","Age","Type","Measurement","AGB_Ref","AGB_Sec","SS")
BGB$Type<-"Belowground biomass"
colnames(Soil_C) <- c("ID", "Site","Disturbance","Age","Type","Measurement","AGB_Ref","AGB_Sec","SS")
Soil_C$Type<-"Soil Carbon"
AGB<-subset(AGB,select=-Lat)

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


#plot figure
theme_set(theme_bw(base_size=12))
windowsFonts(Times=windowsFont("TT Times New Roman"))
a<-ggplot(All,aes(x=Age,y=plogis(Prediction)*2,colour=Disturbance))+geom_line(size=1.5)+geom_point(data=Combined,aes(x=Age,y=prop),shape=1,size=2)+facet_wrap(~Type,ncol=1)
b<-a+opts(panel.grid.major = theme_line(colour =NA))+geom_line(data=All,aes(x=Age,y=plogis(Prediction+(1.96*SE))*2),lty=2)
c<-b+geom_line(data=All,aes(x=Age,y=plogis(Prediction-(1.96*SE))*2),lty=2)
c
c<-c+coord_cartesian(xlim=c(0,85),ylim=c(0,2))+ylab("measure relative to primary forest")
c
d<-c+xlab("time since last disturbance (years)")
e<-d+geom_hline(y=1,lty=2)+ scale_colour_discrete(name = "Land use prior\nto regrowth")
e+theme(text=element_text(family="Times"))+scale_colour_manual(name = "Land use prior\nto regrowth",values=c("grey90","grey70","grey30","black"))
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")
ggsave(filename="Carbon_pools_colour.png",height=180,width=175,dpi=400,units="mm")
ggsave(filename="Carbon_pools_bw.pdf",height=180,width=175,dpi=300,units="mm")
