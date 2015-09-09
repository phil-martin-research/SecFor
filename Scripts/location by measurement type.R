#script to plot all locations for forest recovery paper
#script should include AGB, BGB, Soil, Total, Richness, Proportion of species

#load in necessary libraries
library(RODBC)
library(ggplot2)
library(nlme)
library(lme4)
library(MuMIn)
library(mapproj)
library(reshape)
library(plyr)


#connect to database
sec <- odbcConnect("Secondary/Degraded forests")
sqlTables(sec)

#import aboveground biomass query
AGB<- sqlFetch(sec, "Aboveground biomass query")
BGB<- sqlFetch(sec, "Belowground biomass query")
SoilC<- sqlFetch(sec, "Soil carbon query")
Rich<- sqlFetch(sec, "Species richness query")
Prop<- sqlFetch(sec, "Proportion query")

#import locations
Locations<- sqlFetch(sec, "Site characteristics")
Locations2<-subset(Locations,Latitude>-100)
head(Locations2)
colnames(Locations2)<-c("ID","Study","Site","Disturbance","Age","Type","Ref_Type","Ref_age","Country","Lat","Long","Temp","Rain","Elevation","Soil","Class")
Locations2<-subset(Locations2,Locations2$Disturbance!="Fire")
Locations2<-subset(Locations2,Locations2$Disturbance!="Logging")
Locations2<-subset(Locations2,Locations2$Disturbance!="Agroforestry")
Locations2$LatR<-round(Locations2$Lat*4,-1)/4
Locations2$LongR<-round(Locations2$Long*4,-1)/4
Locations2
Locations3<-data.frame(Locations2$Site,Locations2$LatR,Locations2$LongR)
colnames(Locations3)<-c("Site","Long","Lat")
Locations3

#subset AGB
head(AGB)
colnames(AGB) <- c("ID", "Site","Disturbance","Age","Type","Measurement","AGB_Ref","AGB_Sec","SS","Lat")
head(AGB)
AGB<-subset(AGB,AGB$Disturbance!="Fire")
AGB<-subset(AGB,AGB$Disturbance!="Logging")
AGB<-subset(AGB,AGB$Disturbance!="Agroforestry")
AGB<-subset(AGB,AGB$Type!="NA")
AGB<-subset(AGB,AGB$AGB_Sec!="0")
AGB<-subset(AGB,AGB$Age!="0")

#subset BGB
head(BGB)
colnames(BGB) <- c("ID", "Site","Disturbance","Age","Type","Measurement","AGB_Ref","AGB_Sec")
head(BGB)
BGB<-subset(BGB,BGB$Disturbance!="Fire")
BGB<-subset(BGB,BGB$Disturbance!="Logging")
BGB<-subset(BGB,BGB$Disturbance!="Agroforestry")
BGB<-subset(BGB,BGB$Type!="NA")
BGB<-subset(BGB,BGB$AGB_Sec!="0")
BGB<-subset(BGB,BGB$Age!="0")

#subset soil
head(SoilC)
colnames(SoilC) <- c("ID", "Site","Disturbance","Age","Type","Measurement","AGB_Ref","AGB_Sec")
head(SoilC)
SoilC<-subset(SoilC,SoilC$Disturbance!="Fire")
SoilC<-subset(SoilC,SoilC$Disturbance!="Logging")
SoilC<-subset(SoilC,SoilC$Disturbance!="Agroforestry")
SoilC<-subset(SoilC,SoilC$Type!="NA")
SoilC<-subset(SoilC,SoilC$AGB_Sec!="0")
SoilC<-subset(SoilC,SoilC$Age!="0")


#subset Richness
head(Rich)
colnames(Rich) <- c("ID", "Site","Disturbance","Age","Type","Measurement","AGB_Ref","AGB_Sec","Long","Tax")
head(Rich)
Rich<-subset(Rich,Rich$Disturbance!="Fire")
Rich<-subset(Rich,Rich$Disturbance!="Logging")
Rich<-subset(Rich,Rich$Disturbance!="Agroforestry")
Rich<-subset(Rich,Rich$Type!="NA")
Rich<-subset(Rich,Rich$AGB_Sec!="0")
Rich<-subset(Rich,Rich$Age!="0")
Rich<-subset(Rich,Rich$Tax!="NA")
Rich<-subset(Rich,Rich$Tax!="Herbs")
Rich<-subset(Rich,Rich$Tax!="Shrub")
Rich<-subset(Rich,Rich$Tax!="All plants")


#proportion of plant species query
Prop<- sqlFetch(sec, "Proportion query")
head(Prop)
colnames(Prop) <- c("ID", "Site","Disturbance","Age","Type","Measurement","Prop_Ref","Prop_Sec","Tax")
head(Prop)
Prop<-subset(Prop,Prop$Disturbance!="Fire")
Prop<-subset(Prop,Prop$Disturbance!="Logging")
Prop<-subset(Prop,Prop$Disturbance!="Agroforestry")

#create new dataframes with common variables
#AGB
AGB2<-data.frame(AGB$Site,"Type")
AGB2$X.Type.<-"AGB"
colnames(AGB2)<-c("Site","Type")
AGB2
#merge locations and AGB
AGB3<-merge(AGB2,Locations3,by="Site")
plot(AGB3$Lat,AGB3$Long)
AGB4<-count(AGB3,vars=c("Long","Lat"))
AGB4$Type<-"AGB"
AGB4

#BGB
BGB2<-data.frame(BGB$Site,"Type")
BGB2$X.Type.<-"BGB"
colnames(BGB2)<-c("Site","Type")
BGB2

#merge locations and BGB
BGB3<-merge(BGB2,Locations3,by="Site")
BGB4<-count(BGB3,vars=c("Long","Lat"))
BGB4$Type<-"BGB"
BGB4

#SoilC
SoilC2<-data.frame(SoilC$Site,"Type")
SoilC2$X.Type.<-"SoilC"
colnames(SoilC2)<-c("Site","Type")
SoilC2

#merge locations and soil
SoilC3<-merge(SoilC2,Locations3,by="Site")
SoilC4<-count(SoilC3,vars=c("Long","Lat"))
SoilC4$Type<-"SoilC"
SoilC4

#Rich2
Rich2<-data.frame(Rich$Site,"Type")
Rich2$X.Type.<-"Richness"
colnames(Rich2)<-c("Site","Type")
Rich2


#merge locations and richness
Rich3<-merge(Rich2,Locations3,by="Site")
Rich4<-count(Rich3,vars=c("Long","Lat"))
Rich4$Type<-"Rich"
Rich4


#Tree prop2
Prop
Tree_prop2<-data.frame(Prop$Site,"Type")
Tree_prop2$X.Type.<-"Proportion"
colnames(Tree_prop2)<-c("Site","Type")
Tree_prop2

#merge locations and richness
Prop3<-merge(Tree_prop2,Locations3,by="Site")
Prop4<-count(Prop3,vars=c("Long","Lat"))
Prop4$Type<-"Prop"
Prop4

#bind together datasets for ggplot
comb_loc<-rbind(AGB4,BGB4,SoilC4,Rich4,Prop4)
comb_loc2<-transform(comb_loc,Type=factor
(Type,levels=c("AGB","BGB","SoilC","Rich","Prop"),
                       labels=c("Aboveground biomass","Belowground biomass","Soil carbon","Plant species richness","Proportion of primary forest species"
                       )))


#plot all locations with facets for seperate types of measurement
#plot locations
site_map_a<-ggplot(data=comb_loc2,aes(x=Long, y=Lat))+borders("world", size=0.1,colour="grey",fill="lightgrey")+theme_bw()+theme(panel.grid.major = element_line(colour =NA))+facet_wrap(~Type)
site_map_b<-site_map_a+coord_map(project="rectangular",lat0 = 0)+ coord_cartesian(xlim = c(-130, 160),ylim=c(-30, 30))+geom_bin2d(binwidth = c(2, 2))+ scale_fill_gradient(low = "lightblue",high = "steelblue")
site_map_b+theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.y = element_blank(),axis.title.x = element_blank())


plot(y=comb_loc2$Lat,x=comb_loc2$Long)

head(comb_loc2)

#plot locations with points
site_map_a<-ggplot(data=comb_loc2,aes(x=Lat, y=Long,size=freq))+borders("world", size=0.1,colour="grey",fill="lightgrey")+theme_bw()+theme(panel.grid.major = element_line(colour =NA))+geom_point(alpha=0.5,colour="blue")
site_map_b<-site_map_a+coord_map(project="rectangular",lat0 = 0)+ coord_cartesian(xlim = c(-130, 130),ylim=c(-40, 40))
site_map_b+theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.y = element_blank(),axis.title.x = element_blank())+scale_area(range = c(1, 10),name="No. of sites")+facet_wrap(~Type)

setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")
ggsave("Site_locations_point.jpeg",height=5,width=12,dpi=300)


