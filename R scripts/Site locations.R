#this is a script to plot map positions from secondary forest data

#load in necessary libraries
library(RODBC)
library(ggplot2)
library(maps)
library(mapproj)
library(reshape)
library(plyr)

#load mround function
mround <- function(number, multiple) {
  # if number and multiple have different sign, returning an error.
  if (sign(number) != sign(multiple)) {
    stop("number and multiple have different sign")
  }
}

#connect to database
sec <- odbcConnect("Secondary/Degraded forests")
sqlTables(sec)
?floor
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
round(23*2,-1)/2
Locations2
Locations3<-count(Locations2,vars=c("LongR","LatR"))
colnames(Locations3)<-c("Long","Lat","Count")

#plot locations
theme_set(theme_bw(base_size=16))
site_map_a<-ggplot(data=Locations2,aes(x=Long, y=Lat))+borders("world", size=0.1,colour="grey",fill="lightgrey")+theme_bw()+theme(panel.grid.major = element_line(colour =NA))+geom_bin2d(binwidth = c(2, 2))
site_map_b<-site_map_a+coord_map(project="rectangular",lat0 = 0)+ coord_cartesian(xlim = c(-130, 160),ylim=c(-30, 30))+ scale_fill_gradient(low = "lightblue",high = "steelblue")
site_map_b+theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.y = element_blank(),axis.title.x = element_blank())

#plot locations with points
theme_set(theme_bw(base_size=28))
site_map_a<-ggplot(data=Locations3,aes(x=Long, y=Lat,size=Count))+borders("world", size=0.1,colour="lightgrey",fill="lightgrey")+theme(panel.grid.major = element_line(colour =NA))+geom_point(alpha=0.5,colour="blue")
site_map_b<-site_map_a+coord_map(project="rectangular",lat0 = 0)+ coord_cartesian(xlim = c(-130, 130),ylim=c(-40, 40))
site_map_b+theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.y = element_blank(),axis.title.x = element_blank())+ scale_size_continuous(name="No. of sites",range = c(4, 15))

setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis")
ggsave("Site_locations_point_pres.jpeg",height=6,width=16,dpi=300)

ggplot(Locations2,aes(x=Longitude))+geom_histogram(fill="red")+ coord_cartesian(xlim = c(-130, 160),ylim=c(0,210))+ylab("Frequency")+theme_bw()+theme(panel.grid.major = element_line(colour =NA))

setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis")
ggsave("Site_locations hist.png",height=2,width=8,dpi=1200)