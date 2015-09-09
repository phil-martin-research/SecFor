###################################################################################
#script to check the relationship between above and belowground biomass############
#in secondary forests##############################################################
###################################################################################

#load in necessary libraries
library(RODBC)
library(ggplot2)
library(lme4)
library(reshape2)
library(MuMIn)

#import aboveground biomass query
sec <- odbcConnect("Secondary/Degraded forests")
BGB<- sqlFetch(sec, "Belowground biomass query")
#Rename columns
colnames(BGB) <- c("ID", "Site","Disturbance","Age","Type","Measurement","BGB_Ref","BGB_Sec","SS")
head(BGB)


#Calculate prortion lost relative to primary forest
BGB$Prop<-(BGB$BGB_Sec)/(BGB$BGB_Ref)
BGB$lnRR<-log(BGB$BGB_Sec)-log(BGB$BGB_Ref)
BGB$Proploss<-((BGB$BGB_Sec)-(BGB$BGB_Ref))/(BGB$BGB_Ref)
BGB$Proploss2<-(qlogis((BGB$Proploss+ 1)/2))

#subset data to remove logging, fire and missing values
BGB<-subset(BGB,BGB$Disturbance!="Fire")
BGB<-subset(BGB,BGB$Disturbance!="Logging")
BGB<-subset(BGB,BGB$Disturbance!="Agroforestry")
BGB<-subset(BGB,BGB$Type!="NA")
BGB<-subset(BGB,BGB$BGB_Sec!="0")

#change types
levels(BGB$Type)[levels(BGB$Type)=="Tropical dry forest"] <- "Dry"
levels(BGB$Type)[levels(BGB$Type)=="Tropical moist forest"] <- "Moist"
levels(BGB$Type)[levels(BGB$Type)=="Tropical rainforest"] <- "Wet"
levels(BGB$Type)[levels(BGB$Type)=="Tropical montane forest"] <- "Montane"

#create column for reference as a factor
BGB$Ran<-as.factor(BGB$BGB_Ref)

#connect to database
sec <- odbcConnect("Secondary/Degraded forests")
sqlTables(sec)

#import aboveground biomass query
AGB<- sqlFetch(sec, "Aboveground biomass query")
Sites<-sqlFetch(sec,"Secondary forest table")
head(AGB)

#Rename columns
colnames(AGB) <- c("ID", "Site","Disturbance","Age","Type","Measurement","AGB_Ref","AGB_Sec","SS","Lat","Height","WG","LG")

#subset data to remove logging, fire and missing values
AGB<-subset(AGB,AGB$Disturbance!="Fire")
AGB<-subset(AGB,AGB$Disturbance!="Logging")
AGB<-subset(AGB,AGB$Disturbance!="Agroforestry")
AGB<-subset(AGB,AGB$Type!="NA")
AGB<-subset(AGB,AGB$AGB_Sec!="0")
AGB<-subset(AGB,AGB$Age!="0")

#Calculate aboveground biomass as a proportion of reference forest
AGB$Prop<-(AGB$AGB_Sec)/(AGB$AGB_Ref)
AGB$Proploss<-(AGB$AGB_Sec-AGB$AGB_Ref)/(AGB$AGB_Ref)
AGB$Proploss2<-(qlogis((AGB$Proploss+ 1)/2))
AGB$lnRR<-log((AGB$AGB_Sec))-log(AGB$AGB_Ref)

#change types
levels(AGB$Type)[levels(AGB$Type)=="Tropical dry forest"] <- "Dry"
levels(AGB$Type)[levels(AGB$Type)=="Tropical moist forest"] <- "Moist"
levels(AGB$Type)[levels(AGB$Type)=="Tropical rainforest"] <- "Wet"
levels(AGB$Type)[levels(AGB$Type)=="Tropical montane forest"] <- "Montane"

#create column for reference as a factor
AGB$Ran<-as.factor(AGB$AGB_Ref)

#put data into new dataframe
AGB2<-data.frame(Change=AGB$Proploss2,Change2=AGB$lnRR,Change3=AGB$Prop,Age=AGB$Age,Type=AGB$Type,Disturbance=AGB$Disturbance,Ran=AGB$Ran,Ref=as.numeric(AGB$AGB_Ref),Height=AGB$Height,WG=AGB$WG,LG=AGB$LG)

head(BGB)
head(AGB)

#Merge two together
Merged<-merge(BGB,AGB,by="Site")

head(Merged)

plot(Merged$AGB_Sec,Merged$BGB_Sec)
plot(Merged$Age.x,Merged$Prop.y)

plogis(-.33786)*2
plogis(-.47)*2

((83*5)+76)/6
((83*5)+50)/6
