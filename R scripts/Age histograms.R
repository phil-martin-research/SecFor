#script to plot all histograms for forest recovery paper
#script should include AGB, BGB, Soil, Total, Richness, Proportion of species

#load in necessary libraries
library(RODBC)
library(ggplot2)
library(nlme)
library(lme4)
library(MuMIn)

#connect to database
sec <- odbcConnect("Secondary/Degraded forests")
sqlTables(sec)

#import aboveground biomass query
AGB<- sqlFetch(sec, "Aboveground biomass query")
BGB<- sqlFetch(sec, "Belowground biomass query")
SoilC<- sqlFetch(sec, "Soil carbon query")
TotalC<- sqlFetch(sec, "Total carbon query")
Rich<- sqlFetch(sec, "Species richness query")
Prop<- sqlFetch(sec, "Proportion query")

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

#subset total C
head(TotalC)
colnames(TotalC) <- c("ID", "Site","Disturbance","Age","Type","Measurement","AGB_Ref","AGB_Sec")
head(TotalC)
TotalC<-subset(TotalC,TotalC$Disturbance!="Fire")
TotalC<-subset(TotalC,TotalC$Disturbance!="Logging")
TotalC<-subset(TotalC,TotalC$Disturbance!="Agroforestry")
TotalC<-subset(TotalC,TotalC$Type!="NA")
TotalC<-subset(TotalC,TotalC$AGB_Sec!="0")
TotalC<-subset(TotalC,TotalC$Age!="0")

#subset total C
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
Prop<-subset(Prop,Prop$Tax!="NA")
Prop<-subset(Prop,Prop$Tax!="Herbs")
Prop<-subset(Prop,Prop$Tax!="Shrub")
Prop<-subset(Prop,Prop$Tax!="All plants")
chaz<-read.csv("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning/Chazdon et al 2009 all2.csv",header=T)
head(chaz)
head(Prop)
trees<-subset(chaz,chaz$Type=="Trees")
#combine species proportion datasets
Prop2<-data.frame(trees$Sqrt_age^2,trees$Proportion)
Prop2$age2<-round(Prop2$trees.Sqrt_age.2)
Prop3<-data.frame(Prop2$age2,Prop2$trees.Proportion)
colnames(Prop3)<-c("Age","Prop")
Prop_new<-data.frame(Prop$Age,Prop$Prop_Sec)
colnames(Prop_new)<-c("Age","Prop")
Tree_prop<-rbind(Prop3,Prop_new)
Tree_prop$Type<-"Trees"
Tree_prop


#create new dataframes with common variables
#AGB
AGB2<-data.frame(AGB$Age,"Type")
AGB2$X.Type.<-"AGB"
colnames(AGB2)<-c("Age","Type")
AGB2

#BGB
BGB2<-data.frame(BGB$Age,"Type")
BGB2$X.Type.<-"BGB"
colnames(BGB2)<-c("Age","Type")
BGB2

#SoilC
SoilC2<-data.frame(SoilC$Age,"Type")
SoilC2$X.Type.<-"SoilC"
colnames(SoilC2)<-c("Age","Type")
SoilC2

#Rich2
Rich2<-data.frame(Rich$Age,"Type")
Rich2$X.Type.<-"Richness"
colnames(Rich2)<-c("Age","Type")
Rich2

#Tree prop2
Tree_prop2<-data.frame(Tree_prop$Age,"Type")
Tree_prop2$X.Type.<-"Proportion"
colnames(Tree_prop2)<-c("Age","Type")
Tree_prop2

#bind together datasets for ggplot
comb_ages<-rbind(AGB2,BGB2,SoilC2,Rich2,Tree_prop2)
comb_ages2<-transform(comb_ages,Type=factor
          (Type,levels=c("AGB","BGB","SoilC","Richness","Proportion"),
          labels=c("Aboveground biomass","Belowground biomass","Soil carbon","Plant species richness", "Proportion of primary species"
                   )))


#plot of all ages together
theme_set(theme_bw(base_size=12))
a<-ggplot(comb_ages2,aes(x=Age))+geom_histogram(aes(y=..density..*100),binwidth=5)+facet_wrap(~Type,ncol=2)
b<-a+opts(legend.position = "none")+opts(panel.grid.major = theme_line(colour =NA))+coord_cartesian(ylim=c(0,6),xlim=c(0,105))
b
c<-b+ylab("Percentage of whole dataset")+xlab("Time since last disturbance (Years)")
d<-c
d+scale_x_continuous(breaks=seq(0, 100, 20))+scale_y_continuous(breaks=seq(0, 6, 2))

setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")
ggsave(filename="Hist_ages.jpg",height=4,width=6,dpi=1200)