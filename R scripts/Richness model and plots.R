#script to import, analyse and produce plots for prop biomass in secondary forests

#load in necessary libraries
library(RODBC)
library(ggplot2)
library(nlme)
library(MuMIn)
library(extrafont)
font_import()
Sys.setenv(R_GSCMD = "C:/Program Files(x86)/gs/gs9.05/bin/gswin32c.exe")

#connect to database
sec <- odbcConnect("Secondary/Degraded forests")
sqlTables(sec)

#import aboveground biomass query
Rich<- sqlFetch(sec, "Species richness query")
head(Rich)

#Rename columns
colnames(Rich) <- c("ID", "Site","Disturbance","Age","Type","Measurement","Rich_Ref","Rich_Sec","Det","Tax")
head(Rich)
range(Rich$Age)
levels(Rich$Type)

#subset data to remove logging, fire and missing values
Rich<-subset(Rich,Rich$Disturbance!="Fire")
Rich<-subset(Rich,Rich$Disturbance!="Logging")
Rich<-subset(Rich,Rich$Disturbance!="Agroforestry")
Rich<-subset(Rich,Rich$Type!="NA")
Rich<-subset(Rich,Rich$Rich_Sec!="0")
Rich<-subset(Rich,Rich$Age!="0")
Rich<-subset(Rich,Rich$Tax!="NA")
Rich<-subset(Rich,Rich$Tax!="Herbs")
Rich<-subset(Rich,Rich$Tax!="Shrub")
Rich<-subset(Rich,Rich$Tax!="All plants")



#Calculate richness as a proportion of reference forest
Rich$Prop<-(Rich$Rich_Sec/Rich$Rich_Ref)
Rich$lnRR<-log(Rich$Rich_Sec)-log(Rich$Rich_Ref)
Rich$Proploss<-(Rich$Rich_Sec-Rich$Rich_Ref)/Rich$Rich_Ref
Rich$Proploss2<-(qlogis((Rich$Proploss+ 1) / 2))

plot(Rich$Age,((plogis(Rich$Proploss2)*2)-1)+1)

#change types
levels(Rich$Type)[levels(Rich$Type)=="Tropical dry forest"] <- "Dry"
levels(Rich$Type)[levels(Rich$Type)=="Tropical moist forest"] <- "Moist"
levels(Rich$Type)[levels(Rich$Type)=="Tropical rainforest"] <- "Wet"
levels(Rich$Type)[levels(Rich$Type)=="Tropical montane forest"] <- "Montane"

#create column for reference as a factor
Rich$Ran<-as.factor(Rich$Rich_Ref)

ggplot(Rich,aes(y=Proploss2,x=Age))+geom_point()+facet_wrap(~Ran)

#redo sample sizes
Rich$SS[Rich$SS<0]<-1

#include only complete cases
head(Rich)
Rich2<-data.frame(ID=Rich$ID,Site=Rich$Site,Disturbance=Rich$Disturbance,Age=Rich$Age,Type=Rich$Type,Tax=Rich$Tax,Prop=Rich$Prop,lnRR=Rich$lnRR,Proploss=Rich$Proploss,Proploss2=Rich$Proploss2,Ran=Rich$Ran,Rich_Ref=Rich$Rich_Ref)
Rich3<-Rich2[complete.cases(Rich2), ]
Rich3<-subset(Rich3,Rich3$Prop<3)


#Mixed model of Rich prop


#null model

M0<-lme(Proploss2~1,random=~1+Age|Ran,data=Rich3,method="ML")
summary(M0)
plot(M0)

#set null deviance
nulldev<--2*logLik(M0)[1]
nulldev

#saturated model
M1<-lme(Proploss2~Age+I(Age^2)+log(Age)+Tax,random=~1+Age|Ran,data=Rich3,method="ML")
summary(M1)
plot(M1)
qqnorm(M1)

#diagnostic plot of fitted curves
plot(augPred(M1,primary=~Age),grid=T)

#compare model fits
plot(comparePred(M1,M0,primary=~Age))

#model selection using AICc

#run all possible models
MS1<- dredge(M1, trace = TRUE, rank = "AICc", REML = FALSE)

#subset models with delta<7 (to remove implausible models)
poss_mod<- get.models(MS1, subset = delta <7)
modsumm <- model.sel(poss_mod, rank = "AICc")
modsumm<-subset(modsumm,modsumm$delta<7)

#calculate deviance of model
modsumm$dev<--2*modsumm$logLik

#calculate deviance explained for each model
modsumm$dev_ex<-((nulldev-modsumm$dev)/nulldev)
modsumm$dev_ex

#output table as csv file
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(modsumm, "Model - Richness.csv")

#create predictions based on models >0.6 weight
averaged<-model.avg(MS1,subset=cumsum(weight)<=0.6)
averaged2<-averaged$avg.model

#output parameter estimates
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(averaged2, "Multimodel inferences Richness.csv") #save table


#create new data
Age<-seq(0.5,174,0.1)
Age_epi<-seq(2,115,0.1)
range(subset(Rich$Age,Rich$Tax=="Trees"))
range(subset(Rich$Age,Rich$Tax=="Epiphytes"))

#create predictions
predstrees<-averaged2[1]+(averaged2[3]*log(Age))+averaged2[4]+((Age)*averaged2[2])
predsepi<-averaged2[1]+(averaged2[3]*log(Age_epi))+((Age_epi)*averaged2[2])
SE_tree<-averaged2[1,2]+(averaged2[3,2])+averaged2[4,2]+(averaged2[2,2])
SE_epi<-averaged2[1,2]+(averaged2[3,2])+(averaged2[2,2])

plot(Age,plogis(predstrees)*2)

#put into dataframes
Tree_preds<-data.frame(Age=Age,preds=predstrees,SE=SE_tree,Tax="Trees")
Epi_preds<-data.frame(Age=Age_epi,preds=predsepi,SE=SE_epi,Tax="Epiphytes")

#put into single dataframe
Comb<-rbind(Tree_preds,Epi_preds)
predictions<-data.frame(Comb,Type="Plant species richness")

setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(predictions, "Model predictions - Richness.csv") #save table


#produce plots of model
head(Rich)
theme_set(theme_bw(base_size=12))
windowsFonts(Times=windowsFont("TT Times New Roman"))
a<-qplot(data=Rich3,x=Age,y=Prop,geom="point",shape=I(1),colour=Tax,size=I(2),group=Tax)+geom_line(data=Comb,aes(x=Age,y=plogis(preds)*2),size=1.5)
b<-a+geom_line(data=Comb,aes(x=Age,y=plogis(preds+(1.96*SE))*2),lty=2)
c<-b+geom_line(data=Comb,aes(x=Age,y=plogis(preds-(1.96*SE))*2),,lty=2)+facet_wrap(~Tax)
d<-c+ylab("species richness proportional\nto reference forest")+xlab("time since last disturbance (years)")
e<-d+theme(panel.grid.major = element_line(colour =NA))+scale_colour_discrete("Taxonomic group")
f<-e+coord_cartesian(ylim=c(0,1.8),xlim=c(0,175))+geom_hline(y=1,lty=2)+theme(legend.position="none")
f+theme(text=element_text(family="Times"))+scale_colour_manual(values=c("black","black"))
f+theme(text=element_text(family="Times"))
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")
ggsave(filename="Richness_colour.pdf",height=87.5,width=175,dpi=1200,units="mm")
ggsave(filename="Richness_bw.pdf",height=87.5,width=175,dpi=1200,units="mm")
ggsave(filename="Richness_colour.png",height=87.5,width=175,dpi=1200,units="mm")


?ggsave


#produce plots of model for presentations
head(Rich)
fonts()
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12))
a<-qplot(data=Rich3,x=Age,y=Prop,geom="point",shape=I(1),colour=Tax,size=I(2),group=Tax)+geom_line(data=Comb,aes(x=Age,y=plogis(preds)*2),size=1.5)
b<-a
c<-b+facet_wrap(~Tax)
d<-c+ylab("Species richness proportional\nto reference forest")+xlab("Time since last disturbance (Years)")
e<-d+theme(panel.grid.major = element_line(colour =NA))+scale_colour_discrete("Taxonomic group")
f<-e+coord_cartesian(ylim=c(0,1.8),xlim=c(0,180))+geom_hline(y=1,lty=2)+theme(legend.position="none")
f+theme(text=element_text(family="Times"))


setwd("C:/Users/Phil/Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")
ggsave(filename="Prop_richness.png",height=8,width=12,dpi=300)
ggsave(filename="Prop_richness.jpeg",height=4,width=6,dpi=1200)