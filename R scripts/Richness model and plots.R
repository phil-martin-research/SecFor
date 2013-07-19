
###################################################################################
###############script to import, analyse and produce plots#########################
###############for proportional tree species richness##############################
###############in secondary forests################################################
###################################################################################



#load in necessary libraries
library(RODBC)
library(ggplot2)
library(lmer)
library(MuMIn)
library(extrafont)

#connect to database
sec <- odbcConnect("Secondary/Degraded forests")
sqlTables(sec)

#import aboveground biomass query
Rich<- sqlFetch(sec, "Species richness query")
head(Rich)

#Rename columns
colnames(Rich) <- c("ID", "Site","Disturbance","Age","Type","Measurement","Rich_Ref","Rich_Sec","Det","Tax","Size")
head(Rich)
range(Rich$Age)
levels(Rich$Type)

#subset data to remove logging, fire, missing values and other taxonomic groups
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
Rich<-subset(Rich,Rich$Tax!="Epiphytes")
Rich<-subset(Rich,Rich$Age<150)



#Calculate richness as a proportion of reference forest
Rich$Prop<-(Rich$Rich_Sec/Rich$Rich_Ref)
Rich$lnRR<-log(Rich$Rich_Sec)-log(Rich$Rich_Ref)
Rich$Proploss<-(Rich$Rich_Sec-Rich$Rich_Ref)/Rich$Rich_Ref
Rich$Proploss2<-(qlogis((Rich$Proploss+ 1) / 2))


#change types
levels(Rich$Type)[levels(Rich$Type)=="Tropical dry forest"] <- "Dry"
levels(Rich$Type)[levels(Rich$Type)=="Tropical moist forest"] <- "Moist"
levels(Rich$Type)[levels(Rich$Type)=="Tropical rainforest"] <- "Wet"
levels(Rich$Type)[levels(Rich$Type)=="Tropical montane forest"] <- "Montane"

#create column for reference as a factor
Rich$Ran<-as.factor(Rich$Rich_Ref)
Rich$Size<-as.factor(Rich$Size)


ggplot(Rich,aes(y=Prop,x=Age,colour=Size))+geom_point()+facet_wrap(~Size)

#redo sample sizes
Rich$SS[Rich$SS<0]<-1

#include only complete cases
head(Rich)
Rich2<-data.frame(ID=Rich$ID,Site=Rich$Site,Disturbance=Rich$Disturbance,Age=Rich$Age,Type=Rich$Type,Tax=Rich$Tax,Prop=Rich$Prop,lnRR=Rich$lnRR,Proploss=Rich$Proploss,Proploss2=Rich$Proploss2,Ran=Rich$Ran,Rich_Ref=Rich$Rich_Ref,Size=Rich$Size)
Rich3<-Rich2[complete.cases(Rich2), ]
Rich3<-subset(Rich3,Rich3$Prop<3)


#Mixed model of Rich prop


#null model

M0<-lmer(Proploss2~1+(Age|Ran),data=Rich3,REML=F)
summary(M0)

#set null deviance
nulldev<--2*logLik(M0)[1]
nulldev

#saturated models
M1<-lmer(Proploss2~Age+I(Age^2)+log(Age)+(Age|Ran)+(1|Size),data=Rich3,REML=T)
M2<-lmer(Proploss2~Age+I(Age^2)+log(Age)+(Age|Ran),data=Rich3,REML=T)

plot(fitted(M1),M1@resid)
plot(fitted(M2),M2@resid)

#choose between the two random effects options based on AIC
AIC(M1,M2)

#the one without size class is the better of the two, so we run the model this time
#using maximum liklihood methods

M1<-lmer(Proploss2~Age+I(Age^2)+log(Age)+(Age|Ran),data=Rich3,REML=F)

plot(fitted(M1),M1@resid)

#next we run all possible models
MS1<- dredge(M1, trace = TRUE, rank = "AICc", REML = FALSE)

#subset models with delta<7 to remove implausible models
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

#create predictions based on models >0.95 weight
averaged<-model.avg(MS1,subset=cumsum(weight)<=0.95)
averaged2<-averaged$avg.model
averaged2

#output parameter estimates
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(averaged2, "Multimodel inferences Richness.csv") #save table


#create new data
Age<-seq(0.5,174,0.1)
range(subset(Rich$Age,Rich$Tax=="Trees"))

#create predictions
Int<-averaged2[1]
Agesq<-(Age^2)*-0.0001569525
logAge<-(log(Age))*0.9937220058
Agen<--0.0176539905*Age
preds<-Int+Agesq+logAge+Agen

predstrees<-averaged2[1]+(averaged2[4]*(Age))+(averaged2[3]*log(Age))+((averaged2[2]*(Age^2)))
SE_tree<-averaged2[1,2]+(averaged2[3,2])+averaged2[4,2]+(averaged2[2,2])


plot(Rich3$Age,Rich3$Prop)
lines(Age,plogis(preds)*2)
lines(Age,plogis((predstrees+(SE_tree))*2),lty=2)
lines(Age,plogis((predstrees-(SE_tree))*2),lty=2)

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