
###################################################################################
###############script to import, analyse and produce plots#########################
###############for proportional tree species richness##############################
###############in secondary forests################################################
###################################################################################



#load in necessary libraries
library(RODBC)
library(ggplot2)
library(lme4)
library(MuMIn)
library(extrafont)

#connect to database
sec <- odbcConnect("Secondary/Degraded forests")
sqlTables(sec)

#import species richness query
Rich<- sqlFetch(sec, "Species richness query")
head(Rich)

#Rename columns
colnames(Rich) <- c("ID", "Site","Disturbance","Age","Type","Measurement","Rich_Ref","Rich_Sec","Det","Tax","Size")
head(Rich)

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

#Calculate richness as a proportion of reference forest
Rich$Prop<-(Rich$Rich_Sec/Rich$Rich_Ref)
Rich$lnRR<-log(Rich$Rich_Sec)-log(Rich$Rich_Ref)
Rich$Proploss<-(Rich$Rich_Sec-Rich$Rich_Ref)/Rich$Rich_Ref
Rich$Proploss2<-(qlogis((Rich$Proploss+ 1) / 2))

#change forest types
levels(Rich$Type)[levels(Rich$Type)=="Tropical dry forest"] <- "Dry"
levels(Rich$Type)[levels(Rich$Type)=="Tropical moist forest"] <- "Moist"
levels(Rich$Type)[levels(Rich$Type)=="Tropical rainforest"] <- "Wet"
levels(Rich$Type)[levels(Rich$Type)=="Tropical montane forest"] <- "Montane"

#create column for reference as a factor
Rich$Ran<-as.factor(Rich$Rich_Ref)
Rich$Size<-as.factor(Rich$Size)

#include only complete cases
head(Rich)
Rich2<-data.frame(ID=Rich$ID,Site=Rich$Site,Disturbance=Rich$Disturbance,Age=Rich$Age,Type=Rich$Type,Tax=Rich$Tax,Prop=Rich$Prop,lnRR=Rich$lnRR,Proploss=Rich$Proploss,Proploss2=Rich$Proploss2,Ran=Rich$Ran,Rich_Ref=Rich$Rich_Ref,Size=Rich$Size)
Rich3<-Rich2[complete.cases(Rich2), ]
Rich3<-subset(Rich3,Rich3$Prop<3)

#Mixed model of Rich prop

#saturated models
M1<-lmer(Proploss2~log(Age)+I(log(Age)^2)+(Age|Ran),data=Rich3,REML=T)
M2<-lmer(Proploss2~log(Age)+I(log(Age)^2)+(Age|Ran)+(1|Size),data=Rich3,REML=T)

#choose between the two random effects options based on AIC
AIC(M1,M2)

#the one without size class is the better of the two, so we run the model this time
#using maximum liklihood methods

#test whether I should be using log or linear terms
M1lin<-lmer(Proploss2~Age+I(Age^2)+Disturbance+(Age|Ran),data=Rich3,REML=F)
M1log<-lmer(Proploss2~log(Age)+I(log(Age)^2)+Disturbance+(Age|Ran),data=Rich3,REML=F)

#produce diagnostic plots
plot(fitted(M1lin),M1lin@resid)
plot(fitted(M1log),M1log@resid)

#it looks like the one with the log terms is best - so we'll use that one.
#we need to set REML=T for unbiased parameter estimation first though
M1log<-lmer(Proploss2~log(Age)+I(log(Age)^2)+Disturbance+(Age|Ran),data=Rich3,REML=T)

#next we run all possible models
MS1<-dredge(M1log, trace = TRUE, rank = "AICc", REML = F)

#subset models with delta<7 to remove implausible models
poss_mod<- get.models(MS1, subset = delta <7)
modsumm <- model.sel(poss_mod, rank = "AICc")
modsumm

#calculate marginal r squared for 
#each model using equation from Nakagawa et al  2013
D7_1<-lmer(Proploss2~log(Age)+(Age|Ran),data=Rich3,REML=F)
D7_2<-lmer(Proploss2~log(Age)+Disturbance+(Age|Ran),data=Rich3,REML=F)
D7_3<-lmer(Proploss2~log(Age)+I(log(Age)^2)+Disturbance+(Age|Ran),data=Rich3,REML=T)
D7_4<-lmer(Proploss2~log(Age)+Disturbance+(Age|Ran),data=Rich3,REML=F)

Rsquared_trees<-rbind(r.squaredGLMM(D7_1),r.squaredGLMM(D7_2),r.squaredGLMM(D7_3),r.squaredGLMM(D7_4))

#add this to model summary output
modsumm$MRsquared<-Rsquared_trees[,1]

#output table as csv file
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(modsumm, "Model - Richness.csv")

#get parameter estimates for models with delta<7
averaged<-model.avg(modsumm,subset=delta<7)
averaged2<-averaged$avg.model

#output parameter estimates
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(averaged2, "Multimodel inferences Richness.csv") #save table

#output importance values
#add importance values to estimated
importance<-data.frame(Variable=c("Intercept",labels(importance(modsumm))),Importance=c(1,as.vector(importance(modsumm))))
importance
write.csv(importance, "Importance-Tree_Richness.csv") #save table


#create new data
Age_tree<-seq(0.5,175,0.1)

#create predictions using variables with weight >0.3
predstrees<-averaged2[1]+(averaged2[2]*log(Age_tree))
SE_tree<-averaged2[1,2]+(averaged2[2,2])

#plot these predictions
plot(Rich3$Age,plogis(Rich3$Proploss2)*2)
lines(Age_tree,plogis(predstrees)*2)
lines(Age_tree,plogis(predstrees+(1.96*SE_tree))*2)
lines(Age_tree,plogis(predstrees-(1.96*SE_tree))*2)

#put into dataframes
Tree_preds<-data.frame(Age=Age_tree,preds=predstrees,SE=SE_tree,Tax="Trees")

###########################################################################################
###This bit of the script is for the####################################################### 
###same type of model, but using epiphytes#################################################
###########################################################################################

#connect to database
sec <- odbcConnect("Secondary/Degraded forests")
sqlTables(sec)

#import species richness query
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
Rich<-subset(Rich,Rich$Tax!="Trees")

#Calculate richness as a proportion of reference forest
Rich$Prop<-(Rich$Rich_Sec/Rich$Rich_Ref)
Rich$lnRR<-log(Rich$Rich_Sec)-log(Rich$Rich_Ref)
Rich$Proploss<-(Rich$Rich_Sec-Rich$Rich_Ref)/Rich$Rich_Ref
Rich$Proploss2<-(qlogis((Rich$Proploss+ 1) / 2))



#change forest types
levels(Rich$Type)[levels(Rich$Type)=="Tropical dry forest"] <- "Dry"
levels(Rich$Type)[levels(Rich$Type)=="Tropical moist forest"] <- "Moist"
levels(Rich$Type)[levels(Rich$Type)=="Tropical rainforest"] <- "Wet"
levels(Rich$Type)[levels(Rich$Type)=="Tropical montane forest"] <- "Montane"

#create column for reference as a factor
Rich$Ran<-as.factor(Rich$Rich_Ref)

#include only complete cases
head(Rich)
Rich2<-data.frame(ID=Rich$ID,Site=Rich$Site,Disturbance=Rich$Disturbance,Age=Rich$Age,Type=Rich$Type,Tax=Rich$Tax,Prop=Rich$Prop,lnRR=Rich$lnRR,Proploss=Rich$Proploss,Proploss2=Rich$Proploss2,Ran=Rich$Ran,Rich_Ref=Rich$Rich_Ref,Size=Rich$Size)

#Mixed model of Rich prop


#null model used to calculate deviance
M0<-lmer(Proploss2~1+(Age|Ran),data=Rich2,REML=F)
summary(M0)
str(M0)

#set null deviance
nulldev<--2*logLik(M0)[1]
nulldev

#test whether I should be using log or linear terms
M1lin<-lmer(Proploss2~Age+I(Age^2)+(Age|Ran),data=Rich2,REML=F)
M1log<-lmer(Proploss2~log(Age)+I(log(Age)^2)+(Age|Ran),data=Rich2,REML=F)

#diagnostic plots
plot(fitted(M1lin),M1lin@resid)
plot(plogis(predict(M1lin))*2,plogis(Rich2$Proploss2)*2,col=Rich3$Disturbance)
abline(a=0,b=1)
qplot(plogis(fitted(M1log))*2,plogis(Rich2$Proploss2)*2)+geom_abline()+coord_cartesian(xlim=c(0,1.5),ylim=c(0,1.5))


plot(Rich2$Age,Rich2$Proploss2,col=Rich3$Disturbance)
AIC(M1lin,M1log)

#it looks like the one with the linear terms is best - so we'll use that one.
#we need to set REML=T for unbiased estimation first though
M1lin<-lmer(Proploss2~Age+I(Age^2)+(Age|Ran),data=Rich2,REML=T)

summary(M1lin)
#next we run all possible models
MS1<- dredge(M1lin, trace = TRUE, rank = "AICc", subset=dc(Age,Age^2),REML = F)

#subset models with delta<7 to remove implausible models
poss_mod<- get.models(MS1, subset = delta <7)
modsumm <- model.sel(poss_mod, rank = "AICc")
modsumm
modsumm<-subset(modsumm,delta<7)
modsumm

#this is a crap model, what happens with log terms?
Rich2$logAge<-log(Rich2$Age)
Rich2$logAgesq<-Rich2$logAge^2
M1log<-lmer(Proploss2~logAge+logAgesq+(Age|Ran),data=Rich2,REML=F)
MS1_log<- dredge(M1log, trace = TRUE, rank = "AICc", subset=dc(logAge,logAgesq),REML = F)

#subset models with delta<7 to remove implausible models
poss_mod<- get.models(MS1_log, subset = delta <7)
modsumm <- model.sel(poss_mod, rank = "AICc")
modsumm<-subset(modsumm,delta<7)
modsumm

#calculate marginal r squared for 
#each model using equation from Nakagawa et al  2013
D7_1<-lmer(Proploss2~1+logAge+(Age|Ran),data=Rich2,REML=F)
D7_2<-lmer(Proploss2~1+(Age|Ran),data=Rich2,REML=F)
D7_3<-lmer(Proploss2~1+logAge+logAgesq+(Age|Ran),data=Rich2,REML=F)

Rsquared_epi<-rbind(r.squaredGLMM(D7_1),r.squaredGLMM(D7_2),r.squaredGLMM(D7_3))

#add this to model summary output
modsumm$MRsquared<-Rsquared_epi[,1]
      
#output table as csv file
write.csv(modsumm, "Model - Richness_epi.csv")

#create predictions based on models with delta<7
averaged<-model.avg(modsumm)
averaged
averaged2<-averaged$avg.model
averaged2

#output parameter estimates
write.csv(averaged2, "Multimodel inferences Richness_epi.csv") #save table

#output importance values
#add importance values to estimated
importance<-data.frame(Variable=c("Intercept",labels(importance(modsumm))),Importance=c(1,as.vector(importance(modsumm))))
importance
write.csv(importance, "Importance-Epi_Richness.csv") #save table

#create new data
Age_epi<-seq(min(Rich2$Age),max(Rich2$Age),0.1)
Pred_epi<-averaged2[1]+(averaged2[2]*log(Age_epi))
SE_epi<-averaged2[1,1]+(averaged2[2,1])

#create predictions

#put into dataframes
Epi_preds<-data.frame(Age=Age_epi,preds=Pred_epi,SE=SE_epi,Tax="Epiphytes")
Epi_preds

##########################################################################################
#plot data alongside model predictions####################################################
##########################################################################################


#put into single dataframe

#limit tree preds to <100 years
Tree_preds<-subset(Tree_preds,Age<100)


Comb<-rbind(Tree_preds,Epi_preds)
predictions<-data.frame(Comb,Type="Plant species richness")

#connect to database
sec <- odbcConnect("Secondary/Degraded forests")
sqlTables(sec)

#import species richness query
Rich<- sqlFetch(sec, "Species richness query")
head(Rich)

#Rename columns
colnames(Rich) <- c("ID", "Site","Disturbance","Age","Type","Measurement","Rich_Ref","Rich_Sec","Det","Tax","Size")
head(Rich)

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

#Calculate richness as a proportion of reference forest
Rich$Prop<-(Rich$Rich_Sec/Rich$Rich_Ref)
Rich$lnRR<-log(Rich$Rich_Sec)-log(Rich$Rich_Ref)
Rich$Proploss<-(Rich$Rich_Sec-Rich$Rich_Ref)/Rich$Rich_Ref
Rich$Proploss2<-(qlogis((Rich$Proploss+ 1) / 2))

#output model predictions
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(predictions, "Model predictions - Richness.csv") #save table

#produce plots of models
head(Rich)
theme_set(theme_bw(base_size=12))
windowsFonts(Times=windowsFont("TT Times New Roman"))
a<-qplot(data=Rich,x=Age,y=Prop,geom="point",shape=I(1),colour=Tax,size=I(2),group=Tax)+geom_line(data=Comb,aes(x=Age,y=plogis(preds)*2),size=1.5)
b<-a+facet_wrap(~Tax)
c<-b
d<-c+ylab("species richness proportional\nto reference forest")+xlab("time since last disturbance (years)")
e<-d+theme(panel.grid.major = element_line(colour =NA))+scale_colour_discrete("Taxonomic group")
f<-e+coord_cartesian(ylim=c(0,1.8),xlim=c(0,175))+geom_hline(y=1,lty=2)+theme(legend.position="none")
f+theme(text=element_text(family="Times"))+scale_colour_manual(values=c("black","black"))
f+theme(text=element_text(family="Times"))

setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")
ggsave(filename="Richness_bw.png",height=87.5,width=175,dpi=1200,units="mm")
ggsave(filename="Richness_colour.png",height=87.5,width=175,dpi=1200,units="mm")


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


setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")
ggsave(filename="Prop_richness.png",height=8,width=12,dpi=300)
ggsave(filename="Prop_richness.jpeg",height=4,width=6,dpi=1200)

#produce plots of models
head(Rich)
theme_set(theme_bw(base_size=12))
windowsFonts(Times=windowsFont("TT Times New Roman"))
a<-qplot(data=Rich,x=Age,y=Prop,geom="point",shape=I(1),colour=Tax,size=I(2),group=Tax)+geom_line(data=Comb,aes(x=Age,y=plogis(preds)*2),size=1.5)
b<-a+facet_wrap(~Tax)
c<-b
d<-c+ylab("species richness proportional\nto reference forest")+xlab("time since last disturbance (years)")
e<-d+theme(panel.grid.major = element_line(colour =NA))+scale_colour_discrete("Taxonomic group")
f<-e+coord_cartesian(ylim=c(0,1.8),xlim=c(0,175))+geom_hline(y=1,lty=2)+theme(legend.position="none")
f+theme(text=element_text(family="Times"))+scale_colour_manual(values=c("black","black"))
f+theme(text=element_text(family="Times"))

setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")
ggsave(filename="Richness_bw.png",height=87.5,width=175,dpi=1200,units="mm")
ggsave(filename="Richness_colour.png",height=87.5,width=175,dpi=1200,units="mm")


#produce plots of model for presentations - without epiphytes
Rich$Tax2<-factor(Rich$Tax,c("Trees","Epiphytes"))
Comb$Tax2<-factor(Comb$Tax,c("Trees","Epiphytes"))
theme_set(theme_bw(base_size=30))
a<-qplot(data=Rich,x=Age,y=Prop,geom="point",shape=I(1),colour=Tax,size=I(4),group=Tax2)+geom_line(data=Comb,aes(x=Age,y=plogis(preds)*2),size=2)
b<-a
c<-b+facet_wrap(~Tax2)
c
d<-c+ylab("Species richness relative\nto undisturbed forest")+xlab("Time since last disturbance (Years)")
e<-d+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA))+scale_colour_manual("Taxonomic group",values=c("NA","blue"))
f<-e+coord_cartesian(ylim=c(0,1.8),xlim=c(0,180))+geom_hline(y=1,lty=2)+theme(legend.position="none")
f
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")
ggsave(filename="Prop_richness_pres_1.jpeg",height=8,width=12,dpi=400)

#produce plots of model for presentations - withepiphytes
Rich$Tax2<-factor(Rich$Tax,c("Trees","Epiphytes"))
Comb$Tax2<-factor(Comb$Tax,c("Trees","Epiphytes"))
theme_set(theme_bw(base_size=30))
a<-qplot(data=Rich,x=Age,y=Prop,geom="point",shape=I(1),colour=Tax,size=I(4),group=Tax2)+geom_line(data=Comb,aes(x=Age,y=plogis(preds)*2),size=2)
b<-a
c<-b+facet_wrap(~Tax2)
c
d<-c+ylab("Species richness relative\nto undisturbed forest")+xlab("Time since last disturbance (Years)")
e<-d+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA))+scale_colour_manual("Taxonomic group",values=c("red","blue"))
f<-e+coord_cartesian(ylim=c(0,1.8),xlim=c(0,180))+geom_hline(y=1,lty=2)+theme(legend.position="none")
f
setwd("C:/Users/Phil/Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")
ggsave(filename="Prop_richness_pres_2.jpeg",height=8,width=12,dpi=400)
