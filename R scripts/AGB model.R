##########################################################################
#script to import, analyse and produce plots for relative################# 
#aboveground biomass in secondary tropical forests########################
##########################################################################

#name:Phil Martin
#date:17/07/2013
#version of R used: 2.15.3


#load in necessary libraries
library(RODBC)
library(ggplot2)
library(nlme)
library(lme4)
library(MuMIn)
library(plyr)
library(reshape)

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

#Mixed model of relative AGB
M1<-lmer(Change~Age+I(Age^2)+log(Age)+Disturbance*Age+Type+(Age|Ran)+(1|WG)+(1|Height)+(1|LG),data=AGB2,REML=T)
M2<-lmer(Change~Age+I(Age^2)+log(Age)+Disturbance*Age+Type+(Age|Ran)+(1|WG)+(1|Height),data=AGB2,REML=T)
M3<-lmer(Change~Age+I(Age^2)+log(Age)+Disturbance*Age+Type+(Age|Ran)+(1|Height),data=AGB2,REML=T)
M4<-lmer(Change~Age+I(Age^2)+log(Age)+Disturbance*Age+Type+(Age|Ran)+(1|Height),data=AGB2,REML=T)
M5<-lmer(Change~Age+log(Age)+I(Age^2)+Disturbance*Age+Type+(Age|Ran)+(1|LG),data=AGB2,REML=T)

summary(M5)

#test for best random effects

AIC(M1,M2,M3,M4,M5)

#looks like it's M5 - so we need to include random intercept and
#slope with age for each study along with whehter allometry was local
#or more general

#now we need to check whether it's best to include log or linear terms
M1lin<-lmer(Change~Age+I(Age^2)+Disturbance*Age+Type+(Age|Ran)+(1|LG),data=AGB2,REML=F)
M1log<-lmer(Change~log(Age)+I(log(Age)^2)+Disturbance*Age+Type+(Age|Ran)+(1|LG),data=AGB2,REML=F)

AIC(M1lin,M1log)

#the log model looks best
#run it again but with REML=T
M1<-lmer(Change~log(Age)+I(log(Age)^2)+Disturbance*log(Age)+Type+(Age|Ran)+(1|LG),data=AGB2,REML=T)

#fit null model and calculate the deviance for later calculations
M0<-lmer(Change~1+(Age|Ran)+(1|WG)+(1|Height)+(1|LG),data=AGB2,REML=F)
null_dev<--2*logLik(M0)[1]

#diagnostic plots
plot(fitted(M1),M1@resid)

AGB2$logAge<-log(AGB2$Age)
AGB2$logAgesq<-log(AGB2$Age)^2

M1<-lmer(Change~logAge+Disturbance*logAge+Type+(Age|Ran)+(1|LG),data=AGB2,REML=F)

#fit all models
MS1<- dredge(M1, trace = TRUE, rank = "AICc",REML = FALSE)
     
#subset models with delta<7 (to remove implausible models)
poss_mod<- get.models(MS1, subset = delta<7)
modsumm <- model.sel(poss_mod, rank = "AICc")
modsumm<-subset(modsumm,modsumm$delta<7)

#calculate deviance  for each model
modsumm$dev<--2*modsumm$logLik
     
#calculate deviance explained for each model
modsumm$dev_ex<-1-(modsumm$dev/null_dev)
modsumm
importance(modsumm)

#calculate marginal r squared for 
#each model using equation from Nakagawa et al  2013
D7_1<-lmer(Change~1+logAge+(Age|Ran),data=AGB2,REML=F)
D7_2<-lmer(Change~1+logAge+Disturbance+(Age|Ran),data=AGB2,REML=F)
D7_3<-lmer(Change~1+logAge+Disturbance+Disturbance*logAge+(Age|Ran),data=AGB2,REML=F)
D7_4<-lmer(Change~1+logAge+Type+(Age|Ran),data=AGB2,REML=F)

Rsquared_epi<-rbind(r.squaredGLMM(D7_1),r.squaredGLMM(D7_2),r.squaredGLMM(D7_3),r.squaredGLMM(D7_4))

#add this to model summary output
modsumm$MRsquared<-Rsquared_epi[,1]

#output table as csv file
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(modsumm, "Model - Biomass.csv")

#create predictions based on models >0.95 weight
averaged<-model.avg(MS1,subset=delta<7)
averaged2<-averaged$avg.model
importance(averaged)

#output parameter estimates
write.csv(averaged2, "Multimodel inferences Biomass.csv") #save table

#output importance values
importance<-data.frame(Variable=c("Intercept",labels(importance(modsumm))),Importance=c(1,as.vector(importance(modsumm))))
importance
write.csv(importance, "Importance-AGB.csv") #save table

#create predictions based on model averaged parameters
Age<-seq(0.5,82,.1)

preds<-averaged2[1]+(averaged2[2]*log(Age))
SE<-averaged2[1,2]+(averaged2[2,2])


#plot these results
plot(AGB2$Age,plogis(AGB2$Change)*2)
lines(Age,plogis(preds)*2)
lines(Age,plogis(preds+(2*SE))*2,lty=2)
lines(Age,plogis(preds-(2*SE))*2,lty=2)


#export model predictions for later use
predictions<-data.frame(Prediction=preds,SE=SE,Type="Aboveground biomass")
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(predictions, "Model predictions - Biomass.csv") #save table



