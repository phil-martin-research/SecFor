#belowground biomass plots

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

#Mixed model of relative BGB

#saturated models with log and linear terms
M1lin<-lmer(Proploss2~Age+I(Age^2)+Disturbance*Age+(1|Ran)+(Age|Ran),data=BGB,REML=F)
M1log<-lmer(Proploss2~log(Age)+Disturbance*log(Age)+(1|Ran)+(Age|Ran),data=BGB,REML=F)
AIC(M1lin,M1log)

#model checking looking at residuals
plot(predict(M1lin),resid(M1lin))
plot(predict(M1log),resid(M1log))

#the log model is better
#reset it using REML
M1log<-lmer(Proploss2~log(Age)+Disturbance*log(Age)+(Age|Ran),data=BGB,REML=T)

#model selection using AICc
#run all possible models
MS1<- dredge(M1log, trace = TRUE, rank = "AICc", REML = FALSE)

#subset models with delta<7 (to remove implausible models)
poss_mod<- get.models(MS1, subset =delta<7)
modsumm <- model.sel(poss_mod, rank = "AICc")
modsumm

#calculate marginal r squared for 
#each model using equation from Nakagawa et al  2013
D7_1<-lmer(Proploss2~1+log(Age)+(Age|Ran),data=BGB,REML=F)
D7_2<-lmer(Proploss2~1+log(Age)+Disturbance+(Age|Ran),data=BGB,REML=F)
D7_3<-lmer(Proploss2~1+log(Age)+Disturbance+log(Age)*Disturbance+(Age|Ran),data=BGB,REML=F)

Rsquared_epi<-rbind(r.squaredGLMM(D7_1),r.squaredGLMM(D7_2),r.squaredGLMM(D7_3))

#add this to model summary output
modsumm$MRsquared<-Rsquared_epi[,1]

#output possible models
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(modsumm, "Model - Belowground Biomass.csv")

#calculate model averaged coefficients
#create predictions based on models with delta<7
averaged<-model.avg(MS1,subset=delta<7)
averaged
averaged2<-averaged$avg.model
averaged2

write.csv(averaged2, "Multimodel inferences Belowground Biomass.csv") #save table

#output importance values
#add importance values to estimated
importance<-data.frame(Variable=c("Intercept",labels(importance(modsumm))),Importance=c(1,as.vector(importance(modsumm))))
importance
write.csv(importance, "Importance-BGB.csv") #save table

#create predictions based on model averaged parameters
Age<-seq(0.5,82,.1)

preds_shift<-averaged2[1]+(averaged2[2])+((averaged2[3]*log(Age)))
preds_past<-averaged2[1]+(averaged2[3]*log(Age))
SE_shift<-averaged2[1,2]+(averaged2[2,2])+(averaged2[3,2])
SE_past<-averaged2[1,2]+(averaged2[3,2])

plot(BGB$Age,BGB$Prop,col=BGB$Disturbance)
lines(Age,plogis(preds_shift)*2)
lines(Age,plogis(preds_past)*2)

Shifting<-data.frame(Age=Age,Prediction=preds_shift,SE=SE_shift,Disturbance="Shifting agriculture")
Pasture<-data.frame(Age=Age,Prediction=preds_past,SE=SE_past,Disturbance="Pasture")
Comb<-rbind(Shifting,Pasture)

#export model predictions for later use
predictions<-cbind(Comb,Type="Belowground biomass")
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(predictions, "Model predictions - BGB.csv") #save table

