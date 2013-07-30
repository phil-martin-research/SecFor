#script to import, analyse and produce plots for proportion of Soil C in secondary forests

#load in necessary libraries
library(RODBC)
library(ggplot2)
library(lme4)
library(MuMIn)

#connect to database
sec <- odbcConnect("Secondary/Degraded forests")
sqlTables(sec)

#import aboveground biomass query
Soil_C<- sqlFetch(sec, "Soil carbon query")
head(Soil_C)

#Rename columns
colnames(Soil_C) <- c("ID", "Site","Depth","Disturbance","Age","Type","Measurement","Soil_C_Ref","Soil_C_Sec","Soil_type")
head(Soil_C)

#Calculate soil c as a proportion of reference forest
Soil_C$proploss<-((Soil_C$Soil_C_Sec)-(Soil_C$Soil_C_Ref))/Soil_C$Soil_C_Ref
Soil_C$proploss2<-qlogis((Soil_C$proploss+1)/2)
Soil_C$Prop<-((Soil_C$Soil_C_Sec)/(Soil_C$Soil_C_Ref))


#subset data to remove logging, fire and missing values
Soil_C<-subset(Soil_C,Soil_C$Disturbance!="Fire")
Soil_C<-subset(Soil_C,Soil_C$Disturbance!="Logging")
Soil_C<-subset(Soil_C,Soil_C$Disturbance!="Agroforestry")
Soil_C<-subset(Soil_C,Soil_C$Disturbance!="Plantation")
Soil_C<-subset(Soil_C,Soil_C$Type!="NA")

#change types of forest
levels(Soil_C$Type)[levels(Soil_C$Type)=="Tropical dry forest"] <- "Dry"
levels(Soil_C$Type)[levels(Soil_C$Type)=="Tropical moist forest"] <- "Moist"
levels(Soil_C$Type)[levels(Soil_C$Type)=="Tropical rainforest"] <- "Wet"
levels(Soil_C$Type)[levels(Soil_C$Type)=="Tropical montane forest"] <- "Montane"


#create column for reference as a factor
Soil_C$Ran<-as.factor(Soil_C$Soil_C_Ref)
Soil_C$Ran

#select only the columns I'm interested in
Soil_C<-subset(Soil_C,select=-c(Soil_type,Prop,ID,proploss))

#remove datapoints with na
Soil_C<-Soil_C[complete.cases(Soil_C),]


#plots to look at variation by random factors
ggplot(Soil_C,aes(x=Age,y=proploss2))+geom_point()+facet_wrap(~Ran)


#Mixed model of relative Soil_C
#saturated model
Soil_C$Agesq<-Soil_C$Age^2
M1<-lmer(proploss2~Age+Agesq+Disturbance+(Age|Ran)+(1|Depth),data=Soil_C,REML=F)
summary(M1)

#model checking looking at residuals
plot(fitted(M1),M1@resid)

#model selection using AICc

#run all possible models
MS1<- dredge(M1, trace = TRUE, rank = "AICc",subset=dc(Age,Agesq),REML = FALSE)

#subset models with delta<7 (to remove implausible models)
poss_mod<-subset(MS1,MS1$delta<7)
modsumm <- model.sel(poss_mod, rank = "AICc")
modsumm<-subset(modsumm,modsumm$delta<7)

#calculate marginal r squared for 
#each model using equation from Nakagawa et al  2013
D7_1<-lmer(proploss2~1+(Age|Ran)+(1|Depth),data=Soil_C,REML=F)
D7_2<-lmer(proploss2~Age+Agesq+(Age|Ran)+(1|Depth),data=Soil_C,REML=F)
D7_3<-lmer(proploss2~Age+(Age|Ran)+(1|Depth),data=Soil_C,REML=F)
D7_4<-lmer(proploss2~Disturbance+(Age|Ran)+(1|Depth),data=Soil_C,REML=F)
D7_5<-lmer(proploss2~Age+Agesq+Disturbance+(Age|Ran)+(1|Depth),data=Soil_C,REML=F)
D7_6<-lmer(proploss2~Age+Disturbance+(Age|Ran)+(1|Depth),data=Soil_C,REML=F)


Rsquared_epi<-rbind(r.squaredGLMM(D7_1),r.squaredGLMM(D7_2),r.squaredGLMM(D7_3),r.squaredGLMM(D7_4),r.squaredGLMM(D7_5),r.squaredGLMM(D7_6))

#add this to model summary output
modsumm$MRsquared<-Rsquared_epi[,1]

#output table as csv file
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(modsumm, "Model - Soil C.csv")

#create new table of predictions

#model averaging
averaged<-model.avg(poss_mod)
averaged2<-averaged$avg.model

#output importance values
#add importance values to estimated
importance<-data.frame(Variable=c("Intercept",labels(importance(modsumm))),Importance=c(1,as.vector(importance(modsumm))))
importance
write.csv(importance, "Importance-SoilC.csv") #save table

#create predictions for age
Age<-seq(min(Soil_C$Age),max(Soil_C$Age),0.1)
preds<-averaged2[1]+(Age*averaged2[2])
SE<-averaged2[1,2]+averaged2[2,2]

plot(Soil_C$Age,plogis(Soil_C$proploss2)*2)
lines(Age,plogis(preds)*2)

#export model predictions for later use
predictions<-data.frame(Age=Age,Prediction=preds,SE=SE,Type="Soil Carbon")
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(predictions, "Model predictions - Soil C.csv") #save table
