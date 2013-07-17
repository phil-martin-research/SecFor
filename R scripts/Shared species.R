#script to import, analyse and produce plots for prop biomass in secondary forests

#load in necessary libraries
library(RODBC)
library(ggplot2)
library(nlme)
library(lme4)
library(MuMIn)


#connect to database
sec <- odbcConnect("Secondary/Degraded forests")
sqlTables(sec)

#import proportion query
Prop<- sqlFetch(sec, "Proportion query")
head(Prop)
#Rename columns
colnames(Prop) <- c("ID", "Site","Disturbance","Age","Type","Measurement","Prop_Ref","Prop_Sec","Tax")
head(Prop)
Prop<-data.frame(Prop)
Prop$Prop<-Prop$Prop_Sec/Prop$Prop_Ref

#subset data to remove logging, fire and missing values
Prop<-subset(Prop,Disturbance!="Fire")
Prop<-subset(Prop,Disturbance!="Logging")
Prop<-subset(Prop,Disturbance!="Agroforestry")
Tree_prop<-Prop

#new variable to rescale data
Tree_prop$proploss<-Tree_prop$Prop-1/1
Tree_prop$proploss2<-(qlogis((Tree_prop$proploss+1)))
Tree_prop$asin<-(sqrt(asin(Tree_prop$Prop)))
hist(Tree_prop$proploss2)
head(Tree_prop)

#logit transformation
Tree_prop$logprop<-log(Tree_prop$Prop)-log(1-Tree_prop$Prop)

#Mixed model of Prop prop

#null model
M0<-glmer(Prop~1+(1|ID),family=binomial(logit),data=Tree_prop,REML=F)
nulldev<--2*logLik(M0)[1]

#full model
M1<-glmer(Prop~1+Age+Type+Disturbance+(1|ID),family=binomial(logit),data=Tree_prop,REML=F)
plot(fitted(M1),resid(M1))
summary(M1)
plot(Tree_prop$Age,Tree_prop$Prop)

#model selection using AICc
MS1<- dredge(M1, trace = TRUE, rank = "AICc", REML = FALSE)

#subset models with delta<7 (to remove implausible models)
poss_mod<- get.models(MS1, subset = delta <7)
modsumm <- model.sel(poss_mod, rank = "AICc")
modsumm
plot(Tree_prop$Age,Tree_prop$Prop)
Age<-seq(0,115,0.1)
lines(Age,1/(1+1/(exp(-1.551+(0.01726*(Age))))))
lines(Age,(plogis(-2.2035532+(Age*0.0151852)*2))
#calculate deviance of model
modsumm$dev<--2*modsumm$logLik

#calculate deviance explained for each model
modsumm$dev_ex<-((nulldev-modsumm$dev)/nulldev)
modsumm$dev_ex
#output possible models
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(modsumm, "Model - Species pool.csv")

#calculate model averaged coefficients

#create predictions based on models >0.6 weight
averaged<-model.avg(MS1,subset=cumsum(weight)<=0.6)
averaged2<-averaged$avg.model
write.csv(averaged2, "Multimodel inferences Prop primary.csv") #save table

#set parameter values from best model
Avgest.mod$avg.model
Int<-averaged2[1]
Int
Age<-averaged2[2]
Age

#create new data for predictions
range(Tree_prop$Age)
preds<-expand.grid(Age=seq(1.5,81,0.1))
preds$predicted<-Int+(preds$Age*Age)
preds$Prop<-1/(1+1/(exp(preds$predicted)))


#produce plots of model
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12))
a<-ggplot(Tree_prop,aes(x=Age,y=Prop))+geom_point(size=2,shape=1)+opts(panel.grid.major = theme_line(colour =NA))
a
d<-a+coord_cartesian(ylim = c(0, 1.05),xlim=c(0,120))+ylab("proportion of primary species pool\n found in secondary forest")+geom_hline(y=1,lty=2)
e<-d+xlab("time since last disturbance (years)")+ scale_colour_discrete(name="Type of \nforest")
f<-e+scale_y_continuous(breaks=seq(0, 1, 0.5))+scale_x_continuous(breaks=seq(0, 120, 40))
f+theme(text=element_text(family="Times"))
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")
ggsave(filename="Prop_sp.png",height=100,width=175,dpi=800,units="mm")
