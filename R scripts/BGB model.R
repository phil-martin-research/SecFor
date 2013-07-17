#belowground biomass plots

#load in necessary libraries
library(RODBC)
library(ggplot2)
library(lme4)
library(nlme)
library(reshape2)

#import aboveground biomass query
sec <- odbcConnect("Secondary/Degraded forests")
BGB<- sqlFetch(sec, "Belowground biomass query")
#Rename columns
colnames(BGB) <- c("ID", "Site","Disturbance","Age","Type","Measurement","BGB_Ref","BGB_Sec","SS")
head(BGB)

#redo sample sizes
BGB$SS[BGB$SS<0]<-1

#Calculate prortion lost relative to primary forest
BGB$Prop<-(BGB$BGB_Sec)/(BGB$BGB_Ref)
BGB$lnRR<-log(BGB$BGB_Sec)-log(BGB$BGB_Ref)
BGB$Proploss<-((BGB$BGB_Sec)-(BGB$BGB_Ref))/(BGB$BGB_Ref)
BGB$Proploss2<-(qlogis((BGB$Proploss+ 1) / 2))
plot(BGB$Age,plogis(BGB$Proploss2)*2)
plot(BGB$Age,((plogis(BGB$Proploss2)*2))) 

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

ggplot(BGB,aes(y=lnRR,x=Age))+geom_point()+facet_wrap(~Ran)

#Mixed model of relative BGB

#null model
M0<-lmer(Proploss2~(1|Ran)+(Age|Ran),data=BGB,REML=F)
#set null deviance as equivalent to null model
nuldev<--2*logLik(M0)[1]

#saturated model
M1<-lmer(Proploss2~Age+log(Age)+Disturbance*Age+(1|Ran)+(Age|Ran),data=BGB,REML=F)
dev<--2*logLik(M1)[1]

#model checking looking at residuals
M1_res<-data.frame(predict(M1),(M1@resid))
colnames(M1_res)<-c("Pred","Resid")
ggplot(M1_res,aes(x=Pred,y=Resid))+geom_point()+geom_line(y=0,size=2)+geom_smooth(se=F)
qplot(x=BGB$Age,y=M1@resid)+geom_point()+geom_line(y=0,size=2)+geom_smooth(se=F)

#model selection using AICc

#run all possible models
MS1<- dredge(M1, trace = TRUE, rank = "AICc", REML = FALSE)

#subset models with delta<7 (to remove implausible models)
poss_mod<- get.models(MS1, subset = delta <7)
modsumm <- model.sel(poss_mod, rank = "AICc")
modsumm

#calculate deviance of model
modsumm$dev<--2*modsumm$logLik
#calculate deviance explained for each model
modsumm$dev_ex<-((nuldev-modsumm$dev)/nuldev)
modsumm$dev_ex


#output possible models
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(modsumm, "Model - Belowground Biomass.csv")

#calculate model averaged coefficients
Avgest.mod <- model.avg(poss_mod)
Model_av<-Avgest.mod$avg.model
(Model_av)[,5]
Avgest.mod$term.names
colnames(M1.results)
##Summary table of estimates
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
M1.results <- matrix(data = NA, nrow = length(Avgest.mod$term.names), ncol = 4) #create table
rownames(M1.results) <- Avgest.mod$term.names  #rows named after parameters
colnames(M1.results) <- c("Coefficients","SE","Lower CI","Upper CI") #columns names after attributes
M1.results[,1] <- (Model_av[,1]) #fill up table
M1.results[,2] <- (Model_av[,2])
M1.results[,3] <- (Model_av[,4])
M1.results[,4] <- (Model_av[,5])
M1.results

write.csv(M1.results, "Multimodel inferences Belowground Biomass.csv") #save table

#model averaging
averaged<-get.models(MS1,subset=cumsum(weight)<=0.6)
averaged1<-model.avg(averaged)
M1.results<-averaged$avg.model
averaged


#create predictions based on model averaged parameters
range(subset(BGB$Age,BGB$Disturbance=="Shifting agriculture"))
range(subset(BGB$Age,BGB$Disturbance=="Pasture"))

Age<-seq(0.5,82,.1)

preds_shift<-M1.results[1]+(M1.results[2])+(M1.results[3]*log(Age))+(M1.results[4]*Age)+(M1.results[5]*Age)
preds_past<-M1.results[1]+(M1.results[3]*log(Age))+(M1.results[4]*Age)
SE_shift<-M1.results[1,2]+(M1.results[2,2])+(M1.results[3,2])+(M1.results[4,2])+(M1.results[5,2])
SE_past<-M1.results[1,2]+(M1.results[3,2])+(M1.results[4,2])



preds_shift<-M1.results[1]+(M1.results[2])+(M1.results[3]*log(Age))+(M1.results[4]*Age)+(M1.results[5]*Age)
preds_past<-M1.results[1]+(M1.results[3]*log(Age))+(M1.results[4]*Age)
SE_shift<-M1.results[1,2]+(M1.results[2,2])+(M1.results[3,2])+(M1.results[4,2])+(M1.results[5,2])
SE_past<-M1.results[1,2]+(M1.results[3,2])+(M1.results[4,2])

Shifting<-data.frame(Age=Age,Prediction=preds_shift,SE=SE_shift,Disturbance="Shifting agriculture")
Pasture<-data.frame(Age=Age,Prediction=preds_past,SE=SE_past,Disturbance="Pasture")
Comb<-rbind(Shifting,Pasture)

#export model predictions for later use
predictions<-cbind(Comb,Type="Belowground biomass")
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(predictions, "Model predictions - BGB.csv") #save table


#combine predictions
All<-rbind(Shifting,Pasture)
All$trans<-(plogis(All$prediction)*2)
plot(All$Age,exp(All$prediction))
lines(All$Age,exp(All$prediction-(1.96*All$SE)))

#produce plots of model
a<-ggplot(BGB)+theme_bw()+opts(panel.grid.major = theme_line(colour =NA))
b<-a+geom_line(data=All,aes(y=exp(prediction),x=Age,colour=Disturbance),size=1.5,alpha=0.8)+geom_point(data=BGB,size=4,alpha=0.5,aes(y=exp(lnRR),x=Age,colour=Disturbance))
b
c<-b+opts(panel.grid.major = theme_line(colour ="grey"))+opts(panel.grid.minor = theme_line(colour ="grey"))+opts(axis.title.x = theme_text(size = 20, colour = 'black'))+opts(axis.text.x = theme_text(size = 20, colour = 'black'))+opts(axis.text.y = theme_text(size = 20, colour = 'black'))+ scale_area()
d<-c+coord_cartesian(xlim=c(0,82),ylim=c(0,1.2))+ylab("Biomass relative to primary forest")+theme(axis.title.y = element_text(size = 20, colour = 'black'))
e<-d+xlab("Time since last disturbance (Years)")+ scale_colour_discrete(name="Type of \n disturbance")
f<-e+ theme(legend.title = element_text(colour="black", size=20, face="bold"))+scale_x_continuous(breaks=seq(0, 80, 20))+scale_y_continuous(breaks=seq(0, 1, .5))
g<-f+geom_hline(y=1,lty=2)+opts(strip.text.x = theme_text(size = 20))+theme_bw()+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 12, colour = 'black'))+opts(axis.title.y = theme_text(angle=90,size = 12, colour = 'black'))
g+scale_colour_manual(values = c("grey","black"))

setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures")
ggsave(filename="Prop_BGB.png",height=6.5,width=10,dpi=1200)