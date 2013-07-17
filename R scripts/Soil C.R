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

M1<-lmer(proploss2~Age+I(Age^2)+Type+Disturbance+(Age|Ran)+(1|Depth),data=Soil_C,REML=F)
summary(M1)
dev<--2*logLik(M1)[1]

#model checking looking at residuals
plot(fitted(M1),M1@resid)
qqnorm(M1)

#Mixed model of Soil_C prop
#null model
M0<-lmer(proploss2~1+(Age|Ran)+(1|Depth),data=Soil_C,REML=F)
nuldev<--2*logLik(M0)[1]


#model selection using AICc

#run all possible models
MS1<- dredge(M1, trace = TRUE, rank = "AICc", REML = FALSE)

#subset models with delta<7 (to remove implausible models)
poss_mod<-subset(MS1,MS1$delta<7)
modsumm <- model.sel(poss_mod, rank = "AICc")
modsumm<-subset(modsumm,modsumm$delta<7)

#calculate deviance explained for each model
modsumm$dev<--2*(modsumm$logLik)
modsumm$dev_ex<-1-(modsumm$dev/nuldev)
modsumm

importance(modsumm)

#calculate model averaged coefficients
poss_mod2<-get.models(MS1,subset=cumsum(weight)<=0.95)
Avgest.mod <- model.avg(poss_mod2)
summary(Avgest.mod)

#output table as csv file
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(modsumm, "Model - Soil C.csv")

#create new table of predictions

#model averaging
averaged<-model.avg(poss_mod)
averaged2<-averaged$avg.model

#create predictions for depth
depth<-seq(5,100,0.1)
preds<-depth*0.0043119324
pu<-depth*0.0101405467
pl<-depth*-0.0015166818
preds<-data.frame(depth,preds,pu,pl)

#plot this
theme_set(theme_bw(base_size=12))
windowsFonts(Times=windowsFont("TT Times New Roman"))
a<-ggplot()
b<-a+geom_ribbon(data=preds,aes(x=depth,ymax=plogis(pu)*2,ymin=plogis(pl)*2,y=NULL),fill="grey",alpha=0.5)
c<-b+geom_jitter(data=Soil_C,aes(x=Depth,y=plogis(proploss2)*2,colour=Disturbance),size=2,alpha=0.5)
d<-c+geom_line(data=preds,aes(x=depth,y=plogis(preds)*2),size=1)+ylab("soil carbon relative to primary forest")+xlab("depth to which soil was measured (cm)")
d+geom_hline(y=1,lty=2)
#save plot
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Figures/For reviewers")
ggsave(filename="Depth.jpeg",height=7,width=12,dpi=1200)


#create predicitons based on model averaged parameters
expand.grid(Age=seq(0,90,.1))
range(subset(Soil_C$Age,Soil_C$Disturbance=="Pasture"))
range(subset(Soil_C$Age,Soil_C$Disturbance=="Arable agriculture"))
range(subset(Soil_C$Age,Soil_C$Disturbance=="Shifting agriculture"))

Pasture<-data.frame(Age=seq(0,90,.1),Disturbance="Pasture")
Shifting<-data.frame(Age=seq(0,50,.1),Disturbance="Shifting agriculture")
Arable<-data.frame(Age=seq(0,75,.1),Disturbance="Arable agriculture")

Pasture$Pred<-averaged2[1]+(Pasture$Age*averaged2[2])+(averaged2[3])
Shifting$Pred<-averaged2[1]+(Shifting$Age*averaged2[2])+(averaged2[4])
Arable$Pred<-averaged2[1]+(Arable$Age*averaged2[2])
Pasture$SE<-averaged2[1,2]+(averaged2[2,2])+(averaged2[3,2])
Shifting$SE<-averaged2[1,2]+(averaged2[2,2])+(averaged2[4,2])
Arable$SE<-averaged2[1,2]+(averaged2[2,2])

comb<-rbind(Pasture,Shifting,Arable)
head(comb)

#export model predictions for later use
predictions<-data.frame(Age=comb$Age,Disturbance=comb$Disturbance,Prediction=comb$Pred,SE=comb$SE,Type="Soil Carbon")
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
write.csv(predictions, "Model predictions - Soil C.csv") #save table

averaged[1]

##Summary table of estimates
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis/Statistics")
M1.results <- matrix(data = NA, nrow =length(averaged$term.names), ncol = 4) #create table
rownames(M1.results) <- averaged$term.names  #rows named after parameters
colnames(M1.results) <- c("Coefficients","SE","Lower CI","Upper CI") #columns names after attributes
M1.results[,1] <- (averaged2[,1]) #fill up table
M1.results[,2] <- (averaged2[,2])
M1.results[,3] <- (averaged2[,4])
M1.results[,4] <- (averaged2[,5])
M1.results

write.csv(M1.results, "Multimodel inferences Soil C.csv") #save table
