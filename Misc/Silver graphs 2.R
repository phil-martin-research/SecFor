#put in data from Chazdon et al 2009 on plant recruitment in secondary forest
Silver<-read.csv("C:/Documents and Settings/PMART/My Documents/Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning/Carbon - Silver land use.csv",header=T)
head(Silver)
#load ggplot2
library(ggplot2)

#subset data into moist, wet and dry forest
Moist<-subset(Silver,Silver$Life=="M")
Wet<-subset(Silver,Silver$Life=="W")
Dry<-subset(Silver,Silver$Life=="D")


#years to predict over
yearspred<-(seq(from=0,to=90,0.5))
yearspred

#silver model for moist
plot(Moist$Age,Moist$Biomass)
Moist
moistx<-Moist$Age
moistmodel<-glm(log(Moist$Biomass)~log(moistx))
summary(moistmodel)


moist_out<-data.frame(exp(predict.glm(moistmodel,list(moistx=yearspred))))
moist_out$Type<-"Moist"
moist_out$Age<-yearspred
colnames(moist_out)<-c("Predict","Type","Age")
Moistage<-Moist$Age
Moistmass<-Moist$Biomass


#model for dry forest
dryx<-Dry$Age
drymodel<-glm(log(Dry$Biomass)~log(dryx))
summary(drymodel)
dry_out<-data.frame(exp(predict.glm(drymodel,list(dryx=yearspred))))
dry_out$Type<-"Dry"
dry_out
dry_out$Age<-yearspred
colnames(dry_out)<-c("Predict","Type","Age")

#model for wet forest
wetx<-Wet$Age
wetmodel<-glm(log(Wet$Biomass)~log(wetx))
summary(wetmodel)
wet_out<-data.frame(exp(predict.glm(wetmodel,list(wetx=yearspred))))
wet_out$Type<-"Wet"
wet_out$Age<-yearspred
colnames(wet_out)<-c("Predict","Type","Age")

#model for all forest
allx<-Silver$Age
Life<-Silver$Life
expfun<-1-(exp(-allx))
hist(Silver$Biomass)     
plot(density(Silver$Biomass))
logx<-(log(allx))
logbiomass<-(log(Silver$Biomass))
model1<-glm(logbiomass~+allx+I(allx^2)+I(allx^3)+Life)
plot(Silver$Age,Silver$Biomass)                
summary(model1)
plot(model1)
step(model1)
Silver$Predict<-exp(predict(model1))
Silver$Predict

#non-linear model
a<-175
b<-170     
c<--(log((a-40)/170))/4
model2<-nls(Biomass~a-b*exp(-c*Age),group=Life,data=Silver,start=list(a=175,b=170,c=0.05763091))
summary(model2)
predict(model2)
model3<-nls(Biomass~a*exp(-c*Age),data=Silver,start=list(a=175,c=0.05763091))
summary(model3)
anova(model2,model3)
#produce data for predict
length(yearspred)
predry<-rep("D",181)
premoist<-rep("M",181)
prewet<-rep("W",181)

#moist forest prediction
all_moist<-data.frame(predict.glm(allmodel,list(allx=yearspred,Life=premoist)))
all_moist$Type<-"Moist"
all_moist$Age<-yearspred
all_moist
colnames(all_moist)<-c("Predict","Type","Age")

#wet forest prediction
all_wet<-data.frame(predict.glm(allmodel,list(allx=yearspred,Life=prewet)))
all_wet$Type<-"Wet"
all_wet$Age<-yearspred
all_wet
colnames(all_wet)<-c("Predict","Type","Age")

#dry forest prediction
all_dry<-data.frame(predict.glm(allmodel,list(allx=yearspred,Life=predry)))
all_dry$Type<-"Dry"
all_dry$Age<-yearspred
all_dry
colnames(all_dry)<-c("Predict","Type","Age")


#bind model results
modelresults<-rbind(dry_out,moist_out,wet_out)
modelresults
preds<-data.frame(modelresults)
preds$Predict<-exp(preds$Predict)


#bind model results
modelresults<-rbind(all_dry,all_moist,all_wet)
preds<-data.frame(modelresults)
preds$Predict<-exp(preds$Predict)
head(Silver)
colours<-c(NA,"Red","Blue")
#graph age relationship as a function of forest type
c<-ggplot(Silver,aes(x=Age,y=Biomass,shape=factor(Life),colour=factor(Life),alpha=0.5))
d<-c+geom_point()
e<-d+stat_smooth(method=glm,formula=(y)~x,se=F)+theme_bw()
f<-e+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 12, colour = 'black'))+opts(axis.title.y = theme_text(angle=90,size = 12, colour = 'black'))
g<-f+ylab ('Aboveground biomass (Mg/ha)')+xlab ('Age of secondary forest (Years)')
h<-g+geom_rug(size=0.1,colour="black",position='jitter')+ scale_area()+ylim(0,300)+xlim(0,90)+facet_wrap(~Life)
h+opts(legend.position = "none")

setwd("C:/Documents and Settings/PMART/My Documents/Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning")
ggsave("Silver et al 2000 tick type.png",height=3,width=6,dpi=300)

#graph age relationship as a function of previous land use
c<-ggplot(Silver,aes(x=Age,y=Biomass,alpha=0.5,colour=factor(Past.land.use)))
d<-c+geom_point()
e<-d+stat_smooth(method=glm, se=F,formula=(y)~(x),)+theme_bw()+ylim(0,300)+xlim(0,90)
f<-e+opts(legend.position = "none")+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 12, colour = 'black'))+opts(axis.title.y = theme_text(angle=90,size = 12, colour = 'black'))
g<-f+ylab ('Aboveground biomass (Mg/ha)')+xlab ('Age of secondary forest (Years)')
g+geom_rug(size=0.1,colour="black",position='jitter')+ scale_area()+facet_wrap(~Past.land.use)
setwd("C:/Documents and Settings/PMART/My Documents/Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning")
ggsave("Silver et al 2000 tick land use.png",height=3,width=6,dpi=300)
