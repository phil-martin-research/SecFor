#put in data from Chazdon et al 2009 on plant recruitment in secondary forest
Silver<-read.csv("C:/Documents and Settings/PMART/My Documents/Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning/Carbon - Silver.csv",header=T)
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
moist_out
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
modelresults<-rbind(all_dry,all_moist,all_wet)
preds<-data.frame(modelresults)
preds$Predict<-exp(preds$Predict)


#graph relationship
a<-ggplot(Silver, aes(x=Age,y=Biomass,colour=factor(Life)))
b<-a+geom_line(data=Silver,aes(x=Age,y=Predict))
b
b+geom_point()+theme_bw()
b+facet_wrap(~Life)
b+geom_line(preds,aes(y=Predict))
c<-b+stat_smooth(method="glm",formula = (y ~ 1-exp(0.5*-x)),se=F)
c+theme_bw()


a<-ggplot(chaz,aes(x=Sqrt_age^2,y=Proportion,size=3,alpha=0.5,colour=factor(Type)))
c<-a+geom_point()+geom_smooth(x=chaz$Sqrt_age^2,y=modelresults,method="glm",se=F,size=1)+scale_area()+scale_colour_manual(values=c("red","blue","orange"))
c
d<-c+theme_bw()+facet_wrap(~Type,scales="free")
e<-d+opts(legend.position = "none")+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 20, colour = 'black'))+opts(axis.title.y = theme_text(angle=90,size = 20, colour = 'black'))
f<-e+ylab ('Proportion of old growth species')+xlab ('Age of secondary forest (Years)')
f+geom_rug(size=1,colour="black")

chaz$age<-chaz$Sqrt_age^2
(chaz$age)^exp(-1)
#graph relationship
a<-ggplot(chaz,aes(x=age,y=Proportion,size=2,alpha=0.5,colour=factor(Type)))
c<-a+geom_line(data=chaz,aes(x=age,y=results,size=1))+geom_point()
d<-c+scale_area()+theme_bw()
e<-d+opts(legend.position = "none")+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 20, colour = 'black'))+opts(axis.title.y = theme_text(angle=90,size = 20, colour = 'black'))
f<-e+ylab ('Proportion of old growth species')+xlab ('Age of secondary forest (Years)')
f+geom_polygon(aes(x=seq(from=42,to=100),y=seq(from=0,to=0.8)))


setwd("C:/Documents and Settings/Phil/My Documents/Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning")
ggsave("Chazdon et al 2009 no facet tick.png",height=6,width=6,dpi=300)


write.table(chaz_plants,"chaz_plants.csv",sep=",")