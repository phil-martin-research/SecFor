#put in data from Chazdon et al 2009 on plant recruitment in secondary forest
Silver<-read.csv("C:/Documents and Settings/PMART/My Documents/Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning/Carbon - Silver.csv",header=T)
Silver_use<-read.csv("C:/Documents and Settings/PMART/My Documents/Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning/Carbon - Silver land use.csv",header=T)
head(Silver)
#load ggplot2
library(ggplot2)
library(gridExtra)

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

#model for all forest type
allx<-Silver$Age
Life<-Silver$Life
expfun<-1-(exp(-allx))
hist(Silver$Biomass)     
plot(density(Silver$Biomass))
logx<-(log(allx))
logbiomass<-(log(Silver$Biomass))
model1<-glm(logbiomass~+logx*Life)
plot(Silver$Age,Silver$Biomass)                
summary(model1)
plot(model1)
step(model1)
Silver$Predict<-exp(predict(model1))
Silver$Predict

#model for all forest type
allx<-Silver_use$Age
Life<-Silver_use$Life
expfun<-1-(exp(-allx))
hist(Silver_use$Biomass)     
plot(density(Silver_use$Biomass))
logx<-(log(allx))
logbiomass<-(log(Silver_use$Biomass))
model1<-glm(logbiomass~+logx*Silver_use$Past.land.use)
plot(Silver_use$Age,Silver_use$Biomass)                
summary(model1)
plot(model1)
step(model1)
Silver_use$Predict<-exp(predict(model1))
Silver_use$Predict

#graph age relationship as a function of forest type
type<-ggplot(Silver,aes(x=Age,y=Biomass,colour=factor(Life),alpha=0.5))
type2<-type+geom_point()
type3<-type2+theme_bw()+geom_line(data=Silver,aes(x=Age,y=Predict))
type4<-type3+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 12, colour = 'black'))+opts(axis.title.y = theme_text(angle=90,size = 12, colour = 'black'))
type5<-type4+ylab ('Aboveground biomass (Mg/ha)')+xlab ('Age of secondary forest (Years)')
type6<-type5+geom_rug(size=0.1,colour="black",position='jitter')+ scale_area()+ylim(0,300)+xlim(0,90)+facet_wrap(~Life)
type7<-type6+opts(legend.position = "none")
type7

setwd("C:/Documents and Settings/PMART/My Documents/Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning")
ggsave("Silver type tick.png",height=3,width=6,dpi=300)

#graph age relationship as a function of previous land use
land<-ggplot(Silver_use,aes(x=Age,y=Biomass,colour=factor(Past.land.use),alpha=0.2))
land2<-land+geom_point()
land3<-land2+geom_line(data=Silver_use,aes(x=Age,y=Predict))+theme_bw()
land4<-land3+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 12, colour = 'black'))+opts(axis.title.y = theme_text(angle=90,size = 12, colour = 'black'))
land5<-land4+ylab ('Aboveground biomass (Mg/ha)')+xlab ('Age of secondary forest (Years)')
land6<-land5+geom_rug(size=0.1,colour="black",position='jitter')+ scale_area()+ylim(0,300)+xlim(0,90)+facet_wrap(~Past.land.use)
land7<-land6+opts(legend.position = "none")
land7 
setwd("C:/Documents and Settings/PMART/My Documents/Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning")
ggsave("Silver land use tick.png",height=3,width=6,dpi=300)

grid.arrange(type7,land7,ncol=1,clip=T)
png(file = "Silver_summary.png", width = 1800, height = 1800)
grid.arrange(type7,land7,ncol=1,clip=T)
dev.off() 

