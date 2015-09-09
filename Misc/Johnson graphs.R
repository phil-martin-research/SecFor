#put in data from Chazdon et al 2009 on plant recruitment in secondary forest
Johnson<-read.csv("C:/Documents and Settings/PMART/My Documents/Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning/Biomass - Johnson 2000.csv",header=T)
Johnson<-data.frame(Johnson)
#load ggplot2
library(ggplot2)



#model for biomass accumulation
model1<-glm(Johnson$Biomass~Johnson$Age)
ggplot(data=Johnson,aes(x=Age,y=Biomass))+geom_point()
       
+Johnson$Growing_season+Johnson$Season_temp+Johnson$Season_precip+Johnson$Soil.type+Johnson$Soil.type*Johnson$Age)             
summary(model1)
model2<-glm(log(Johnson$Biomass)~Johnson$Age+Johnson$Soil.type+Johnson$Soil.type*Johnson$Age)
summary(model2)
plot(model1)
Johnson$Predict<-exp(predict(model1))
Johnson$Predict
ages1<-data.frame(seq(from=0,to=50,by=0.1))
ages2<-data.frame(seq(from=0,to=50,by=0.1))
#ages to predict over
agespredict<-data.frame(-1.152+ages*6.841)
sandypred<-data.frame(agespredict*(6.841-4.362)+7.383)
predictions<-rbind(agespredict,sandypred)
age<-rbind(ages1,ages2)
Soil_type<-rep(c("Non-sandy","Sandy"),c(501,501))
Soil_predictions<-cbind(predictions,age,Soil_type)
colnames(Soil_predictions)[1]<-"Predictions"
colnames(Soil_predictions)[2]<-"Age"
colnames(Soil_predictions)[3]<-"Soil.type"
head(Soil_predictions)
Johnson$Growing_season
rep(c("Non-sandy","Sandy"),c(51))
new.data<-expand.grid(Age=rep(0:50,2),Soil_type=rep(c("Non-sandy","Sandy"),c(51)))
smoothx<-seq(from=0,to=50)
smoothx
glm(model3,newdata=smoothx,type="response")

model3<-lm(Johnson$Biomass~Johnson$Age)
Johnson$predict
?predict

#graph age relationship as a function of soil type
a<-ggplot(Johnson,aes(Age,Biomass,colour=factor(Soil.type),alpha=0.5,size=2))
b<-a+geom_point()
c<-b+geom_line(data=Johnson,aes(y=predict),size=0.5)+theme_bw()
d<-c+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 12, colour = 'black'))+opts(axis.title.y = theme_text(angle=90,size = 12, colour = 'black'))
d
e<-d+ylab ('Aboveground biomass (Mg/ha)')+xlab ('Age of secondary forest (Years)')
e
f<-e+ scale_size()+ylim(0,300)+xlim(0,50)
f
f+opts(legend.position = "none")

setwd("C:/Documents and Settings/PMART/My Documents/Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning")
ggsave("Silver et al 2000 tick type.png",height=3,width=6,dpi=300)

#graph growing season relationship as a function of soil type
a<-ggplot(Johnson,aes(x=Growing_season,y=Biomass,alpha=0.5,size=2))
b<-a+geom_point()
c<-b+stat_smooth(method=glm,fullrange=TRUE,size=0.5,se=FALSE)+theme_bw()
c
d<-c+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 12, colour = 'black'))+opts(axis.title.y = theme_text(angle=90,size = 12, colour = 'black'))
d
e<-d+ylab ('Aboveground biomass (Mg/ha)')+xlab ('Gowing season length (Days)')
e
f<-e+ scale_area()
f
f+opts(legend.position = "none")