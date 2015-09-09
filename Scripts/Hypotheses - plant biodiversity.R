#load libraries

library(plyr)
library(ggplot2)

#species richness
Richness_mean<-2*(1-exp(-0.01*Age)^8)-(0.00003*Age^2)+(0.00000005*Age^3)
Richness_min<-1.5*(1-exp(-0.01*Age)^8)-(0.00003*Age^2)+(0.00000005*Age^3)
Richness_max<-2.5*(1-exp(-0.01*Age)^8)-(0.00003*Age^2)+(0.00000005*Age^3)
Richness<-data.frame(cbind(Age,Richness_min,Richness_mean,Richness_max))
Richness$Measure<-"Species richness"
Richness$Type<-"Plant biodiversity"
colnames(Richness)<-c("Age","Min","Mean","Max","Measure","Type")
ggplot(Richness,aes(x=Age,y=Mean,ymin=Min,ymax=Max))+geom_ribbon(fill="red")+geom_line()

#old growth species
OG_mean<-1*(1-exp(-0.0006*Age)^8)
OG_min<-1*(1-exp(-0.0003*Age)^6)
OG_max<-1*(1-exp(-0.001*Age)^8)
OG<-data.frame(cbind(Age,OG_min,OG_mean,OG_max))
OG$Measure<-"Old growth species"
OG$Type<-"Plant biodiversity"
colnames(OG)<-c("Age","Min","Mean","Max","Measure","Type")
ggplot(OG,aes(x=Age,y=Mean,ymin=Min,ymax=Max))+geom_ribbon(fill="red")+geom_line()

#species similarity
Sim_mean<-1*(1-exp(-0.001*Age)^8)
Sim_min<-1*(1-exp(-0.0003*Age)^6)
Sim_max<-1*(1-exp(-0.002*Age)^8)
Sim<-data.frame(cbind(Age,Sim_min,Sim_mean,Sim_max))
Sim$Measure<-"Sorensen similarity"
Sim$Type<-"Plant biodiversity"
colnames(Sim)<-c("Age","Min","Mean","Max","Measure","Type")
ggplot(Sim,aes(x=Age,y=Mean,ymin=Min,ymax=Max))+geom_ribbon(fill="red")+geom_line()


#compile all data on plant biodiversity
pbd<-rbind(Richness,OG,Sim)

#graphs for biodiversity
a<-ggplot(pbd,aes(x=Age,y=Mean,ymax=Max,ymin=Min,fill=factor(Measure)))+geom_ribbon(alpha=0.5)+geom_line(size=1,alpha=0.8)
a
b<-a+theme_bw()+opts(legend.position = "none")+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 20, colour = 'black'))
c<-b+geom_hline(y=1,lty=2,size=1)+facet_wrap(~Measure)+ylab("Proportion of measure \nin mature forest")+opts(axis.title.y = theme_text(size = 20, colour = 'black'))+xlab("Time since last disturbance")
d<-c+theme(axis.text.x = theme_text(size = 12),axis.text.y = theme_text(size = 14),strip.text.x = theme_text(size = 16))
d

#save plot
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis")
ggsave(filename="Plant BD hypothetical.png")

