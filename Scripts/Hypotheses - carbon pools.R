#load libraries

library(plyr)
library(ggplot2)

#litter
Litter_min<-1.0*(1-exp(-0.01*Age)^16)
Litter_mean<-1.0*(1-exp(-0.01*Age)^10)
Litter_max<-1.0*(1-exp(-0.013*Age)^6)
Litter<-data.frame(cbind(Age,Litter_min,Litter_mean,Litter_max))
Litter$Measure<-"Leaf litter mass"
Litter$Type<-"Carbon pools"
colnames(Litter)<-c("Age","Min","Mean","Max","Measure","Type")

ggplot(Litter,aes(x=Age,y=Mean,ymin=Min,ymax=Max))+geom_ribbon(fill="red")+geom_line()

#aboveground biomass
Age<-seq(0,200,0.01)
Biomass_min<-1.0*(1-exp(-0.01*Age)^8)
Biomass_mean<-1.0*(1-exp(-0.01*Age)^3)
Biomass_max<-1.0*(1-exp(-0.03*Age)^0.6)
Biomass<-data.frame(cbind(Age,Biomass_min,Biomass_mean,Biomass_max))
Biomass$Measure<-"Aboveground biomass"
Biomass$Type<-"Carbon pools"
colnames(Biomass)<-c("Age","Min","Mean","Max","Measure","Type")

ggplot(Biomass,aes(x=Age,y=Mean,ymin=Min,ymax=Max))+geom_ribbon(fill="red")+geom_line()



#belowground biomass
B_Biomass_min<-1.0*(1-exp(-0.01*Age)^8)
B_Biomass_mean<-1.0*(1-exp(-0.01*Age)^5)
B_Biomass_max<-1.0*(1-exp(-0.013*Age)^3)
B_Biomass<-data.frame(cbind(Age,B_Biomass_min,B_Biomass_mean,B_Biomass_max))
B_Biomass$Measure<-"Belowground biomass"
B_Biomass$Type<-"Carbon pools"
colnames(B_Biomass)<-c("Age","Min","Mean","Max","Measure","Type")

ggplot(B_Biomass,aes(x=Age,y=Mean,ymin=Min,ymax=Max))+geom_ribbon(fill="red")+geom_line()

#soil carbon
SoilC_min<-rep(0.5,2001)
SoilC_max<-rep(1.5,2001)
SoilC_mean<-rep(1,2001)
SoilC<-data.frame(cbind(Age,SoilC_min,SoilC_mean,SoilC_max))
SoilC$Measure<-"Soil Carbon"
SoilC$Type<-"Carbon pools"
colnames(SoilC)<-c("Age","Min","Mean","Max","Measure","Type")

ggplot(SoilC,aes(x=Age,y=Mean,ymin=Min,ymax=Max))+geom_ribbon(fill="red")+geom_line()


#combine carbon poools
carbon_pools<-rbind(Biomass,B_Biomass,Litter,SoilC)
head(carbon_pools)
carbon_pools$Measure<-factor(carbon_pools$Measure)

#graphs for carbon
a<-ggplot(carbon_pools,aes(x=Age,y=Mean,ymax=Max,ymin=Min,fill=factor(Measure)))+geom_ribbon(alpha=0.5)+geom_line(size=1,alpha=0.8)
a
b<-a+theme_bw()+opts(legend.position = "none")+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 20, colour = 'black'))
c<-b+geom_hline(y=1,lty=2,size=1)+facet_wrap(~Measure)+ylab("Proportion of measure \nin mature forest")+opts(axis.title.y = theme_text(size = 20, colour = 'black'))+xlab("Time since last disturbance")
d<-c+coord_cartesian(ylim=c(0,1.55))+theme(axis.text.x = theme_text(size = 12),axis.text.y = theme_text(size = 14),strip.text.x = theme_text(size = 16))
d
#save plot
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis")
ggsave(filename="Carbon hypothetical.png")

#species richness
Richness_mean<-2*(1-exp(-0.01*Age)^8)-(0.00003*Age^2)+(0.00000005*Age^3)
Richness_min<-1.5*(1-exp(-0.01*Age)^8)-(0.00003*Age^2)+(0.00000005*Age^3)
Richness_max<-2.5*(1-exp(-0.01*Age)^8)-(0.00003*Age^2)+(0.00000005*Age^3)
Richness<-data.frame(cbind(Age,Richness_min,Richness_mean,Richness_max))
Age2<-seq(0,200,0.1)
Age_richness2<-(rep(1,900))
Richness<-(1.5*Age2/(10+Age2))
Richness2<-(Richness)/max(Richness)
Richness2<-(log(Age2)-(0.0002*Age2^2)+(0.000000001*Age2^3)+3)/4
Richness$Measure<-"Species richness"
Richness$Type<-"Plant biodiversity"
colnames(Richness)<-c("Age","Min","Mean","Max","Measure","Type")
ggplot(Richness,aes(x=Age,y=Mean,ymin=Min,ymax=Max))+geom_ribbon(fill="red")+geom_line()


#graphs for biodiversity
a<-ggplot(carbon_pools,aes(x=Age,y=Value,colour=factor(Measure)))+geom_line(size=3,alpha=0.5)
a
b<-a+theme_bw()+opts(legend.position = "none")+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 20, colour = 'black'))
c<-b+geom_hline(y=1,lty=2)+facet_wrap(~Measure)+ylab("Proportion of measure \nin mature forest")+opts(axis.title.y = theme_text(size = 20, colour = 'black'))+xlab("Time since last disturbance")
c+theme(axis.text.y=theme_blank(),axis.ticks=theme_blank())
#save plot
setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Analysis")
ggsave(filename="Carbon hypothetical.png")
                      
