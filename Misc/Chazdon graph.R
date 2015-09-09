#put in data from Chazdon et al 2009 on plant recruitment in secondary forest
chaz<-read.csv("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning/Chazdon et al 2009 all2.csv",header=T)
head(chaz)
dent<-read.csv("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning/Dent et al 2009.csv",header=T)
head(dent)
#load ggplot2
library(ggplot2)

#subset data into trees, volant and nonvolant species
trees<-subset(chaz,chaz$Type=="Trees")
trees$age<-trees$Sqrt_age^2
volant<-subset(chaz,chaz$Type=="Flying animals")
volant$age<-volant$Sqrt_age^2
nonvolant<-subset(chaz,chaz$Type=="Non-flying animals")
nonvolant$age<-nonvolant$Sqrt_age^2

#years to predict over
yearspred<-data.frame(seq(from=0,to=150,0.5))
yearspred
#model for trees
treemodel<-glm(trees$Proportion~I(trees$age^exp(-10)))
summary(treemodel)
tree_out<-predict(object=treemodel,newdata=yearspred)

#model for volant species
flymodel1<-glm(volant$Proportion~volant$age)
flymodel2<-glm(volant$Proportion~I(volant$age^exp(-9)))
summary(flymodel2)
fly_out<-predict(flymodel2,yearspred)

#model for non-volant species
noflymodel1<-glm(nonvolant$Proportion~nonvolant$age)
noflymodel2<-glm(nonvolant$Proportion~I(nonvolant$age^exp(-18)))
summary(noflymodel2)
nofly_out<-predict(noflymodel2,yearspred)
?predict
#bind model results
modelresults<-c(as.vector(fly_out),as.vector(nofly_out),as.vector(tree_out))
chaz$results<-modelresults
chaz$results
head(chaz)
#graph relationship
a<-ggplot(chaz,aes(x=Sqrt_age^2,y=Proportion,alpha=0.5,colour=factor(Type)))
c<-a+geom_point(size=2)+stat_smooth(method="glm",formula = y ~ x,se=F,size=1)+scale_area()+scale_colour_manual(values=c("red","blue","orange"))
d<-c+theme_bw()+facet_wrap(~Type)
e<-d+opts(legend.position = "none")+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 20, colour = 'black'))+opts(axis.title.y = theme_text(angle=90,size = 20, colour = 'black'))
f<-e+ylab ('Proportion of \nold growth species')+xlab ('Age of secondary forest (Years)')
f+coord_cartesian(xlim=c(0,110),ylim=c(0,1))

setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning")
ggsave("Chazdon et al 2009 facet.png",height=3,width=6,dpi=1200)


chaz$age<-chaz$Sqrt_age^2
(chaz$age)^exp(-1)
#graph relationship
a<-ggplot(chaz,aes(x=age,y=Proportion,alpha=0.5,colour=factor(Type)))
c<-a+geom_line(data=chaz,aes(x=age,y=results),size=0.5)+geom_point(size=3)
c
d<-c+scale_area()+theme_bw()
d
e<-d+opts(legend.position = "none")+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 12, colour = 'black'))+opts(axis.title.y = theme_text(angle=90,size = 12, colour = 'black'))
e
f<-e+ylab ('Proportion of old\ngrowth species')+xlab ('Age of secondary forest (Years)')
f+facet_wrap(~Type)+xlim(0,100)+ylim(0,1)+geom_hline(y=1,lty=2)+geom_rug(size=0.5,colour="black")

setwd("C:/Documents and Settings/PMART/My Documents/Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning")
ggsave("Chazdon et al 2009 facet tick.png",height=3,width=6,dpi=300)


write.table(chaz_plants,"chaz_plants.csv",sep=",")

#graph relationship
a<-ggplot(dent,aes(x=Age,y=Similarity2,alpha=0.5))
b<-a+geom_point(size=3,shape=16,colour="red")
d<-b+scale_area(c(1,3))+theme_bw()
d
e<-d+opts(legend.position = "none")+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 12, colour = 'black'))+opts(axis.title.y = theme_text(angle=90,size = 12, colour = 'black'))
e
f<-e+ylab ('Sorensen similarity')+xlab ('Age of secondary forest (Years)')
f+xlim(0,160)+ylim(0,1)+geom_hline(y=1,lty=2)+stat_smooth(se=F,method="lm",formula = y ~ x+I(x^2),size=1)+coord_cartesian(xlim =c(0,151), ylim =c(0,1.1), wise = NULL)

setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning")
ggsave("Dent et al 2009.png",height=3,width=6,dpi=1200)

