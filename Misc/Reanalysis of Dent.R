#put in data from Chazdon et al 2009 on plant recruitment in secondary forest
Dent<-read.csv("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Chapters/4. Forest restoration trajectories/Planning/Dent data.csv",header=T)
head(Dent)

#subset data to remove missing values
Dent_sub<-Dent[complete.cases(Dent),]
head(Dent_sub)

#subset data to remove -9999 for disturbance and isolation
Dent_sub2<-subset(Dent_sub,Dent_sub$Disturbance!="-9999")
Dent_sub2<-subset(Dent_sub,Dent_sub$Isolation!="-9999")

#put in species richness measure
Dent_sub2$Rich_prop<-Dent_sub2$Sec_rich/Dent_sub2$Prim_rich
Dent_sub2$Rich_proploss<-(Dent_sub2$Sec_rich-Dent_sub2$Prim_rich)/Dent_sub2$Prim_rich
Dent_sub2$Rich_proploss2<-qlogis((Dent_sub2$Rich_proploss+1)/4)
summary(Dent_sub2$Rich_proploss2)
Dent3<-Dent_sub2[complete.cases(Dent_sub2),]
Dent4<-subset(Dent3,Rich_proploss2!=-Inf)
Dent4$Isolation<-as.factor(Dent4$Isolation)
plot(Dent_sub2$Rich_prop,Dent_sub2$Rich_proploss2)

#load libraries
library(nlme)
library(MuMIn)
library(ggplot2)



#plot graphs for data exploration

ggplot(Dent_sub,aes(x=Age,y=Prop_sp,colour=Organism))+geom_point()+facet_wrap(~Organism)
ggplot(Dent_sub,aes(x=Age,y=Sec_rich/Prim_rich,colour=Organism))+geom_point()+facet_wrap(~Organism)
ggplot(Dent_sub,aes(x=Age,y=Jaccard,colour=Organism))+geom_point()+facet_wrap(~Organism)
ggplot(Dent_sub2,aes(x=Age,y=Soren,colour=Organism))+geom_point()+facet_grid(Disturbance~Organism)


#mixed model for species richness
head(Dent_sub2)
rich_mod1.1<-lme(Rich_proploss2~Age+log(Age)+Disturbance+Isolation+Organism*Age,random=~1+Age|Study,data=Dent4,method="ML")
rich_mod1.2<-lme(Rich_proploss2~Age+I(Age^2)+Disturbance*Age+Isolation*Age+Organism*Age,random=~1+Age|Study,data=Dent4)
rich_mod1.3<-lme(Rich_proploss2~Age+I(Age^2)+Disturbance*Age+Isolation*Age+Organism*Age,random=~1+Age+I(Age^2)|Study,data=Dent4)

AIC(rich_mod1.1,rich_mod1.2,rich_mod1.3)
summary(rich_mod1.1)

#model checking
plot(rich_mod1.1)
qqnorm(rich_mod1.1)
plot(augPred(rich_mod1.1,primary=~Age),grid=T)

#model selection using AICc
MS1<- dredge(rich_mod1.1, trace = TRUE, rank = "AICc", REML = F)
poss_mod<-subset(MS1,MS1$delta<7)
poss_mod

mod.av<-model.avg(MS1,subset=cumsum(weight)<=0.6)
mo.av.co<-mod.av$avg.model



#predicted change with age
range(Dent_sub2$Age)
head(Dent_sub2)

Age<-seq(1,150,0.1)
Pred<-mo.av.co[1]+(mo.av.co[3]*log(Age_pred))
Preds<-data.frame(Age=Age,Pred=Pred)
Pred_rich2<-mo.av.co[1]+(mo.av.co[3]*log(Age_pred))+mo.av.co[2]

#plot model results
theme_set(theme_bw(base_size=26))
a<-qplot(Dent4$Age,plogis(Dent4$Rich_proploss2)*4,size=I(4),shape=I(1))+ylab("Richness relative to primary forest")+xlab("Time since last disturbance(Years)")
b<-a+coord_cartesian(ylim=c(0,4),xlim=c(0,150))+theme(panel.grid.major = element_line(colour =NA))
c<-b+geom_line(data=Preds,aes(x=Age,y=plogis(Pred)*4),size=2)+geom_hline(y=1,lty=2,size=1.5)

setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Presentations/Secondary forest recovery - 15min")
ggsave(filename="Dent_richness.png",height=8,width=12,dpi=300)

#blank plot for presentation
theme_set(theme_bw(base_size=26))
a<-qplot(Dent4$Age,plogis(Dent4$Rich_proploss2)*4,size=I(4),shape=I(1),geom="blank")+ylab("Richness relative to primary forest")+xlab("Time since last disturbance(Years)")
b<-a+coord_cartesian(ylim=c(0,4),xlim=c(0,150))+theme(panel.grid.major = element_line(colour =NA))
c<-b+geom_hline(y=1,lty=2,size=1.5)
c

setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Presentations/Secondary forest recovery - 15min")
ggsave(filename="Dent_richness_blank.png",height=8,width=12,dpi=300)

hist(Dent_sub$Prop_sp)
#mixed model for proportion of forest sp
head(Dent_sub)
Dent_sub$Prop_sqrt<-sqrt(asin((Dent_sub$Prop_sp)))
plot(Dent_sub$Age,sqrt(asin(Dent_sub$Prop_sp)))
Dent5<-subset(Dent4,Dent4$logisProp<Inf)


head(Dent_sub)
prop_mod1<-lme(Prop_sqrt~Age+log(Age)+Isolation,random=~1|Study,data=Dent_sub,method="ML")
summary(prop_mod1)
prop_mod2<-update(prop_mod1,.~.-I(Age^2))
summary(prop_mod2)
prop_mod3<-update(prop_mod2,.~.-Age)
summary(prop_mod3)

dredge(prop_mod1)

plot(prop_mod1)
qqnorm(prop_mod1)
AICc(prop_mod1,prop_mod2,prop_mod3)

prop_mod4<-update(prop_mod2,method="REML")
summary(prop_mod4)

Age<-seq(1,150,0.1)
preds<-0.5974+(0.003565*Age)+(0.04934*(log(Age)))
transpred<-data.frame(Age=Age,preds=sin(preds^2))
plot(Dent_sub$Age,Dent_sub$Prop_sp)
lines(Age,transpred)
#model checking
M1_res<-data.frame(predict(prop_mod1),(prop_mod1@resid))
colnames(M1_res)<-c("Pred","Resid")
plot(Dent5$Age,plogis(predict(prop_mod1)))
ggplot(M1_res,aes(x=(Pred),y=Resid))+geom_point()+geom_line(y=0,size=2)
plot(prop_mod1)
qqnorm(prop_mod1)


Age_pred<-seq(1,150,0.1)
Age_predC<-seq(1,50,0.1)
Age_predP<-seq(1,70,0.1)
Age_predS<-seq(1,30,0.1)
Pred_prop_A<-Averaged2[1]+(Averaged2[6]*Age_pred)+(Averaged2[5]*(log(Age_pred)))+(Averaged2[7]*(log(Age_pred)))
plot(Age_pred,plogis(Pred_prop_A))
Pred_prop_C<-Int+(Age*Age_predC)+C+(Age*C_slope)
Pred_prop_P<-Int+(Age*Age_predP)+P+(Age*P_slope)
Pred_prop_S<-Int+(Age*Age_predS)+S+(Age*S_slope)

Prop_preds<-data.frame(Age_pred,Pred_prop_A,"A")
(Prop_preds)
Prop_predsC<-data.frame(Age_predC,Pred_prop_C,"C")
Prop_predsP<-data.frame(Age_predP,Pred_prop_P,"P")
Prop_predsS<-data.frame(Age_predS,Pred_prop_S,"S")
colnames(Prop_preds)<-c("Age","Prop","Disturbance")
colnames(Prop_predsC)<-c("Age","Prop","Disturbance")
colnames(Prop_predsP)<-c("Age","Prop","Disturbance")
colnames(Prop_predsS)<-c("Age","Prop","Disturbance")
Prop_preds_com<-rbind(Prop_preds,Prop_predsC,Prop_predsP,Prop_predsS)
head(Prop_preds_com)
Prop_preds_com$Trans<-(sin(Prop_preds_com$Prop))^2

plot(Dent$Age,Dent$Prop_sp)
lines(Age_pred,plogis(0.3344789+(0.3213808*log(Age_pred))))

#plot results of proportions
theme_set(theme_bw(base_size=26))
a<-ggplot(Dent_sub,aes(y=Prop_sp,x=Age))+geom_point(size=4,shape=1)
a
b<-a+opts(panel.grid.major = theme_line(colour =NA))
c<-b+ylab ('Proportion of shared species')+xlab ('Time since last disturbance (Years)')
d<-c+xlim(0,150)+ylim(0,1)+geom_hline(y=1,lty=2)+coord_cartesian(xlim =c(0,151), ylim =c(0,1.1), wise = NULL)
d+geom_line(data=transpred,aes(x=Age,y=preds),size=2)

setwd("C:/Documents and Settings/Phil/My Documents/My Dropbox/Publications, Reports and Responsibilities/Presentations/Secondary forest recovery - 15min")
ggsave("Dent et al 2009 reanalysis.jpeg",height=6,width=10,dpi=1200)

#mixed model for Sorensen
head(Dent_sub2)
Dent_sub2$ASqrtSoren<-asin(sqrt(Dent_sub2$Soren))


soren_mod1<-lmer(ASqrtSoren~Age+Disturbance*Age+Isolation+(1|Study),data=Dent_sub2)

#model checking
M1_res<-data.frame(predict(soren_mod1),(prop_mod1@resid))
colnames(M1_res)<-c("Pred","Resid")
ggplot(M1_res,aes(x=Pred,y=Resid))+geom_point()+geom_line(y=0,size=2)

#model selection using AICc
MS1<- dredge(soren_mod1, trace = TRUE, rank = "AICc", REML = F)
poss_mod<-subset(MS1,MS1$delta<9)
poss_mod
mod_coef<-data.frame(coef(poss_mod))
mod_coef$weight<-poss_mod$weight
head(mod_coef)
Int<-sum(mod_coef$X.Intercept*mod_coef$weight,na.rm=F)
Int
Age<-sum(mod_coef$Age*mod_coef$weight,na.rm=F)
Age
C<-sum(mod_coef$DisturbanceC*mod_coef$weight,na.rm=T)
P<-sum(mod_coef$DisturbanceP*mod_coef$weight,na.rm=T)
S<-sum(mod_coef$DisturbanceS*mod_coef$weight,na.rm=T)
C_slope<-sum(mod_coef$Age.DisturbanceC*mod_coef$weight,na.rm=T)
P_slope<-sum(mod_coef$Age.DisturbanceP*mod_coef$weight,na.rm=T)
S_slope<-sum(mod_coef$Age.DisturbanceS*mod_coef$weight,na.rm=T)
Is<-sum(mod_coef$Isolation2*mod_coef$weight,na.rm=T)

#predicted change with age
range(Dent_sub2$Age)
head(Dent_sub2)

Age_pred<-seq(1,150,0.1)
Age_predC<-seq(1,50,0.1)
Age_predP<-seq(1,70,0.1)
Age_predS<-seq(1,30,0.1)
Pred_prop_A<-Int+(Age*Age_pred)
Pred_prop_A
Pred_prop_C<-Int+(Age*Age_predC)+C+(Age*C_slope)
Pred_prop_P<-Int+(Age*Age_predP)+P+(Age*P_slope)
Pred_prop_S<-Int+(Age*Age_predS)+S+(Age*S_slope)

Prop_preds<-data.frame(Age_pred,Pred_prop_A,"A")
(Prop_preds)
Prop_predsC<-data.frame(Age_predC,Pred_prop_C,"C")
Prop_predsP<-data.frame(Age_predP,Pred_prop_P,"P")
Prop_predsS<-data.frame(Age_predS,Pred_prop_S,"S")
colnames(Prop_preds)<-c("Age","Prop","Disturbance")
colnames(Prop_predsC)<-c("Age","Prop","Disturbance")
colnames(Prop_predsP)<-c("Age","Prop","Disturbance")
colnames(Prop_predsS)<-c("Age","Prop","Disturbance")
Soren_preds_com<-rbind(Prop_preds,Prop_predsC,Prop_predsP,Prop_predsS)
head(Prop_preds_com)
Soren_preds_com$Trans<-(sin(Soren_preds_com$Prop))^2

#predicted change with age with different isolation

Pred_prop_Con<-Int+(Age*Age_predP)
Pred_prop_Iso<-Int+(Age*Age_pred)+Is
Prop_preds_con<-data.frame(Age_predP,Pred_prop_Con,"1")
Prop_preds_Iso<-data.frame(Age_pred,Pred_prop_Iso,"2")
colnames(Prop_preds_con)<-c("Age","Prop","Isolation")
colnames(Prop_preds_Iso)<-c("Age","Prop","Isolation")

Soren_preds_isol<-rbind(Prop_preds_con,Prop_preds_Iso)
head(Soren_preds_isol)
Soren_preds_isol$Trans<-(sin(Soren_preds_isol$Prop))^2


#plot results of sorensen with different disturbances
a<-ggplot(Dent_sub2,aes(y=Soren,x=Age,colour=Disturbance))+geom_point(size=2,alpha=0.5)+geom_line(data=Soren_preds_com,aes(y=Trans,x=Age),size=1,alpha=0.8)
b<-a+theme_bw()+opts(legend.position = "none")+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 12, colour = 'black'))+opts(axis.title.y = theme_text(angle=90,size = 12, colour = 'black'))
c<-b+ylab ('Sorensen similarity')+xlab ('Age of secondary forest (Years)')
c+xlim(0,160)+ylim(0,1)+geom_hline(y=1,lty=2)+coord_cartesian(xlim =c(0,151), ylim =c(0,1.1), wise = NULL)
ggsave("Dent et al 2009 reanalysis - Sorenson.png",height=3,width=6,dpi=1200)

#plot results of sorensen with different isolation
a<-ggplot(Dent_sub2,aes(y=Soren,x=Age,colour=Isolation))+geom_point(size=2,alpha=0.5)+geom_line(data=Soren_preds_isol,aes(y=Trans,x=Age),size=1,alpha=0.8)
a
b<-a+theme_bw()+opts(legend.position = "none")+opts(panel.grid.major = theme_line(colour =NA))+opts(axis.title.x = theme_text(size = 12, colour = 'black'))+opts(axis.title.y = theme_text(angle=90,size = 12, colour = 'black'))
c<-b+ylab ('Sorensen similarity')+xlab ('Age of secondary forest (Years)')
c+xlim(0,160)+ylim(0,1)+geom_hline(y=1,lty=2)+coord_cartesian(xlim =c(0,151), ylim =c(0,1.1), wise = NULL)
ggsave("Dent - Sorensen isolation.png",height=3,width=6,dpi=1200)
