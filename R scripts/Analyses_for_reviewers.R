#extra analyses for reviewers

#testing effect of soil depth
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