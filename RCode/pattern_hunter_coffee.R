#
require(reshape)
require(plyr)
require(stringr)
#
require(ggplot2)
require(Cairo)
require(scales)
require(RColorBrewer)
require(grid)
require(gridExtra)
require(lattice)

#
data=read.table("../data/Coffee//Coffee_TEST")
#unique(data$V1)

series = c(4,5,8,12)
starts = c(193,195,194,194)
stops = c(284,286,285,285)
patterndf=data.frame(series,starts,stops)
class_id=1
series_idx=1
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p1=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + ggtitle("Coffee, class #0, pattern #1")+
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p1=p1+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p1

series = c(1,2,3,4,5,6,8,9,10,11)
starts = c(197,196,197,194,196,196,195,195,196,195)
stops = c(275,274,275,272,274,274,273,273,274,273)
patterndf=data.frame(series,starts,stops)
class_id=1
series_idx=2
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p2=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + ggtitle("Coffee, class #0, pattern #2")+
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p2=p2+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p2

series = c(1,2,3,4,5,6,8,9,10,11)
starts = c(196,195,196,193,195,195,194,194,195,194)
stops = c(275,274,275,272,274,274,273,273,274,273)
patterndf=data.frame(series,starts,stops)
class_id=1
series_idx=5
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p3=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + ggtitle("Coffee, class #0, pattern #3")+
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p3=p3+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p3

series = c(2,3,4,5,6,7,8,9,10,11)
starts = c(7,6,6,7,6,5,6,7,6,6)
stops = c(90,89,89,90,89,88,89,90,89,89)
patterndf=data.frame(series,starts,stops)
class_id=1
series_idx=8
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p4=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + ggtitle("Coffee, class #0, pattern #4")+
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p4=p4+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p4

gg=arrangeGrob(p1, p2, p3, p4, ncol=2)
gg
CairoPDF(file = "Coffee_Class0",
         width = 10, height = 5, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
gg
dev.off()
###########
###########
series = c(1,3,5,6,7,8,9,11,12,13)
starts = c(10,11,10,11,11,10,11,11,10,11)
stops = c(97,98,97,98,98,97,98,98,97,98)
patterndf=data.frame(series,starts,stops)
class_id=1
series_idx=1
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p1=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + ggtitle("Coffee, class #1, pattern #1")+
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p1=p1+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p1

series = c(1,3,6,7,8,11,12)
starts = c(10,11,11,11,10,11,10)
stops = c(99,100,100,100,99,100,99)
patterndf=data.frame(series,starts,stops)
class_id=1
series_idx=2
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p2=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + ggtitle("Coffee, class #1, pattern #2")+
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p2=p2+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p2

series = c(3,5,6,7,8,9,10,11,12,13)
starts = c(198,198,197,199,198,198,198,198,197,198)
stops = c(285,285,284,286,285,285,285,285,284,285)
patterndf=data.frame(series,starts,stops)
class_id=1
series_idx=5
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p3=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + ggtitle("Coffee, class #1, pattern #3")+
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p3=p3+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p3

series = c(1,7,8,12)
starts = c(9,10,9,9)
stops = c(99,100,99,99)
patterndf=data.frame(series,starts,stops)
class_id=1
series_idx=4
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p4=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + ggtitle("Coffee, class #1, pattern #4")+
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p4=p4+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p4

gg=arrangeGrob(p1, p2, p3, p4, ncol=2)
gg
CairoPDF(file = "Coffee_Class1",
         width = 10, height = 5, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
gg
dev.off()