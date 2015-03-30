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
data=read.table("../data/cbf/CBF_TEST")
#unique(data$V1)

series = c(1,1,1,1,1,2,2,2,3,3,4,4,4,5,5,5,5,5,6,6,6,6,6,7,7,7,7,8,8,8,9,9,9,9,9,10,10,10)
starts = c(13,14,15,16,17,15,16,17,14,15,22,23,24,17,18,19,20,21,19,20,21,22,23,13,14,15,16,16,17,18,7,8,9,10,11,10,11,12)
stops = c(53,54,55,56,57,55,56,57,54,55,62,63,64,57,58,59,60,61,59,60,61,62,63,53,54,55,56,56,57,58,47,48,49,50,51,50,51,52)
patterndf=data.frame(series,starts,stops)
class_id=1
series_idx=8
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p1=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + 
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p1=p1+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p1

series = c(1,1,1,2,2,2,2,3,3,3,5,5,5,5,6,6,6,6,6,7,7,7,7,9,9,9,9,9,10,10,10,10,11,11,11,12)
starts = c(67,68,69,61,62,63,64,42,43,44,35,36,37,38,32,33,34,35,36,56,57,58,59,73,74,75,76,77,35,36,37,38,45,46,47,89)
stops = c(107,108,109,101,102,103,104,82,83,84,75,76,77,78,72,73,74,75,76,96,97,98,99,113,114,115,116,117,75,76,77,78,85,86,87,129)
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
  theme_bw()+geom_line(col="blue") + 
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p2=p2+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p2

series = c(1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,4,5,5,5,5,5,5,5,6,6,6,6,6,6,6,7,7,7,7,7,7,8,8,8,8,8,9,9,9,9,9,9,9,10,10,10,10,10)
starts = c(13,14,15,16,17,18,19,15,16,17,18,19,14,15,16,17,22,23,24,25,26,17,18,19,20,21,22,23,19,20,21,22,23,24,25,13,14,15,16,17,18,16,17,18,19,20,7,8,9,10,11,12,13,10,11,12,13,14)
stops = c(51,52,53,54,55,56,57,53,54,55,56,57,52,53,54,55,60,61,62,63,64,55,56,57,58,59,60,61,57,58,59,60,61,62,63,51,52,53,54,55,56,54,55,56,57,58,45,46,47,48,49,50,51,48,49,50,51,52)
patterndf=data.frame(series,starts,stops)
class_id=1
series_idx=6
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p3=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + 
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p3=p3+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p3

series = c(1,1,1,1,2,2,2,3,3,4,4,4,4,4,5,5,5,6,6,7,7,7,8,8,10,11,11,11,11)
starts = c(3,4,5,6,5,6,7,3,4,11,12,13,14,15,7,8,9,11,12,3,4,5,6,7,1,1,2,3,4)
stops = c(41,42,43,44,43,44,45,41,42,49,50,51,52,53,45,46,47,49,50,41,42,43,44,45,39,39,40,41,42)
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
  theme_bw()+geom_line(col="blue") + 
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
###########
###########
series = c(2,5,5,5,5,7,7,7,7,9,9,9,9,9,10,11,11,11,11,11,11,11,11,11,11,11,11,11,12,12,12,12,13,13,13,13,13,13,13,13,13,15,15,16)
starts = c(18,10,11,12,13,44,45,46,47,15,16,17,18,19,67,2,3,4,5,6,16,17,39,40,41,42,43,44,17,27,28,29,18,19,20,21,22,23,24,25,26,36,37,30)
stops = c(58,50,51,52,53,84,85,86,87,55,56,57,58,59,107,42,43,44,45,46,56,57,79,80,81,82,83,84,57,67,68,69,58,59,60,61,62,63,64,65,66,76,77,70)
patterndf=data.frame(series,starts,stops)
class_id=2
series_idx=1
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p1=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + 
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p1=p1+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p1

series = c(2,3,3,3,5,7,7,8,8,8,8,8,8,8,8,10,11,11,12,12,12,12,12,12,12,12,12,13,13,14)
starts = c(37,60,61,62,30,35,36,54,55,56,57,58,59,60,61,71,21,32,6,7,8,9,43,44,45,46,47,40,41,86)
stops = c(77,100,101,102,70,75,76,94,95,96,97,98,99,100,101,111,61,72,46,47,48,49,83,84,85,86,87,80,81,126)
patterndf=data.frame(series,starts,stops)
class_id=2
series_idx=2
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p2=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + 
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p2=p2+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p2

series = c(3,5,5,5,6,6,6,6,7,8,8,8,8,9,11,13,15,16,16,16)
starts = c(54,23,24,25,21,22,23,24,27,12,13,14,15,29,12,33,32,19,20,21)
stops = c(92,61,62,63,59,60,61,62,65,50,51,52,53,67,50,71,70,57,58,59)
patterndf=data.frame(series,starts,stops)
class_id=2
series_idx=3
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p3=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + 
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p3=p3+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p3

series = c(1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,6,7,8,8,8,8,8,8,10,10,10,10,10,10,10,10,10,10,12,12)
starts = c(6,9,10,22,23,24,25,26,27,45,46,47,48,49,50,51,6,12,39,72,73,74,17,47,65,66,67,68,2,3,4,45,14,2,3,38,42,43,44,50,51,52,53,54,55,56,57,58,62,2,63)
stops = c(44,47,48,60,61,62,63,64,65,83,84,85,86,87,88,89,44,50,77,110,111,112,55,85,103,104,105,106,40,41,42,83,52,40,41,76,80,81,82,88,89,90,91,92,93,94,95,96,100,40,101)
patterndf=data.frame(series,starts,stops)
class_id=2
series_idx=8
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p4=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + 
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

###########
###########
series = c(1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,8,9,9,9,10,10,10,10,10,10,10,10,10)
starts = c(44,45,46,47,48,56,22,23,24,25,26,27,28,29,37,38,56,57,58,55,56,57,58,59,60,61,62,63,64,75,78,79,26,27,28,31,32,33,40,41,42,43,44,45,46,34,35,36,37,38,39,40,19,20,21,22,47,33,34,38,39,51,52,61,71,72,73,74,37,21,37,60,53,54,55,56,73,86,87,88,89)
stops = c(82,83,84,85,86,94,60,61,62,63,64,65,66,67,75,76,94,95,96,93,94,95,96,97,98,99,100,101,102,113,116,117,64,65,66,69,70,71,78,79,80,81,82,83,84,72,73,74,75,76,77,78,57,58,59,60,85,71,72,76,77,89,90,99,109,110,111,112,75,59,75,98,91,92,93,94,111,124,125,126,127)
patterndf=data.frame(series,starts,stops)
class_id=3
series_idx=1
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p1=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + 
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p1=p1+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p1

series = c(1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,5,5,5,5,6,6,7,7,10,10,10,10,11,11,11,11,11,12,12)
starts = c(44,45,46,22,23,24,25,26,27,56,55,56,57,58,59,60,61,62,26,31,40,41,42,43,44,34,35,36,37,38,19,20,71,72,53,54,86,87,31,32,33,34,35,32,33)
stops = c(84,85,86,62,63,64,65,66,67,96,95,96,97,98,99,100,101,102,66,71,80,81,82,83,84,74,75,76,77,78,59,60,111,112,93,94,126,127,71,72,73,74,75,72,73)
patterndf=data.frame(series,starts,stops)
class_id=3
series_idx=2
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p2=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + 
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p2=p2+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p2

series = c(2,7,19,25,26,28,34,34,40,42,42,48)
starts = c(34,58,36,31,54,31,48,59,40,27,41,52)
stops = c(75,99,77,72,95,72,89,100,81,68,82,93)
patterndf=data.frame(series,starts,stops)
class_id=3
series_idx=3
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p3=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + 
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p3=p3+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p3

series = c(1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,7,7,7,7,7,7,10,10,10,10,10,10,11,11,11,11,11,11,12,12,12)
starts = c(44,45,46,47,22,23,24,25,26,27,28,37,56,57,55,56,57,58,59,60,61,62,63,78,26,27,31,32,40,41,42,43,44,45,34,35,36,37,38,39,19,20,21,33,38,51,71,72,73,53,54,55,86,87,88,31,32,33,34,35,36,32,33,34)
stops = c(83,84,85,86,61,62,63,64,65,66,67,76,95,96,94,95,96,97,98,99,100,101,102,117,65,66,70,71,79,80,81,82,83,84,73,74,75,76,77,78,58,59,60,72,77,90,110,111,112,92,93,94,125,126,127,70,71,72,73,74,75,71,72,73)
patterndf=data.frame(series,starts,stops)
class_id=3
series_idx=4
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p4=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + 
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
