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
data=read.table("../data/Gun_Point/class_1.txt",fill=TRUE)
data=cbind(1,data)
plot(as.numeric(data),type="l")
ser=as.numeric(data)

series = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
starts = c(141,142,143,1042,1043,1044,1045,1200,1201,1202,1496,1497,1498,1499,1500,1641,1642,1643,1644,1645,1950,1951,1952,1953,1954,1955,3002,3003,3004,3152,3296,3297,3444,3445,3592,3593,3894,3895,3896,4199,4200,4201,4345,4493,4644,4645,4646,4944,4945,5248,5400,5401,5547,5548,5549,5700,5848,5849,7496,7645,8246,8247,9297,9446,9447,9597,9598,9599,9746,9747)
stops = c(208,209,210,1109,1110,1111,1112,1267,1268,1269,1563,1564,1565,1566,1567,1708,1709,1710,1711,1712,2017,2018,2019,2020,2021,2022,3069,3070,3071,3219,3363,3364,3511,3512,3659,3660,3961,3962,3963,4266,4267,4268,4412,4560,4711,4712,4713,5011,5012,5315,5467,5468,5614,5615,5616,5767,5915,5916,7563,7712,8313,8314,9364,9513,9514,9664,9665,9666,9813,9814)
patterndf=data.frame(series,starts,stops)
class_id=1
series_idx=1
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(as.numeric(ser),type="l")
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

series = c(1,1,2,2,2,4,4,4,7,7,8,8,9,9,16,16,16,17,17,29,29,29,29,29,29,33,33,33)
starts = c(32,33,38,39,40,33,34,35,42,43,32,33,34,35,29,30,31,32,33,32,33,34,35,36,37,32,33,34)
stops = c(95,96,101,102,103,96,97,98,105,106,95,96,97,98,92,93,94,95,96,95,96,97,98,99,100,95,96,97)
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

series = c(3,3,3,5,5,5,5,5,9,9,10,10,10,11,11,11,11,11,12,12,13,15,16,16,17,17,17)
starts = c(70,71,72,68,69,70,71,72,81,82,73,74,75,64,65,66,67,68,72,73,71,74,80,81,87,88,89)
stops = c(131,132,133,129,130,131,132,133,142,143,134,135,136,125,126,127,128,129,133,134,132,135,141,142,148,149,150)
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

series = c(4,8,8,9,9,9,12,17,17,17,25,25,33,33,33,36,39,39,39,39,39,42,42,42,42)
starts = c(4,1,2,3,4,5,1,1,2,3,1,2,1,2,3,1,1,2,3,4,5,5,6,7,8)
stops = c(65,62,63,64,65,66,62,62,63,64,62,63,62,63,64,62,62,63,64,65,66,66,67,68,69)
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
series = c(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,13,13,13,14,14,14,14,14,16,16,17,23,23,23,23,23,23,23,23,23,23,35,35,35,35,35,35,36,36,46)
starts = c(33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,55,56,57,58,48,49,50,52,53,54,55,56,70,71,65,64,65,66,67,68,69,70,71,72,73,39,40,41,42,43,44,40,41,48)
stops = c(100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,122,123,124,125,115,116,117,119,120,121,122,123,137,138,132,131,132,133,134,135,136,137,138,139,140,106,107,108,109,110,111,107,108,115)
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

series = c(1,1,3,3,5,5,5,6,8,8,9,9,12,12,12,15,19,19,19,20,20)
starts = c(10,11,7,8,6,7,8,6,9,10,9,10,5,6,7,4,7,8,9,8,9)
stops = c(71,72,68,69,67,68,69,67,70,71,70,71,66,67,68,65,68,69,70,69,70)
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

series = c(1,1,1,4,4,5,5,7,7,7,8,9,9,10,10,13,13,15,20,20)
starts = c(70,71,72,84,85,79,80,66,67,68,72,73,74,83,84,85,86,74,79,80)
stops = c(133,134,135,147,148,142,143,129,130,131,135,136,137,146,147,148,149,137,142,143)
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

series = c(2,2,4,4,10,11,13,14,16,16,17,18,18,22,22)
starts = c(18,19,13,14,12,19,12,19,14,15,19,17,18,23,24)
stops = c(79,80,74,75,73,80,73,80,75,76,80,78,79,84,85)
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



series = c(6,27,27,65,75,75)
starts = c(3,1,2,1,1,2)
stops = c(70,68,69,68,68,69)
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
  theme_bw()+geom_line(col="blue") + 
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p1=p1+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p1

series = c(1,1,2,2,2,4,4,4,7,7,8,8,9,9,16,16,16,17,17,29,29,29,29,29,29,33,33,33)
starts = c(32,33,38,39,40,33,34,35,42,43,32,33,34,35,29,30,31,32,33,32,33,34,35,36,37,32,33,34)
stops = c(95,96,101,102,103,96,97,98,105,106,95,96,97,98,92,93,94,95,96,95,96,97,98,99,100,95,96,97)
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
data=read.table("../data/Gun_Point/class_1.txt",fill=TRUE)
data=cbind(1,data)
plot(as.numeric(data),type="l")
ser=as.numeric(data)

series = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
starts = c(141,142,143,1042,1043,1044,1045,1200,1201,1202,1496,1497,1498,1499,1500,1641,1642,1643,1644,1645,1950,1951,1952,1953,1954,1955,3002,3003,3004,3152,3296,3297,3444,3445,3592,3593,3894,3895,3896,4199,4200,4201,4345,4493,4644,4645,4646,4944,4945,5248,5400,5401,5547,5548,5549,5700,5848,5849,7496,7645,8246,8247,9297,9446,9447,9597,9598,9599,9746,9747)
stops = c(208,209,210,1109,1110,1111,1112,1267,1268,1269,1563,1564,1565,1566,1567,1708,1709,1710,1711,1712,2017,2018,2019,2020,2021,2022,3069,3070,3071,3219,3363,3364,3511,3512,3659,3660,3961,3962,3963,4266,4267,4268,4412,4560,4711,4712,4713,5011,5012,5315,5467,5468,5614,5615,5616,5767,5915,5916,7563,7712,8313,8314,9364,9513,9514,9664,9665,9666,9813,9814)
patterndf=data.frame(series,starts,stops)
class_id=1
series_idx=1
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(as.numeric(ser),type="l")
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

series = c(1,1,2,2,2,4,4,4,7,7,8,8,9,9,16,16,16,17,17,29,29,29,29,29,29,33,33,33)
starts = c(32,33,38,39,40,33,34,35,42,43,32,33,34,35,29,30,31,32,33,32,33,34,35,36,37,32,33,34)
stops = c(95,96,101,102,103,96,97,98,105,106,95,96,97,98,92,93,94,95,96,95,96,97,98,99,100,95,96,97)
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

series = c(3,3,3,5,5,5,5,5,9,9,10,10,10,11,11,11,11,11,12,12,13,15,16,16,17,17,17)
starts = c(70,71,72,68,69,70,71,72,81,82,73,74,75,64,65,66,67,68,72,73,71,74,80,81,87,88,89)
stops = c(131,132,133,129,130,131,132,133,142,143,134,135,136,125,126,127,128,129,133,134,132,135,141,142,148,149,150)
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

series = c(4,8,8,9,9,9,12,17,17,17,25,25,33,33,33,36,39,39,39,39,39,42,42,42,42)
starts = c(4,1,2,3,4,5,1,1,2,3,1,2,1,2,3,1,1,2,3,4,5,5,6,7,8)
stops = c(65,62,63,64,65,66,62,62,63,64,62,63,62,63,64,62,62,63,64,65,66,66,67,68,69)
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
series = c(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,13,13,13,14,14,14,14,14,16,16,17,23,23,23,23,23,23,23,23,23,23,35,35,35,35,35,35,36,36,46)
starts = c(33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,55,56,57,58,48,49,50,52,53,54,55,56,70,71,65,64,65,66,67,68,69,70,71,72,73,39,40,41,42,43,44,40,41,48)
stops = c(100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,122,123,124,125,115,116,117,119,120,121,122,123,137,138,132,131,132,133,134,135,136,137,138,139,140,106,107,108,109,110,111,107,108,115)
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

series = c(1,1,3,3,5,5,5,6,8,8,9,9,12,12,12,15,19,19,19,20,20)
starts = c(10,11,7,8,6,7,8,6,9,10,9,10,5,6,7,4,7,8,9,8,9)
stops = c(71,72,68,69,67,68,69,67,70,71,70,71,66,67,68,65,68,69,70,69,70)
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

series = c(1,1,1,4,4,5,5,7,7,7,8,9,9,10,10,13,13,15,20,20)
starts = c(70,71,72,84,85,79,80,66,67,68,72,73,74,83,84,85,86,74,79,80)
stops = c(133,134,135,147,148,142,143,129,130,131,135,136,137,146,147,148,149,137,142,143)
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

series = c(2,2,4,4,10,11,13,14,16,16,17,18,18,22,22)
starts = c(18,19,13,14,12,19,12,19,14,15,19,17,18,23,24)
stops = c(79,80,74,75,73,80,73,80,75,76,80,78,79,84,85)
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



series = c(6,27,27,65,75,75)
starts = c(3,1,2,1,1,2)
stops = c(70,68,69,68,68,69)
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
  theme_bw()+geom_line(col="blue") + 
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p1=p1+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p1

series = c(1,1,2,2,2,4,4,4,7,7,8,8,9,9,16,16,16,17,17,29,29,29,29,29,29,33,33,33)
starts = c(32,33,38,39,40,33,34,35,42,43,32,33,34,35,29,30,31,32,33,32,33,34,35,36,37,32,33,34)
stops = c(95,96,101,102,103,96,97,98,105,106,95,96,97,98,92,93,94,95,96,95,96,97,98,99,100,95,96,97)
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
