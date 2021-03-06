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
ser=as.numeric(data)

#CairoPNG(file = "gun_class1_Ann_Centroid1.png",
#         width = 1200, height = 600, pointsize = 12, bg = "white")

CairoPDF(file = "gun_class1_Ann_CentroidA",
         width = 13, height = 5, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")

par(mar=c(0,0,3,0),mfrow=c(2,2))

series = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
starts = c(4411,4858,5768,6365,6663,6959,7264,7708,7863,8315,8461,8616,8765,8909,9209,9959,10408,11011)
stops = c(4467,4914,5824,6421,6719,7015,7320,7764,7919,8371,8517,8672,8821,8965,9265,10015,10464,11067)
patterndf=data.frame(series,starts,stops)
class_id=1
series_idx=1
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(as.numeric(ser[4000:5000]),type="l",xaxt='n', ann=T, yaxt='n',bty="n")
title("ddddddddcbbaaaaa ddddddddcbaaaaaa dddddddccbaaaaaa dddddddccbaaaaaa \n dddddddcbbaaaaaa dddddddcbbaaaaaa, weight=0.07947", cex.main = 0.8,   font.main=1, col.main= "blue",
      cex.sub = 0.22, font.sub = 3, col.sub = "red")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red",lwd=1.2)})

series = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
starts = c(513,1108,1266,1414,1572,1709,2608,3076,3508,3657,3962,4273,4419,4564,4714,4865,5017,5323,5475,5621,5776,5923,6670,6967,7117,7272,7431,7566,7870,8170,8322,8468,8623,8772,9064,9216,9365,9667,11018,11172)
stops = c(567,1162,1320,1468,1626,1763,2662,3130,3562,3711,4016,4327,4473,4618,4768,4919,5071,5377,5529,5675,5830,5977,6724,7021,7171,7326,7485,7620,7924,8224,8376,8522,8677,8826,9118,9270,9419,9721,11072,11226)
patterndf=data.frame(series,starts,stops)
class_id=1
series_idx=1
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(as.numeric(ser),type="l",xaxt='n', ann=FALSE, yaxt='n',bty="n")
title("ddddddccbaaaaaaa ddddddcbbaaaaaaa ddddddcbbaaaaaaa ddddddcbbaaaaaaa,\n weight=0.07835",
      cex.main = 0.8,   font.main=1, col.main= "blue", cex.sub = 0.22, font.sub = 3, col.sub = "red")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red",lwd=1.2)})

series = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
starts = c(2875,4073,4227,4823,6918,8123,8874,9022,9172,9473,9773,9922,10222,10372,10521,10827,10974)
stops = c(2931,4129,4283,4879,6974,8179,8930,9078,9228,9529,9829,9978,10278,10428,10577,10883,11030)
patterndf=data.frame(series,starts,stops)
class_id=1
series_idx=1
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(as.numeric(ser),type="l",xaxt='n', ann=FALSE, yaxt='n',bty="n")
title("aaaaaaaabcdddddd aaaaaaaabcdddddd aaaaaaaabcdddddd aaaaaaabbcdddddd\n aaaaaaabcddddddd aaaaaaabcddddddd, weight=0.0783",
      cex.main = 0.8,   font.main=1, col.main= "blue", cex.sub = 0.22, font.sub = 3, col.sub = "red")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red",lwd=1.2)})

series = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
starts = c(3806,3957,4108,4268,4413,4558,4708,4860,5011,5317,5470,5615,5770,5917,6367,6665,6961,7111,7266,7560,7710,7865,8164,8317,8463,8618,8767,8911,9211,9360,9509,9808,9961,10114,10258,10410,11013,11167)
stops = c(3860,4011,4162,4322,4467,4612,4762,4914,5065,5371,5524,5669,5824,5971,6421,6719,7015,7165,7320,7614,7764,7919,8218,8371,8517,8672,8821,8965,9265,9414,9563,9862,10015,10168,10312,10464,11067,11221)
patterndf=data.frame(series,starts,stops)
class_id=1
series_idx=1
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(as.numeric(ser),type="l",xaxt='n', ann=FALSE, yaxt='n',bty="n")
title("dddddddccbaaaaaa dddddddccbaaaaaa dddddddcbbaaaaaa dddddddcbbaaaaaa,\n weight=0.07787",
      cex.main = 0.8,   font.main=1, col.main= "blue", cex.sub = 0.22, font.sub = 3, col.sub = "red")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red",lwd=1.2)})

dev.off()
############
############
data=read.table("../data/Gun_Point/class_2.txt",fill=TRUE)
series = c(10,10,13,14,16,18,22,36,46,47,47,47,47,47,47,49)
starts = c(58,59,61,67,82,70,69,52,59,53,54,55,56,57,58,48)
stops = c(114,115,117,123,138,126,125,108,115,109,110,111,112,113,114,104)
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
  theme_bw()+geom_line(col="blue") + ggtitle("GunPoint, class #2 (point), pattern #1")+
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p1=p1+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p1

series = c(4,16,18,46,47,57,59,62,68,71)
starts = c(13,14,17,17,8,11,13,11,19,19)
stops = c(74,75,78,78,69,72,74,72,80,80)
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
  theme_bw()+geom_line(col="blue") + ggtitle("GunPoint, class #2, pattern #2")+
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p2=p2+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p2

series = c(2,4,11,13,14,16,17,18,22,23)
starts = c(24,19,25,18,25,20,25,23,29,29)
stops = c(79,74,80,73,80,75,80,78,84,84)
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
  theme_bw()+geom_line(col="blue") + ggtitle("GunPoint, class #2, pattern #3")+
  theme(plot.title=element_text(size=24), legend.position="none",
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.y=element_blank(),plot.margin = unit(c(0.2,0,0,0), "lines"))
p3=p3+geom_line(data=ldply(segments),aes(x=x,y=y),col="red",size=1.2)
p3

series = c(4,4,10,10,10,10,13,13,13,14,14,14,16,16,16,17,18,18,18,22,22,22,23,23,24)
starts = c(61,62,58,59,60,61,61,62,63,67,68,69,82,83,84,75,70,71,72,69,70,71,85,86,69)
stops = c(115,116,112,113,114,115,115,116,117,121,122,123,136,137,138,129,124,125,126,123,124,125,139,140,123)
patterndf=data.frame(series,starts,stops)
class_id=2
series_idx=4
sid=as.numeric((unique(series)[series_idx]))
dd=patterndf[patterndf$series==sid,]
ser=as.numeric(((data[data$V1==class_id,])[,-1])[sid,])
segments=dlply(dd,.(starts),function(x){data.frame(x=c(x$starts:x$stops),y=ser[x$starts:x$stops])})
plot(ser,type="l")
laply(segments,function(d){lines(x=d$x,y=d$y,col="red")})
p4=ggplot(data.frame(x=c(1:length(ser)),y=ser),aes(x,y,))+
  theme_bw()+geom_line(col="blue") + ggtitle("GunPoint, class #2, pattern #4")+
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
CairoPDF(file = "GunPoint_Class2",
         width = 12, height = 6, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
gg
dev.off()
