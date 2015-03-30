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
data=read.table("../data/Passgraph//Passgraph_TEST")
data=read.table("../data/cbf/CBF_TEST")
unique(data$V1)

series = c(1,3,3,7,7,7,9,9,9,10,10,16,16,28,31,31,31,32,32,32,32,37,37)
starts = c(214,219,220,214,215,216,221,222,223,224,225,217,218,217,220,221,222,218,219,220,221,221,222)
stops = c(353,358,359,353,354,355,360,361,362,363,364,356,357,356,359,360,361,357,358,359,360,360,361)

df=data.frame(series,starts,stops)
class_id=4
series_idx=31
series=((data[data$V1==class_id,])[,-1])[series_idx+1,]
segments=dlply(df[df$series==series_idx+1,],.(starts),.fun=function(x){
  data.frame("x"=c(x$starts:x$stops),"y"=as.numeric(series[x$starts:x$stops]))})
plot(as.numeric(series),type="l")
lapply(segments,function(x){lines(x=x$x,y=x$y,col="red",lwd=2)})

# pattern="ddde ddde dddd dddd"; weight=0.22124
# 27: [54]
# 70: [71]
# 212: [44]
series = c(4,6,6,7,7,7,7,7,7,7,7,7,7,7,10,12,12,12,12,12,12,12,12,12,12,12,16,16,16,22,22,22,22,22,22,22,22,22,22,22,22,22,22,27,27,27,27,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,30,30,30,30,30,30,30)
starts = c(70,40,41,14,15,16,17,18,19,20,21,22,23,24,59,14,28,29,30,31,32,33,34,35,36,37,9,10,11,31,32,33,34,58,59,60,61,62,63,64,65,66,67,61,62,63,64,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,12,13,14,15,16,17,18)
stops = c(123,93,94,67,68,69,70,71,72,73,74,75,76,77,112,67,81,82,83,84,85,86,87,88,89,90,62,63,64,84,85,86,87,111,112,113,114,115,116,117,118,119,120,114,115,116,117,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,65,66,67,68,69,70,71)

df=data.frame(series,starts,stops)
series_idx=27
series=bell[series_idx+1,]
segments=dlply(df[df$series==series_idx+1,],.(starts),.fun=function(x){
  data.frame("x"=c(x$starts:x$stops),"y"=as.numeric(series[x$starts:x$stops]))})
plot(as.numeric(series),type="l")
lapply(segments,function(x){lines(x=x$x,y=x$y,col="red",lwd=2)})