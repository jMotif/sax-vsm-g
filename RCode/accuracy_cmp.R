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
data=data.frame(read.table("../data/perf_accuracy.csv",header=T,as.is=T))
pe=ggplot(data=data,aes(x=data$"SAX.VSM.G",y=data$"Euclidean1NN")) + geom_point() + 
  scale_x_continuous(limits=c(0,0.75)) + scale_y_continuous(limits=c(0,0.75)) + 
  theme_bw()
pe
positions <- data.frame(x=c(0,0.7,0.7),y=c(0,0.7,0))
pe + geom_polygon(data=positions, aes(x,y))


plot(x=data$"SAX.VSM.G",y=data$"Euclidean1NN",xlim=c(0, 0.8), ylim=c(0, 0.8),
     xlab="Euclidean wins", ylab="SAX-VSM-G wins",pch=16,main="SAX-VSM-G vs Euclidean 1NN")
positions <- data.frame(x=c(-1,1,1),y=c(-1,1,-1))
polygon(positions$x, positions$y, col = alpha("cyan", 0.5), border=NA)


plot(x=data$"SAX.VSM.G",y=data$"DTW.1NN",xlim=c(0, 0.8), ylim=c(0, 0.8),
     xlab="DTW wins", ylab="SAX-VSM-G wins",pch=16,main="SAX-VSM-G vs DTW 1NN")
positions <- data.frame(x=c(-1,1,1),y=c(-1,1,-1))
polygon(positions$x, positions$y, col = alpha("cyan", 0.5), border=NA)

plot(x=data$"SAX.VSM.G",y=data$"SAX.VSM",xlim=c(0, 0.8), ylim=c(0, 0.8),
     xlab="SAX-VSM wins", ylab="SAX-VSM-G wins",pch=16,main="SAX-VSM-G vs SAX-VSM")
positions <- data.frame(x=c(-1,1,1),y=c(-1,1,-1))
polygon(positions$x, positions$y, col = alpha("cyan", 0.5), border=NA)
