require(ggplot2)
require(Cairo)
require(reshape)
require(scales)
require(RColorBrewer)
require(grid)
require(gridExtra)
require(lattice)

cylinders=read.table("CBF_explorer/cylinder.csv")
plot(unlist(data[1,-1]),type="l")

bells=read.table("CBF_explorer/bell.csv")
plot(unlist(bells[1,-1]),type="l")

funnels=read.table("CBF_explorer/funnel.csv")
plot(unlist(funnels[1,-1]),type="l")



lines(x=c(26:78),y=cylinders[1,26:78],lwd=2,col="red")
lines(cylinders[2,]);lines(x=c(11:57),y=cylinders[2,11:57],lwd=2,col="red")
lines(cylinders[3,])


data = read.table("../data/cbf/cbf5x.txt",header=F)
cylinders=matrix(unlist(data[data$V1==1,-1]),byrow=F,ncol=640)
bells=matrix(unlist(dat[data$V1==2,-1]),byrow=F,ncol=640)
funnels=matrix(unlist(dat[data$V1==3,-1]),byrow=F,ncol=640)
len=640

par(mfrow=c(1,1))
plot(cylinders[3,],type="l");

lines(x=c(526:576),y=cylinders[3,526:576],lwd=2,col="red");
lines(x=c(403:453),y=cylinders[3,403:453],lwd=2,col="red");
lines(x=c(144:190),y=cylinders[3,144:190],lwd=2,col="red");

lines(cylinders[2,]);lines(x=c(11:57),y=cylinders[2,11:57],lwd=2,col="red")
lines(cylinders[3,])





p1 = ggplot(dm[dm$X2=="ones",], aes(x = index, y = value, group = X2, color="black")) +
  theme_bw() + geom_line(colour="black") + geom_hline(yintercept=0,lty=2) +
  ggtitle("Class 1") +
  scale_x_continuous("time ticks", limits=c(0,350), breaks=seq(0,350,50)) + 
  scale_y_continuous("Value",limits=c(-3,3),breaks=seq(-3,3,1))+
  theme(plot.title=element_text(size = 18, vjust = 2))
p1



Cairo(width = 750, height = 250, file="FaceFour/all_classes.png", type="png", pointsize=12, 
      bg = "transparent", canvas = "white", units = "px", dpi = "auto")
print(p)
dev.off()
#
#
#
p1 = ggplot(dm[dm$X2=="ones",], aes(x = index, y = value, group = X2, color="black")) +
  theme_bw() + geom_line(colour="black") + geom_hline(yintercept=0,lty=2) +
  ggtitle("Class 1") +
  scale_x_continuous("time ticks", limits=c(0,350), breaks=seq(0,350,50)) + 
  scale_y_continuous("Value",limits=c(-3,3),breaks=seq(-3,3,1))+
  theme(plot.title=element_text(size = 18, vjust = 2))
p1
#
#
#
p2 = ggplot(dm[dm$X2=="twos",], aes(x = index, y = value, group = X2)) +
  theme_bw() + geom_line(colour="black") + geom_hline(yintercept=0,lty=2) +
  ggtitle("Class 2") +
  scale_x_continuous("time ticks", limits=c(0,350), breaks=seq(0,350,50)) + 
  scale_y_continuous("Value",limits=c(-3,3),breaks=seq(-3,3,1))+
  theme(plot.title=element_text(size = 18, vjust = 2))
p2
#
#
#
p3 = ggplot(dm[dm$X2=="threes",], aes(x = index, y = value, group = X2)) +
  theme_bw() + geom_line(colour="black") + geom_hline(yintercept=0,lty=2) +
  ggtitle("Class 3") +
  scale_x_continuous("time ticks", limits=c(0,350), breaks=seq(0,350,50)) + 
  scale_y_continuous("Value",limits=c(-3,3),breaks=seq(-3,3,1))+
  theme(plot.title=element_text(size = 18, vjust = 2))
p3
#
#
#
p4 = ggplot(dm[dm$X2=="fours",], aes(x = index, y = value, group = X2)) +
  theme_bw() + geom_line(colour="black") + geom_hline(yintercept=0,lty=2) +
  ggtitle("Class 4") +
  scale_x_continuous("time ticks", limits=c(0,350), breaks=seq(0,350,50)) + 
  scale_y_continuous("Value",limits=c(-3,3),breaks=seq(-3,3,1))+
  theme(plot.title=element_text(size = 18, vjust = 2))
p4
#
#
print(arrangeGrob(p1, p2, p3, p4, ncol=2))

Cairo(width = 750, height = 400, file="face4/classes.png", type="png", pointsize=12, 
      bg = "transparent", canvas = "white", units = "px", dpi = "auto")
print(arrangeGrob(p1, p2, p3, p4, ncol=2))
dev.off()

#
#
#
data=read.csv("synthetic/out.single.exact.csv",head=F)
names(data)=c("paa","Alphabet","WINDOW","acc","Error")
min(data$Error)
data=data.frame(data)
d=data[data[3]==40,]
p=wireframe(Error ~ paa * Alphabet, data = d, scales = list(arrows = FALSE),
            drape = TRUE, colorkey = TRUE, pretty=TRUE, screen = list(z = 10, x = -60, y = 00),
            aspect = c(87/97, 0.6),
            xlim=range(data$paa), ylim=range(data$Alphabet), zlim=c(0, 0.9),
            main=paste("Synthetic Control classifier error rate, SLIDING_WINDOW=40"),
            col.regions = terrain.colors(100, alpha = 1) )
p

Cairo(width = 750, height = 600, file="synthetic/parameters.png", type="png", pointsize=12, 
      bg = "white", canvas = "white", units = "px", dpi = "auto")
print(p)
dev.off()