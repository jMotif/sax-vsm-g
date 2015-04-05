require(ggplot2)
require(Cairo)
require(reshape)
require(scales)
require(RColorBrewer)
require(grid)
library(gridExtra)
require(lattice)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pal2=brewer.pal(8, "Set1")

dat = read.table("../data/cbf/CBF_TRAIN",header=F)
dat=t(dat)
ones=(dat[,dat[1,]==1])[-1,1]
twos=(dat[,dat[1,]==2])[-1,1]
threes=(dat[,dat[1,]==3])[-1,1]
# Take the twelve series and melt (or equivalently, stack) them:
dm <- melt(cbind(ones,twos,threes))
# add an index variable:
dm$index <- rep(1:128, 3)
dm$class <- c(rep("Cylinder",(128*1)), rep("Bell",(128*1)), rep("Funnel",(128*1)))

pc = ggplot(dm[dm$class=="Cylinder",], aes(x = index, y = value, group = X2, color=class)) +
  theme_bw() + geom_line(size=0.8,col=cbPalette[1]) + labs(colour="Classes: ") +
  theme(axis.line=element_blank(),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        #panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position="bottom",
        plot.title = element_text(size = 20),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16)) +
  ggtitle("Cylinder")
pc

pb = ggplot(dm[dm$class=="Bell",], aes(x = index, y = value, group = X2, color=class)) +
  theme_bw() + geom_line(size=0.8,col=cbPalette[2]) + labs(colour="Classes: ") +
  theme(axis.line=element_blank(),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        #panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position="bottom",
        plot.title = element_text(size = 20),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16)) +
  ggtitle("Bell")
pb

pf = ggplot(dm[dm$class=="Funnel",], aes(x = index, y = value, group = X2, color=class)) +
  theme_bw() + geom_line(size=0.8,col=cbPalette[3]) + labs(colour="Classes: ") +
  theme(axis.line=element_blank(),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        #panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position="bottom",
        plot.title = element_text(size = 20),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16)) +
  ggtitle("Funnel")
pf

gg=arrangeGrob(pc, pb, pf, ncol=3)
gg

CairoPDF(file = "cbf_classes",
         width = 12, height = 3, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print(gg)
dev.off()


########################
########################
dat = read.table("../data/cbf/CBF_TRAIN",header=F)
class0=(dat[dat$V1==1,])[,-1]
df0=data.frame(x=rep(c(1:128),10), 
               series=rep(paste("ser",c(1:10),sep=""),each=128),
               y=as.numeric(unlist(t(class0))))

class1=(dat[dat$V1==2,])[,-1]
df1=data.frame(x=rep(c(1:128),12), 
               series=rep(paste("ser",c(1:12),sep=""),each=128),
               y=as.numeric(unlist(t(class1))))

class2=(dat[dat$V1==3,])[,-1]
df2=data.frame(x=rep(c(1:128),8), 
               series=rep(paste("ser",c(1:8),sep=""),each=128),
               y=as.numeric(unlist(t(class2))))

p1=ggplot(df0,aes(x,y,group=series)) + theme_bw() + 
  geom_line(size=0.2,col=cbPalette[1],alpha=0.7)+ 
  theme(axis.line=element_blank(),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #legend.position="none",
        panel.background=element_blank(),
        #panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position="bottom",
        plot.title = element_text(size = 16),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.key.width=unit(2, "cm")) +
  ggtitle("Class Cylinder and best rep. patterns") 
p1

pattern1=data.frame(y=as.numeric(class0[10,14:(14+37)]),
                    x=c(14:(14+37)),series=rep("pattern1",38))
pattern2=data.frame(y=as.numeric(class0[7,71:(71+46)]),
                    x=c(71:(71+46)),series=rep("pattern1",47))
p1=p1 + geom_line(data=pattern1, col=pal2[4], linetype=1, size=1.5) +
   geom_line(data=pattern2, col=pal2[1], linetype=1, size=1.5)


p2=ggplot(df1,aes(x,y,group=series)) + theme_bw() + 
  geom_line(size=0.2,col=cbPalette[2],alpha=0.7)+ 
  theme(axis.line=element_blank(),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #legend.position="none",
        panel.background=element_blank(),
        #panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position="bottom",
        plot.title = element_text(size = 16),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.key.width=unit(2, "cm")) +
  ggtitle("Class Bell and best rep. pattern") 
p2

pattern1=data.frame(y=as.numeric(class1[3,10:(10+49)]),
                    x=c(10:(10+49)),series=rep("pattern1",50))
p2 = p2 + geom_line(data=pattern1, col=pal2[3], linetype=1, size=1.5)
p2


p3=ggplot(df2,aes(x,y,group=series)) + theme_bw() + 
  geom_line(size=0.2,col=cbPalette[3],alpha=0.65)+ 
  theme(axis.line=element_blank(),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #legend.position="none",
        panel.background=element_blank(),
        #panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position="bottom",
        plot.title = element_text(size = 16),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.key.width=unit(2, "cm")) +
  ggtitle("Class Funnel and best rep. pattern") 
p3

pattern1=data.frame(y=as.numeric(class2[3,31:(31+49)]),
                    x=c(31:(31+49)),series=rep("pattern1",50))
p3 = p3 + geom_line(data=pattern1, col=pal2[7], linetype=1, size=1.5)
p3

gg=arrangeGrob(p1, p2, p3, ncol=3)
gg

CairoPDF(file = "cbf_class_patterns",
         width = 12, height = 4, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print(gg)
dev.off()

grid.arrange(p, arrangeGrob(p1, p2, p3, ncol=2), ncol=1, heights=c(3/5, 2/5))

CairoPDF(file = "cbf_figure",
         width = 12, height = 4.5, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print( 
  grid.arrange(
    arrangeGrob(pc, pb, pf, ncol=3), 
    arrangeGrob(p1, p2, p3, ncol=3), ncol=1, heights=c(0.48, 0.52)) )
dev.off()
