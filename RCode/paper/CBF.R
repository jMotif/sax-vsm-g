require(ggplot2)
require(Cairo)
require(reshape)
require(scales)
require(RColorBrewer)
require(grid)
library(gridExtra)
require(lattice)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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
