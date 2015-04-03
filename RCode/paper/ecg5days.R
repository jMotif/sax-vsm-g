require(ggplot2)
require(Cairo)
require(reshape)
require(scales)
require(RColorBrewer)
require(grid)
library(gridExtra)
require(lattice)

# http://jfly.iam.u-tokyo.ac.jp/color/
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# To use for fills, add
#scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
#scale_colour_manual(values=cbPalette)
pal2=brewer.pal(8, "Set1")


dat = read.table("../data/ECGFiveDays/ECGFiveDays_TRAIN",header=F)
unique(dat[,1])
range(dat[2,])
dat=t(dat)

ones=(dat[,dat[1,]==1])[-1,3]
twos=(dat[,dat[1,]==2])[-1,5]
# Take the twelve series and melt (or equivalently, stack) them:
dm <- melt(cbind(ones,twos))
# add an index variable:
dm$index <- rep(1:136, 2)
dm$class <- c(rep("Class 1",(136*1)), rep("Class 2",(136*1)))

# dm$variable is a factor with a level for each series; use it
# as a grouping variable. dm$value holds the values of each series.

# This produces a 'spaghetti plot' (familiar to mixed modelers):
p = ggplot(dm, aes(x = index, y = value, group = X2, color=class,linetype=class)) +
  theme_bw() + geom_line(size=1)+ 
  scale_colour_manual(values=c(cbPalette[2],cbPalette[3]), name="Classes: ", labels=c("Class 1", "Class 2")) + 
  scale_linetype_manual(values=c(1,5), name="Classes: ", labels=c("Class 1", "Class 2")) +
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
        plot.title = element_text(size = 20),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16),
        legend.key.width=unit(2, "cm")) +
  ggtitle("Two classes from ECG Five Days dataset")
p

CairoPDF(file = "ecg5days_classes",
         width = 10, height = 4, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print(p)
dev.off()

########################
########################

dat = read.table("../data/ECGFiveDays/ECGFiveDays_TRAIN",header=F)

class0=(dat[dat$V1==1,])[,-1]
df0=data.frame(x=rep(c(1:136),14), 
               series=rep(paste("ser",c(1:14),sep=""),each=136),
               y=as.numeric(unlist(t(class0))))

class1=(dat[dat$V1==2,])[,-1]
df1=data.frame(x=rep(c(1:136),9), 
               series=rep(paste("ser",c(1:9),sep=""),each=136),
               y=as.numeric(unlist(t(class1))))

p1=ggplot(df0,aes(x,y,group=series)) + theme_bw() + 
  geom_line(size=0.2,col=cbPalette[2],alpha=0.8)+ 
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
  ggtitle("Class #1 and the best representative pattern") 

pattern1=data.frame(y=as.numeric(class0[9,56:(56+60)]),
                    x=c(56:(56+60)),series=rep("pattern1",61))
p1 = p1 + geom_line(data=pattern1, col=pal2[1], linetype=1, size=2)
p1

p2=ggplot(df1,aes(x,y,group=series)) + theme_bw() + 
  geom_line(size=0.2,col=cbPalette[3],alpha=0.8)+ 
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
  ggtitle("Class #2 and the best representative pattern") 
p2

pattern1=data.frame(y=as.numeric(class1[1,23:(23+58)]),
                    x=c(23:(23+58)),series=rep("pattern1",59))
p2 = p2 + geom_line(data=pattern1, col=pal2[4], linetype=1, size=2)

gg=arrangeGrob(p1, p2, ncol=2)
gg

CairoPDF(file = "ecg5days_class_patterns",
         width = 12, height = 4, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print(gg)
dev.off()

grid.arrange(p, arrangeGrob(p1, p2, ncol=2), ncol=1, heights=c(3/5, 2/5))

CairoPDF(file = "ecg5days_figure",
         width = 12, height = 6, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print( grid.arrange(p, arrangeGrob(p1, p2, ncol=2), ncol=1, heights=c(0.52, 0.48)) )
dev.off()

