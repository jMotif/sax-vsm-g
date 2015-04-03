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


dat = read.table("../data/Coffee/Coffee_TRAIN",header=F)
unique(dat[,1])
range(dat[2,])
dat=t(dat)

ones=(dat[,dat[1,]==0])[-1,1]
twos=(dat[,dat[1,]==1])[-1,5]
# Take the twelve series and melt (or equivalently, stack) them:
dm <- melt(cbind(ones,twos))
# add an index variable:
dm$index <- rep(1:286, 2)
dm$class <- c(rep("Class 0",(286*1)), rep("Class 1",(286*1)))

# dm$variable is a factor with a level for each series; use it
# as a grouping variable. dm$value holds the values of each series.

# This produces a 'spaghetti plot' (familiar to mixed modelers):
p = ggplot(dm, aes(x = index, y = value, group = X2, color=class,linetype=class)) +
  theme_bw() + geom_line(size=1)+ 
  scale_colour_manual(values=c(cbPalette[2],cbPalette[3]), name="Classes: ", labels=c("Robusta (#0)", "Arabica (#1)")) + 
  scale_linetype_manual(values=c(1,5), name="Classes: ", labels=c("Robusta (#0)", "Arabica (#1)")) +
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
  ggtitle("Two classes from the Coffee dataset")
p

CairoPDF(file = "coffee_classes",
         width = 10, height = 4, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print(p)
dev.off()

########################
########################

s1=c(10.680395,10.490485,10.447814,10.487509,10.595824,10.479158,10.209243,10.078654,9.895661,9.712807,9.766268,9.889087,9.936869,9.917874,9.66278,9.3478,9.090635,9.032518,9.233356,9.328724,9.173173,8.91048,8.840363,8.886847,9.01745,9.168898,9.17845,9.338318,9.615915,9.854314,10.097516,10.496529,10.985124,11.474178,11.985992,12.378202,12.704909,12.982611,12.994957,13.164608,13.521349,13.622842,14.098871,14.799419,15.332591,16.094534,16.848969,17.710203,18.760384,19.74135,20.173527,20.711599,21.465806,21.530194,21.583179,22.246706,22.818398,22.726997,23.335544,24.249498,25.534178,26.31795)
s2=c(20.714994,22.022289,23.209382,24.682651,25.753599,26.305101,27.755019,29.439765,29.567787,30.34246,31.349539,31.444708,32.026699,32.353775,31.385339,30.692584,30.823826,30.052898,29.946157,30.460884,29.946748,29.947184,30.630942,31.617454,30.941542,29.693242,27.925813,26.2306,25.058287,23.649021,23.104436,24.10755,25.064126,25.523844,26.058379,28.164515,29.228167,28.002577,25.638624,22.920198,20.576152,18.402266,16.622488,14.928548,13.218559,12.000851,10.675888,9.662025,8.858905,8.339107,8.029009,7.949262,7.981703,8.029814,8.093526,7.96437,7.578899,6.956225,6.220442,5.334824,4.57612,3.917181)
s3=c(15.854723,15.802377,16.030059,16.685388,17.482574,18.315315,19.01651,19.113752,19.733572,20.223873,20.50778,21.243257,21.549031,21.781629,22.16252,22.194055,22.036379,22.266409,22.243855,21.883068,20.944176,19.914675,19.32711,18.537658,17.738871,17.214245,16.964735,16.782607,16.485097,16.29583,16.098404,15.901756,15.93714,16.159881,16.476501,17.178495,18.131231,19.250867,20.318748,20.730991,21.091108,21.61467,21.3218,20.727574,21.153097,21.545647,21.616796,21.435797,21.660225,21.699385,21.333535,21.075754,20.190231,19.607194,19.293779,18.916834,18.734651,18.880478,18.737002,18.466888,18.390721,17.960827)
s4=c(14.539312,14.296876,13.870481,13.305166,12.83516,12.833233,13.607615,14.32354,15.179103,15.715336,16.398556,17.355489,17.529748,18.192629,19.204946,20.830702,22.078577,22.686193,23.432067,24.755603,25.923219,26.838882,27.717579,27.976133,27.583717,27.343367,26.819092,25.765985,24.966902,24.332885,24.449519,26.282336,27.068477,27.619316,28.626223,30.595453,30.53977,28.573366,26.625698,24.544415,22.706766,21.89759,21.79429,22.77001,24.005728,25.199106,26.778743,28.390917,29.155506,27.685789,24.768335,20.455827,16.979124,14.760387,12.416862,10.611281,9.109819,7.901985,6.761077,5.966757,5.325892,4.938615)
  
dat = read.table("../data/Coffee/Coffee_TRAIN",header=F)

class0=(dat[dat$V1==0,])[,-1]
df0=data.frame(x=rep(c(1:286),14), 
               series=rep(paste("ser",c(1:14),sep=""),each=286),
               y=as.numeric(unlist(t(class0))))

class1=(dat[dat$V1==1,])[,-1]
df1=data.frame(x=rep(c(1:286),14), 
               series=rep(paste("ser",c(1:14),sep=""),each=286),
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
  ggtitle("Class Robusta and the best representative patterns") 

pattern1=data.frame(y=as.numeric(class0[7,9:(9+62)]),
                    x=c(9:(9+62)),series=rep("pattern1",63))
pattern2=data.frame(y=as.numeric(class0[7,196:(196+62)]),
                    x=c(196:(196+62)),series=rep("pattern2",63))
p1 = p1 + geom_line(data=pattern1, col=pal2[1], linetype=1, size=2) +
     geom_line(data=pattern2, col=pal2[3], linetype=1, size=2)

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
  ggtitle("Class Arabica and the best representative patterns") 

pattern1=data.frame(y=as.numeric(class1[7,7:(7+62)]),
                    x=c(7:(7+62)),series=rep("pattern1",63))
pattern2=data.frame(y=as.numeric(class1[11,184:(184+62)]),
                    x=c(184:(184+62)),series=rep("pattern2",63))
p2 = p2 + geom_line(data=pattern1, col=pal2[7], linetype=1, size=2) +
  geom_line(data=pattern2, col=pal2[4], linetype=1, size=2)

gg=arrangeGrob(p1, p2, ncol=2)
gg

CairoPDF(file = "coffee_class_patterns",
         width = 12, height = 4, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print(gg)
dev.off()

grid.arrange(p, arrangeGrob(p1, p2, ncol=2), ncol=1, heights=c(3/5, 2/5))

CairoPDF(file = "coffee_figure",
         width = 12, height = 6, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print( grid.arrange(p, arrangeGrob(p1, p2, ncol=2), ncol=1, heights=c(0.57, 0.43)) )
dev.off()
