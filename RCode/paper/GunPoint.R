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


dat = read.table("../data/Gun_Point/Gun_Point_TRAIN",header=F)
unique(dat[,1])

ones=as.numeric((dat[dat$V1==1,])[5,-1])
twos=as.numeric((dat[dat$V1==2,])[2,-1])
# Take the twelve series and melt (or equivalently, stack) them:
dm <- melt(cbind(ones,twos))
# add an index variable:
dm$index <- rep(1:150, 2)
dm$class <- c(rep("Class 1",(150*1)), rep("Class 2",(150*1)))

# This produces a 'spaghetti plot' (familiar to mixed modelers):
pn = ggplot(dm, aes(x = index, y = value, group = X2, color=class,linetype=class)) +
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
        plot.title = element_text(size = 16),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.key.width=unit(2, "cm")) +
  ggtitle("Two classes from GunPoint dataset")
pn


dat = read.table("../data/GunPoint_data_shifted",sep=" ",header=F)[,-152]
unique(dat[,1])

ones=as.numeric((dat[dat$V1==1,])[2,-1])
twos=as.numeric((dat[dat$V1==2,])[2,-1])
# Take the twelve series and melt (or equivalently, stack) them:
dm <- melt(cbind(ones,twos))
# add an index variable:
dm$index <- rep(1:150, 2)
dm$class <- c(rep("Class 1",(150*1)), rep("Class 2",(150*1)))

# This produces a 'spaghetti plot' (familiar to mixed modelers):
ps = ggplot(dm, aes(x = index, y = value, group = X2, color=class,linetype=class)) +
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
        plot.title = element_text(size = 16),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.key.width=unit(2, "cm")) +
  ggtitle("Two classes from the shifted Gun Point dataset")
ps

gg = arrangeGrob(pn, ps, ncol=2)

CairoPDF(file = "gunpoint_figure",
         width = 12, height = 4, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print(gg)
dev.off()

########################
########################
  
dat = read.table("../data/GunPoint_data_shifted",header=F)[,-152]

class0=(dat[dat$V1==1,])[,-1]
df0=data.frame(x=rep(c(1:150),76), 
               series=rep(paste("ser",c(1:76),sep=""),each=150),
               y=as.numeric(unlist(t(class0))))

class1=(dat[dat$V1==2,])[,-1]
df1=data.frame(x=rep(c(1:150),74), 
               series=rep(paste("ser",c(1:74),sep=""),each=150),
               y=as.numeric(unlist(t(class1))))

p1=ggplot(df0,aes(x,y,group=series)) + theme_bw() + 
  geom_line(size=0.2,col=cbPalette[2],alpha=0.65)+ 
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
  ggtitle("Shifted Class 1 and the best representative patterns") 
p1
pattern1=data.frame(y=as.numeric(class0[72,67:(67+29)]),
                    x=c(67:(67+29)),series=rep("pattern1",30))
pattern2=data.frame(y=as.numeric(class0[70,24:(24+28)]),
                    x=c(24:(24+28)),series=rep("pattern2",29))
p1 = p1 + geom_line(data=pattern1, col=pal2[1], linetype=1, size=2) +
     geom_line(data=pattern2, col=pal2[3], linetype=1, size=2)

p2=ggplot(df1,aes(x,y,group=series)) + theme_bw() + 
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
  ggtitle("Shifted Class 2 and the best representative patterns") 

pattern1=data.frame(y=as.numeric(class1[32,97:(97+43)]),
                    x=c(97:(97+43)),series=rep("pattern1",44))
pattern2=data.frame(y=as.numeric(class1[4,43:(43+42)]),
                    x=c(43:(43+42)),series=rep("pattern2",43))
p2 = p2 + geom_line(data=pattern1, col=pal2[7], linetype=1, size=2) +
  geom_line(data=pattern2, col=pal2[4], linetype=1, size=2)

gg=arrangeGrob(p1, p2, ncol=2)
gg

CairoPDF(file = "shifted_gunpoint_patterns",
         width = 12, height = 4, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print(gg)
dev.off()

grid.arrange(arrangeGrob(pn, ps, ncol=2), arrangeGrob(p1, p2, ncol=2), ncol=1, heights=c(3/5, 2/5))

CairoPDF(file = "gunpoint_figure",
         width = 12, height = 6, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print(grid.arrange(arrangeGrob(pn, ps, ncol=2), 
             arrangeGrob(p1, p2, ncol=2), ncol=1, heights=c(0.53, 0.47)))
dev.off()
