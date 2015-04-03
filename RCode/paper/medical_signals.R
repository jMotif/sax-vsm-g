#get the data
require(ggplot2)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pal2=brewer.pal(8, "Set1")


data=read.table("paper/datainpaper/abp_normal")
data=as.numeric(data)
df = data.frame(x=c(1:length(data)),y=data)
pn = ggplot(df,aes(x,y)) +
  theme_bw() + geom_line(size=0.8,col=cbPalette[4]) + 
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
  ggtitle("Normal signal")
pn


#get the data
data=read.table("paper/datainpaper/abp_with_alarm")
data=as.numeric(data)
df = data.frame(x=c(1:length(data)),y=data)
pa = ggplot(df,aes(x,y)) +
  theme_bw() + geom_line(size=0.8,col=cbPalette[7]) + 
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
  ggtitle("Signal with alarm")
pa

# combine plots
require(grid)
library(gridExtra)
gg=arrangeGrob(pn, pa, ncol=1)
print(gg)
#
library(Cairo)
CairoPDF(file = "signals_summary", width = 10, height = 5, onefile = TRUE, 
         family = "Helvetica", paper = "special")
print( gg ) 
dev.off()

########################
########################

dat = read.table("../data/alarm_TRAIN",header=F)

class0=(dat[dat$V1==1,])[,-1]
df0=data.frame(x=rep(c(1:2126),8), 
               series=rep(paste("ser",c(1:8),sep=""),each=2126),
               y=as.numeric(unlist(t(class0))))

class1=(dat[dat$V1==2,])[,-1]
df1=data.frame(x=rep(c(1:2126),8), 
               series=rep(paste("ser",c(1:8),sep=""),each=2126),
               y=as.numeric(unlist(t(class1))))

p1=ggplot(df0,aes(x,y,group=series)) + theme_bw() + 
  geom_line(size=0.2,col=cbPalette[4],alpha=0.5)+ 
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
  ggtitle("Class Normal and the best representative patterns") 
p1
pattern1=data.frame(y=as.numeric(class0[6,953:(953+224)]),
                    x=c(953:(953+224)),series=rep("pattern1",225))
pattern2=data.frame(y=as.numeric(class0[2,75:(75+209)]),
                    x=c(75:(75+209)),series=rep("pattern2",210))
pattern3=data.frame(y=as.numeric(class0[7,389:(389+221)]),
                    x=c(389:(389+221)),series=rep("pattern3",222))
p1 = p1 + geom_line(data=pattern1, col=pal2[1], alpha=1, linetype=1, size=2) +
   geom_line(data=pattern2, col=pal2[5], alpha=1, linetype=1, size=2) +
   geom_line(data=pattern3, col=pal2[8], alpha=1, linetype=1, size=2)
p1

p2=ggplot(df1,aes(x,y,group=series)) + theme_bw() + 
  geom_line(size=0.2,col=cbPalette[7],alpha=0.6)+ 
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
  ggtitle("Class Anomalous and the best representative patterns") 
p2
pattern1=data.frame(y=as.numeric(class1[3,593:(593+260)]),
                    x=c(593:(593+260)),series=rep("pattern1",261))
pattern2=data.frame(y=as.numeric(class1[5,987:(987+236)]),
                    x=c(987:(987+236)),series=rep("pattern2",237))
pattern3=data.frame(y=as.numeric(class1[1,1712:(1712+235)]),
                    x=c(1712:(1712+235)),series=rep("pattern3",236))
p2 = p2 + geom_line(data=pattern1, col=pal2[3], alpha=1, linetype=1, size=2) +
  geom_line(data=pattern2, col=pal2[4], alpha=1, linetype=1, size=2) +
  geom_line(data=pattern3, col=pal2[2], alpha=1, linetype=1, size=2)
p2

gg=arrangeGrob(p1, p2, ncol=2)
gg

CairoPDF(file = "medical_class_patterns",
         width = 12, height = 4, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print(gg)
dev.off()

grid.arrange(arrangeGrob(pn, pa, ncol=1), arrangeGrob(p1, p2, ncol=2), ncol=1, heights=c(3/5, 2/5))

CairoPDF(file = "medical_figure",
         width = 12, height = 7, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print( grid.arrange(arrangeGrob(pn, pa, ncol=1), arrangeGrob(p1, p2, ncol=2), ncol=1, heights=c(0.62, 0.38)) )
dev.off()

