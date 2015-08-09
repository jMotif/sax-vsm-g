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
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pal2=brewer.pal(8, "Set1")


#get the data
data=read.table("../data/alarm_train")
unique(data[,1])
#[1] 1 2 3 4 5
l1=dim(data[data[,1]==1,])[1]
l2=dim(data[data[,1]==2,])[1]
l3=dim(data[data[,1]==3,])[1]
l4=dim(data[data[,1]==4,])[1]
l5=dim(data[data[,1]==5,])[1]
sl=length(as.numeric((data[data[,1]==1,])[1,-1]))

df1=data.frame(x=rep(c(1:sl),l1), 
               series=rep(paste("ser",c(1:l1),sep=""),each=sl),
               y=as.numeric(unlist(t(data[data[,1]==1,-1]))))
p1=ggplot(df1,aes(x,y,group=series)) + theme_bw() + 
  geom_line(size=0.2,col=cbPalette[2],alpha=0.8)+ 
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(), plot.background=element_blank(),
        legend.position="bottom", plot.title = element_text(size = 20),
        legend.text=element_text(size=14), legend.title=element_text(size=16),
        legend.key.width=unit(2, "cm")) +
  ggtitle("Asystole") 
p1
df1=data[data[,1]==1,-1]
pattern1=data.frame(y=as.numeric(df1[3,1479:(1479+239)]),x=c(1479:(1479+239)),series=rep("pattern1",240))
pattern2=data.frame(y=as.numeric(df1[10,1395:(1395+209)]),x=c(1395:(1395+209)),series=rep("pattern2",210))
pattern3=data.frame(y=as.numeric(df1[9,1646:(1646+245)]),x=c(1646:(1646+245)),series=rep("pattern3",246))
p1=p1 + geom_line(data=pattern1, col=pal2[1], alpha=1, linetype=1, size=2) +
  geom_line(data=pattern2, col=pal2[2], alpha=1, linetype=1, size=2)+
  geom_line(data=pattern3, col=pal2[3], alpha=1, linetype=1, size=2)  
  
df2=data.frame(x=rep(c(1:sl),l1), 
               series=rep(paste("ser",c(1:l1),sep=""),each=sl),
               y=as.numeric(unlist(t(data[data[,1]==2,-1]))))
p2=ggplot(df2,aes(x,y,group=series)) + theme_bw() + 
  geom_line(size=0.2,col=cbPalette[2],alpha=0.8)+ 
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(), plot.background=element_blank(),
        legend.position="bottom", plot.title = element_text(size = 20),
        legend.text=element_text(size=14), legend.title=element_text(size=16),
        legend.key.width=unit(2, "cm")) +
  ggtitle("Extreme bradycardia") 
p2
df2=data[data[,1]==2,-1]
pattern1=data.frame(y=as.numeric(df2[7,296:(296+256)]),x=c(296:(296+256)),series=rep("pattern1",257))
pattern2=data.frame(y=as.numeric(df2[2,1169:(1169+249)]),x=c(1169:(1169+249)),series=rep("pattern2",250))
pattern3=data.frame(y=as.numeric(df2[2,1666:(1666+255)]),x=c(1666:(1666+255)),series=rep("pattern3",256))
p2=p2 + geom_line(data=pattern1, col=pal2[2], alpha=1, linetype=1, size=2) +
  geom_line(data=pattern2, col=pal2[3], alpha=1, linetype=1, size=2)+
  geom_line(data=pattern3, col=pal2[4], alpha=1, linetype=1, size=2)  

df3=data.frame(x=rep(c(1:sl),l1), 
               series=rep(paste("ser",c(1:l1),sep=""),each=sl),
               y=as.numeric(unlist(t(data[data[,1]==3,-1]))))
p3=ggplot(df3,aes(x,y,group=series)) + theme_bw() + 
  geom_line(size=0.2,col=cbPalette[3],alpha=0.8)+ 
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(), plot.background=element_blank(),
        plot.title = element_text(size = 20),
        legend.text=element_text(size=14), legend.title=element_text(size=16)) +  ggtitle("Extreme tachycardia") 
p3
df3=data[data[,1]==3,-1]
pattern1=data.frame(y=as.numeric(df3[5,1072:(1072+204)]),x=c(1072:(1072+204)),series=rep("pattern1",205))
pattern2=data.frame(y=as.numeric(df3[2,1547:(1547+206)]),x=c(1547:(1547+206)),series=rep("pattern2",207))
pattern3=data.frame(y=as.numeric(df3[3,632:(632+208)]),x=c(632:(632+208)),series=rep("pattern3",209))
p3=p3 + geom_line(data=pattern1, col=pal2[1], alpha=1, linetype=1, size=2) +
  geom_line(data=pattern2, col=pal2[2], alpha=1, linetype=1, size=2)+
  geom_line(data=pattern3, col=pal2[3], alpha=1, linetype=1, size=2)  


df4=data.frame(x=rep(c(1:sl),l1), 
               series=rep(paste("ser",c(1:l1),sep=""),each=sl),
               y=as.numeric(unlist(t(data[data[,1]==4,-1]))))
p4=ggplot(df4,aes(x,y,group=series)) + theme_bw() + 
  geom_line(size=0.2,col=cbPalette[7],alpha=0.8)+ 
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(), plot.background=element_blank(),
        plot.title = element_text(size = 20),
        legend.text=element_text(size=14), legend.title=element_text(size=16)) +  ggtitle("VTach") 
p4
df4=data[data[,1]==4,-1]
pattern1=data.frame(y=as.numeric(df4[6,424:(424+211)]),x=c(424:(424+211)),series=rep("pattern1",212))
pattern2=data.frame(y=as.numeric(df4[7,667:(667+216)]),x=c(667:(667+216)),series=rep("pattern2",217))
pattern3=data.frame(y=as.numeric(df4[8,1003:(1003+224)]),x=c(1003:(1003+224)),series=rep("pattern3",225))
p4=p4 + geom_line(data=pattern1, col=pal2[2], alpha=1, linetype=1, size=2) +
  geom_line(data=pattern2, col=pal2[3], alpha=1, linetype=1, size=2)+
  geom_line(data=pattern3, col=pal2[4], alpha=1, linetype=1, size=2)  


df5=data.frame(x=rep(c(1:sl),l1), 
               series=rep(paste("ser",c(1:l1),sep=""),each=sl),
               y=as.numeric(unlist(t(data[data[,1]==5,-1]))))
p5=ggplot(df5,aes(x,y,group=series)) + theme_bw() + 
  geom_line(size=0.2,col=cbPalette[8],alpha=0.8)+ 
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(), plot.background=element_blank(),
        plot.title = element_text(size = 20),legend.text=element_text(size=14), legend.title=element_text(size=16)) +  ggtitle("VTach/VFib") 
p5
df5=data[data[,1]==5,-1]
pattern1=data.frame(y=as.numeric(df5[7,177:(177+207)]),x=c(177:(177+207)),series=rep("pattern1",208))
pattern2=data.frame(y=as.numeric(df5[7,1549:(1549+211)]),x=c(1549:(1549+211)),series=rep("pattern2",212))
pattern3=data.frame(y=as.numeric(df5[4,501:(501+211)]),x=c(501:(501+211)),series=rep("pattern3",212))
p5=p5 + geom_line(data=pattern1, col=pal2[1], alpha=1, linetype=1, size=2) +
  geom_line(data=pattern2, col=pal2[2], alpha=1, linetype=1, size=2)+
  geom_line(data=pattern3, col=pal2[3], alpha=1, linetype=1, size=2)  

grid.arrange(
  arrangeGrob(p1, p2, ncol=2), 
  arrangeGrob(p3, p4, ncol=2), 
  p5, ncol=1
)

CairoPDF(file = "med5_figure_new",
         width = 12, height = 8, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print(grid.arrange(
  arrangeGrob(p1, p2, ncol=2), 
  arrangeGrob(p3, p4, ncol=2), 
  p5, ncol=1
))
dev.off()
