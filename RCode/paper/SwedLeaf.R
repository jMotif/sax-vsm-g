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

#get the data
data=read.table("/home/psenin/workspace/data/SwedishLeaf/SwedishLeaf_TRAIN")
unique(data[,1])

fours=(data[data$V1==4,])[,-1]
ser=as.numeric(fours[1,])
for(i in 2:14){
  ser=c(ser,as.numeric(fours[i,]))
}
plot(ser,type="l")
dm=data.frame(x=c(1:length(ser)),y=ser)
p = ggplot(dm, aes(x, y)) +
  theme_bw() + geom_line(size=0.5,col=cbbPalette[1])+
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
        plot.title = element_text(size = 14),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        legend.key.width=unit(2, "cm")) +
  ggtitle("Concatenation of multiple time series from Class 4.")
p

dm=data.frame(x=c(1:128),y=as.numeric(fours[32,]))
p0 = ggplot(dm, aes(x, y)) +
  theme_bw() + geom_line(size=0.5,col=cbbPalette[1])+
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
        plot.title = element_text(size = 14),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        legend.key.width=unit(2, "cm")) +
  ggtitle("A single time series from Class 4")
p0

gg=arrangeGrob(p0, p, ncol=2, widths=c(0.3,0.7))
gg

CairoPDF(file = "/home/psenin/git/sax-vsm-g.git/RCode/swed_leaf_concatenation",
         width = 10, height = 3, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print(gg)
dev.off()
