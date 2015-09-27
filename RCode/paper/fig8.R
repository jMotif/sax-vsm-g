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
data=t(read.table("paper/datainpaper/runningTimeCompareFigure8.txt"))
str(data)
colnames(data)=paste(c(1:41))
rownames(data)=paste(c(10,30,50,70,90))
dm=melt(data)
dm$X2=factor(dm$X2)
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
#gs.pal <- colorRampPalette(c("#FFFFFF","#000000"),bias=.1,space="rgb")

gs.pal <- colorRampPalette(c("dodgerblue2","#E31A1C", # red
                             "green4",
                             "#6A3D9A", # purple
                             "#FF7F00", # orange
                             "black","gold1",
                             "skyblue2","#FB9A99", # lt pink
                             "palegreen2",
                             "#CAB2D6", # lt purple
                             "#FDBF6F", # lt orange
                             "gray70", "khaki2",
                             "maroon","orchid1","deeppink1","blue1","steelblue4",
                             "darkturquoise","green1","yellow4","yellow3",
                             "darkorange4","brown"))

scientific_10 <- function(x) {
  parse(text=gsub("e", "%*%10^", scientific_format()(x)))
}

p0=ggplot(dm,aes(x=X1,y=value,color=X2,group=X2)) + theme_bw() + 
  geom_line(size=0.7) + 
  scale_colour_manual(values=gs.pal(42)) + 
  scale_x_continuous(limits = c(10,90), breaks=c(10,30,50,70,90))+
  scale_y_continuous(limits = c(0, 6000), breaks=seq(0,6000,by=1000))+
  theme(axis.line=element_blank(),
        axis.text.x=element_text(size = 18),
        axis.text.y=element_text(size = 15),
        #axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #legend.position="none",
        panel.background=element_blank(),
        #panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        #legend.position="bottom",
        legend.position="none",
        plot.title = element_text(size = 24)
        #legend.text=element_text(size=14),
        #legend.title=element_text(size=16),
        #legend.key.width=unit(2, "cm")
  ) +
  ggtitle(expression(paste("Running time with different ",tau)))
p0

#get the data
data=t(read.table("paper/datainpaper/accuracyCompareFigure8.txt"))
str(data)
colnames(data)=paste(c(1:42))
rownames(data)=paste(c(10,30,50,70,90))
dm=melt(data)
dm$X2=factor(dm$X2)
getPalette = colorRampPalette(brewer.pal(9, "Set2"))
#gs.pal <- colorRampPalette(c("#FFFFFF","#000000"),bias=.1,space="rgb")

gs.pal <- colorRampPalette(c("dodgerblue2","#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black","gold1",
  "skyblue2","#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon","orchid1","deeppink1","blue1","steelblue4",
  "darkturquoise","green1","yellow4","yellow3",
  "darkorange4","brown"))

p1=ggplot(dm,aes(x=X1,y=value,color=X2,group=X2)) + theme_bw() + 
  geom_line(size=0.7) + 
  scale_colour_manual(values=gs.pal(42)) + 
  scale_x_continuous(limits = c(10,90), breaks=c(10,30,50,70,90))+
  scale_y_continuous(limits = c(0, 0.7), breaks=seq(0,0.7,by=0.1))+
  theme(axis.line=element_blank(),
        axis.text.x=element_text(size = 18),
        axis.text.y=element_text(size = 15),
        #axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #legend.position="none",
        panel.background=element_blank(),
        #panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        #legend.position="bottom",
        legend.position="none",
        plot.title = element_text(size = 24)
        #legend.text=element_text(size=14),
        #legend.title=element_text(size=16),
        #legend.key.width=unit(2, "cm")
        ) +
  ggtitle(expression(paste("Classification error with different ",tau)))
p1

CairoPDF(file = "performance_new",
         width = 12, height = 5, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print( arrangeGrob(p0, p1, ncol=2))
dev.off()
