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
#
#
dat = read.table("paper/datainpaper/ECG5DFigure5.txt",header=F)
names(dat)=c("label","d1","d2")
dat$label=factor(dat$label)

p = ggplot(dat, aes(x = d1, y = d2, color=label, shape=label)) +
  theme_bw() + geom_point(size=5) + 
  scale_colour_manual(values=c(cbPalette[2],cbPalette[3]), name="Classes: ", 
    labels=c("Class #1", "Class #2")) + 
  scale_shape_manual(values=c(17,19), name="Classes: ", labels=c("Class #1", "Class #2")) +
  xlab("Distance to the first representative pattern") + scale_x_continuous(limits = c(0, 0.12), breaks=c(0.0,0.03,0.06,0.09,0.12))+
  ylab("Distance to the second pattern") + scale_y_continuous(limits = c(0, 0.12), breaks=c(0.0,0.03,0.06,0.09,0.12))+
  theme(axis.line=element_blank(),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks=element_blank(),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=13),
        #legend.position="none",
        panel.background=element_blank(),
        #panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position="bottom",
        plot.title = element_text(size = 20),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16,hjust = 0.5),
        legend.key.width=unit(2, "cm")) +
  ggtitle("Train instances of ECGFiveDays dataset in the\n representative patterns feature space")

CairoPDF(file = "ecg5days_feature",
         width = 7, height = 5, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print(p)
dev.off()
