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
  scale_colour_manual(values=cbPalette, name="Classes: ", labels=c("Arabica", "Robusta")) + 
  scale_linetype_manual(values=c(1,5), name="Classes: ", labels=c("Arabica", "Robusta")) +
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
