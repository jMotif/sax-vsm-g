#get the data
require(ggplot2)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
data=read.table("/home/psenin/Downloads/abp_normal")
data=as.numeric(data)
df = data.frame(x=c(1:length(data)),y=data)
pn = ggplot(df,aes(x,y)) +
  theme_bw() + geom_line(size=0.8,col=cbPalette[1]) + 
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
data=read.table("/home/psenin/Downloads/abp_with_alarm")
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
CairoPDF(file = "/home/psenin/git/sax-vsm-g.git/RCode/signals_summary", width = 10, height = 5, onefile = TRUE, 
         family = "Helvetica", paper = "special")
print( gg ) 
dev.off()
