require(ggplot2)
require(Cairo)
require(reshape)
require(scales)
require(RColorBrewer)
require(grid)
library(gridExtra)
require(RColorBrewer)

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


#
znorm <- function(ts){
  ts.mean <- mean(ts)
  ts.dev <- sd(ts)
  (ts - ts.mean)/ts.dev
}
#
dat = read.table("../data/Cricket/Cricket_TEST",header=F)
ones=as.data.frame(t(dat[dat[,1]==1,]-1))
names(ones)<-paste("series_",c(1:5),sep="")
dm=melt(ones)
dm$X=rep(c(1:309),5)
p0=ggplot(dm,aes(x=X,y=value,group=variable)) + geom_line(color=cbPalette[1])+theme_bw()
p0
print(arrangeGrob(p, p0, ncol=1))

write.table(dat,"../data/Cricket/Cricket_TEST00",col.names=F,row.names=F)
dat = read.table("../data/Cricket/Cricket_TRAIN",header=F)
write.table(dat,"../data/Cricket/Cricket_TRAIN00",col.names=F,row.names=F)
#
# first goes SAX-VSM pattern, not the map
# [data/Cricket/Cricket_TRAIN, data/Cricket/Cricket_TEST, 88, 16, 6, CLASSIC]
#Class key: 1
#"bcfffbcbccbbbdcc", 0.14002800840280097
#"cccccfffeccccbba", 0.14002800840280097
#"fffbbbbccaabdccc", 0.14002800840280097
#"bbdffebcccdbbbdc", 0.14002800840280097
#"cccccdfffcccccba", 0.14002800840280097
#Class key: 2
#"ffdbbbbbccccbbbb", 0.17488288414773762
#"bbfffcccbccbbbcc", 0.17488288414773762
#"fffcbbbbbccccbbb", 0.10328864859220424
#"bdddcbbbbbfffbbb", 0.10328864859220424
#"bbcffdbbbbbccccb", 0.10328864859220424
#Class key: 1
#pattern="bcfffbcbccbbbdcc"; weight=0.14003
#2: [28]
ones=dat[dat[,1]==1,-1]
series=unlist(ones[3,])
plot(series,type="l")
#
rl_start=29
rl_len=88
red_line=series[rl_start:(rl_start+rl_len)]
lines(x=c(rl_start:(rl_start+rl_len)),y=red_line,lwd=2,col="red")
#
df1=data.frame(time=c(1:length(series)),value=series)
df1_red<-df1;df1_red$value<-rep(NA,length(series));df1_red[rl_start:(rl_start+rl_len),]$value<-red_line
p1 <- ggplot(df1, aes(time, value)) + geom_line(color=cbPalette[3], lwd=1) + theme_bw() + 
  theme(axis.line=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(), plot.background=element_blank(),
        legend.position="bottom", plot.title = element_text(size = 20),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width=unit(2, "cm")) +
  ggtitle("SAX-VSM, the best pattern for Class #1")
p1=p1 + geom_line(aes(df1_red$time,df1_red$value),color=cbPalette[8],size=2)
p1
#
#Class key: 2
#pattern="ffdbbbbbccccbbbb"; weight=0.17488
#2: [180],
twos=dat[dat[,1]==2,-1]
series=unlist(twos[3,])
plot(series,type="l")
#
rl_start=180
rl_len=88
red_line=series[rl_start:(rl_start+rl_len)]
lines(x=c(rl_start:(rl_start+rl_len)),y=red_line,lwd=2,col="red")
#
df2=data.frame(time=c(1:length(series)),value=series)
df2_red<-df2;df2_red$value<-rep(NA,length(series));df2_red[rl_start:(rl_start+rl_len),]$value<-red_line
p2 <- ggplot(df2, aes(time, value)) + geom_line(color=cbPalette[2], lwd=1) + theme_bw() + 
  theme(axis.line=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(), plot.background=element_blank(),
        legend.position="bottom", plot.title = element_text(size = 20),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width=unit(2, "cm")) +
  ggtitle("SAX-VSM, the best pattern for Class #2")
p2=p2 + geom_line(aes(df2_red$time,df2_red$value),color=cbPalette[6],size=2)
p2
#
# following Xing's results
#
pat1=c(0.0352,0.0332,0.0332,0.0293,0.0117,-0.0469,-0.1016,-0.1172,-0.1211,-0.1367,-0.1211,-0.125,-0.125,-0.123,-0.127,-0.125,-0.125,-0.125,-0.125,-0.125,-0.127,-0.125,-0.125,-0.125,-0.125,-0.125,-0.125,-0.125,-0.125,-0.127,-0.127,-0.127,-0.127,-0.127,-0.125,-0.125,-0.125,-0.123,-0.125,-0.125,-0.1055,-0.0977,-0.084,-0.17,-0.1758,-0.1465,-0.125,-0.1387,-0.1582,-0.1602,-0.1582,-0.127,-0.1055,-0.1778,-0.2012,-0.1661,-0.1426,-0.0879,-0.1133,-0.1133,-0.1153,-0.1094,-0.1075,-0.1094,-0.1133,-0.1133,-0.1133,-0.1133,-0.1094,-0.1133,-0.1094,-0.1133,-0.1094,-0.1133,-0.1094,-0.1094,-0.1094,-0.1094,-0.1094,-0.1094,-0.1094,-0.1094,-0.1133,-0.1133,-0.1133,-0.1133,-0.1133,-0.1055,-0.1075,-0.0528,-0.0411,-0.084,-0.1055,-0.1075,-0.1231,-0.1036,-0.1055,-0.1016,-0.1114,-0.1251,-0.0919,-0.0997,-0.0821,-0.0997,-0.0176,-0.1251,-0.1075,-0.1075,-0.1036,-0.1036,-0.1055,-0.1036,-0.1055,-0.1055,-0.1055,-0.1075,-0.1055,-0.1055,-0.1036,-0.1055,-0.1075,-0.1075,-0.1075,-0.1055,-0.1055,-0.1055,-0.1075,-0.1075,-0.1075,-0.1055,-0.1075,-0.1055,-0.1055,-0.1075,-0.1075,-0.1016,-0.1075,-0.0958,-0.1055,-0.1075,-0.1075,-0.0977,-0.1075,-0.1016,-0.1036,-0.1016,-0.1016,-0.1055,-0.1094,-0.1016,-0.1094,-0.0997,-0.1075,-0.1016,-0.1055,-0.1036,-0.1055)
dd_ones=rep(0,3)
for(i in c(1:5)){
  dd = unlist(ones[i,])
  for(k in c(1:(length(dd)-length(pat1)))){
    distance=dist(rbind(pat1,dd[k:(k+length(pat1))]))
    print(paste(i,k,distance))
    dd_ones=rbind(dd_ones,c(i,k,distance))
  }
}
dd_ones=dd_ones[-1,]
which(dd_ones[,3] == min(dd_ones[,3]))
dd_ones[38,]
#
plot(unlist(ones[1,]),type="l")
lines(y=pat1,x=c(39:(38+length(pat1))),lwd=8,col="red")
#
series=unlist(ones[1,])
df3=data.frame(time=c(1:length(series)),value=series)
df3_red<-df3;df3_red$value<-rep(NA,length(series));df3_red[38:(37+length(pat1)),]$value<-pat1
#
p3 <- ggplot(df3, aes(time, value)) + geom_line(color=cbPalette[3], lwd=1) + theme_bw() + 
  theme(axis.line=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(), plot.background=element_blank(),
        legend.position="bottom", plot.title = element_text(size = 20),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width=unit(2, "cm")) +
  ggtitle("FRP, the best pattern for Class #1")
p3=p3 + geom_line(aes(df3_red$time,df3_red$value),color=cbPalette[7],size=2)
p3
#
#
#
pat2=c(-0.123,-0.1191,-0.123,-0.1191,-0.123,-0.123,-0.123,-0.123,-0.123,-0.123,-0.123,-0.123,-0.123,-0.123,-0.123,-0.123,-0.123,-0.123,-0.123,-0.123,-0.123,-0.123,-0.1211,-0.0625,-0.0586,-0.1054,-0.0996,-0.0937,-0.1054,-0.0976,-0.0742,-0.0742,-0.08,-0.082,-0.082,-0.082,-0.123,-0.1152,-0.0761,-0.0703,-0.08,-0.1093,-0.125,-0.1289,-0.125,-0.1269,-0.1211,-0.1211,-0.123,-0.1191,-0.125,-0.123,-0.1211,-0.1211,-0.125,-0.123,-0.1211,-0.123,-0.125,-0.123,-0.125,-0.123,-0.123,-0.125,-0.123,-0.123,-0.123,-0.123,-0.1211,-0.1328,-0.1347,-0.1562,-0.0996,-0.0469,-0.0019,0.0274,0.0293,0.0313,0.0293,0.0293,0.0254,0.0274,0.0254,0.0059,-0.041,-0.1094,-0.1211,-0.1328,-0.1289,-0.123,-0.1269,-0.1269,-0.1269,-0.125,-0.125,-0.1191,-0.123,-0.125,-0.1269,-0.1191,-0.1211,-0.1211,-0.1211,-0.1269,-0.1211,-0.1211,-0.1211,-0.1269,-0.125,-0.1269,-0.1269,-0.123,-0.1269,-0.125,-0.123,-0.123,-0.1484,-0.1465,-0.084,-0.082,-0.0977,-0.0957,-0.0937,-0.0879,-0.0898,-0.0879,-0.0781,-0.082,-0.0801,-0.0996,-0.1152,-0.084,-0.0664,-0.0742,-0.0977,-0.1836,-0.1152,-0.1152,-0.123,-0.123,-0.125,-0.1211,-0.123,-0.1211,-0.123,-0.123,-0.1211,-0.123,-0.123,-0.123,-0.123,-0.123,-0.123,-0.1211,-0.1211,-0.1211)
plot(pat2,type="l")
twos=dat[dat[,1]==2,-1]
dd_twos=rep(0,3)
for(i in c(1:4)){
  dd = unlist(twos[i,])
  for(k in c(1:(length(dd)-length(pat2)))){
    distance=dist(rbind(pat2,dd[k:(k+length(pat2))]))
    print(paste(i,k,distance))
    dd_twos=rbind(dd_twos,c(i,k,distance))
  }
}
dd_twos=dd_twos[-1,]
which(dd_twos[,3] == min(dd_twos[,3]))
dd_twos[103,]
#
series=unlist(twos[1,])
df4=data.frame(time=c(1:length(series)),value=series)
df4_red<-df4;df4_red$value<-rep(NA,length(series));df4_red[103:(102+length(pat2)),]$value<-pat2
#
p4 <- ggplot(df4, aes(time, value)) + geom_line(color=cbPalette[2], lwd=1) + theme_bw() + 
  theme(axis.line=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(), plot.background=element_blank(),
        legend.position="bottom", plot.title = element_text(size = 20),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width=unit(2, "cm")) +
  ggtitle("FRP, the best pattern for Class #2")
p4=p4 + geom_line(aes(df4_red$time,df4_red$value),color=cbPalette[4],size=2)
p4
#
#
print(grid.arrange(p3, p4, p1, p2, ncol=2, heights=c(2.2/5, 2.8/5)))
#
shapelet=c(-0.166000, -0.152300, -0.136700, -0.126900, -0.144500, -0.140600, -0.142500, -0.140600, -0.148400, -0.148400, -0.144500, -0.125000, -0.113200, -0.158200, -0.152300, -0.146500, -0.138600, -0.113200, -0.119100, -0.125000, -0.123000, -0.119100, -0.123000, -0.119100, -0.123000, -0.119100, -0.123000, -0.123000, -0.123000, -0.123000, -0.123000, -0.123000, -0.123000, -0.123000, -0.123000, -0.123000, -0.123000, -0.123000, -0.123000, -0.123000, -0.123000, -0.123000, -0.123000, -0.123000, -0.121100, -0.062500, -0.058600, -0.105400, -0.099600, -0.093700, -0.105400, -0.097600, -0.074200, -0.074200, -0.080000, -0.082000, -0.082000, -0.082000, -0.123000, -0.115200, -0.076100, -0.070300, -0.080000, -0.109300, -0.125000, -0.128900, -0.125000, -0.126900, -0.121100, -0.121100, -0.123000, -0.119100, -0.125000, -0.123000, -0.121100, -0.121100, -0.125000, -0.123000, -0.121100, -0.123000, -0.125000, -0.123000, -0.125000, -0.123000, -0.123000, -0.125000, -0.123000, -0.123000, -0.123000, -0.123000, -0.121100, -0.132800, -0.134700, -0.156200, -0.099600, -0.046900, -0.001900, 0.027400, 0.029300, 0.031300, 0.029300, 0.029300, 0.025400, 0.027400, 0.025400, 0.005900, -0.041000, -0.109400, -0.121100, -0.132800, -0.128900, -0.123000, -0.126900, -0.126900, -0.126900, -0.125000, -0.125000, -0.119100, -0.123000, -0.125000, -0.126900, -0.119100, -0.121100, -0.121100, -0.121100, -0.126900, -0.121100)
plot(shapelet,type="l")
dd_twos=rep(0,3)
for(i in c(1:4)){
  dd = unlist(twos[i,])
  for(k in c(1:(length(dd)-length(shapelet)))){
    distance=dist(rbind(shapelet,dd[k:(k+length(shapelet))]))
    print(paste(i,k,distance))
    dd_twos=rbind(dd_twos,c(i,k,distance))
  }
}
dd_twos=dd_twos[-1,]
which(dd_twos[,3] == min(dd_twos[,3]))
dd_twos[81,]
#
dd_ones=rep(0,3)
for(i in c(1:5)){
  dd = unlist(ones[i,])
  for(k in c(1:(length(dd)-length(shapelet)))){
    distance=dist(rbind(shapelet,dd[k:(k+length(shapelet))]))
    print(paste(i,k,distance))
    dd_ones=rbind(dd_ones,c(i,k,distance))
  }
}
dd_ones=dd_ones[-1,]
which(dd_ones[,3] == min(dd_ones[,3]))
dd_ones[704,]
#
series=unlist(twos[1,])
df5=data.frame(time=c(1:length(series)),value=series)
df5_red<-df5;df5_red$value<-rep(NA,length(series));df5_red[81:(80+length(shapelet)),]$value<-shapelet
#
p5 <- ggplot(df5, aes(time, value)) + geom_line(color=cbPalette[2], lwd=1) + theme_bw() + 
  theme(axis.line=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(), plot.background=element_blank(),
        legend.position="bottom", plot.title = element_text(size = 20),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.width=unit(2, "cm")) +
  ggtitle("Fast Shapelets, the best branching shapelet")
p5=p5 + geom_line(aes(df5_red$time,df5_red$value),color=pal2[1],size=2)
p5
#
#
CairoPDF(file = "cricket_saxvsm",
         width = 11, height = 6.5, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")
print(
grid.arrange(
  arrangeGrob(p3, p4, ncol=2), 
  arrangeGrob(p1, p2, ncol=2), 
  arrangeGrob(rectGrob(gp=gpar(col=NA)), p5, 
              rectGrob(gp=gpar(col=NA)), ncol=3, widths=c(0.4,1.2,0.4)), ncol=1
)
)
dev.off()
#print(grid.arrange(p3, p4, p1, p2, ncol=2, heights=c(2.2/5, 2.8/5)))