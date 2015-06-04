#
require(reshape)
require(plyr)
require(stringr)
#
require(ggplot2)
require(Cairo)
require(scales)
require(RColorBrewer)
require(grid)
require(gridExtra)
require(lattice)

#
data=read.table("../data/cbf/CBF_TRAIN",as.is=T)
ones=as.matrix((data[data$V1==1,])[,-1])
series=matrix(t(ones),nrow=1,byrow=T)
ctr=dim(ones)[1]

# single curve plot
coverage=as.numeric(read.table("../repair_coverage1.txt",as.is=T)$V1)
maxc=max(coverage)
#png("cylinder-40-6-6-NR.png", width=800, height=450)
plot(x=c(1:length(coverage)),y=coverage,type="l",col="darkorchid2",lwd=2,
     ylim=c(-2,maxc),
     xlab="Position",main="Cylinder: time series concatenation and grammar coverage")
mtext("median coverage shown, p and a {3,7}, w 0.2, 0.4, 0.6")
lines(series[1,],col="red")
abline(h=0,col="cyan3",lwd=1,lty=2)
abline(h=6.01,col="red",lwd=1,lty=2)
for(i in c(1:ctr)){
  abline(v=i*128,col="cyan2",lwd=2)
}
#
dev.off()
hist(coverage)
median(coverage)



#
coverage=as.numeric(read.table("../sequitur_coverage1.txt",as.is=T)$V1)
coverage2=as.numeric(read.table("../repair_coverage1.txt",as.is=T)$V1)
maxc=max(c(coverage, coverage2))
png("cylinder-40-6-6-NR.png", width=800, height=450)
plot(x=c(1:length(coverage)),y=coverage,type="l",col="darkorchid2",lwd=2,
     ylim=c(-2,maxc),yaxp=c(-2,maxc,maxc+2),
     xlab="Position",main="Cylinder: time series concatenation and grammar coverage")
lines(series[1,],col="red")
abline(h=0,col="cyan3",lwd=1,lty=2)
for(i in c(1:ctr)){
  abline(v=i*128,col="cyan2",lwd=2)
}
lines(coverage2,col="green",lwd=1)
legend("topright", c("Sequitur","RePair"),bg = "white",
       lty=1, col=c('darkorchid2', 'green'), bty='n', cex=.75)
#
dev.off()


png("cylinder-repair-rules.png", width=800, height=450)
ocs=c(12, 129, 267, 389, 518, 650, 652, 782, 1037, 1165)
plot(x=c(1:length(coverage2)),y=coverage2,type="l",col="green",lwd=2,
     ylim=c(-2,maxc),yaxp=c(-2,maxc,maxc+2),
     xlab="Position",main="Cylinder-Repair: time series concatenation and grammar coverage")
mtext("Blue lines mark most frequent rule subsequences")
lines(series[1,],col="red")
abline(h=0,col="cyan3",lwd=1,lty=2)
for(i in c(1:ctr)){
  abline(v=i*128,col="cyan2",lwd=2)
}
for(k in ocs){
  lines(x=c(k:(k+41)),y=series[1,k:(k+41)],col="blue",lwd=2)
}
dev.off()


plot(ones[1,],type="l")
plot(series[1:128],type="l")
