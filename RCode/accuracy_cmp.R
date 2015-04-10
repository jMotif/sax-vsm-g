#
require(ggplot2)
require(Cairo)
#
data=data.frame(read.table("paper/datainpaper/accuracy.csv",header=T,as.is=T))
names(data) <- c("N","Dataset","1NN.Euclidean","1NN.DTW","SAX.VSM","FastShapelet","PatternsSelection")

CairoPDF(file = "accuracy_plots",
         width = 10, height = 3.5, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special")

par(mfrow=c(1,3))
plot(x=data$"1NN.DTW",y=data$"PatternsSelection",xlim=c(0, 0.8), ylim=c(0, 0.8),
     xlab="", ylab="",pch=16,col="brown1",
     main="1NN DTW vs PatternsSelection",cex.main=1.6)
positions <- data.frame(x=c(-1,1,1),y=c(-1,1,-1))
polygon(positions$x, positions$y, col = alpha("cyan", 0.5), border=NA)
points(x=data$"1NN.DTW",y=data$"PatternsSelection",pch=16,col="brown1")
text(x=0.56, y=0.05, "Our method wins",cex=1.5)
text(x=0.2, y=0.75, "1NN DTW wins",cex=1.5)

plot(x=data$"SAX.VSM",y=data$"PatternsSelection",xlim=c(0, 0.8), ylim=c(0, 0.8),
     xlab="", ylab="",pch=16,col="brown1",
     main="SAX-VSM vs PatternsSelection",cex.main=1.6)
positions <- data.frame(x=c(-1,1,1),y=c(-1,1,-1))
polygon(positions$x, positions$y, col = alpha("cyan", 0.5), border=NA)
points(x=data$"SAX.VSM",y=data$"PatternsSelection",pch=16,col="brown1",)
text(x=0.56, y=0.05, "Our method wins",cex=1.5)
text(x=0.2, y=0.75, "SAX-VSM wins",cex=1.5)

plot(x=data$"FastShapelet",y=data$"PatternsSelection",xlim=c(0, 0.8), ylim=c(0, 0.8),
     xlab="", ylab="",pch=16,col="brown1",
     main="FastShapelets vs PatternsSelection",cex.main=1.6)
positions <- data.frame(x=c(-1,1,1),y=c(-1,1,-1))
polygon(positions$x, positions$y, col = alpha("cyan", 0.5), border=NA)
points(x=data$"FastShapelet",y=data$"PatternsSelection",pch=16,col="brown1",)
text(x=0.56, y=0.05, "Our method wins",cex=1.5)
text(x=0.25, y=0.75, "FastShapelets wins",cex=1.5)

dev.off()
