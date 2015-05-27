require(reshape)
require(ggplot2)
require(Cairo)

series1=c(2.02, 2.33, 2.99, 6.85, 9.20, 8.80, 7.50, 6.00, 5.85, 3.85, 4.85, 3.85, 2.22, 1.45, 1.34)
series2=c(0.50, 1.29, 2.58, 3.83, 3.25, 4.25, 3.83, 5.63, 6.44, 6.25, 8.75, 8.83, 3.25, 0.75, 0.72)

df=data.frame(x=c(0:14),series1,series2)
dd=melt(df, id.var=c("x"))
p0=ggplot(dd, aes(x,value,color=variable))+theme_bw()+geom_line()+geom_point()+
  ggtitle("Raw data")
p0

png(filename = "paa_raw.png",
    width = 600, height = 400, units = "px", pointsize = 12,
    bg = "white",  type ="cairo-png")
print(p0)
dev.off()

# PAA transform function
#
#
paa <- function(ts, paa_size){
  len = length(ts)
  if (len == paa_size) {
    ts
  }
  else {
    if (len %% paa_size == 0) {
      colMeans(matrix(ts, nrow=len %/% paa_size, byrow=F))
    }
    else {
      res = rep.int(0, paa_size)
      for (i in c(0:(len * paa_size - 1))) {
        idx = i %/% len + 1# the spot
        pos = i %/% paa_size + 1 # the col spot
        res[idx] = res[idx] + ts[pos]
      }
      for (i in c(1:paa_size)) {
        res[i] = res[i] / len
      }
      res
    }
  }
}

df=data.frame(x=c(0:14),raw=series1)
dd=melt(df, id.var=c("x"))
p1=ggplot(dd, aes(x,value,color=variable))+theme_bw()+geom_line()+geom_point()+
  ggtitle("Time series 1, PAA 7")
p1

paa_size=5
s1_paa = paa(series1,paa_size)
slength=14/paa_size
segments=data.frame(x=seq(0,slength*(paa_size-1),b=slength), 
  xend=seq(slength,slength*(paa_size),b=slength), 
  y=s1_paa, yend=s1_paa, variable=rep("PAA 5",paa_size))
p2=p1+geom_segment(data=segments, aes(x=x,y=y,xend=xend,yend=yend), size=2)

png(filename = "paa_5_series1.png",
    width = 600, height = 400, units = "px", pointsize = 12,
    bg = "white",  type ="cairo-png")
print(p2)
dev.off()

paa_size=7
s1_paa = paa(series1,paa_size)
slength=14/paa_size
segments=data.frame(x=seq(0,slength*(paa_size-1),b=slength), 
                    xend=seq(slength,slength*(paa_size),b=slength), 
                    y=s1_paa, yend=s1_paa, variable=rep("PAA 7",paa_size))
p3=p1+geom_segment(data=segments, aes(x=x,y=y,xend=xend,yend=yend), size=2)
p3

png(filename = "paa_7_series1.png",
    width = 600, height = 400, units = "px", pointsize = 12,
    bg = "white",  type ="cairo-png")
print(p3)
dev.off()
